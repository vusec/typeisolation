//===-- safestack.cc ------------------------------------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements the runtime support for the safe stack protection
// mechanism. The runtime manages allocation/deallocation of the unsafe stack
// for the main thread, as well as all pthreads that are created/destroyed
// during program execution.
//
//===----------------------------------------------------------------------===//

// MUSL_HORROR: We have to override the interceptor for pthread_create when statically linking.
// #define MUSL_HORROR

#include <limits.h>
#include <pthread.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/resource.h>
#include <sys/types.h>
#include <sys/user.h>

#include "sanitizer_common.h"
#include "interception.h"
#include "helper.h"

// TODO: The runtime library does not currently protect the safe stack beyond
// relying on the system-enforced ASLR. The protection of the (safe) stack can
// be provided by three alternative features:
//
// 1) Protection via hardware segmentation on x86-32 and some x86-64
// architectures: the (safe) stack segment (implicitly accessed via the %ss
// segment register) can be separated from the data segment (implicitly
// accessed via the %ds segment register). Dereferencing a pointer to the safe
// segment would result in a segmentation fault.
//
// 2) Protection via software fault isolation: memory writes that are not meant
// to access the safe stack can be prevented from doing so through runtime
// instrumentation. One way to do it is to allocate the safe stack(s) in the
// upper half of the userspace and bitmask the corresponding upper bit of the
// memory addresses of memory writes that are not meant to access the safe
// stack.
//
// 3) Protection via information hiding on 64 bit architectures: the location
// of the safe stack(s) can be randomized through secure mechanisms, and the
// leakage of the stack pointer can be prevented. Currently, libc can leak the
// stack pointer in several ways (e.g. in longjmp, signal handling, user-level
// context switching related functions, etc.). These can be fixed in libc and
// in other low-level libraries, by either eliminating the escaping/dumping of
// the stack pointer (i.e., %rsp) when that's possible, or by using
// encryption/PTR_MANGLE (XOR-ing the dumped stack pointer with another secret
// we control and protect better, as is already done for setjmp in glibc.)
// Furthermore, a static machine code level verifier can be ran after code
// generation to make sure that the stack pointer is never written to memory,
// or if it is, its written on the safe stack.
//
// Finally, while the Unsafe Stack pointer is currently stored in a thread
// local variable, with libc support it could be stored in the TCB (thread
// control block) as well, eliminating another level of indirection and making
// such accesses faster. Alternatively, dedicating a separate register for
// storing it would also be possible.

/// Minimum stack alignment for the unsafe stack.
const unsigned kStackAlign = 16;

/// Default size of the unsafe stack. This value is only used if the stack
/// size rlimit is set to infinity.
const unsigned kDefaultUnsafeStackSize = 0x2800000;
const unsigned kMaxUnsafeStackSize = 0x10000000;
const unsigned kMaxUnsafeGuardSize = 0x10000000;

const unsigned long long kArenaSize = 0x100000000ull;

// TODO: To make accessing the unsafe stack pointer faster, we plan to
// eventually store it directly in the thread control block data structure on
// platforms where this structure is pointed to by %fs or %gs. This is exactly
// the same mechanism as currently being used by the traditional stack
// protector pass to store the stack guard (see getStackCookieLocation()
// function above). Doing so requires changing the tcbhead_t struct in glibc
// on Linux and tcb struct in libc on FreeBSD.
//
// For now, store it in a thread-local variable.
extern __attribute__((visibility("default")))
  __thread void *__typesafestack_unsafe_stack_ptrs[0];
extern __attribute__((visibility("default")))
  size_t __typesafestack_unsafe_stack_ptr_count;

// Per-thread unsafe stack information. It's not frequently accessed, so there
// it can be kept out of the tcb in normal thread-local variables.
static __thread void *unsafe_stack_start = nullptr;
static __thread size_t unsafe_stack_size = 0;
static __thread size_t unsafe_stack_guard = 0;

static inline void *unsafe_stack_alloc(size_t size, size_t guard,
  size_t count) {
  size_t i;
  size_t allocsize = size + guard;
  size_t totalsize = (size + guard) * count;
  CHECK_GE((long) size, 0);
  CHECK_GE((long) guard, 0);
  CHECK_GE((long) count, 0);
  CHECK_GE(allocsize, size);
  CHECK_GE(totalsize, allocsize);
  /* arena start */
  // FIXME this is super inefficient because we allocate loads of memory we then drop with mprotect....
  // FIXME we should cooperate with tcmalloc
  if (size == kArenaSize) {
	  // for alignment
	  totalsize += kArenaSize;
  }
  /* arena end */
  void *addr = MmapOrDie(totalsize, "unsafe_stack_alloc");
  /* arena start */
  if (size == kArenaSize) {
	// align stack region start to 4GB boundary
  	addr = (void *)(((unsigned long long)addr  + kArenaSize - 1) & ~(kArenaSize - 1));
  }
  /* arena end */
  for (i = 0; i < count; i++) {
    MprotectNoAccess((uptr)addr + allocsize*i, (uptr)guard);
  }
  return (char *)addr + guard;
}

static inline void unsafe_stack_setup(void *start, size_t size, size_t guard,
  void **stack_ptrs_start, size_t count) {
  size_t i;
  size_t allocsize = size + guard;
  char *currstack = (char *) start;

  for (i = 0; i < count; i++) {
    CHECK_GE(currstack + size, currstack);
    CHECK_GE(currstack + guard, currstack);
    void *stack_ptr = currstack + size;
    CHECK_EQ((((size_t)stack_ptr) & (kStackAlign - 1)), 0);

    if (!stack_ptrs_start[i]) {
      stack_ptrs_start[i] = stack_ptr - kStackAlign; /* typeisolation will cause problems if we start the stack in a guard zone */
    }

    currstack += allocsize;
  }
  unsafe_stack_start = start;
  unsafe_stack_size = size;
  unsafe_stack_guard = guard;
}

static void unsafe_stack_free() {
  if (unsafe_stack_start) {
    UnmapOrDie((char *)unsafe_stack_start - unsafe_stack_guard,
               unsafe_stack_size + unsafe_stack_guard);
  }
  unsafe_stack_start = nullptr;
}

/// Thread data for the cleanup handler
static pthread_key_t thread_cleanup_key;

/// Safe stack per-thread information passed to the thread_start function
struct tinfo {
  void *(*start_routine)(void *);
  void *start_routine_arg;

  void *unsafe_stack_start;
  size_t unsafe_stack_size;
  size_t unsafe_stack_guard;
};

/// Wrap the thread function in order to deallocate the unsafe stack when the
/// thread terminates by returning from its main function.
static void *thread_start(void *arg) {
  struct tinfo *tinfo = (struct tinfo *)arg;

  void *(*start_routine)(void *) = tinfo->start_routine;
  void *start_routine_arg = tinfo->start_routine_arg;

  // Setup the unsafe stack; this will destroy tinfo content
  unsafe_stack_setup(tinfo->unsafe_stack_start, tinfo->unsafe_stack_size,
                     tinfo->unsafe_stack_guard,
		     __typesafestack_unsafe_stack_ptrs,
		     __typesafestack_unsafe_stack_ptr_count);

  // Make sure out thread-specific destructor will be called
  // FIXME: we can do this only any other specific key is set by
  // intercepting the pthread_setspecific function itself
  pthread_setspecific(thread_cleanup_key, (void *)1);

  return start_routine(start_routine_arg);
}

/// Thread-specific data destructor
static void thread_cleanup_handler(void *_iter) {
  // We want to free the unsafe stack only after all other destructors
  // have already run. We force this function to be called multiple times.
  // User destructors that might run more then PTHREAD_DESTRUCTOR_ITERATIONS-1
  // times might still end up executing after the unsafe stack is deallocated.
  size_t iter = (size_t)_iter;
  if (iter < PTHREAD_DESTRUCTOR_ITERATIONS) {
    pthread_setspecific(thread_cleanup_key, (void *)(iter + 1));
  } else {
    // This is the last iteration
    unsafe_stack_free();
  }
}

#ifdef MUSL_HORROR
int __pthread_create(pthread_t *restrict res, const pthread_attr_t *restrict attrp, void *(*entry)(void *), void *restrict arg);
#endif

/// Intercept thread creation operation to allocate and setup the unsafe stack
INTERCEPTOR(int, pthread_create, pthread_t *thread,
            const pthread_attr_t *attr,
            void *(*start_routine)(void*), void *arg) {

  size_t size = 0;
  size_t guard = 0;

  if (attr) {
    pthread_attr_getstacksize(attr, &size);
    pthread_attr_getguardsize(attr, &guard);
  } else {
    // get pthread default stack size
    pthread_attr_t tmpattr;
    pthread_attr_init(&tmpattr);
    pthread_attr_getstacksize(&tmpattr, &size);
    pthread_attr_getguardsize(&tmpattr, &guard);
    pthread_attr_destroy(&tmpattr);
  }

  if (size > kMaxUnsafeStackSize) size = kMaxUnsafeStackSize;
  if (guard > kMaxUnsafeGuardSize) size = kMaxUnsafeGuardSize;

  CHECK_NE(size, 0);
  CHECK_EQ((size & (kStackAlign - 1)), 0);
  CHECK_EQ((guard & (PAGE_SIZE - 1)), 0);

  /* arena start */
  // FIXME: we should re-use the stack arenas from the main thread
  /* arena end */
  void *addr = unsafe_stack_alloc(size, guard, __typesafestack_unsafe_stack_ptr_count);
  struct tinfo *tinfo =
      (struct tinfo *)(((char *)addr) + size - sizeof(struct tinfo));
  tinfo->start_routine = start_routine;
  tinfo->start_routine_arg = arg;
  tinfo->unsafe_stack_start = addr;
  tinfo->unsafe_stack_size = size;
  tinfo->unsafe_stack_guard = guard;

#ifdef MUSL_HORROR
  return __pthread_create(thread, attr, thread_start, tinfo);
#else
  return REAL(pthread_create)(thread, attr, thread_start, tinfo);
#endif
}

static void unsafe_stack_alloc_and_setup(void **stack_ptrs_start,
  size_t count) {
  /* arena start */
  // Determine the stack size for the main thread.
  /*size_t size = kDefaultUnsafeStackSize;
  size_t guard = 4096;

  struct rlimit limit;
  if (getrlimit(RLIMIT_STACK, &limit) == 0 && limit.rlim_cur != RLIM_INFINITY)
    size = limit.rlim_cur;

  if (size > kMaxUnsafeStackSize) size = kMaxUnsafeStackSize;*/

  // 4GB stacks, 4GB*8=32GB guard zones
  size_t size = kArenaSize;
  size_t guard = kArenaSize*8;
  /* arena end */
 
  // Allocate unsafe stack for main thread
  void *addr = unsafe_stack_alloc(size, guard, count);

  unsafe_stack_setup(addr, size, guard, stack_ptrs_start, count);
}

#if !SANITIZER_CAN_USE_PREINIT_ARRAY
// On ELF platforms, the constructor is invoked using .preinit_array (see below)
__attribute__((constructor(0)))
#endif
__attribute__((visibility("default")))
void __typesafestack_init() {
  unsafe_stack_alloc_and_setup(__typesafestack_unsafe_stack_ptrs,
    __typesafestack_unsafe_stack_ptr_count);

  // Initialize pthread interceptors for thread allocation
  INTERCEPT_FUNCTION(pthread_create);

  // Setup the cleanup handler
  pthread_key_create(&thread_cleanup_key, thread_cleanup_handler);
}

__attribute__((visibility("default")))
__attribute__((used))
void __typesafestack_debug(const char *func, const char *stackname,
  const void **stackptr, int callType) {
  fprintf(stderr, "typesafestack: %s:%p: %s=%p, callType=%d\n",
    func, __builtin_return_address(0), stackname, *stackptr, callType);
  fflush(stderr);
}

#if SANITIZER_CAN_USE_PREINIT_ARRAY
// On ELF platforms, run safestack initialization before any other constructors.
// On other platforms we use the constructor attribute to arrange to run our
// initialization early.
__attribute__((section(".preinit_array"),
               used, init_priority(101))) void (*__typesafestack_preinit)(void) = __typesafestack_init;
#endif
