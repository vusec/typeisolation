#include <assert.h>

#define INLINE inline

#if defined(_WIN64)
// 64-bit Windows uses LLP64 data model.
typedef unsigned long long uptr;  // NOLINT
typedef signed   long long sptr;  // NOLINT
#else
typedef unsigned long uptr;  // NOLINT
typedef signed   long sptr;  // NOLINT
#endif  // defined(_WIN64)
#if defined(__x86_64__)
// Since x32 uses ILP32 data model in 64-bit hardware mode, we must use
// 64-bit pointer to unwind stack frame.
typedef unsigned long long uhwptr;  // NOLINT
#else
typedef uptr uhwptr;   // NOLINT
#endif
typedef unsigned char u8;
typedef unsigned short u16;  // NOLINT
typedef unsigned int u32;
typedef unsigned long long u64;  // NOLINT
typedef signed   char s8;
typedef signed   short s16;  // NOLINT
typedef signed   int s32;
typedef signed   long long s64;  // NOLINT
#if SANITIZER_WINDOWS
// On Windows, files are HANDLE, which is a synonim of void*.
// Use void* to avoid including <windows.h> everywhere.
typedef void* fd_t;
typedef unsigned error_t;
#else
typedef int fd_t;
typedef int error_t;
#endif

// WARNING: OFF_T may be different from OS type off_t, depending on the value of
// _FILE_OFFSET_BITS. This definition of OFF_T matches the ABI of system calls
// like pread and mmap, as opposed to pread64 and mmap64.
// FreeBSD, Mac and Linux/x86-64 are special.
#if SANITIZER_FREEBSD || SANITIZER_MAC || \
  (SANITIZER_LINUX && defined(__x86_64__))
typedef u64 OFF_T;
#else
typedef uptr OFF_T;
#endif
typedef u64  OFF64_T;

#if (SANITIZER_WORDSIZE == 64) || SANITIZER_MAC
typedef uptr operator_new_size_type;
#else
typedef u32 operator_new_size_type;
#endif

#define CHECK_IMPL(c1, op, c2) assert((c1) op (c2))

#define CHECK(a)       CHECK_IMPL((a), !=, 0)
#define CHECK_EQ(a, b) CHECK_IMPL((a), ==, (b))
#define CHECK_NE(a, b) CHECK_IMPL((a), !=, (b))
#define CHECK_LT(a, b) CHECK_IMPL((a), <,  (b))
#define CHECK_LE(a, b) CHECK_IMPL((a), <=, (b))
#define CHECK_GT(a, b) CHECK_IMPL((a), >,  (b))
#define CHECK_GE(a, b) CHECK_IMPL((a), >=, (b))

