#include "cpp.h"
#include "sanitizer_internal_defs.h"

INLINE bool IsPowerOfTwo(uptr x) {
  return (x & (x - 1)) == 0;
}

INLINE uptr RoundUpTo(uptr size, uptr boundary) {
  CHECK(IsPowerOfTwo(boundary));
  return (size + boundary - 1) & ~(boundary - 1);
}

// Memory management
void *MmapOrDie(uptr size, const char *mem_type);
void UnmapOrDie(void *addr, uptr size);
// Disallow access to a memory range.  Use MmapNoAccess to allocate an
// unaccessible memory.
bool MprotectNoAccess(uptr addr, uptr size);

