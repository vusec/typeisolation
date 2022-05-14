#include "sanitizer_internal_defs.h"

void *MmapOrDie(uptr size, const char *mem_type);
void UnmapOrDie(void *addr, uptr size);
bool MprotectNoAccess(uptr addr, uptr size);

