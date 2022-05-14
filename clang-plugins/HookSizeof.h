#include <stddef.h>

#define sizeof(x) ({typeof(x) dummy; __sizeofwrapper(&dummy, sizeof(x));})

extern size_t __attribute__((noinline)) __sizeofwrapper(
  void *dummy, size_t size);
