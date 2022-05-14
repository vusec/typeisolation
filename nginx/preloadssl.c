#include <stdio.h>
#include <stdlib.h>

extern void *tc_typed_malloc(size_t sz, size_t tag);
extern void *tc_typed_realloc(void *ptr, size_t sz, size_t tag);

extern int CRYPTO_set_mem_functions(
        void *(*m)(size_t, const char *, int),
        void *(*r)(void *, size_t, const char *, int),
        void (*f)(void *, const char *, int));

size_t myhash(const char *fname, int line) {
	return line | ((size_t)(fname) << 16);
}

void *mymalloc(size_t sz, const char *fname, int line) {
//	printf("malloc %d %s %d %llx\n", sz, fname, line, myhash(fname, line));
	return tc_typed_malloc(sz, myhash(fname, line));
}

void *myrealloc(void *ptr, size_t sz, const char *fname, int line) {
//	printf("realloc %d %s %d %llx\n", sz, fname, line, myhash(fname, line));
    return tc_typed_realloc(ptr, sz, myhash(fname, line));
}

void myfree(void *ptr, const char *fname, int line) {
//	printf("free %s %d\n", fname, line);
    free(ptr);
}

static __attribute__((constructor)) void override_mallocs() {
    printf("overriding mallocs\n");
    CRYPTO_set_mem_functions(mymalloc, myrealloc, myfree);
}
