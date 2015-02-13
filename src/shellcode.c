#include "shellcode.h"
#include <inttypes.h>
#include <sys/mman.h>

void shellcode(void* buf, const size_t len) {
    const int pagesize = getpagesize();
    void* page = (void*) ((uintptr_t) buf & ~(pagesize - 1));
    mprotect(page, (len / pagesize + 1) * pagesize, PROT_EXEC);

    ((void (*)()) buf)();
}
