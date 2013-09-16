#include "lexical_cmp.h"
#include <string.h>

#define MIN(x, y)  ((x) < (y) ? (x) : (y))

int sp_compare_lexicographically(
    char *a, size_t asz, char *b, size_t bsz, void *arg)
{
    register int rc = memcmp(a, b, MIN(asz, bsz));
    if(0 == rc) {
        if(asz == bsz) return 0;
        return asz > bsz ? 1 : -1;
    }
    return (rc > 0 ? 1 : -1);
}
