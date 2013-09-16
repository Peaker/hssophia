#ifndef __lexical_cmp_h_
#define __lexical_cmp_h_

#include <sys/types.h>          /* size_t */

int sp_compare_lexicographically(
    char *a, size_t asz, char *b, size_t bsz, void *arg);

#endif
