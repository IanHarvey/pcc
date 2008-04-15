/*
 * Just compatibility function prototypes.
 * Public domain.
 */

#ifndef COMPAT_H
#define COMPAT_H

#include <string.h>

#ifndef HAVE_STRLCPY
size_t strlcpy(char *dst, const char *src, size_t siz);
#endif

#ifndef HAVE_STRLCAT
size_t strlcat(char *dst, const char *src, size_t siz);
#endif

#ifndef HAVE_GETOPT
extern char *optarg;
extern int optind;
int getopt(int, char **, char *);
#endif

#ifndef HAVE_BASENAME
char *basename(char *);
#endif

#ifndef HAVE_MKSTEMP
int mkstemp(char *);
#endif

#ifndef HAVE_FFS
int ffs(int);
#endif

#endif
