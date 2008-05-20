#ifndef LIBDIR
#define LIBDIR "/usr/lib/"
#endif

/*
 * Currently only supports console applications.
 */

#define	CPPADD { "-DWIN32", NULL }

#ifdef WIN32
#define LIBCLIBS { "/subsystem:console", "msvcrt.lib", NULL }
#else
/* requires w32api-3.2.tar.gz and mingw-runtime-3.14.tar.gz */
#define CRT0FILE LIBDIR "crt2.o"
#define CRT0FILE_PROFILE LIBDIR "gcrt2.o"
#define LIBCLIBS { "--enable-stdcall-fixup", "-lmsvcrt", "-lmingw32", "-lkernel32", "-luser32", "-lpcc", NULL }
#endif

#define CPPMDADD { "-D__i386__", NULL }
