#ifndef LIBDIR
#define LIBDIR "/usr/lib/"
#endif

#ifdef WIN32
#define	CPPADD { "-DWIN32", NULL }
#define LIBCLIBS { "/subsystem:console", "msvcrt.lib", NULL }
#else
/* requires w32api-3.2.tar.gz and mingw-runtime-3.14.tar.gz */
#define CRT0FILE LIBDIR "crt2.o"
#define CRT0FILE_PROFILE LIBDIR "gcrt2.o"
#define	CPPADD { "-DWIN32", "-D__cdecl=", "-D__stdcall", NULL }
#define LIBCLIBS { "-lmsvcrt", "-lmingw32", "-lkernel32", NULL }
#endif

#define CPPMDADD { "-D__i386__", NULL }
