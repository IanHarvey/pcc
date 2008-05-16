#ifndef LIBDIR
#define LIBDIR "/usr/lib"
#endif

/* common cpp predefines */
#define	CPPADD	{ "-DWIN32", NULL }

/* host-dependent */
#ifdef WIN32
#define LIBCLIBS { "/subsystem:console", "msvcrt.lib", NULL }
#else
/* requires w32api-3.2.tar.gz and mingw-runtime-3.14.tar.gz */
#define LIBCLIBS { LIBDIR "libmsvcrt.a", NULL }
#endif

#define CPPMDADD { "-D__i386__", NULL }
