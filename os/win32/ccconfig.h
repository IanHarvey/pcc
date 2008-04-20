#ifndef LIBDIR
#define LIBDIR "/usr/lib"
#endif

/* common cpp predefines */
#define	CPPADD	{ "-DWIN32", NULL }

/* host-dependent */
#ifdef WIN32
#define LIBCLIBS { "/subsystem:console", "msvcrt.lib", NULL }
#else
#define LIBCLIBS { LIBDIR "msvcrt.lib", NULL }
#endif

#define CPPMDADD { "-D__i386__", NULL }
