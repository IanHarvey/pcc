#ifndef LIBDIR
#define LIBDIR "/usr/lib"
#endif

/* common cpp predefines */
#define	CPPADD	{ "-DWIN32", NULL }

/* host-dependent */
#ifdef WIN32
#define CRT0FILE LIBDIR ""
#define CRT0FILE_PROFILE LIBDIR ""
#define STARTFILES { NULL }
#define	ENDFILES { NULL }
#define LIBCLIBS { "/subsystem:console", "libc.lib", NULL }
#else
#define CRT0FILE ""
#define CRT0FILE_PROFILE ""
#define STARTFILES { NULL }
#define	ENDFILES { NULL }
#define LIBCLIBS { LIBDIR "/libc.lib", NULL }
#endif

/* shared libraries linker files */
#ifdef WIN32
#define STARTFILES_S { NULL }
#define	ENDFILES_S { NULL }
#define STARTLABEL ""
#else
#define STARTFILES_S { NULL }
#define	ENDFILES_S { NULL }
#define STARTLABEL "_main"
#endif

/* host-independent */
#ifdef WIN32
#define	DYNLINKER { NULL }
#else
#define	DYNLINKER { NULL }
#endif

#ifdef WIN32
#define CPPMDADD { NULL }
#else
#define CPPMDADD { NULL }
#endif
