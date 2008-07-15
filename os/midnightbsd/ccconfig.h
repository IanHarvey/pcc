/* $Id$ */
/*-
 * Copyright (c) 2007, 2008
 *	Thorsten Glaser <tg@mirbsd.de>
 *
 * Provided that these terms and disclaimer and all copyright notices
 * are retained or reproduced in an accompanying document, permission
 * is granted to deal in this work without restriction, including un-
 * limited rights to use, publicly perform, distribute, sell, modify,
 * merge, give away, or sublicence.
 *
 * This work is provided "AS IS" and WITHOUT WARRANTY of any kind, to
 * the utmost extent permitted by applicable law, neither express nor
 * implied; without malicious intent or gross negligence. In no event
 * may a licensor, author or contributor be held liable for indirect,
 * direct, other damage, loss, or other issues arising in any way out
 * of dealing in the work, even if advised of the possibility of such
 * damage or existence of a defect, except proven that it results out
 * of said person's immediate fault when using the work as intended.
 */

/**
 * Configuration for pcc on a MidnightBSD (amd64, i386 or sparc64) target
 */

#ifndef LIBDIR
#define LIBDIR "/usr/lib/"
#endif

/* mi part */

#define CPPADD		{			\
	"-D__MidnightBSD__",			\
	"-D__FreeBSD__",			\
	"-D__unix__",				\
	"-D__unix",				\
	"-D__ELF__",				\
	"-D_LONGLONG",				\
	NULL					\
}
#define DYNLINKER	{			\
	"-dynamic-linker",			\
	"/libexec/ld-elf.so.1",			\
	NULL					\
}
#define STARTFILES	{			\
	LIBDIR "crti.o",			\
	LIBDIR "crtbegin.o",			\
	NULL					\
}
#define ENDFILES	{			\
	LIBDIR "crtend.o",			\
	LIBDIR "crtn.o",			\
	NULL					\
}
#define LIBCLIBS	{			\
	"-lc",					\
	"-lpcc",				\
	NULL					\
}
#define LIBCLIBS_PROFILE	{		\
	"-lc_p",				\
	"-lpcc",				\
	NULL					\
}

#define CRT0FILE		"/usr/lib/crt1.o"
#define CRT0FILE_PROFILE	"/usr/lib/gcrt1.o"
#define STARTLABEL		"_start"
#define STABS

/* md part */

#if defined(mach_i386)
#define CPPMDADD	{			\
	"-D__i386__",				\
	"-D__i386",				\
	"-Di386",				\
	NULL,					\
}
#elif defined(mach_sparc64)
#error untested
#define CPPMDADD	{			\
	"-D__sparc64__",			\
	NULL,					\
}
#elif defined(mach_amd64)
#error pcc does not support amd64 yet
#else
#error this architecture is not supported by MidnightBSD
#endif
