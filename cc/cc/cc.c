/*	$Id$	*/

/*-
 * Copyright (c) 2011 Joerg Sonnenberger <joerg@NetBSD.org>.
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

/*
 * Copyright(C) Caldera International Inc. 2001-2002. All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * Redistributions of source code and documentation must retain the above
 * copyright notice, this list of conditions and the following disclaimer.
 * Redistributions in binary form must reproduce the above copyright
 * notice, this list of conditionsand the following disclaimer in the
 * documentation and/or other materials provided with the distribution.
 * All advertising materials mentioning features or use of this software
 * must display the following acknowledgement:
 * 	This product includes software developed or owned by Caldera
 *	International, Inc.
 * Neither the name of Caldera International, Inc. nor the names of other
 * contributors may be used to endorse or promote products derived from
 * this software without specific prior written permission.
 *
 * USE OF THE SOFTWARE PROVIDED FOR UNDER THIS LICENSE BY CALDERA
 * INTERNATIONAL, INC. AND CONTRIBUTORS ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED.  IN NO EVENT SHALL CALDERA INTERNATIONAL, INC. BE LIABLE
 * FOR ANY DIRECT, INDIRECT INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OFLIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING
 * IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
 * POSSIBILITY OF SUCH DAMAGE.
 */

/*
 * Front-end to the C compiler.
 *
 * Brief description of its syntax:
 * - Files that end with .c are passed via cpp->ccom->as->ld
 * - Files that end with .i are passed via ccom->as->ld
 * - Files that end with .S are passed via cpp->as->ld
 * - Files that end with .s are passed via as->ld
 * - Files that end with .o are passed directly to ld
 * - Multiple files may be given on the command line.
 * - Unrecognized options are all sent directly to ld.
 * -c or -S cannot be combined with -o if multiple files are given.
 *
 * This file should be rewritten readable.
 */
#include "config.h"

#include <sys/types.h>
#ifdef HAVE_SYS_WAIT_H
#include <sys/wait.h>
#endif

#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif
#include <signal.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include <assert.h>

#ifdef os_win32
#include <windows.h>
#include <process.h>
#include <io.h>
#define F_OK	0x00
#define R_OK	0x04
#define W_OK	0x02
#define X_OK	R_OK
#endif

#include "compat.h"

#include "macdefs.h"

#include "xalloc.h"
#include "strlist.h"

#include "ccconfig.h"
/* C command */

#define	MKS(x) _MKS(x)
#define _MKS(x) #x

/* default program names in pcc */
/* May be overridden if cross-compiler is generated */
#ifndef CPPROGNAME
#define	CPPROGNAME	"cpp"	/* cc used as cpp */
#endif
#ifndef PREPROCESSOR
#define	PREPROCESSOR	"cpp"	/* "real" preprocessor name */
#endif
#ifndef COMPILER
#define COMPILER	"ccom"
#endif
#ifndef CXXCOMPILER
#define CXXCOMPILER	"cxxcom"
#endif
#ifndef ASSEMBLER
#define ASSEMBLER	"as"
#endif
#ifndef LINKER
#define LINKER		"ld"
#endif
char	*passp = PREPROCESSOR;
char	*pass0 = COMPILER;
char	*passxx0 = CXXCOMPILER;
char	*as = ASSEMBLER;
char	*ld = LINKER;
char	*sysroot = "", *isysroot;


/* crt files using pcc default names */
#ifndef CRTBEGIN_S
#define	CRTBEGIN_S	"crtbeginS.o"
#endif
#ifndef CRTEND_S
#define	CRTEND_S	"crtendS.o"
#endif
#ifndef CRTBEGIN_T
#define	CRTBEGIN_T	"crtbeginT.o"
#endif
#ifndef CRTEND_T
#define	CRTEND_T	"crtendT.o"
#endif
#ifndef CRTBEGIN
#define	CRTBEGIN	"crtbegin.o"
#endif
#ifndef CRTEND
#define	CRTEND		"crtend.o"
#endif
#ifndef CRTI
#define	CRTI		"crti.o"
#endif
#ifndef CRTN
#define	CRTN		"crtn.o"
#endif
#ifndef CRT0
#define	CRT0		"crt0.o"
#endif
#ifndef GCRT0
#define	GCRT0		"gcrt0.o"
#endif

/* preprocessor stuff */
#ifndef STDINC
#define	STDINC	  	"/usr/include/"
#endif

char *cppadd[] = CPPADD;
char *cppmdadd[] = CPPMDADD;

/* Dynamic linker definitions, per-target */
#ifndef DYNLINKER
#define	DYNLINKER { 0 }
#endif

/* Default libraries and search paths */
#ifndef PCCLIBDIR	/* set by autoconf */
#define PCCLIBDIR	NULL
#endif
#ifndef DEFLIBDIRS	/* default library search paths */
#define DEFLIBDIRS	{ "/usr/lib/", 0 }
#endif
#ifndef DEFLIBS		/* default libraries included */
#define	DEFLIBS		{ "-lpcc", "-lc", "-lpcc", 0 }
#endif
#ifndef DEFPROFLIBS	/* default profiling libraries */
#define	DEFPROFLIBS	{ "-lpcc", "-lc_p", "-lpcc", 0 }
#endif
#ifndef DEFCXXLIBS	/* default c++ libraries */
#define	DEFCXXLIBS	{ "-lp++", "-lpcc", "-lc", "-lpcc", 0 }
#endif
#ifndef STARTLABEL
#define STARTLABEL "__start"
#endif

char *dynlinker[] = DYNLINKER;
char *pcclibdir = PCCLIBDIR;
char *deflibdirs[] = DEFLIBDIRS;
char *deflibs[] = DEFLIBS;
char *defproflibs[] = DEFPROFLIBS;
char *defcxxlibs[] = DEFCXXLIBS;

char	*outfile;
static char **lav;
static int lac;
static char *find_file(const char *file, struct strlist *path, int mode);
static int preprocess_input(char *input, char *output, int dodep);
static int compile_input(char *input, char *output);
static int assemble_input(char *input, char *output);
static int run_linker(void);
static int strlist_exec(struct strlist *l);

char *cat(const char *, const char *);
char *setsuf(char *, char);
int cxxsuf(char *);
int getsuf(char *);
char *getsufp(char *s);
int main(int, char *[]);
void error(char *, ...);
void errorx(int, char *, ...);
int callsys(char [], char *[]);
int cunlink(char *);
void exandrm(char *);
void dexit(int);
void idexit(int);
char *gettmp(void);
void aerror(char *);
void oerror(char *);
void owarning(char *);
char *argnxt(char *, char *);
char *nxtopt(char *o);
void setup_cpp_flags(void);
void setup_ccom_flags(void);
void setup_as_flags(void);
void setup_ld_flags(void);
static void expand_sysroot(void);
#ifdef os_win32
char *win32pathsubst(char *);
char *win32commandline(char *, char *[]);
#endif
int	sspflag;
int	freestanding;
int	Sflag;
int	cflag;
int	gflag;
int	rflag;
int	vflag;
int	tflag;
int	Eflag;
int	Oflag;
int	kflag;	/* generate PIC/pic code */
#define F_PIC	1
#define F_pic	2
int	Mflag, needM;	/* dependencies only */
int	pgflag;
int	Xflag;
int	Wallflag;
int	Wflag;
int	nostartfiles, Bstatic, shared;
int	nostdinc, nostdlib;
int	pthreads;
int	xasm, xcflag, xgnu89, xgnu99;
int 	ascpp;
#ifdef CHAR_UNSIGNED
int	xuchar = 1;
#else
int	xuchar = 0;
#endif
int	cxxflag;
int	cppflag;

#ifdef mach_amd64
int amd64_i386;
#endif

#define	match(a,b)	(strcmp(a,b) == 0)

/* handle gcc warning emulations */
struct Wflags {
	char *name;
	int flags;
#define	INWALL		1
} Wflags[] = {
	{ "truncate", 0 },
	{ "strict-prototypes", 0 },
	{ "missing-prototypes", 0 },
	{ "implicit-int", INWALL },
	{ "implicit-function-declaration", INWALL },
	{ "shadow", 0 },
	{ "pointer-sign", INWALL },
	{ "sign-compare", 0 },
	{ "unknown-pragmas", INWALL },
	{ "unreachable-code", 0 },
	{ NULL, 0 },
};

#ifndef USHORT
/* copied from mip/manifest.h */
#define	USHORT		5
#define	INT		6
#define	UNSIGNED	7
#endif

/*
 * Wide char defines.
 */
#if WCHAR_TYPE == USHORT
#define	WCT "short unsigned int"
#define WCM "65535U"
#if WCHAR_SIZE != 2
#error WCHAR_TYPE vs. WCHAR_SIZE mismatch
#endif
#elif WCHAR_TYPE == INT
#define WCT "int"
#define WCM "2147483647"
#if WCHAR_SIZE != 4
#error WCHAR_TYPE vs. WCHAR_SIZE mismatch
#endif
#elif WCHAR_TYPE == UNSIGNED
#define WCT "unsigned int"
#define WCM "4294967295U"
#if WCHAR_SIZE != 4
#error WCHAR_TYPE vs. WCHAR_SIZE mismatch
#endif
#else
#error WCHAR_TYPE not defined or invalid
#endif

#ifdef GCC_COMPAT
#ifndef REGISTER_PREFIX
#define REGISTER_PREFIX ""
#endif
#ifndef USER_LABEL_PREFIX
#define USER_LABEL_PREFIX ""
#endif
#endif

#ifndef PCC_WINT_TYPE
#define PCC_WINT_TYPE "unsigned int"
#endif

#ifndef PCC_SIZE_TYPE
#define PCC_SIZE_TYPE "unsigned long"
#endif

#ifndef PCC_PTRDIFF_TYPE
#define PCC_PTRDIFF_TYPE "long int"
#endif


struct strlist preprocessor_flags;
struct strlist depflags;
struct strlist incdirs;
struct strlist user_sysincdirs;
struct strlist includes;
struct strlist sysincdirs;
struct strlist dirafterdirs;
struct strlist crtdirs;
struct strlist libdirs;
struct strlist progdirs;
struct strlist early_linker_flags;
struct strlist middle_linker_flags;
struct strlist late_linker_flags;
struct strlist inputs;
struct strlist assembler_flags;
struct strlist temp_outputs;
struct strlist compiler_flags;

int
main(int argc, char *argv[])
{
	struct Wflags *Wf;
	struct string *s;
	char *t, *u, *argp;
	int ninput, j;

	lav = argv;
	lac = argc;

	strlist_init(&crtdirs);
	strlist_init(&libdirs);
	strlist_init(&progdirs);
	strlist_init(&preprocessor_flags);
	strlist_init(&incdirs);
	strlist_init(&user_sysincdirs);
	strlist_init(&includes);
	strlist_init(&sysincdirs);
	strlist_init(&dirafterdirs);
	strlist_init(&depflags);
	strlist_init(&early_linker_flags);
	strlist_init(&middle_linker_flags);
	strlist_init(&late_linker_flags);
	strlist_init(&inputs);
	strlist_init(&assembler_flags);
	strlist_init(&temp_outputs);
	strlist_init(&compiler_flags);

	t = argv[0];
	if ((t = strrchr(argv[0], '/')))
		t++;

	if (match(t, "p++")) {
		cxxflag = 1;
	} else if (match(t, "cpp") || match(t, CPPROGNAME)) {
		Eflag = cppflag = 1;
	}

#ifdef os_win32
	/* have to prefix path early.  -B may override */
	incdir = win32pathsubst(incdir);
	altincdir = win32pathsubst(altincdir);
	libdir = win32pathsubst(libdir);
#ifdef PCCINCDIR
	pccincdir = win32pathsubst(pccincdir);
	pxxincdir = win32pathsubst(pxxincdir);
#endif
#ifdef PCCLIBDIR
	pcclibdir = win32pathsubst(pcclibdir);
#endif
	passp = win32pathsubst(passp);
	pass0 = win32pathsubst(pass0);
#ifdef STARTFILES
	for (i = 0; startfiles[i] != NULL; i++)
		startfiles[i] = win32pathsubst(startfiles[i]);
	for (i = 0; endfiles[i] != NULL; i++)
		endfiles[i] = win32pathsubst(endfiles[i]);
#endif
#ifdef STARTFILES_T
	for (i = 0; startfiles_T[i] != NULL; i++)
		startfiles_T[i] = win32pathsubst(startfiles_T[i]);
	for (i = 0; endfiles_T[i] != NULL; i++)
		endfiles_T[i] = win32pathsubst(endfiles_T[i]);
#endif
#ifdef STARTFILES_S
	for (i = 0; startfiles_S[i] != NULL; i++)
		startfiles_S[i] = win32pathsubst(startfiles_S[i]);
	for (i = 0; endfiles_S[i] != NULL; i++)
		endfiles_S[i] = win32pathsubst(endfiles_S[i]);
#endif
#endif

	while (--lac) {
		++lav;
		argp = *lav;

#ifdef EARLY_ARG_CHECK
		EARLY_ARG_CHECK;
#endif

		if (*argp != '-' || match(argp, "-")) {
			/* Check for duplicate .o files. */
			if (getsuf(argp) == 'o') {
				j = 0;
				STRLIST_FOREACH(s, &inputs)
					if (match(argp, s->value))
						j++;
				if (j)
					continue; /* skip it */
			}
			strlist_append(&inputs, argp);
			continue;
		}

		switch (argp[1]) {
		default:
			owarning(argp);
			break;

		case '-': /* double -'s */
			if (match(argp, "--version")) {
				printf("%s\n", VERSSTR);
				return 0;
			} else if (strncmp(argp, "--sysroot=", 10) == 0) {
				sysroot = argp + 10;
			} else if (strcmp(argp, "--param") == 0) {
				/* NOTHING YET */;
				(void)nxtopt(0); /* ignore arg */
			} else
				owarning(argp);
			break;

		case 'B': /* other search paths for binaries */
			t = nxtopt("-B");
			strlist_append(&crtdirs, t);
			strlist_append(&libdirs, t);
			strlist_append(&progdirs, t);
			break;

		case 'C':
			if (match(argp, "-C") || match(argp, "-CC"))
				strlist_append(&preprocessor_flags, argp);
			else
				oerror(argp);
			break;

		case 'X':
			Xflag++;
			break;
		case 'W': /* Ignore (most of) W-flags */
			if ((t = argnxt(argp, "-Wl,"))) {
				u = strtok(t, ",");
				do {
					strlist_append(&middle_linker_flags, u);
				} while ((u = strtok(NULL, ",")) != NULL);
			} else if ((t = argnxt(argp, "-Wa,"))) {
				u = strtok(t, ",");
				do {
					strlist_append(&assembler_flags, u);
				} while ((u = strtok(NULL, ",")) != NULL);
			} else if ((t = argnxt(argp, "-Wc,"))) {
				u = strtok(t, ",");
				do {
					strlist_append(&compiler_flags, u);
				} while ((u = strtok(NULL, ",")) != NULL);
			} else if ((t = argnxt(argp, "-Wp,"))) {
				u = strtok(t, ",");
				do {
					strlist_append(&preprocessor_flags, u);
				} while ((u = strtok(NULL, ",")) != NULL);
			} else if (strcmp(argp, "-Werror") == 0) {
				strlist_append(&compiler_flags, "-Werror");
			} else if (strcmp(argp, "-Wall") == 0) {
				Wallflag = 1;
			} else if (strcmp(argp, "-WW") == 0) {
				Wflag = 1;
			} else {
				/* pass through, if supported */
				t = &argp[2];
				if (strncmp(t, "no-", 3) == 0)
					t += 3;
				if (strncmp(t, "error=", 6) == 0)
					t += 6;
				for (Wf = Wflags; Wf->name; Wf++) {
					if (strcmp(t, Wf->name) == 0)
						strlist_append(&compiler_flags,
						    argp);
				}
			}
			break;

		case 'f': /* GCC compatibility flags */
			if (strcmp(argp, "-fPIC") == 0)
				kflag = F_PIC;
			else if (strcmp(argp, "-fpic") == 0)
				kflag = F_pic;
			else if (strcmp(argp, "-ffreestanding") == 0)
				freestanding = 1;
			else if (match(argp, "-fsigned-char") ||
			    match(argp, "-fno-unsigned-char"))
				xuchar = 0;
			else if (match(argp, "-fno-signed-char") ||
			    match(argp, "-funsigned-char"))
				xuchar = 1;
			else if (match(argp, "-fstack-protector") ||
			    match(argp, "-fstack-protector-all")) {
				sspflag++;
			} else if (match(argp, "-fno-stack-protector") ||
			    match(argp, "-fno-stack-protector-all")) {
				sspflag = 0;
			}
			/* silently ignore the rest */
			break;

		case 'g': /* create debug output */
			if (argp[2] == '0')
				gflag = 0;
			else
				gflag++;
			break;

		case 'D':
		case 'U':
			strlist_append(&preprocessor_flags, argp);
			if (argp[2] != 0)
				break;
			strlist_append(&preprocessor_flags, nxtopt(argp));
			break;

		case 'I': /* Add include dirs */
			strlist_append(&incdirs, nxtopt("-I"));
			break;

		case 'i':
			if (match(argp, "-isystem")) {
				strlist_append(&user_sysincdirs, nxtopt(0));
			} else if (match(argp, "-include")) {
				strlist_append(&includes, nxtopt(0));
			} else if (match(argp, "-isysroot")) {
				isysroot = nxtopt(0);
			} else if (strcmp(argp, "-idirafter") == 0) {
				strlist_append(&dirafterdirs, nxtopt(0));
			} else
				owarning(argp);
			break;

		case 'k': /* generate PIC code */
			kflag = F_pic;
			break;

		case 'l':
		case 'L':
			strlist_append(&late_linker_flags, argp);
			if (argp[2] == 0)
				strlist_append(&late_linker_flags, nxtopt(0));
			break;


		case 'm': /* target-dependent options */
#ifdef mach_amd64
			/* need to call i386 ccom for this */
			if (strcmp(argp, "-m32") == 0) {
				pass0 = LIBEXECDIR "/ccom_i386";
				amd64_i386 = 1;
				break;
			}
#endif
			strlist_append(&middle_linker_flags, argp);
			if (argp[2] == 0) {
				t = nxtopt(0);
				strlist_append(&middle_linker_flags, t);
			}
			break;

		case 'n': /* handle -n flags */
			if (strcmp(argp, "-nostdinc") == 0)
				nostdinc++;
			else if (strcmp(argp, "-nostdlib") == 0) {
				nostdlib++;
				nostartfiles++;
			} else if (strcmp(argp, "-nostartfiles") == 0)
				nostartfiles = 1;
			else if (strcmp(argp, "-nodefaultlibs") == 0)
				nostdlib++;
			else
				owarning(argp);
			break;

		case 'p':
			if (strcmp(argp, "-pg") == 0 ||
			    strcmp(argp, "-p") == 0)
				pgflag++;
			else if (strcmp(argp, "-pthread") == 0)
				pthreads++;
			else if (strcmp(argp, "-pipe") == 0)
				/* NOTHING YET */;
			else if (strcmp(argp, "-pedantic") == 0)
				/* NOTHING YET */;
			else if (strcmp(argp,
			    "-print-prog-name=ld") == 0) {
				printf("%s\n", LINKER);
				return 0;
#ifdef notdef
			/* does not exist in gcc??? */
			} else if (strcmp(argp,
			    "-print-multi-os-directory") == 0) {
				printf("%s\n", MULTIOSDIR);
				return 0;
#endif
			} else
				oerror(argp);
			break;

		case 'r':
			rflag = 1;
			break;

		case 'x':
			t = nxtopt("-x");
			if (match(t, "c"))
				xcflag = 1; /* default */
			else if (match(t, "assembler"))
				xasm = 1;
			else if (match(t, "assembler-with-cpp"))
				ascpp = 1;
			else if (match(t, "c++"))
				cxxflag++;
			else {
				strlist_append(&compiler_flags, "-x");
				strlist_append(&compiler_flags, t);
			}
			break;
		case 't':
			tflag++;
			break;
		case 'S':
			Sflag++;
			cflag++;
			break;
		case 'o':
			if (outfile)
				errorx(8, "too many -o");
			outfile = nxtopt("-o");
			break;
		case 'O':
			if (argp[2] == '\0')
				Oflag++;
			else if (argp[3] == '\0' &&
			    isdigit((unsigned char)argp[2]))
				Oflag = argp[2] - '0';
			else if (argp[3] == '\0' && argp[2] == 's')
				Oflag = 1;	/* optimize for space only */
			else
				oerror(argp);
			break;
		case 'E':
			Eflag++;
			break;
		case 'P':
			strlist_append(&preprocessor_flags, argp);
			break;

		case 'c':
			cflag++;
			break;

		case 'M':
			switch (argp[2]) {
			case '\0':
				Mflag++;
				strlist_append(&depflags, argp);
				break;
			case 'P':
				needM = 1;
				strlist_append(&depflags, "-xMP");
				break;
			case 'F':
				needM = 1;
				outfile = nxtopt("-MF");
				break;
			case 'T':
			case 'Q':
				needM = 1;
				t = cat("-xMT,", nxtopt("-MT"));
				t[3] = argp[2];
				strlist_append(&depflags, t);
				break;
			default:
				oerror(argp);
			}
			break;

		case 'd':
			owarning(argp);
			break;
		case 'v':
			printf("%s\n", VERSSTR);
			vflag++;
			break;

		case 's':
			if (strcmp(argp, "-shared") == 0) {
				shared = 1;
			} else if (strcmp(argp, "-static") == 0) {
				Bstatic = 1;
			} else if (match(argp, "-symbolic")) {
				strlist_append(&middle_linker_flags,
				    "-Bsymbolic");
			} else if (strncmp(argp, "-std", 4) == 0) {
				if (strcmp(&argp[5], "gnu99") == 0 ||
				    strcmp(&argp[5], "gnu9x") == 0)
					xgnu99 = 1;
				if (strcmp(&argp[5], "gnu89") == 0)
					xgnu89 = 1;
			} else
				owarning(argp);
			break;
		}
		continue;

	}

	/* Sanity checking */
	ninput = 0;
	STRLIST_FOREACH(s, &inputs)
		ninput++;
	if (cppflag) {
		if (ninput == 0) {
			strlist_append(&inputs, "-");
		} else if (ninput > 2 || (ninput == 2 && outfile)) {
			errorx(8, "too many files");
		}
	}
	if (ninput == 0)
		errorx(8, "no input files");
	if (outfile && (cflag || Sflag || Eflag) && ninput > 1)
		errorx(8, "-o given with -c || -E || -S and more than one file");
#if 0
	if (outfile && clist[0] && strcmp(outfile, clist[0]) == 0)
		errorx(8, "output file will be clobbered");
#endif

	if (needM && !Mflag)
		errorx(8, "to make dependencies needs -M");


	if (signal(SIGINT, SIG_IGN) != SIG_IGN)	/* interrupt */
		signal(SIGINT, idexit);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)	/* terminate */
		signal(SIGTERM, idexit);

	/* after arg parsing */
	strlist_append(&progdirs, LIBEXECDIR);
	if (pcclibdir)
		strlist_append(&crtdirs, pcclibdir);
	for (j = 0; deflibdirs[j]; j++)
		strlist_append(&crtdirs, deflibdirs[j]);

	setup_cpp_flags();
	setup_ccom_flags();
	setup_as_flags();

	if (isysroot == NULL)
		isysroot = sysroot;
	expand_sysroot();

	STRLIST_FOREACH(s, &inputs) {
		char *suffix;
		char *ifile, *ofile;

		ifile = s->value;
		if (ninput > 1 && !Eflag)
			printf("%s:\n", ifile);

		suffix = getsufp(ifile);
		if (xasm)
			suffix = "s";
		else if (ascpp)
			suffix = "S";
		else if (xcflag)
			suffix = "c";
		/*
		 * C preprocessor
		 */
		if (match(suffix, "c") || match(suffix, "S") ||
		    cxxsuf(s->value)) {
			/* find out next output file */
			if (Eflag || Mflag) {
				/* last pass */
				ofile = outfile;
			} else {
				/* to temp file */
				strlist_append(&temp_outputs, ofile = gettmp());
			}
			if (Mflag /* || MDflag */)
				if (preprocess_input(ifile, ofile, 1))
					exandrm(ofile);
			if (Mflag)
				continue;
			if (preprocess_input(ifile, ofile, 0))
				exandrm(ofile);
			if (Eflag)
				continue;
			ifile = ofile;
			suffix = match(suffix, "S") ? "s" : "i";
		}

		/*
		 * C compiler
		 */
		if (match(suffix, "i")) {
			/* find out next output file */
			if (Sflag) {
				ofile = outfile;
				if (outfile == NULL)
					ofile = setsuf(s->value, 's');
			} else
				strlist_append(&temp_outputs, ofile = gettmp());
			if (compile_input(ifile, ofile))
				exandrm(ofile);
			if (Sflag)
				continue;
			ifile = ofile;
			suffix = "s";
		}

		/*
		 * Assembler
		 */
		if (match(suffix, "s")) {
			if (cflag) {
				ofile = outfile;
				if (ofile == NULL)
					ofile = setsuf(s->value, 'o');
			} else {
				strlist_append(&temp_outputs, ofile = gettmp());
				/* strlist_append linker */
			}
			if (assemble_input(ifile, ofile))
				exandrm(ofile);
			ifile = ofile;
		}
		strlist_append(&middle_linker_flags, ifile);
	}

	if (cflag)
		dexit(0);

	/*
	 * Linker
	 */
	setup_ld_flags();
	if (run_linker())
		exandrm(0);

#ifdef notdef
	strlist_free(&crtdirs);
	strlist_free(&libdirs);
	strlist_free(&progdirs);
	strlist_free(&incdirs);
	strlist_free(&preprocessor_flags);
	strlist_free(&user_sysincdirs);
	strlist_free(&includes);
	strlist_free(&sysincdirs);
	strlist_free(&dirafterdirs);
	strlist_free(&depflags);
	strlist_free(&early_linker_flags);
	strlist_free(&middle_linker_flags);
	strlist_free(&late_linker_flags);
	strlist_free(&inputs);
	strlist_free(&assembler_flags);
	strlist_free(&temp_outputs);
	strlist_free(&compiler_flags);
#endif
	dexit(0);
	return 0;
}

/*
 * exit and cleanup after interrupt.
 */
void
idexit(int arg)
{
	dexit(100);
}

/*
 * exit and cleanup.
 */
void
dexit(int eval)
{
	struct string *s;

	if (!Xflag) {
		STRLIST_FOREACH(s, &temp_outputs)
			cunlink(s->value);
	}
	exit(eval);
}

/*
 * Called when something failed.
 */
void
exandrm(char *s)
{
	if (s && *s)
		strlist_append(&temp_outputs, s);
	dexit(1);
}

static void
ccerror(char *s, va_list ap)
{
	vfprintf(Eflag ? stderr : stdout, s, ap);
	putc('\n', Eflag? stderr : stdout);
}

/*
 * complain a bit.
 */
void
error(char *s, ...)
{
	va_list ap;

	va_start(ap, s);
	ccerror(s, ap);
	va_end(ap);
	dexit(1);
}

/*
 * complain a bit and then exit.
 */
void
errorx(int eval, char *s, ...)
{
	va_list ap;

	va_start(ap, s);
	ccerror(s, ap);
	va_end(ap);
	dexit(eval);
}

static char *
find_file(const char *file, struct strlist *path, int mode)
{
	struct string *s;
	char *f;
	size_t lf, lp;
	int need_sep;

	lf = strlen(file);
	STRLIST_FOREACH(s, path) {
		lp = strlen(s->value);
		need_sep = (lp && s->value[lp - 1] != '/') ? 1 : 0;
		f = xmalloc(lp + lf + need_sep + 1);
		memcpy(f, s->value, lp);
		if (need_sep)
			f[lp] = '/';
		memcpy(f + lp + need_sep, file, lf + 1);
		if (access(f, mode) == 0)
			return f;
		free(f);
	}
	return xstrdup(file);
}

static int
compile_input(char *input, char *output)
{
	struct strlist args;
	int retval;

	strlist_init(&args);
	strlist_append_list(&args, &compiler_flags);
	strlist_append(&args, input);
	strlist_append(&args, output);
	strlist_prepend(&args,
	    find_file(cxxflag ? passxx0 : pass0, &progdirs, X_OK));
	retval = strlist_exec(&args);
	strlist_free(&args);
	return retval;
}

static int
assemble_input(char *input, char *output)
{
	struct strlist args;
	int retval;

	strlist_init(&args);
	strlist_append_list(&args, &assembler_flags);
	strlist_append(&args, input);
	strlist_append(&args, "-o");
	strlist_append(&args, output);
	strlist_prepend(&args,
	    find_file(as, &progdirs, X_OK));
	retval = strlist_exec(&args);
	strlist_free(&args);
	return retval;
}

static int
preprocess_input(char *input, char *output, int dodep)
{
	struct strlist args;
	struct string *s;
	int retval;

	strlist_init(&args);
	strlist_append_list(&args, &preprocessor_flags);
	STRLIST_FOREACH(s, &includes) {
		strlist_append(&args, "-i");
		strlist_append(&args, s->value);
	}
	STRLIST_FOREACH(s, &incdirs) {
		strlist_append(&args, "-I");
		strlist_append(&args, s->value);
	}
	STRLIST_FOREACH(s, &user_sysincdirs) {
		strlist_append(&args, "-S");
		strlist_append(&args, s->value);
	}
	if (!nostdinc) {
		STRLIST_FOREACH(s, &sysincdirs) {
			strlist_append(&args, "-S");
			strlist_append(&args, s->value);
		}
	}
	if (dodep)
		strlist_append_list(&args, &depflags);
	strlist_append(&args, input);
	if (output)
		strlist_append(&args, output);

	strlist_prepend(&args, find_file(passp, &progdirs, X_OK));
	retval = strlist_exec(&args);
	strlist_free(&args);
	return retval;
}

static int
run_linker(void)
{
	struct strlist linker_flags;
	int retval;

	if (outfile) {
		strlist_prepend(&early_linker_flags, outfile);
		strlist_prepend(&early_linker_flags, "-o");
	}
	strlist_init(&linker_flags);
	strlist_append_list(&linker_flags, &early_linker_flags);
	strlist_append_list(&linker_flags, &middle_linker_flags);
	strlist_append_list(&linker_flags, &late_linker_flags);
	strlist_prepend(&linker_flags, find_file(ld, &progdirs, X_OK));

	retval = strlist_exec(&linker_flags);

	strlist_free(&linker_flags);
	return retval;
}

static char *cxxt[] = { "cc", "cp", "cxx", "cpp", "CPP", "c++", "C" };
int
cxxsuf(char *s)
{
	unsigned i;
	for (i = 0; i < sizeof(cxxt)/sizeof(cxxt[0]); i++)
		if (strcmp(s, cxxt[i]) == 0)
			return 1;
	return 0;
}

char *
getsufp(char *s)
{
	register char *p;

	if ((p = strrchr(s, '.')) && p[1] != '\0')
		return &p[1];
	return "";
}

int
getsuf(char *s)
{
	register char *p;

	if ((p = strrchr(s, '.')) && p[1] != '\0' && p[2] == '\0')
		return p[1];
	return(0);
}

/*
 * Get basename of string s and change its suffix to ch.
 */
char *
setsuf(char *s, char ch)
{
	char *p;
	int l;

	if ((p = strrchr(s, '.'))) {
		p[1] = ch;
		p[2] = '\0';
	}
	l = strlen(s)+3;
	p = xmalloc(l);
	strlcpy(p, s, l);
	s = p;
	p += l-3;
	*p++ = '.';
	*p++ = ch;
	*p++ = 0;
	return(s);
}

#ifdef os_win32
#define MAX_CMDLINE_LENGTH 32768
int
callsys(char *f, char *v[])
{
	char *cmd;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	DWORD exitCode;
	BOOL ok;

	cmd = win32commandline(f, v);
	if (vflag)
		printf("%s\n", cmd);

	ZeroMemory(&si, sizeof(STARTUPINFO));
	si.cb = sizeof(STARTUPINFO);
	ZeroMemory(&pi, sizeof(PROCESS_INFORMATION));

	ok = CreateProcess(NULL,  // the executable program
		cmd,   // the command line arguments
		NULL,       // ignored
		NULL,       // ignored
		TRUE,       // inherit handles
		HIGH_PRIORITY_CLASS,
		NULL,       // ignored
		NULL,       // ignored
		&si,
		&pi);

	if (!ok) {
		fprintf(stderr, "Can't find %s\n", f);
		return 100;
	}

	WaitForSingleObject(pi.hProcess, INFINITE);
	GetExitCodeProcess(pi.hProcess, &exitCode);
	CloseHandle(pi.hProcess);
	CloseHandle(pi.hThread);

	return (exitCode != 0);
}

#endif

static int
strlist_exec(struct strlist *l)
{
	sig_atomic_t exit_now = 0;
	sig_atomic_t child;
	char **argv;
	size_t argc;
	int result;

	strlist_make_array(l, &argv, &argc);
	if (vflag) {
		printf("Calling ");
		strlist_print(l, stdout);
		printf("\n");
	}

	switch ((child = fork())) {
	case 0:
		execvp(argv[0], argv);
		result = write(STDERR_FILENO, "Exec of ", 8);
		result = write(STDERR_FILENO, argv[0], strlen(argv[0]));
		result = write(STDERR_FILENO, "failed\n", 7);
		(void)result;
		_exit(127);
	case -1:
		error("fork failed");
	default:
		while (waitpid(child, &result, 0) == -1 && errno == EINTR)
			/* nothing */(void)0;
		result = WEXITSTATUS(result);
		if (result)
			error("%s terminated with status %d", argv[0], result);
		while (argc-- > 0)
			free(argv[argc]);
		free(argv);
		break;
	}
	return exit_now;
}

/*
 * Catenate two (optional) strings together
 */
char *
cat(const char *a, const char *b)
{
	size_t len;
	char *rv;

	len = (a ? strlen(a) : 0) + (b ? strlen(b) : 0) + 1;
	rv = xmalloc(len);
	snprintf(rv, len, "%s%s", (a ? a : ""), (b ? b : ""));
	return rv;
}

int
cunlink(char *f)
{
	if (f==0 || Xflag)
		return(0);
	return (unlink(f));
}

#ifdef os_win32
char *
gettmp(void)
{
#define BUFFSIZE 1000
	DWORD pathSize;
	char pathBuffer[BUFFSIZE];
	char tempFilename[MAX_PATH];
	UINT uniqueNum;

	pathSize = GetTempPath(BUFFSIZE, pathBuffer);
	if (pathSize < BUFFSIZE)
		pathBuffer[pathSize] = 0;
	else
		pathBuffer[0] = 0;
	uniqueNum = GetTempFileName(pathBuffer, "ctm", 0, tempFilename);
	if (uniqueNum == 0) {
		fprintf(stderr, "%s:\n", pathBuffer);
		exit(8);
	}
	return xstrdup(tempFilename);
}

#else

char *
gettmp(void)
{
	char *sfn = xstrdup("/tmp/ctm.XXXXXX");
	int fd = -1;

	if ((fd = mkstemp(sfn)) == -1) {
		fprintf(stderr, "%s: %s\n", sfn, strerror(errno));
		exit(8);
	}
	close(fd);
	return sfn;
}
#endif

static void
expand_sysroot(void)
{
	struct string *s;
	struct strlist *lists[] = { &crtdirs, &sysincdirs, &incdirs,
	    &user_sysincdirs, &libdirs, &progdirs, &dirafterdirs, NULL };
	const char *sysroots[] = { sysroot, isysroot, isysroot, isysroot,
	    sysroot, sysroot, isysroot, NULL };
	size_t i, sysroot_len, value_len;
	char *path;

	assert(sizeof(lists) / sizeof(lists[0]) ==
	       sizeof(sysroots) / sizeof(sysroots[0]));

	for (i = 0; lists[i] != NULL; ++i) {
		STRLIST_FOREACH(s, lists[i]) {
			if (s->value[0] != '=')
				continue;
			sysroot_len = strlen(sysroots[i]);
			/* Skipped '=' compensates additional space for '\0' */
			value_len = strlen(s->value);
			path = xmalloc(sysroot_len + value_len);
			memcpy(path, sysroots[i], sysroot_len);
			memcpy(path + sysroot_len, s->value + 1, value_len);
			free(s->value);
			s->value = path;
		}
	}
}


void
owarning(char *s)
{
	fprintf(stderr, "warning: unknown option '%s'\n", s);
}

void
oerror(char *s)
{
	fprintf(stderr, "error: unknown option '%s'\n", s);
	exit(1);
}

void
aerror(char *s)
{
	fprintf(stderr, "error: missing argument to '%s'\n", s);
	exit(1);
}

/*
 * See if m matches the beginning of string str, if it does return the
 * remaining of str, otherwise NULL.
 */
char *
argnxt(char *str, char *m)
{
	if (strncmp(str, m, strlen(m)))
		return NULL; /* No match */
	return str + strlen(m);
}

/*
 * Return next argument to option, or complain.
 */
char *
nxtopt(char *o)
{
	int l;

	if (o != NULL) {
		l = strlen(o);
		if (lav[0][l] != 0)
			return &lav[0][l];
	}
	if (lac == 0)
		aerror(o);
	lav++;
	lac--;
	return lav[0];
}

struct flgcheck {
	int *flag;
	int set;
	char *def;
} cppflgcheck[] = {
	{ &vflag, 1, "-v" },
	{ &freestanding, 1, "-D__STDC_HOSTED__=0" },
	{ &freestanding, 0, "-D__STDC_HOSTED__=1" },
	{ &cxxflag, 1, "-D__cplusplus" },
	{ &xuchar, 1, "-D__CHAR_UNSIGNED__" },
	{ &ascpp, 1, "-D__ASSEMBLER__" },
	{ &sspflag, 1, "-D__SSP__" },
	{ &pthreads, 1, "-D_PTHREADS" },
	{ &Oflag, 1, "-D__OPTIMIZE__" },
	{ &tflag, 1, "-t" },
	{ 0 },
};

static void
cksetflags(struct flgcheck *fs, struct strlist *sl)
{

	for (; fs->flag; fs++) {
		if (fs->set && *fs->flag)
			strlist_append(sl, fs->def);
		if (!fs->set && !*fs->flag)
			strlist_append(sl, fs->def);
	}
}

static char *defflags[] = {
	"-D__PCC__=" MKS(PCC_MAJOR),
	"-D__PCC_MINOR__=" MKS(PCC_MINOR),
	"-D__PCC_MINORMINOR__=" MKS(PCC_MINORMINOR),
	"-D__VERSION__=" MKS(VERSSTR),
	"-D__SCHAR_MAX__=" MKS(MAX_CHAR),
	"-D__SHRT_MAX__=" MKS(MAX_SHORT),
	"-D__INT_MAX__=" MKS(MAX_INT),
	"-D__LONG_MAX__=" MKS(MAX_LONG),
	"-D__LONG_LONG_MAX__=" MKS(MAX_LONGLONG),

	"-D__STDC_ISO_10646__=200009L",
	"-D__WCHAR_TYPE__=" WCT,
	"-D__SIZEOF_WCHAR_T__=" MKS(WCHAR_SIZE),
	"-D__WCHAR_MAX__=" WCM,
	"-D__WINT_TYPE__=" PCC_WINT_TYPE,
	"-D__SIZE_TYPE__=" PCC_SIZE_TYPE,
	"-D__PTRDIFF_TYPE__=" PCC_PTRDIFF_TYPE,
	"-D__SIZEOF_WINT_T__=4",
};

static char *gcppflags[] = {
#ifndef os_win32
#ifdef GCC_COMPAT
	"-D__GNUC__=4",
	"-D__GNUC_MINOR__=3",
	"-D__GNUC_PATCHLEVEL__=1",
	"-D__REGISTER_PREFIX__=" REGISTER_PREFIX,
	"-D__USER_LABEL_PREFIX__=" USER_LABEL_PREFIX,
#endif
#endif
};

/* These should _not_ be defined here */
static char *fpflags[] = {
#if defined(os_darwin) || defined(os_netbsd)
	"-D__FLT_RADIX__=2",
	"-D__FLT_DIG__=6",
	"-D__FLT_EPSILON__=1.19209290e-07F",
	"-D__FLT_MANT_DIG__=24",
	"-D__FLT_MAX_10_EXP__=38",
	"-D__FLT_MAX_EXP__=128",
	"-D__FLT_MAX__=3.40282347e+38F",
	"-D__FLT_MIN_10_EXP__=(-37)",
	"-D__FLT_MIN_EXP__=(-125)",
	"-D__FLT_MIN__=1.17549435e-38F",
	"-D__DBL_DIG__=15",
	"-D__DBL_EPSILON__=2.2204460492503131e-16",
	"-D__DBL_MANT_DIG__=53",
	"-D__DBL_MAX_10_EXP__=308",
	"-D__DBL_MAX_EXP__=1024",
	"-D__DBL_MAX__=1.7976931348623157e+308",
	"-D__DBL_MIN_10_EXP__=(-307)",
	"-D__DBL_MIN_EXP__=(-1021)",
	"-D__DBL_MIN__=2.2250738585072014e-308",
#if defined(mach_i386) || defined(mach_amd64)
	"-D__LDBL_DIG__=18",
	"-D__LDBL_EPSILON__=1.08420217248550443401e-19L",
	"-D__LDBL_MANT_DIG__=64",
	"-D__LDBL_MAX_10_EXP__=4932",
	"-D__LDBL_MAX_EXP__=16384",
	"-D__LDBL_MAX__=1.18973149535723176502e+4932L",
	"-D__LDBL_MIN_10_EXP__=(-4931)",
	"-D__LDBL_MIN_EXP__=(-16381)",
	"-D__LDBL_MIN__=3.36210314311209350626e-4932L",
#else
	"-D__LDBL_DIG__=15",
	"-D__LDBL_EPSILON__=2.2204460492503131e-16",
	"-D__LDBL_MANT_DIG__=53",
	"-D__LDBL_MAX_10_EXP__=308",
	"-D__LDBL_MAX_EXP__=1024",
	"-D__LDBL_MAX__=1.7976931348623157e+308",
	"-D__LDBL_MIN_10_EXP__=(-307)",
	"-D__LDBL_MIN_EXP__=(-1021)",
	"-D__LDBL_MIN__=2.2250738585072014e-308",
#endif
#endif
};


/*
 * Configure the standard cpp flags.
 */
void
setup_cpp_flags(void)
{
	int i;

	/* a bunch of misc defines */
	for (i = 0; i < (int)sizeof(defflags)/(int)sizeof(char *); i++)
		strlist_append(&preprocessor_flags, defflags[i]);

	for (i = 0; i < (int)sizeof(gcppflags)/(int)sizeof(char *); i++)
		strlist_append(&preprocessor_flags, gcppflags[i]);
	strlist_append(&preprocessor_flags, xgnu89 ?
	    "-D__GNUC_GNU_INLINE__" : "-D__GNUC_STDC_INLINE__");

	cksetflags(cppflgcheck, &preprocessor_flags);

	for (i = 0; i < (int)sizeof(fpflags)/(int)sizeof(char *); i++)
		strlist_append(&preprocessor_flags, fpflags[i]);

	for (i = 0; cppadd[i]; i++)
		strlist_append(&preprocessor_flags, cppadd[i]);
	for (i = 0; cppmdadd[i]; i++)
		strlist_append(&preprocessor_flags, cppmdadd[i]);

	/* Include dirs */
	strlist_append(&sysincdirs, "=" INCLUDEDIR "pcc/");
	strlist_append(&sysincdirs, "=" STDINC);
	if (cxxflag)
		strlist_append(&sysincdirs, "=" PCCINCDIR "/c++");
	strlist_append(&sysincdirs, "=" PCCINCDIR);
}

struct flgcheck ccomflgcheck[] = {
	{ &Oflag, 1, "-xtemps" },
	{ &Oflag, 1, "-xdeljumps" },
	{ &Oflag, 1, "-xinline" },
#ifdef notyet
	{ &Oflag, 1, "-xdce" },
	{ &Oflag, 1, "-xssa" },
#endif
	{ &freestanding, 1, "-ffreestanding" },
	{ &pgflag, 1, "-p" },
	{ &gflag, 1, "-g" },
	{ &xgnu89, 1, "-xgnu89" },
	{ &xgnu99, 1, "-xgnu99" },
	{ &xuchar, 1, "-xuchar" },
#if !defined(os_sunos) && !defined(mach_i386)
	{ &vflag, 0, "-v" },
#endif
#ifdef os_darwin
	{ &Bstatic, 1, "-k" },
#elif defined(os_sunos) && defined(mach_i386)
	{ &kflag, 1, "-K" },
	{ &kflag, 1, "pic" },
#else
	{ &kflag, 1, "-k" },
#endif
	{ &sspflag, 1, "-fstack-protector" },
	{ 0 }
};

void
setup_ccom_flags(void)
{
	struct Wflags *Wf;

	cksetflags(ccomflgcheck, &compiler_flags);
	if (Wflag || Wallflag) {
		/* -Wall is same as gcc, -WW is all flags */
		for (Wf = Wflags; Wf->name; Wf++) {
			if (Wflag || Wf->flags == INWALL)
				strlist_append(&compiler_flags,
				    cat("-W", Wf->name));

		}
	}
}

static int one = 1;

struct flgcheck asflgcheck[] = {
#if defined(USE_YASM)
	{ &one, 1, "-p" },
	{ &one, 1, "gnu" },
	{ &one, 1, "-f" },
#if defined(os_win32)
	{ &one, 1, "win32" },
#elif defined(os_darwin)
	{ &one, 1, "macho" },
#else
	{ &one, 1, "elf" },
#endif
#endif
#if defined(os_sunos) && defined(mach_sparc64)
	{ &one, 1, "-m64" },
#endif
#if defined(os_darwin)
	{ &Bstatic, 1, "-static" },
#endif
#if !defined(USE_YASM)
	{ &vflag, 1, "-v" },
#endif
	{ &kflag, 1, "-k" },
#ifdef os_darwin
	{ &one, 1, "-arch" },
#if mach_amd64
	{ &amd64_i386, 1, "i386" },
	{ &amd64_i386, 0, "x86_64" },
#else
	{ &one, 1, "i386" },
#endif
#else
#ifdef mach_amd64
	{ &amd64_i386, 1, "--32" },
#endif
#endif
	{ 0 }
};
void
setup_as_flags(void)
{
	one = one;
	cksetflags(asflgcheck, &assembler_flags);
}

struct flgcheck ldflgcheck[] = {
#ifndef MSLINKER
	{ &vflag, 1, "-v" },
#endif
#if !defined(os_sunos) && !defined(os_win32) && !defined(os_darwin)
	{ &one, 1, "-X" },
#endif
#ifdef os_darwin
	{ &shared, 1, "-dylib" },
#elif defined(os_win32)
	{ &shared, 1, "-Bdynamic" },
else
	{ &shared, 1, "-shared" },
#endif
#if !defined(os_sunos) && !defined(os_win32)
#ifndef os_darwin
	{ &shared, 0, "-d" },
#endif
	{ &rflag, 1, "-r" },
	{ &rflag, 0, "-e" },
	{ &rflag, 0, STARTLABEL },
#endif
#ifdef os_darwin
	{ &Bstatic, 1, "-static" },
#else
	{ &Bstatic, 1, "-Bstatic" },
#endif
#if !defined(os_darwin) && !defined(os_sunos)
	{ &gflag, 1, "-g" },
#endif
	{ &pthreads, 1, "-lpthread" },
	{ 0 },
};

void
setup_ld_flags(void)
{
	int i;

	cksetflags(ldflgcheck, &early_linker_flags);
	if (Bstatic == 0)
		for (i = 0; dynlinker[i]; i++)
			strlist_append(&early_linker_flags, dynlinker[i]);
	if (sysroot && *sysroot)
		strlist_append(&early_linker_flags, cat("--sysroot=", sysroot));
	if (!nostdlib) {
		/* library search paths */
		if (pcclibdir)
			strlist_append(&late_linker_flags,
			    cat("-L", pcclibdir));
		for (i = 0; deflibdirs[i]; i++)
			strlist_append(&late_linker_flags,
			    cat("-L", deflibdirs[i]));
		/* standard libraries */
		if (pgflag) {
			for (i = 0; defproflibs[i]; i++)
				strlist_append(&late_linker_flags,
				     defproflibs[i]);
		} else if (cxxflag) {
			for (i = 0; defcxxlibs[i]; i++)
				strlist_append(&late_linker_flags,
				    defcxxlibs[i]);
		} else {
			for (i = 0; deflibs[i]; i++)
				strlist_append(&late_linker_flags, deflibs[i]);
		}
	}
	if (!nostartfiles) {
		if (shared) {
			strlist_prepend(&middle_linker_flags,
			    find_file(CRTBEGIN_S, &crtdirs, R_OK));
			strlist_append(&late_linker_flags,
			    find_file(CRTEND_S, &crtdirs, R_OK));
		} else {
			if (Bstatic) {
				strlist_prepend(&middle_linker_flags,
				    find_file(CRTBEGIN_T, &crtdirs, R_OK));
				strlist_append(&late_linker_flags,
				    find_file(CRTEND_T, &crtdirs, R_OK));
#ifdef notyet
			} else if (pieflag) {
				strlist_prepend(&middle_linker_flags,
				    find_file(CRTBEGIN_S, &crtdirs, R_OK));
				strlist_append(&late_linker_flags,
				    find_file(CRTEND_S, &crtdirs, R_OK));
#endif
			} else {
				strlist_prepend(&middle_linker_flags,
				    find_file(CRTBEGIN, &crtdirs, R_OK));
				strlist_append(&late_linker_flags,
				    find_file(CRTEND, &crtdirs, R_OK));
			}
			strlist_prepend(&middle_linker_flags,
			    find_file(CRTI, &crtdirs, R_OK));
			strlist_append(&late_linker_flags,
			    find_file(CRTN, &crtdirs, R_OK));
			if (pgflag)
				strlist_prepend(&middle_linker_flags,
				    find_file(GCRT0, &crtdirs, R_OK));
#ifdef notyet
			else if (pieflag)
				strlist_prepend(&middle_linker_flags,
				    find_file(SCRT0, &crtdirs, R_OK));
#endif
			else
				strlist_prepend(&middle_linker_flags,
				    find_file(CRT0, &crtdirs, R_OK));
		}
	}
}

#ifdef os_win32
char *
win32pathsubst(char *s)
{
	char env[1024];
	char *rv;
	int len;

	len = ExpandEnvironmentStrings(s, env, sizeof(env));
	if (len <= 0)
		return s;

	while (env[len-1] == '/' || env[len-1] == '\\' || env[len-1] == '\0')
		env[--len] = 0;

	rv = xmalloc(len+1);
	strlcpy(rv, env, len+1);

	return rv;
}

char *
win32commandline(char *f, char *args[])
{
	char *cmd;
	char *p;
	int len;
	int i, j, k;

	len = strlen(f) + 3;

	for (i = 1; args[i] != NULL; i++) {
		for (j = 0; args[i][j] != '\0'; j++) {
			len++;
			if (args[i][j] == '\"') {
				for (k = j-1; k >= 0 && args[i][k] == '\\'; k--)
					len++;
			}
		}
		for (k = j-1; k >= 0 && args[i][k] == '\\'; k--)
			len++;
		len += j + 3;
	}

	p = cmd = xmalloc(len);
	*p++ = '\"';
	p += strlcpy(p, f, len-1);
	*p++ = '\"';
	*p++ = ' ';

	for (i = 1; args[i] != NULL; i++) {
		*p++ = '\"';
		for (j = 0; args[i][j] != '\0'; j++) {
			if (args[i][j] == '\"') {
				for (k = j-1; k >= 0 && args[i][k] == '\\'; k--)
					*p++ = '\\';
				*p++ = '\\';
			}
			*p++ = args[i][j];
		}
		for (k = j-1; k >= 0 && args[i][k] == '\\'; k--)
			*p++ = '\\';
		*p++ = '\"';
		*p++ = ' ';
	}
	p[-1] = '\0';

	return cmd;
}
#endif
