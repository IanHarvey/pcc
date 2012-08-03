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

#include "ccconfig.h"
#include "macdefs.h"

#include "xalloc.h"
#include "strlist.h"
/* C command */

#define	MKS(x) _MKS(x)
#define _MKS(x) #x

/*
 * Many specific definitions, should be declared elsewhere.
 */

#ifndef STDINC
#define	STDINC	  	"/usr/include/"
#endif

#ifndef LIBDIR
#define LIBDIR		"/usr/lib/"
#endif

#ifndef PREPROCESSOR
#define PREPROCESSOR	"cpp"
#endif

#ifndef COMPILER
#define COMPILER	"ccom"
#endif

#ifndef ASSEMBLER
#define ASSEMBLER	"as"
#endif

#ifndef LINKER
#define LINKER		"ld"
#endif

#ifndef MULTIOSDIR
#define MULTIOSDIR	"."
#endif


#define MAXAV  10000
char	*tmp3;
char	*tmp4;
char	*outfile, *ermfile;
static char **lav;
static int lac;
static void add_prefix(const char *);
static char *find_file(const char *, int);
static char *find_file2(const char *file, struct strlist *path, int mode);
char *copy(const char *, int);
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
void dexit(int);
void idexit(int);
char *gettmp(void);
void *ccmalloc(int size);
void aerror(char *);
void oerror(char *);
void owarning(char *);
char *argnxt(char *, char *);
char *nxtopt(char *o);
void setup_cpp_flags(void);
static void expand_sysroot(void);
#ifdef os_win32
char *win32pathsubst(char *);
char *win32commandline(char *, char *[]);
#endif
char	*av[MAXAV];
char	*xlist[100];
int	xnum;
char	*mlist[100];
char	*flist[100];
char	*wlist[100];
int	nm;
int	nf;
int	nw;
int	sspflag;
int	freestanding;
int	pflag;
int	sflag;
int	cflag;
int	eflag;
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
int	exfail;
int	Xflag;
int	Wallflag;
int	Wflag;
int	nostartfiles, Bstatic, shared;
int	nostdinc, nostdlib;
int	onlyas;
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

char	*passp = LIBEXECDIR PREPROCESSOR;
char	*pass0 = LIBEXECDIR COMPILER;
char	*passxx0 = LIBEXECDIR "cxxcom";
char	*as = ASSEMBLER;
char	*ld = LINKER;
char	*sysroot = "", *isysroot;
char *cppadd[] = CPPADD;
#ifdef DYNLINKER
char *dynlinker[] = DYNLINKER;
#endif
#ifdef CRT0FILE
char *crt0file = CRT0FILE;
#endif
#ifdef CRT0FILE_PROFILE
char *crt0file_profile = CRT0FILE_PROFILE;
#endif
#ifdef STARTFILES
char *startfiles[] = STARTFILES;
char *endfiles[] = ENDFILES;
#endif
#ifdef STARTFILES_T
char *startfiles_T[] = STARTFILES_T;
char *endfiles_T[] = ENDFILES_T;
#endif
#ifdef STARTFILES_S
char *startfiles_S[] = STARTFILES_S;
char *endfiles_S[] = ENDFILES_S;
#endif
char *cppmdadd[] = CPPMDADD;
#ifdef LIBCLIBS
char *libclibs[] = LIBCLIBS;
#else
char *libclibs[] = { "-lc", NULL };
#endif
#ifdef LIBCLIBS_PROFILE
char *libclibs_profile[] = LIBCLIBS_PROFILE;
#else
char *libclibs_profile[] = { "-lc_p", NULL };
#endif
#ifndef STARTLABEL
#define STARTLABEL "__start"
#endif
#ifdef PCCLIBDIR
char *pcclibdir = PCCLIBDIR;
#endif
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

#ifndef CPPROGNAME
#define	CPPROGNAME	"cpp"
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

int
main(int argc, char *argv[])
{
	struct string *s, *ss;
	struct Wflags *Wf;
	char *t, *u, *argp;
	char *assource;
	int ninput, i, j, nxo, na;

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

	if ((t = strrchr(argv[0], '/')))
		t = copy(t+1, 0);
	else
		t = argv[0];
	if (strcmp(t, "p++") == 0) {
		cxxflag = 1;
		pass0 = passxx0;
	} else if (strcmp(t, "cpp") == 0 || strcmp(t, CPPROGNAME) == 0) {
		cppflag = 1;
		Eflag = 1;
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

	nxo = 0;
	while (--lac) {
		++lav;
		argp = *lav;

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
			add_prefix(t);
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
			} else if (strncmp(argp, "-Wc,", 4) == 0) {
				/* options to ccom */
				t = &argp[4];
				while ((u = strchr(t, ','))) {
					*u++ = 0;
					wlist[nw++] = t;
					t = u;
				}
				wlist[nw++] = t;
			} else if ((t = argnxt(argp, "-Wp,"))) {
				u = strtok(t, ",");
				do {
					strlist_append(&preprocessor_flags, u);
				} while ((u = strtok(NULL, ",")) != NULL);
			} else if (strcmp(argp, "-Werror") == 0) {
				wlist[nw++] = argp;
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
						wlist[nw++] = argp;
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
			else if (strcmp(argp, "-fstack-protector") == 0) {
				flist[nf++] = argp;
				sspflag++;
			} else if (strcmp(argp, "-fstack-protector-all") == 0) {
				flist[nf++] = argp;
				sspflag++;
			} else if (strcmp(argp, "-fno-stack-protector") == 0) {
				flist[nf++] = argp;
				sspflag = 0;
			} else if (strcmp(argp, "-fno-stack-protector-all") == 0) {
				flist[nf++] = argp;
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
#ifdef os_darwin
			} else if (match(argp, "-install_name")) {
				strlist_append(&middle_linker_flags, argp);
				strlist_append(&middle_linker_flags, nxtopt(0));
#endif
			} else
				owarning(argp);
			break;

		case 'k': /* generate PIC code */
			kflag = F_pic;
			break;

		case 'l':
		case 'L':
			strlist_append(&late_linker_flags, argp);
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
			mlist[nm++] = argp;
			strlist_append(&middle_linker_flags, argp);
			if (argp[2] == 0) {
				t = nxtopt(0);
				strlist_append(&middle_linker_flags, t);
				mlist[nm++] = t;
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
			} else if (strcmp(argp,
			    "-print-multi-os-directory") == 0) {
				printf("%s\n", MULTIOSDIR);
				return 0;
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
			else if (argp[2])
				xlist[xnum++] = argp;
			else
				xlist[xnum++] = "-x", xlist[xnum++] = t;
			break;
		case 't':
			tflag++;
			break;
		case 'S':
			sflag++;
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
			else if (argp[3] == '\0' && isdigit((unsigned char)argp[2]))
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
			pflag++;
			strlist_append(&preprocessor_flags, argp);
			break;

		case 'c':
#ifdef os_darwin
			if (match(argp, "-compatibility_version") ||
			    match(argp, "-current_version")) {
				strlist_append(&middle_linker_flags, argp);
				strlist_append(&middle_linker_flags, nxtopt(0));
			} else
#endif
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
				j = strlen(u = nxtopt("-MT"));
				t = copy("-xMT,", j);
				strlcat(t, u, j+6);
				t[3] = argp[2];
				strlist_append(&depflags, t);
				break;
			default:
				oerror(argp);
			}
			break;

		case 'd':
#ifdef os_darwin
			if (strcmp(argp, "-dynamiclib") == 0) {
				shared = 1;
			} else
#endif
			break;
		case 'v':
			printf("%s\n", VERSSTR);
			vflag++;
			break;

		case 's':
#ifndef os_darwin
			if (strcmp(argp, "-shared") == 0) {
				shared = 1;
			} else
#endif
			if (strcmp(argp, "-static") == 0) {
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
#if 0
		} else if (ninput == 2) {
			outfile = clist[--nc];
#endif
		}
	}
	if (ninput == 0)
		errorx(8, "no input files");
	if (outfile && (cflag || sflag || Eflag) && ninput > 1)
		errorx(8, "-o given with -c || -E || -S and more than one file");
#if 0
	if (outfile && clist[0] && strcmp(outfile, clist[0]) == 0)
		errorx(8, "output file will be clobbered");
#endif

	if (needM && !Mflag)
		errorx(8, "to make dependencies needs -M");


	if (ninput == 0)
		goto nocom;
	if (pflag==0) {
		if (!sflag)
			tmp3 = gettmp();
		tmp4 = gettmp();
	}
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)	/* interrupt */
		signal(SIGINT, idexit);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)	/* terminate */
		signal(SIGTERM, idexit);

	setup_cpp_flags();

	if (isysroot == NULL)
		isysroot = sysroot;
	expand_sysroot();

	STRLIST_FOREACH(ss, &inputs) {
		/*
		 * C preprocessor
		 */
		if (getsuf(ss->value) == 'o' || getsuf(ss->value) == 'a')
			continue;

		if (ninput > 1 && !Eflag)
			printf("%s:\n", ss->value);
		onlyas = 0;
		assource = tmp3;
		if (getsuf(ss->value)=='S')
			ascpp = 1;
		if (getsuf(ss->value)=='i') {
			if(Eflag)
				continue;
			goto com;
		} else if (ascpp) {
			onlyas = 1;
		} else if (xasm || getsuf(ss->value)=='s') {
			assource = ss->value;
			goto assemble;
		}


		if (pflag)
			tmp4 = setsuf(s->value, 'i');
		na = 0;
		av[na++] = "cpp";

		STRLIST_FOREACH(s, &depflags) {
			av[na++] = s->value;
		}
		STRLIST_FOREACH(s, &preprocessor_flags) {
			av[na++] = s->value;
		}
		STRLIST_FOREACH(s, &includes) {
			av[na++] = "-i";
			av[na++] = s->value;
		}
		STRLIST_FOREACH(s, &incdirs) {
			av[na++] = "-I";
			av[na++] = s->value;
		}
		STRLIST_FOREACH(s, &user_sysincdirs) {
			av[na++] = "-S";
			av[na++] = s->value;
		}
		if (!nostdinc) {
			STRLIST_FOREACH(s, &sysincdirs) {
				av[na++] = "-S";
				av[na++] = s->value;
			}
		}
		STRLIST_FOREACH(s, &dirafterdirs) {
			av[na++] = "-S";
			av[na++] = s->value;
		}

		av[na++] = ss->value;
		if (!Eflag && !Mflag)
			av[na++] = tmp4;
		if ((Eflag || Mflag) && outfile)
			 ermfile = av[na++] = outfile;
		av[na++]=0;
		if (callsys(passp, av)) {
			exfail++;
			eflag++;
		}
		if (Eflag || Mflag)
			continue;
		if (onlyas) {
			assource = tmp4;
			goto assemble;
		}

		/*
		 * C compiler
		 */
	com:
		na = 0;
		av[na++]= cxxflag ? "c++com" : "ccom";
		if (Wflag || Wallflag) {
			/* -Wall is same as gcc, -WW is all flags */
			for (Wf = Wflags; Wf->name; Wf++) {
				if (Wflag || Wf->flags == INWALL)
					av[na++] = cat("-W", Wf->name);
			}
		}
		for (j = 0; j < nw; j++)
			av[na++] = wlist[j];
		for (j = 0; j < nf; j++)
			av[na++] = flist[j];
		if (freestanding)
			av[na++] = "-ffreestanding";
#if !defined(os_sunos) && !defined(mach_i386)
		if (vflag)
			av[na++] = "-v";
#endif
		if (pgflag)
			av[na++] = "-p";
		if (gflag)
			av[na++] = "-g";
#ifdef os_darwin
		/* darwin always wants PIC compilation */
		if (!Bstatic)
			av[na++] = "-k";
#elif defined(os_sunos) && defined(mach_i386)
		if (kflag) {
			av[na++] = "-K";
			av[na++] = "pic";
		}
#else
		if (kflag)
			av[na++] = "-k";
#endif
		if (Oflag) {
			av[na++] = "-xtemps";
			av[na++] = "-xdeljumps";
			av[na++] = "-xinline";
		}
		if (xgnu89)
			av[na++] = "-xgnu89";
		if (xgnu99)
			av[na++] = "-xgnu99";
		if (xuchar)
			av[na++] = "-xuchar";
		for (j = 0; j < xnum; j++)
			av[na++] = xlist[j];
		for (j = 0; j < nm; j++)
			av[na++] = mlist[j];
		if (getsuf(ss->value)=='i')
			av[na++] = ss->value;
		else
			av[na++] = tmp4; /* created by cpp */
		if (pflag || exfail)
			{
			cflag++;
			continue;
			}
		if(sflag) {
			if (outfile)
				tmp3 = outfile;
			else
				tmp3 = setsuf(ss->value, 's');
		}
		ermfile = av[na++] = tmp3;

		av[na++] = NULL;
		if (callsys(pass0, av)) {
			cflag++;
			eflag++;
			continue;
		}
		if (sflag)
			continue;

		/*
		 * Assembler
		 */
	assemble:
		na = 0;
		av[na++] = as;
		STRLIST_FOREACH(s, &assembler_flags) {
			av[na++] = s->value;
		}
#if defined(USE_YASM)
		av[na++] = "-p";
		av[na++] = "gnu";
		av[na++] = "-f";
#if defined(os_win32)
		av[na++] = "win32";
#elif defined(os_darwin)
		av[na++] = "macho";
#else
		av[na++] = "elf";
#endif
#endif
#if defined(os_sunos) && defined(mach_sparc64)
		av[na++] = "-m64";
#endif
#if defined(os_darwin)
		if (Bstatic)
			av[na++] = "-static";
#endif
#if !defined(USE_YASM)
		if (vflag)
			av[na++] = "-v";
#endif
		if (kflag)
			av[na++] = "-k";
#ifdef os_darwin
		av[na++] = "-arch";
#if mach_amd64
		av[na++] = amd64_i386 ? "i386" : "x86_64";
#else
		av[na++] = "i386";
#endif
#else
#ifdef mach_amd64
		if (amd64_i386)
			av[na++] = "--32";
#endif
#endif
		av[na++] = "-o";
		if (outfile && cflag)
			ermfile = outfile;
		else if (cflag)
			ermfile = setsuf(ss->value, 'o');
		else {
			ermfile = ss->value = gettmp();
			strlist_append(&temp_outputs, ermfile);
		}

		av[na++] = ermfile;
		av[na++] = assource;
		av[na++] = 0;
		if (callsys(as, av)) {
			cflag++;
			eflag++;
			cunlink(tmp4);
		}
		cunlink(tmp4);
	}

	if (Eflag || Mflag)
		dexit(eflag);

	/*
	 * Linker
	 */
nocom:
	if (cflag==0 && ninput != 0) {
		j = 0;
		av[j++] = ld;
#ifndef MSLINKER
		if (vflag)
			av[j++] = "-v";
#endif
#if !defined(os_sunos) && !defined(os_win32) && !defined(os_darwin)
		av[j++] = "-X";
#endif
		if (sysroot && *sysroot)
			av[j++] = cat("--sysroot=", sysroot);
		if (shared) {
#ifdef os_darwin
			av[j++] = "-dylib";
#else
			av[j++] = "-shared";
#endif
#ifdef os_win32
			av[j++] = "-Bdynamic";
#endif
#ifndef os_sunos
		} else {
#ifndef os_win32
#ifndef os_darwin
			av[j++] = "-d";
#endif
			if (rflag) {
				av[j++] = "-r";
			} else {
				av[j++] = "-e";
				av[j++] = STARTLABEL;
			}
#endif
#endif
			if (Bstatic == 0) { /* Dynamic linkage */
#ifdef DYNLINKER
				for (i = 0; dynlinker[i]; i++)
					av[j++] = dynlinker[i];
#endif
			} else {
#ifdef os_darwin
				av[j++] = "-static";
#else
				av[j++] = "-Bstatic";
#endif
			}
		}
		if (outfile) {
#ifdef MSLINKER
			av[j++] = cat("/OUT:", outfile);
#else
			av[j++] = "-o";
			av[j++] = outfile;
#endif
		}
#ifdef STARTFILES_S
		if (shared) {
			if (!nostartfiles) {
				for (i = 0; startfiles_S[i]; i++)
					av[j++] = find_file(startfiles_S[i], R_OK);
			}
		} else
#endif
		{
			if (!nostartfiles) {
#ifdef CRT0FILE_PROFILE
				if (pgflag) {
					av[j++] = find_file(crt0file_profile, R_OK);
				} else
#endif
				{
#ifdef CRT0FILE
					av[j++] = find_file(crt0file, R_OK);
#endif
				}
#ifdef STARTFILES_T
				if (Bstatic) {
					for (i = 0; startfiles_T[i]; i++)
						av[j++] = find_file(startfiles_T[i], R_OK);
				} else
#endif
				{
#ifdef STARTFILES
					for (i = 0; startfiles[i]; i++)
						av[j++] = find_file(startfiles[i], R_OK);
#endif
				}
			}
		}
		i = 0;
		STRLIST_FOREACH(s, &middle_linker_flags)
			av[j++] = s->value;
		STRLIST_FOREACH(s, &inputs)
			av[j++] = s->value;
		STRLIST_FOREACH(s, &late_linker_flags)
			av[j++] = s->value;

		i = 0;
#if !defined(os_darwin) && !defined(os_sunos)
		/* darwin assembler doesn't want -g */
		if (gflag)
			av[j++] = "-g";
#endif
#if 0
		if (gflag)
			av[j++] = "-lg";
#endif
		if (pthreads)
			av[j++] = "-lpthread";
		if (!nostdlib) {
#ifdef MSLINKER
#define	LFLAG	"/LIBPATH:"
#else
#define	LFLAG	"-L"
#endif
#ifdef PCCLIBDIR
			av[j++] = cat(LFLAG, pcclibdir); 
#endif
#ifdef os_win32
			av[j++] = cat(LFLAG, libdir);
#endif
			if (pgflag) {
				for (i = 0; libclibs_profile[i]; i++)
					av[j++] = find_file(libclibs_profile[i], R_OK);
			} else {
				if (cxxflag)
					av[j++] = "-lp++";
				for (i = 0; libclibs[i]; i++)
					av[j++] = find_file(libclibs[i], R_OK);
			}
		}
		if (!nostartfiles) {
#ifdef STARTFILES_S
			if (shared) {
				for (i = 0; endfiles_S[i]; i++)
					av[j++] = find_file(endfiles_S[i], R_OK);
			} else 
#endif
			{
#ifdef STARTFILES_T
				if (Bstatic) {
					for (i = 0; endfiles_T[i]; i++)
						av[j++] = find_file(endfiles_T[i], R_OK);
				} else
#endif
				{
#ifdef STARTFILES
					for (i = 0; endfiles[i]; i++)
						av[j++] = find_file(endfiles[i], R_OK);
#endif
				}
			}
		}
		av[j++] = 0;
		eflag |= callsys(ld, av);
		STRLIST_FOREACH(s, &temp_outputs)
			cunlink(s->value);
	}
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
#endif
	dexit(eflag);
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
	if (!pflag && !Xflag) {
		if (sflag==0)
			cunlink(tmp3);
		cunlink(tmp4);
	}
	if (exfail || eflag)
		cunlink(ermfile);
	if (eval == 100)
		_exit(eval);
	exit(eval);
}

static void
ccerror(char *s, va_list ap)
{
	vfprintf(Eflag ? stderr : stdout, s, ap);
	putc('\n', Eflag? stderr : stdout);
	exfail++;
	cflag++;
	eflag++;
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
find_file2(const char *file, struct strlist *path, int mode)
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


static size_t file_prefixes_cnt;
static char **file_prefixes;

static void
add_prefix(const char *prefix)
{
	file_prefixes = realloc(file_prefixes,
	    sizeof(*file_prefixes) * (file_prefixes_cnt + 1));
	if (file_prefixes == NULL)
		errorx(1, "malloc failed");
	file_prefixes[file_prefixes_cnt++] = copy(prefix, 0);
}

static char *
find_file(const char *base, int mode)
{
	char *path;
	size_t baselen = strlen(base);
	size_t sysrootlen = sysroot ? strlen(sysroot) : 0;
	size_t len, prefix_len, i;

	for (i = 0; i < file_prefixes_cnt; ++i) {
		prefix_len = strlen(file_prefixes[i]);
		len = prefix_len + baselen + 2;
		if (file_prefixes[i][0] == '=') {
			len += sysrootlen;
			path = ccmalloc(len);
			snprintf(path, len, "%s%s/%s", sysroot,
			    file_prefixes[i] + 1, base);
		} else {
			path = ccmalloc(len);
			snprintf(path, len, "%s/%s", file_prefixes[i], base);
		}
		if (access(path, mode) == 0)
			return path;
		free(path);
	}

	return copy(base, 0);
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

	s = copy(basename(s), 2);
	if ((p = strrchr(s, '.')) == NULL) {
		p = s + strlen(s);
		p[0] = '.';
	}
	p[1] = ch;
	p[2] = '\0';
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

#else

int
callsys(char *f, char *v[])
{
	int t, status = 0;
	pid_t p;
	char *prog;

	if (vflag) {
		fprintf(stderr, "%s ", f);
		for (t = 1; v[t]; t++)
			fprintf(stderr, "%s ", v[t]);
		fprintf(stderr, "\n");
	}

	prog = find_file(f, X_OK);
#ifdef HAVE_VFORK
	if ((p = vfork()) == 0) {
#else
	if ((p = fork()) == 0) {
#endif
		static const char msg[] = "Can't find ";
		execvp(prog, v);
		t = write(STDERR_FILENO, msg, sizeof(msg));
		t = write(STDERR_FILENO, prog, strlen(prog));
		t = write(STDERR_FILENO, "\n", 1);
		_exit(100);
	}
	if (p == -1) {
		fprintf(stderr, "fork() failed, try again\n");
		return(100);
	}
	free(prog);
	while (waitpid(p, &status, 0) == -1 && errno == EINTR)
		;
	if (WIFEXITED(status))
		return (WEXITSTATUS(status));
	if (WIFSIGNALED(status))
		dexit(eflag ? eflag : 1);
	errorx(8, "Fatal error in %s", f);

	return 0;
}
#endif

#if 0
static int
strlist_exec(struct strlist *l)
{
	char **argv;
	size_t argc;
	int result;

	strlist_make_array(l, &argv, &argc);
	if (verbose_mode) {
		printf("Calling ");
		strlist_print(l, stdout);
		printf("\n");
	}

	if (exit_now)
		return 1;

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
#endif

/*
 * Make a copy of string as, mallocing extra bytes in the string.
 */
char *
copy(const char *s, int extra)
{
	int len = strlen(s)+1;
	char *rv;

	rv = ccmalloc(len+extra);
	strlcpy(rv, s, len);
	return rv;
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
	rv = ccmalloc(len);
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
	return copy(tempFilename, 0);
}

#else

char *
gettmp(void)
{
	char *sfn = copy("/tmp/ctm.XXXXXX", 0);
	int fd = -1;

	if ((fd = mkstemp(sfn)) == -1) {
		fprintf(stderr, "%s: %s\n", sfn, strerror(errno));
		exit(8);
	}
	close(fd);
	return sfn;
}
#endif

void *
ccmalloc(int size)
{
	void *rv;

	if ((rv = malloc(size)) == NULL)
		error("malloc failed");
	return rv;
}

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
	"-D__FLT_RADIX__=2";
	"-D__FLT_DIG__=6";
	"-D__FLT_EPSILON__=1.19209290e-07F";
	"-D__FLT_MANT_DIG__=24";
	"-D__FLT_MAX_10_EXP__=38";
	"-D__FLT_MAX_EXP__=128";
	"-D__FLT_MAX__=3.40282347e+38F";
	"-D__FLT_MIN_10_EXP__=(-37)";
	"-D__FLT_MIN_EXP__=(-125)";
	"-D__FLT_MIN__=1.17549435e-38F";
	"-D__DBL_DIG__=15";
	"-D__DBL_EPSILON__=2.2204460492503131e-16";
	"-D__DBL_MANT_DIG__=53";
	"-D__DBL_MAX_10_EXP__=308";
	"-D__DBL_MAX_EXP__=1024";
	"-D__DBL_MAX__=1.7976931348623157e+308";
	"-D__DBL_MIN_10_EXP__=(-307)";
	"-D__DBL_MIN_EXP__=(-1021)";
	"-D__DBL_MIN__=2.2250738585072014e-308";
#if defined(mach_i386) || defined(mach_amd64)
	"-D__LDBL_DIG__=18";
	"-D__LDBL_EPSILON__=1.08420217248550443401e-19L";
	"-D__LDBL_MANT_DIG__=64";
	"-D__LDBL_MAX_10_EXP__=4932";
	"-D__LDBL_MAX_EXP__=16384";
	"-D__LDBL_MAX__=1.18973149535723176502e+4932L";
	"-D__LDBL_MIN_10_EXP__=(-4931)";
	"-D__LDBL_MIN_EXP__=(-16381)";
	"-D__LDBL_MIN__=3.36210314311209350626e-4932L";
#else
	"-D__LDBL_DIG__=15";
	"-D__LDBL_EPSILON__=2.2204460492503131e-16";
	"-D__LDBL_MANT_DIG__=53";
	"-D__LDBL_MAX_10_EXP__=308";
	"-D__LDBL_MAX_EXP__=1024";
	"-D__LDBL_MAX__=1.7976931348623157e+308";
	"-D__LDBL_MIN_10_EXP__=(-307)";
	"-D__LDBL_MIN_EXP__=(-1021)";
	"-D__LDBL_MIN__=2.2250738585072014e-308";
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

	rv = ccmalloc(len+1);
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

	p = cmd = ccmalloc(len);
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
