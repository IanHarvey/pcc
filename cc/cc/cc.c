/*	$Id$	*/
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
 * - Files that end with .s are passed as->ld
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

#ifdef WIN32
#include <windows.h>
#include <process.h>
#include <io.h>
#endif

#include "compat.h"

#include "ccconfig.h"
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
#define COMPILER	"ccom";
#endif

#ifndef ASSEMBLER
#define ASSEMBLER	"as"
#endif

#ifndef LINKER
#define LINKER		"ld"
#endif

#define OS MKS(TARGOS)
#define MACH MKS(TARGMACH)
#ifndef PCCINCDIR
#define PCCINCDIR	LIBDIR "pcc/" MACH "-" OS "/" PACKAGE_VERSION "/include"
#endif
#ifndef PCCLIBDIR
#define PCCLIBDIR	LIBDIR "pcc/" MACH "-" OS "/" PACKAGE_VERSION "/lib"
#endif

#define MAXFIL 10000
#define MAXLIB 10000
#define MAXAV  10000
#define MAXOPT 100
char	*tmp3;
char	*tmp4;
char	*outfile;
char *copy(char *, int),*setsuf(char *, char);
int getsuf(char *);
int main(int, char *[]);
void error(char *, ...);
void errorx(int, char *, ...);
int callsys(char [], char *[]);
int cunlink(char *);
void dexit(int);
void idexit(int);
char *gettmp(void);
void *ccmalloc(int size);
char	*av[MAXAV];
char	*clist[MAXFIL];
char	*llist[MAXLIB];
char	alist[20];
char	*xlist[100];
int	xnum;
char	*mlist[100];
char	*flist[100];
char	*wlist[100];
char	*idirafter;
int	nm;
int	nf;
int	nw;
int	sspflag;
int	Cflag;
int	dflag;
int	pflag;
int	sflag;
int	cflag;
int	eflag;
int	gflag;
int	vflag;
int	tflag;
int	Eflag;
int	Oflag;
int	kflag;	/* generate PIC/pic code */
#define F_PIC	1
#define F_pic	2
int	Mflag;	/* dependencies only */
int	pgflag;
int	exfail;
int	Xflag;
int	nostartfiles, Bstatic, shared;
int	nostdinc, nostdlib;
int	onlyas;
int	pthreads;
int	xcflag;

char	*passp = LIBEXECDIR "/" PREPROCESSOR;
char	*pass0 = LIBEXECDIR "/" COMPILER;
char	*as = ASSEMBLER;
char	*ld = LINKER;
char	*Bflag;
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

/* handle gcc warning emulations */
struct Wflags {
	char *name;
	int flags;
#define	INWALL		1
#define	NEGATIVE	2
} Wflags[] = {
	{ "-Werror", 0 },
	{ "-Wshadow", 0 },
	{ "-Wno-shadow", NEGATIVE },
	{ "-Wno-pointer-sign", NEGATIVE },
	{ "-Wsign-compare", 0 },
	{ "-Wno-sign-compare", NEGATIVE },
	{ "-Wunknown-pragmas", INWALL },
	{ "-Wno-unknown-pragmas", NEGATIVE },
	{ "-Wunreachable-code", 0 },
	{ "-Wno-unreachable-code", NEGATIVE },
	{ 0, 0 },
};

#define	SZWFL	(sizeof(Wflags)/sizeof(Wflags[0]))

int
main(int argc, char *argv[])
{
	struct Wflags *Wf;
	char *t, *u;
	char *assource;
	char **pv, *ptemp[MAXOPT], **pvt;
	int nc, nl, i, j, c, nxo, na;

	i = nc = nl = nxo = 0;
	pv = ptemp;
	while(++i < argc) {
		if (argv[i][0] == '-') {
			switch (argv[i][1]) {
			default:
				goto passa;
#ifdef notyet
	/* must add library options first (-L/-l/...) */
				error("unrecognized option `-%c'", argv[i][1]);
				break;
#endif

			case 'B': /* other search paths for binaries */
				Bflag = &argv[i][2];
				break;

			case 'X':
				Xflag++;
				break;
			case 'W': /* Ignore (most of) W-flags */
				if (strncmp(argv[i], "-Wl,", 4) == 0) {
					/* options to the linker */
					t = &argv[i][4];
					while ((u = strchr(t, ','))) {
						*u++ = 0;
						llist[nl++] = t;
						t = u;
					}
					llist[nl++] = t;
				} else if (strncmp(argv[i], "-Wp,", 4) == 0) {
					/* preprocessor */
					if (!strncmp(argv[i], "-Wp,-C", 6))
						Cflag++;
				} else if (strcmp(argv[i], "-Wall") == 0) {
					/* Set only the same flags as gcc */
					for (Wf = Wflags; Wf->name; Wf++) {
						if (Wf->flags != INWALL)
							continue;
						wlist[nw++] = Wf->name;
					}
				} else if (strcmp(argv[i], "-WW") == 0) {
					/* set all positive flags */
					for (Wf = Wflags; Wf->name; Wf++) {
						if (Wf->flags == NEGATIVE)
							continue;
						wlist[nw++] = Wf->name;
					}
				} else {
					/* check and set if available */
					for (Wf = Wflags; Wf->name; Wf++) {
						if (strcmp(argv[i], Wf->name))
							continue;
						wlist[nw++] = Wf->name;
					}
				}
				break;

			case 'f': /* GCC compatibility flags */
				if (strcmp(argv[i], "-fPIC") == 0)
					kflag = F_PIC;
				else if (strcmp(argv[i], "-fpic") == 0)
					kflag = F_pic;
				else if (strcmp(argv[i],
				    "-fsigned-char") == 0)
					flist[nf++] = argv[i];
				else if (strcmp(argv[i],
				    "-fno-signed-char") == 0)
					flist[nf++] = argv[i];
				else if (strcmp(argv[i],
				    "-funsigned-char") == 0)
					flist[nf++] = argv[i];
				else if (strcmp(argv[i],
				    "-fno-unsigned-char") == 0)
					flist[nf++] = argv[i];
				else if (strcmp(argv[i],
				    "-fstack-protector") == 0) {
					flist[nf++] = argv[i];
					sspflag++;
				} else if (strcmp(argv[i],
				    "-fno-stack-protector") == 0) {
					flist[nf++] = argv[i];
					sspflag = 0;
				}
				/* silently ignore the rest */
				break;

			case 'g': /* create debug output */
				gflag++;
				break;

			case 'i':
				if (strcmp(argv[i], "-isystem") == 0) {
					*pv++ = "-S";
					*pv++ = argv[++i];
				} else if (strcmp(argv[i], "-include") == 0) {
					*pv++ = "-i";
					*pv++ = argv[++i];
				} else if (strcmp(argv[i], "-idirafter") == 0) {
					idirafter = argv[++i];
				} else
					goto passa;
				break;

			case 'k': /* generate PIC code */
				kflag = F_pic;
				break;

			case 'm': /* target-dependent options */
				mlist[nm++] = argv[i];
				break;

			case 'n': /* handle -n flags */
				if (strcmp(argv[i], "-nostdinc") == 0)
					nostdinc++;
				else if (strcmp(argv[i], "-nostdlib") == 0) {
					nostdlib++;
					nostartfiles++;
				} else if (strcmp(argv[i], "-nostartfiles") == 0)
					nostartfiles = 1;
				else
					goto passa;
				break;

			case 'p':
				if (strcmp(argv[i], "-pg") == 0 ||
				    strcmp(argv[i], "-p") == 0)
					pgflag++;
				else if (strcmp(argv[i], "-pthread") == 0)
					pthreads++;
				else if (strcmp(argv[i], "-pipe") == 0) {
				} else
					errorx(1, "unknown option %s", argv[i]);
				break;

			case 'x':
				t = &argv[i][2];
				if (*t == 0)
					t = argv[++i];
				if (strcmp(t, "c") == 0)
					xcflag = 1; /* default */
#ifdef notyet
				else if (strcmp(t, "c++")
					cxxflag++;
#endif
				else
					xlist[xnum++] = argv[i];
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
				outfile = argv[++i];
				break;
			case 'O':
				if (argv[i][2] == '0')
					Oflag = 0;
				else
					Oflag++;
				break;
			case 'E':
				Eflag++;
				break;
			case 'P':
				pflag++;
				*pv++ = argv[i];
			case 'c':
				cflag++;
				break;

#if 0
			case '2':
				if(argv[i][2] == '\0')
					pref = "/lib/crt2.o";
				else {
					pref = "/lib/crt20.o";
				}
				break;
#endif
			case 'C':
				Cflag = 1;
				break;
			case 'D':
			case 'I':
			case 'U':
				*pv++ = argv[i];
				if (argv[i][2] == 0)
					*pv++ = argv[++i];
				if (pv >= ptemp+MAXOPT) {
					error("Too many DIU options");
					--pv;
				}
				break;

			case 'M':
				Mflag++;
				break;

			case 'd':
				dflag++;
				strlcpy(alist, argv[i], sizeof (alist));
				break;
			case 'v':
				printf("%s\n", VERSSTR);
				vflag++;
				break;

			case 's':
				if (strcmp(argv[i], "-static") == 0)
					Bstatic = 1;
				else if (strcmp(argv[i], "-shared") == 0) {
					shared = 1;
					nostdlib = 1;
				} else
					goto passa;
				break;
			}
		} else {
		passa:
			t = argv[i];
			if (*argv[i] == '-' && argv[i][1] == 'L')
				;
			else if((c=getsuf(t))=='c' || c=='S' || c=='i' ||
			    c=='s'|| Eflag || xcflag) {
				clist[nc++] = t;
				if (nc>=MAXFIL) {
					error("Too many source files");
					exit(1);
				}
				t = setsuf(t, 'o');
			}

			/* Check for duplicate .o files. */
			for (j = getsuf(t) == 'o' ? 0 : nl; j < nl; j++) {
				if (strcmp(llist[j], t) == 0)
					break;
			}
			if (j == nl) {
				llist[nl++] = t;
				if (nl >= MAXLIB) {
					error("Too many object/library files");
					exit(1);
				}
				if (getsuf(t)=='o')
					nxo++;
			}
		}
	}
	/* Sanity checking */
	if (nc == 0 && nl == 0)
		errorx(8, "no input files");
	if (outfile && (cflag || sflag || Eflag) && nc > 1)
		errorx(8, "-o given with -c || -E || -S and more than one file");
	if (outfile && clist[0] && strcmp(outfile, clist[0]) == 0)
		errorx(8, "output file will be clobbered");
	if (gflag) Oflag = 0;
#if 0
	if (proflag)
		pref = "/lib/mcrt0.o";
#endif
	if(nc==0)
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
	pvt = pv;
	for (i=0; i<nc; i++) {
		/*
		 * C preprocessor
		 */
		if (nc>1 && !Eflag)
			printf("%s:\n", clist[i]);
		onlyas = 0;
		assource = tmp3;
		if (getsuf(clist[i])=='i') {
			if(Eflag)
				continue;
			goto com;
		} else if (getsuf(clist[i])=='s') {
			assource = clist[i];
			goto assemble;
		} else if (getsuf(clist[i])=='S')
			onlyas = 1;
		if (pflag)
			tmp4 = setsuf(clist[i], 'i');
		na = 0;
		av[na++] = "cpp";
		if (vflag)
			av[na++] = "-v";
		av[na++] = "-D__PCC__=" MKS(PCC_MAJOR);
		av[na++] = "-D__PCC_MINOR__=" MKS(PCC_MINOR);
		av[na++] = "-D__PCC_MINORMINOR__=" MKS(PCC_MINORMINOR);
		if (getsuf(clist[i])=='S')
			av[na++] = "-D__ASSEMBLER__";
		if (sspflag)
			av[na++] = "-D__SSP__=1";
		if (pthreads)
			av[na++] = "-D_PTHREADS";
		if (Cflag)
			av[na++] = "-C";
		if (Mflag)
			av[na++] = "-M";
		if (dflag)
			av[na++] = alist;
		for (j = 0; cppadd[j]; j++)
			av[na++] = cppadd[j];
		for (j = 0; cppmdadd[j]; j++)
			av[na++] = cppmdadd[j];
		if (tflag)
			av[na++] = "-t";
		for(pv=ptemp; pv <pvt; pv++)
			av[na++] = *pv;
		if (!nostdinc) {
			av[na++] = "-S", av[na++] = STDINC;
			av[na++] = "-I" PCCINCDIR;
		}
		if (idirafter) {
			av[na++] = "-I";
			av[na++] = idirafter;
		}
		av[na++] = clist[i];
		if (!Eflag && !Mflag)
			av[na++] = tmp4;
		if (Eflag && outfile)
			 av[na++] = outfile;
		av[na++]=0;
		if (callsys(passp, av))
			{exfail++; eflag++;}
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
		av[na++]= "ccom";
		for (j = 0; j < nw; j++)
			av[na++] = wlist[j];
		for (j = 0; j < nf; j++)
			av[na++] = flist[j];
		if (vflag)
			av[na++] = "-v";
		if (pgflag)
			av[na++] = "-p";
		if (gflag)
			av[na++] = "-g";
#ifdef os_darwin
		/* darwin always wants PIC compilation */
		av[na++] = "-k";
#else
		if (kflag)
			av[na++] = "-k";
#endif
		if (Oflag) {
			av[na++] = "-xtemps";
			av[na++] = "-xdeljumps";
		}
		for (j = 0; j < xnum; j++)
			av[na++] = xlist[j];
		for (j = 0; j < nm; j++)
			av[na++] = mlist[j];
		if (getsuf(clist[i])=='i')
			av[na++] = clist[i];
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
				tmp3 = setsuf(clist[i], 's');
		}
		av[na++] = tmp3;
#if 0
		if (proflag) {
			av[3] = "-XP";
			av[4] = 0;
		} else
			av[3] = 0;
#endif
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
#if defined(os_sunos) && defined(mach_sparc64)
		av[na++] = "-m64";
#endif
		if (vflag)
			av[na++] = "-v";
		if (kflag)
			av[na++] = "-k";
		av[na++] = "-o";
		if (outfile && cflag)
			av[na++] = outfile;
		else
			av[na++] = setsuf(clist[i], 'o');
		av[na++] = assource;
		if (dflag)
			av[na++] = alist;
		av[na++] = 0;
		if (callsys(as, av)) {
			cflag++;
			eflag++;
			cunlink(tmp4);
			continue;
		}
		cunlink(tmp4);
	}

	if (Eflag || Mflag)
		dexit(eflag);

	/*
	 * Linker
	 */
nocom:
	if (cflag==0 && nl!=0) {
		j = 0;
		av[j++] = ld;
#ifndef os_win32
		if (vflag)
			av[j++] = "-v";
#ifndef os_sunos
		av[j++] = "-X";
#endif
		if (shared) {
			av[j++] = "-shared";
#ifndef os_sunos
		} else {
#ifndef os_darwin
			av[j++] = "-d";
#endif
			av[j++] = "-e";
			av[j++] = STARTLABEL;
#endif
			if (Bstatic == 0) { /* Dynamic linkage */
#ifdef DYNLINKER
				for (i = 0; dynlinker[i]; i++)
					av[j++] = dynlinker[i];
#endif
			} else
				av[j++] = "-Bstatic";
		}
#endif
		if (outfile) {
			av[j++] = "-o";
			av[j++] = outfile;
		}
		if (shared) {
#ifdef STARTFILES_S
			for (i = 0; startfiles_S[i]; i++)
				av[j++] = startfiles_S[i];
#endif
		} else {
			if (!nostartfiles) {
#ifdef CRT0FILE_PROFILE
				if (pgflag)
				{
					av[j++] = crt0file_profile;
				}
				else
#endif
				{
#ifdef CRT0FILE
					av[j++] = crt0file;
#endif
				}
#ifdef STARTFILES
				for (i = 0; startfiles[i]; i++)
					av[j++] = startfiles[i];
#endif
			}
		}
		i = 0;
		while(i<nl) {
			av[j++] = llist[i++];
			if (j >= MAXAV)
				error("Too many ld options");
		}
#ifndef MACHOABI
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
			av[j++] = "-L" PCCLIBDIR;
			if (pgflag) {
				for (i = 0; libclibs_profile[i]; i++)
					av[j++] = libclibs_profile[i];
			} else {
				for (i = 0; libclibs[i]; i++)
					av[j++] = libclibs[i];
			}
		}
		if (shared) {
#ifdef STARTFILES_S
			for (i = 0; endfiles_S[i]; i++)
				av[j++] = endfiles_S[i];
#endif
		} else {
#ifdef STARTFILES
			if (!nostartfiles) {
				for (i = 0; endfiles[i]; i++)
					av[j++] = endfiles[i];
			}
#endif
		}
		av[j++] = 0;
		eflag |= callsys(ld, av);
		if (nc==1 && nxo==1 && eflag==0)
			cunlink(setsuf(clist[0], 'o'));
		else if (nc > 0 && eflag == 0) {
			/* remove .o files XXX ugly */
			for (i = 0; i < nc; i++)
				cunlink(setsuf(clist[i], 'o'));
		}
	}
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

#ifdef WIN32
int
callsys(char *f, char *v[])
{
	int t, status = 0;
	char cmd[MAX_PATH];
	int len;
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	DWORD exitCode;
	BOOL ok;

	len = strlcpy(cmd, f, MAX_PATH);
	for (t = 1; v[t] && len < MAX_PATH; t++) {
		len = strlcat(cmd, " ", MAX_PATH);
		len = strlcat(cmd, v[t], MAX_PATH);
	}

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
		printf("Try Again\n");
		return 100;
	}

	WaitForSingleObject(pi.hProcess, INFINITE);
	GetExitCodeProcess(pi.hProcess, &exitCode);
	return (exitCode != 0);
}

#else

int
callsys(char *f, char *v[])
{
	int t, status = 0;
	pid_t p;
	char *s;

	if (vflag) {
		fprintf(stderr, "%s ", f);
		for (t = 1; v[t]; t++)
			fprintf(stderr, "%s ", v[t]);
		fprintf(stderr, "\n");
	}

	if ((p = fork()) == 0) {
		if (Bflag) {
			size_t len = strlen(Bflag) + 8;
			char *a = malloc(len);
			if (a == NULL) {
				error("callsys: malloc failed");
				exit(1);
			}
			if ((s = strrchr(f, '/'))) {
				strlcpy(a, Bflag, len);
				strlcat(a, s, len);
				execv(a, v);
			}
		}
		execvp(f, v);
		if ((s = strrchr(f, '/')))
			execvp(s+1, v);
		fprintf(stderr, "Can't find %s\n", f);
		_exit(100);
	} else {
		if (p == -1) {
			printf("Try again\n");
			return(100);
		}
	}
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

/*
 * Make a copy of string as, mallocing extra bytes in the string.
 */
char *
copy(char *s, int extra)
{
	int len = strlen(s)+1;
	char *rv;

	rv = ccmalloc(len+extra);
	strlcpy(rv, s, len);
	return rv;
}

int
cunlink(char *f)
{
	if (f==0 || Xflag)
		return(0);
	return(unlink(f));
}

#ifdef WIN32
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
