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

#define MAXFIL 10000
#define MAXLIB 10000
#define MAXAV  10000
#define MAXOPT 100
char	*tmp3;
char	*tmp4;
char	*outfile;
char *copy(char *),*setsuf(char *, char);
int getsuf(char *);
int main(int, char *[]);
void error(char *, ...);
void errorx(int, char *, ...);
int callsys(char [], char *[]);
int cunlink(char *);
void dexit(int);
void idexit(int);
char *gettmp();
char	*av[MAXAV];
char	*clist[MAXFIL];
char	*llist[MAXLIB];
char	alist[20];
char	*xlist[100];
int	xnum;
char	*mlist[100];
char	*idirafter;
int	nm;
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
int	Werror;
int	nostartfiles, Bstatic, shared;
int	nostdinc, nostdlib;
int	onlyas;
int	pthreads;

char	*passp = LIBEXECDIR "/" PREPROCESSOR;
char	*pass0 = LIBEXECDIR "/" COMPILER;
char	*as = ASSEMBLER;
char	*ld = LINKER;
char	*Bflag;
char *cppadd[] = CPPADD;
char *dynlinker[] = DYNLINKER;
char *crt0file = CRT0FILE;
char *crt0file_profile = CRT0FILE_PROFILE;
char *startfiles[] = STARTFILES;
char *endfiles[] = ENDFILES;
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

int
main(int argc, char *argv[])
{
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
				if (strncmp(argv[i], "-Werror", 7) == 0) {
					Werror = 1;
				} else if (strncmp(argv[i], "-Wl,", 4) == 0) {
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
				}
				break;

			case 'f': /* GCC compatibility flags */
				if (strcmp(argv[i], "-fPIC") == 0)
					kflag = F_PIC;
				if (strcmp(argv[i], "-fpic") == 0)
					kflag = F_pic;
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
				if (strcmp(argv[i], "-pg") == 0)
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
					; /* default */
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
				if (argv[i][2] != '0')
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
			    c=='s'|| Eflag) {
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
		if (!nostdinc)
			av[na++] = "-S", av[na++] = STDINC;
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
		if (vflag)
			av[na++] = "-v";
		if (pgflag)
			av[na++] = "-p";
		if (gflag)
			av[na++] = "-g";
#ifdef MACHOABI
		/* darwin always wants PIC compilation */
		av[na++] = "-k";
#else
		if (kflag)
			av[na++] = "-k";
#endif
		if (Werror)
			av[na++] = "-Werror";
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
		if (vflag)
			av[j++] = "-v";
		av[j++] = "-X";
		if (shared) {
			av[j++] = "-shared";
		} else {
			av[j++] = "-d";
			av[j++] = "-e";
			av[j++] = STARTLABEL;
			if (Bstatic == 0) { /* Dynamic linkage */
				for (i = 0; dynlinker[i]; i++)
					av[j++] = dynlinker[i];
			} else
				av[j++] = "-Bstatic";
		}
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
				av[j++] = pgflag ? crt0file_profile : crt0file;
				for (i = 0; startfiles[i]; i++)
					av[j++] = startfiles[i];
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
			if (!nostartfiles) {
				for (i = 0; endfiles[i]; i++)
					av[j++] = endfiles[i];
			}
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
getsuf(char *as)
{
	register char *s;

	if ((s = strrchr(as, '.')) && s[1] != '\0' && s[2] == '\0')
		return s[1];
	return(0);
}

/*
 * Get basename of string s and change its suffix to ch.
 */
char *
setsuf(char *s, char ch)
{
	s = copy(basename(s));
	s[strlen(s) - 1] = ch;
	return(s);
}


int
callsys(char *f, char *v[])
{
	int t, status = 0;
#ifndef WIN32
	pid_t p;
#endif
	char *s;

	if (vflag) {
		fprintf(stderr, "%s ", f);
		for (t = 1; v[t]; t++)
			fprintf(stderr, "%s ", v[t]);
		fprintf(stderr, "\n");
	}

#ifdef WIN32
	{
	STARTUPINFO si;
	PROCESS_INFORMATION pi;
	DWORD exitCode;
	BOOL ok;

	// Set up the start up info struct.
	ZeroMemory(&si, sizeof(STARTUPINFO));
	si.cb = sizeof(STARTUPINFO);
	GetStartupInfo(&si);
	ok = CreateProcess(f,  // the executable program
		NULL,   // the command line arguments
		NULL,       // ignored
		NULL,       // ignored
		TRUE,       // inherit handles
		DETACHED_PROCESS | HIGH_PRIORITY_CLASS,
		NULL,       // ignored
		NULL,       // ignored
		&si,
		&pi);

	if (!ok) {
		printf("Try Again\n");
		return 100;
	}

	GetExitCodeProcess(pi.hProcess, &exitCode);
	return (exitCode != 0);
	}
#else

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
		dexit(eflag);
	errorx(8, "Fatal error in %s", f);
#endif
}

char *
copy(char *as)
{
	char *p;

	if ((p = strdup(as)) == NULL)
		errorx(8, "no space for file names");

	return p;
}

int
cunlink(char *f)
{
	if (f==0 || Xflag)
		return(0);
	return(unlink(f));
}

char *
gettmp(void)
{
#ifdef WIN32
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
	return copy(tempFilename);
#else
	char *sfn = copy("/tmp/ctm.XXXXXX");
	int fd = -1;

	if ((fd = mkstemp(sfn)) == -1) {
		fprintf(stderr, "%s: %s\n", sfn, strerror(errno));
		exit(8);
	}
	close(fd);
	return sfn;
#endif
}
