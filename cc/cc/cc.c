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
 * - Files that end with .s are passed as->ld
 * - Files that end with .o are passed directly to ld
 * - Multiple files may be given on the command line.
 * - Unrecognized options are all sent directly to ld.
 * -c or -S cannot be combined with -o if multiple files are given.
 *
 * This file should be rewritten readable.
 */
#include <sys/types.h>

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/wait.h>

#include "ccconfig.h"
/* C command */

#define	MKS(x) _MKS(x)
#define _MKS(x) #x

/*
 * Many specific definitions, should be declared elsewhere.
 */
#define	PCC_MINOR 0
#define PCC_MAJOR 1
#define	STDINC	  "/usr/include"

#define SBSIZE 10000
#define MAXINC 100
#define MAXFIL 100
#define MAXLIB 10000
#define MAXAV  10000
#define MAXOPT 100
char	*tmp3;
char	*tmp4;
char	*outfile;
char *copy(char *as),*setsuf(char *as, char ch);
int getsuf(char []);
int main(int, char *[]);
void error(char *, char *);
void errorx(char *, char *, int);
int nodup(char **, char *);
int callsys(char [], char *[]);
int cunlink(char *);
void dexit(void);
void idexit(int);
char *gettmp();
# define CHSPACE 1000
char	ts[CHSPACE+50];
char	*tsp = ts;
char	*av[MAXAV];
char	*clist[MAXFIL];
char	*llist[MAXLIB];
char	alist[20];
int dflag;
int	xflag;
int	pflag;
int	sflag;
int	cflag;
int	eflag;
int	gflag;
int	vflag;
int	tflag;
int	Eflag;
int	Oflag;
int	proflag;
int	exfail;
int	Xflag;
int	nostartfiles, Bstatic;
int	nostdinc;

char	*pass0 = LIBEXECDIR "/ccom";
char	*passp = LIBEXECDIR "/cpp";
char	*sysinc;
char *cppadd[] = CPPADD;
char *dynlinker[] = DYNLINKER;
char *crt0file = CRT0FILE;
char *startfiles[] = STARTFILES;
char *endfiles[] = ENDFILES;
char *cppmdadd[] = CPPMDADD;

int
main(int argc, char *argv[])
{
	char *t, *u;
	char *savetsp;
	char *assource;
	char **pv, *ptemp[MAXOPT], **pvt;
	int nc, nl, i, j, c, f20, nxo, na;

	i = nc = nl = f20 = nxo = 0;
	pv = ptemp;
	while(++i < argc) {
		if (argv[i][0] == '-')
		switch (argv[i][1]) {
		default:
			goto passa;

		case 'B': /* other search paths for binaries XXX support? */
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
			}
			break;

		case 'f': /* Ignore -ffreestanding */
			break;

		case 'g': /* create debug output */
			gflag++;
			break;

		case 'i':
			if (strcmp(argv[i], "-isystem") == 0) {
				sysinc = argv[++i];
			} else
				goto passa;
			break;

		case 'n': /* handle -n flags */
			if (strcmp(argv[i], "-nostdinc") == 0)
				nostdinc++;
			else if (strcmp(argv[i], "-nostartfiles") == 0)
				nostartfiles = 1;
			else
				goto passa;
			break;

		case 'x':
			xflag++;
			i++; /* skip args */
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
				errorx("too many -o", "", 8);
			outfile = argv[++i];
			break;
		case 'O':
			Oflag++;
			break;
		case 'p':
			proflag++;
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
				f20 = 1;
			}
			break;
#endif
		case 'D':
		case 'I':
		case 'U':
		case 'C':
			*pv++ = argv[i];
			if (pv >= ptemp+MAXOPT)
				{
				error("Too many DIUC options", 0);
				--pv;
				}
			break;

		case 'd':
			dflag++;
			strncpy(alist, argv[i], 19);
			break;
		case 'v':
			vflag++;
			break;

		case 's':
			if (strcmp(argv[i], "-static") == 0)
				Bstatic = 1;
			else
				goto passa;
			break;
		} else {
		passa:
			t = argv[i];
			if (*argv[i] == '-' && argv[i][1] == 'L')
				;
			else if((c=getsuf(t))=='c' || c=='s'|| Eflag) {
				clist[nc++] = t;
				if (nc>=MAXFIL)
					{
					error("Too many source files",0);
					exit(1);
					}
				t = setsuf(t, 'o');
			}
			if (nodup(llist, t)) {
				llist[nl++] = t;
				if (nl >= MAXLIB)
					{
					error("Too many object/library files",0);
					exit(1);
					}
				if (getsuf(t)=='o')
					nxo++;
			}
		}
	}
	/* Sanity checking */
	if (nc == 0 && nl == 0)
		errorx("no input files", "", 8);
	if (outfile && (cflag || sflag) && nc > 1)
		errorx("-o given with -c || -S and more than one file", "", 8);
	if (outfile && clist[0] && strcmp(outfile, clist[0]) == 0)
		errorx("output file will be clobbered", "", 8);

	if (gflag) Oflag = 0;
#if 0
	if (proflag)
		pref = "/lib/mcrt0.o";
#endif
	if(nc==0)
		goto nocom;
	if (pflag==0) {
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
		if (nc>1)
			printf("%s:\n", clist[i]);
		assource = tmp3;
		if (getsuf(clist[i])=='s') {
			assource = clist[i];
			if (!xflag)
				goto assemble;
		}
		if (pflag)
			tmp4 = setsuf(clist[i], 'i');
		savetsp = tsp;
		na = 0;
		av[na++] = "cpp";
		av[na++] = "-D__PCC__=" MKS(PCC_MAJOR);
		av[na++] = "-D__PCC_MINOR__" MKS(PCC_MINOR);
		if (!nostdinc)
			av[na++] = "-S", av[na++] = STDINC;
		if (sysinc)
			av[na++] = "-S", av[na++] = sysinc;
		for (j = 0; cppadd[j]; j++)
			av[na++] = cppadd[j];
		for (j = 0; cppmdadd[j]; j++)
			av[na++] = cppmdadd[j];
		if (tflag)
			av[na++] = "-t";
		for(pv=ptemp; pv <pvt; pv++)
			av[na++] = *pv;
		av[na++] = clist[i];
		if (!Eflag)
			av[na++] = tmp4;
		av[na++]=0;
		if (callsys(passp, av))
			{exfail++; eflag++;}
		if (Eflag)
			dexit();
		if (xflag)
			goto assemble;

		/*
		 * C compiler
		 */
		na = 0;
		av[na++]= "ccom";
		if (gflag)
			av[na++] = "-g";
		av[na++] = tmp4;
		tsp = savetsp;
		if (pflag || exfail)
			{
			cflag++;
			continue;
			}
		if(sflag) {
			if (outfile)
				assource = tmp3 = outfile;
			else
				assource = tmp3 = setsuf(clist[i], 's');
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
		av[0] = "as";
		av[1] = "-o";
		if (outfile && cflag)
			av[2] = outfile;
		else
			av[2] = setsuf(clist[i], 'o');
		av[3] = xflag ? tmp4 : assource;
		if (dflag) {
			av[4] = alist;
			av[5] = 0;
		} else
			av[4] = 0;
		if (callsys("/bin/as", av)) {
			cflag++;
			eflag++;
			cunlink(tmp4);
			continue;
		}
		cunlink(tmp4);
	}

	/*
	 * Linker
	 */
nocom:
	if (cflag==0 && nl!=0) {
		j = 0;
		av[j++] = "ld";
		av[j++] = "-X";
		av[j++] = "-d";
		av[j++] = "-e";
		av[j++] = "__start";
		if (Bstatic == 0) { /* Dynamic linkage */
			for (i = 0; dynlinker[i]; i++)
				av[j++] = dynlinker[i];
		}
		if (outfile) {
			av[j++] = "-o";
			av[j++] = outfile;
		}
		if (!nostartfiles) {
			av[j++] = crt0file;
			for (i = 0; startfiles[i]; i++)
				av[j++] = startfiles[i];
		}
		i = 0;
		while(i<nl) {
			av[j++] = llist[i++];
			if (j >= MAXAV)
				error("Too many ld options", 0);
		}
#if 0
		if (gflag)
			av[j++] = "-lg";
#endif
		av[j++] = "-lc";
		if (!nostartfiles) {
			for (i = 0; endfiles[i]; i++)
				av[j++] = endfiles[i];
		}
		if(f20)
			av[j++] = "-l2";
		av[j++] = 0;
		eflag |= callsys("/bin/ld", av);
		if (nc==1 && nxo==1 && eflag==0)
			cunlink(setsuf(clist[0], 'o'));
	}
	dexit();
	return 0;
}

void
idexit(int arg)
{
	eflag = 100;
	dexit();
}

void
dexit()
{
	if (!pflag && !Xflag) {
		if (sflag==0)
			cunlink(tmp3);
		cunlink(tmp4);
	}
	exit(eflag);
}

void
error(char *s, char *x)
{
	fprintf(Eflag?stderr:stdout , s, x);
	putc('\n', Eflag? stderr : stdout);
	exfail++;
	cflag++;
	eflag++;
}

void
errorx(char *s, char *x, int eval)
{
	error(s, x);
	exit(eval);
}

int
getsuf(as)
char as[];
{
	register char *s;

	if ((s = strrchr(as, '.')) && s[1] != '\0' && s[2] == '\0')
		return s[1];
	return(0);
}

char *
setsuf(char *as, char ch)
{
	char *s, *s1;

	s = s1 = as; 
	while(*s)
		if (*s++ == '/')
			s1 = s;

	s = copy(s1);
	s[strlen(s) - 1] = ch;
	return(s1);
}

int
callsys(f, v)
char f[], *v[]; {
	int t, status;
	char *s;

	if (vflag) {
		for (t = 0; v[t]; t++)
			fprintf(stderr, "%s ", v[t]);
		fprintf(stderr, "\n");
	}

	if ((t=fork())==0) {
		execv(f, v);
		if ((s = strrchr(f, '/')))
			execvp(s+1, v);
		printf("Can't find %s\n", f);
		exit(100);
	} else
		if (t == -1) {
			printf("Try again\n");
			return(100);
		}
	while(t!=wait(&status));
	if ((t=(status&0377)) != 0 && t!=14) {
		if (t!=2)		/* interrupt */
			{
			printf("Fatal error in %s\n", f);
			eflag = 8;
			}
		dexit();
	}
	return((status>>8) & 0377);
}

char *
copy(char *as)
{
	char *p;

	if ((p = strdup(as)) == NULL) {
		error("no space for file names", 0);
		eflag = 8;
		dexit();
	}

	return p;
}

int
nodup(l, os)
char **l, *os;
{
	register char *t, *s;
	register int c;

	s = os;
	if (getsuf(s) != 'o')
		return(1);
	while((t = *l++)) {
		while((c = *s++))
			if (c != *t++)
				break;
		if (*t=='\0' && c=='\0')
			return(0);
		s = os;
	}
	return(1);
}

int
cunlink(f)
char *f;
{
	if (f==0 || Xflag)
		return(0);
	return(unlink(f));
}

char *
gettmp()
{
	char *sfn = strdup("/tmp/ctm.XXXXXX");
	int fd = -1;

	if ((fd = mkstemp(sfn)) == -1) {
		fprintf(stderr, "%s: %s\n", sfn, strerror(errno));
		exit(8);
	}
	close(fd);

	return sfn;
}
