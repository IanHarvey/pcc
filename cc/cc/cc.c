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
/* C command */

#define FOR_X86
#ifdef FOR_X86
char *cppadd[] = {
	"-D__NetBSD__", "-D__PCC__=1", "-D__PCC_MINOR__=0",
	"-D__ELF__", "-Asystem(unix)", "-Asystem(NetBSD)", "-Acpu(i386)",
	"-Amachine(i386)", "-D__i386__", "-D__OPTIMIZE__", "-Di386",
	"-nostdinc", "-I/usr/include", NULL,
};
#endif

#define SBSIZE 10000
#define MAXINC 100
#define MAXFIL 100
#define MAXLIB 10000
#define MAXOPT 100
char	*tmp0;
char	*tmp1;
char	*tmp2;
char	*tmp3;
char	*tmp4;
char	*tmp5;
char	*outfile;
char *copy(char []),*setsuf(char [], int);
int getsuf(char []);
int main(int, char *[]);
void error(char *, char *);
int nodup(char **, char *);
int callsys(char [], char *[]);
int cunlink(char *);
void dexit(void);
void idexit(int);
# define CHSPACE 1000
char	ts[CHSPACE+50];
char	*tsa = ts;
char	*tsp = ts;
char	*av[50];
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
int	noflflag;
int	exfail;
int	Xflag;
char	*pass0 = "/lib/ccom";
char	*passp = "/usr/libexec/cpp0";
char	*pref = "/usr/lib/crt0.o";

int
main(argc, argv)
char *argv[]; {
	char *t;
	char *savetsp;
	char *assource;
	char **pv, *ptemp[MAXOPT], **pvt;
	int nc, nl, i, j, c, f20, nxo, na, tt;
	FILE *f;

	i = nc = nl = f20 = nxo = 0;
	pv = ptemp;
	while(++i < argc) {
		if(*argv[i] == '-') switch (argv[i][1]) {
		default:
			goto passa;
		case 'X':
			Xflag++;
			break;
		case 'W': /* Ignore W-flags */
		case 'f': /* Ignore -ffreestanding */
		case 'n': /* Ignore -nostdinc */
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
			if (++i < argc) {
				outfile = argv[i];
				if ((tt=getsuf(outfile))=='c'||tt=='o') {
					error("Would overwrite %s", outfile);
					exit(8);
				}
			}
			break;
		case 'O':
			Oflag++;
			break;
		case 'p':
			proflag++;
			break;
		case 'g':
			gflag++;
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

		case '2':
			if(argv[i][2] == '\0')
				pref = "/lib/crt2.o";
			else {
				pref = "/lib/crt20.o";
				f20 = 1;
			}
			break;
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
	if (gflag) Oflag = 0;
	if (noflflag)
		pref = proflag ? "/lib/fmcrt0.o" : "/lib/fcrt0.o";
	else if (proflag)
		pref = "/lib/mcrt0.o";
	if(nc==0)
		goto nocom;
	if (pflag==0) {
		tmp0 = copy("/tmp/ctm0a");
		while((f=fopen(tmp0, "r")) != NULL) {
			fclose(f);
			tmp0[9]++;
		}
		while((creat(tmp0, 0400))<0)
			tmp0[9]++;
	}
	if (signal(SIGINT, SIG_IGN) != SIG_IGN)	/* interrupt */
		signal(SIGINT, idexit);
	if (signal(SIGTERM, SIG_IGN) != SIG_IGN)	/* terminate */
		signal(SIGTERM, idexit);
	(tmp1 = copy(tmp0))[8] = '1';
	(tmp2 = copy(tmp0))[8] = '2';
	(tmp3 = copy(tmp0))[8] = '3';
	if (pflag==0)
		(tmp4 = copy(tmp0))[8] = '4';
	pvt = pv;
	for (i=0; i<nc; i++) {
		if (nc>1)
			printf("%s:\n", clist[i]);
		if (getsuf(clist[i])=='s') {
			assource = clist[i];
			if (!xflag)
				goto assemble;
		} else
			assource = tmp3;
		if (pflag)
			tmp4 = setsuf(clist[i], 'i');
		savetsp = tsp;
		na = 0;
		av[na++] = "cpp";
		av[na++] = xflag ? "-lang-asm" : "-lang-c";
		av[na++] = "-$";
		for (j = 0; cppadd[j]; j++)
			av[na++] = cppadd[j];
		if (tflag)
			av[na++] = "-traditional";
		for(pv=ptemp; pv <pvt; pv++)
			av[na++] = *pv;
		av[na++] = clist[i];
		av[na++] = Eflag ? "-" : tmp4;
		av[na++]=0;
		if (callsys(passp, av))
			{exfail++; eflag++;}
		if (Eflag)
			dexit();
		if (xflag)
			goto assemble;
		av[0]= "ccom";
		av[1] =tmp4;
		tsp = savetsp;
		if (pflag || exfail)
			{
			cflag++;
			continue;
			}
		if(sflag)
			assource = tmp3 = setsuf(clist[i], 's');
		av[2] = tmp3;
		if (proflag) {
			av[3] = "-XP";
			av[4] = 0;
		} else
			av[3] = 0;
		if (gflag) {
			int i;
			i = av[3] ? 4 : 3;
			av[i++] = "-Xg";
			av[i] = 0;
		}
		if (callsys(pass0, av)) {
			cflag++;
			eflag++;
			continue;
		}
		if (sflag)
			continue;
	assemble:
		av[0] = "as";
		av[1] = "-o";
		av[2] = setsuf(clist[i], 'o');
		av[3] = xflag ? tmp4 : assource;
		if (dflag) {
			av[4] = alist;
			av[5] = 0;
		} else
			av[4] = 0;
		cunlink(tmp1);
		cunlink(tmp2);
		if (callsys("/bin/as", av) > 1) {
			cflag++;
			eflag++;
			cunlink(tmp4);
			continue;
		}
		cunlink(tmp4);
	}
nocom:
	if (cflag==0 && nl!=0) {
		i = 0;
		j = 0;
		av[j++] = "ld";
		av[j++] = "-X";
		av[j++] = "-d";
		av[j++] = "-e";
		av[j++] = "__start";
		av[j++] = "-static";
		if (outfile) {
			av[j++] = "-o";
			av[j++] = outfile;
		}
		av[j++] = pref;
		av[j++] = "/usr/lib/crti.o";
		av[j++] = "/usr/lib/crtbegin.o";
		while(i<nl)
			av[j++] = llist[i++];
#if 0
		if (gflag)
			av[j++] = "-lg";
#endif
		av[j++] = "-lc";
		av[j++] = "/usr/lib/crtend.o";
		av[j++] = "/usr/lib/crtn.o";
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
		cunlink(tmp1);
		cunlink(tmp2);
		if (sflag==0)
			cunlink(tmp3);
		cunlink(tmp4);
		cunlink(tmp5);
		cunlink(tmp0);
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
setsuf(as, ch)
char as[];
{
	register char *s, *s1;

	s = s1 = copy(as);
	while(*s)
		if (*s++ == '/')
			s1 = s;
	s[-1] = ch;
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
copy(as)
char as[];
{
	register char *otsp, *s;

	otsp = tsp;
	s = as;
	while((*tsp++ = *s++));
	if (tsp >tsa+CHSPACE)
		{
		tsp = tsa = calloc(CHSPACE+50,1);
		if (tsa== 0){
			error("no space for file names", 0);
			eflag = 8;
			dexit();
			}
		}
	return(otsp);
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
	if (f==0)
		return(0);
	return(unlink(f));
}
