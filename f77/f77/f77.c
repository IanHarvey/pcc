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

char xxxvers[] = "\n FORTRAN 77 DRIVER, VERSION 1.11,   28 JULY 1978\n";

#include <sys/wait.h>

#include <stdio.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>
#include <string.h>
#include <stdlib.h>
#include <stdarg.h>

#include "f77config.h"
#include "defines.h"
#include "ftypes.h"

#include "macdefs.h"

static FILEP diagfile	= {stderr} ;
static int pid;
static int sigivalue	= 0;
static int sigqvalue	= 0;

#ifndef FCOM
#define	FCOM		"fcom"
#endif

static char *fcom	= LIBEXECDIR "/" FCOM ;
static char *asmname	= ASMNAME ;
static char *ldname	= LDNAME ;
static char *footname	= FOOTNAME;
static char *proffoot	= PROFFOOT;
static char *macroname	= "m4";
static char *shellname	= "/bin/sh";
static char *aoutname	= "a.out" ;
static char *liblist[] = LIBLIST;

static char *infname;
static char textfname[15];
static char asmfname[15];
static char asmpass2[15];
static char initfname[15];
static char sortfname[15];
static char prepfname[15];
static char objfdefault[15];
static char optzfname[15];
static char setfname[15];

static char fflags[30]	= "-";
static char cflags[20]	= "-c";
static char eflags[30]	= "";
static char rflags[30]	= "";
static char lflag[3]	= "-x";
static char *fflagp	= fflags+1;
static char *cflagp	= cflags+2;
static char *eflagp	= eflags;
static char *rflagp	= rflags;
static char **loadargs;
static char **loadp;

static flag loadflag	= YES;
static flag saveasmflag	= NO;
static flag profileflag	= NO;
static flag optimflag	= NO;
static flag debugflag	= NO;
static flag verbose	= NO;
static flag nofloating	= NO;
static flag fortonly	= NO;
static flag macroflag	= NO;

char *setdoto(char *), *lastchar(char *), *lastfield(char *);
ptr ckalloc(int);
void intrupt(int);
void enbint(void (*k)(int));
void crfnames(void);
static void fatal1(char *, ...);
void done(int), fatal(char *), texec(char *, char **), rmfiles(void);
char *copys(char *), *copyn(int, char *);
int dotchar(char *), unreadable(char *), sys(char *), dofort(char *);
int nodup(char *), content(char *), dodata(void), dopass2(void);
int await(int);
void rmf(char *), doload(char *[], char *[]), doasm(char *);
LOCAL void fname(char *, char *);
void clf(FILEP *p);
void badfile(char *s);
void err(char *s);
ftnint doeven(ftnint, int);
int rdname(int *vargroupp, char *name);
int rdlong(ftnint *n);
void prspace(ftnint n);
void prch(int c);

int
main(int argc, char **argv)
{
int i, c, status;
register char *s;
char fortfile[20], *t;
char buff[100];

sigivalue = (int) signal(SIGINT, SIG_IGN) & 01;
sigqvalue = (int) signal(SIGQUIT, SIG_IGN) & 01;
enbint(intrupt);

pid = getpid();
crfnames();

loadargs = (char **) ckalloc( (argc+20) * sizeof(*loadargs) );
loadargs[1] = "-X";
loadargs[2] = "-u";
loadargs[3] = "_MAIN__";
loadp = loadargs + 4;

--argc;
++argv;

while(argc>0 && argv[0][0]=='-' && argv[0][1]!='\0')
	{
	for(s = argv[0]+1 ; *s ; ++s) switch(*s)
		{
		case 'T':  /* use special passes */
			switch(*++s)
				{
				case '1':
					fcom = s+1; goto endfor;
				case 'a':
					asmname = s+1; goto endfor;
				case 'l':
					ldname = s+1; goto endfor;
				case 'F':
					footname = s+1; goto endfor;
				case 'm':
					macroname = s+1; goto endfor;
				default:
					fatal1("bad option -T%c", *s);
				}
			break;

		case 'w':
			if(s[1]=='6' && s[2]=='6')
				{
				*fflagp++ = *s++;
				*fflagp++ = *s++;
				}

		copyfflag:
		case 'u':
		case 'U':
		case 'M':
		case '1':
		case 'C':
			*fflagp++ = *s;
			break;

		case 'O':
			optimflag = YES;
			*fflagp++ = 'O';
			if( isdigit(s[1]) )
				*fflagp++ = *++s;
			break;

		case 'm':
			if(s[1] == '4')
				++s;
			macroflag = YES;
			break;

		case 'S':
			saveasmflag = YES;

		case 'c':
			loadflag = NO;
			break;

		case 'v':
			verbose = YES;
			break;

		case 'd':
			debugflag = YES;
			goto copyfflag;

		case 'p':
			profileflag = YES;
			*cflagp++ = 'p';
			goto copyfflag;

		case 'o':
			if( ! strcmp(s, "onetrip") )
				{
				*fflagp++ = '1';
				goto endfor;
				}
			aoutname = *++argv;
			--argc;
			break;

		case 'F':
			fortonly = YES;
			loadflag = NO;
			break;

		case 'I':
			if(s[1]=='2' || s[1]=='4' || s[1]=='s')
				{
				*fflagp++ = *s++;
				goto copyfflag;
				}
			fprintf(diagfile, "invalid flag -I%c\n", s[1]);
			done(1);

		case 'l':	/* letter ell--library */
			s[-1] = '-';
			*loadp++ = s-1;
			goto endfor;

		case 'E':	/* EFL flag argument */
			while(( *eflagp++ = *++s))
				;
			*eflagp++ = ' ';
			goto endfor;
		case 'R':
			while(( *rflagp++ = *++s ))
				;
			*rflagp++ = ' ';
			goto endfor;
		default:
			lflag[1] = *s;
			*loadp++ = copys(lflag);
			break;
		}
endfor:
	--argc;
	++argv;
	}

loadargs[0] = ldname;
#ifdef mach_pdp11
	if(nofloating)
		*loadp++ = (profileflag ? NOFLPROF : NOFLFOOT);
	else
#endif
*loadp++ = (profileflag ? proffoot : footname);

for(i = 0 ; i<argc ; ++i)
	switch(c =  dotchar(infname = argv[i]) )
		{
		case 'r':	/* Ratfor file */
		case 'e':	/* EFL file */
			if( unreadable(argv[i]) )
				break;
			s = fortfile;
			t = lastfield(argv[i]);
			while(( *s++ = *t++))
				;
			s[-2] = 'f';

			if(macroflag)
				{
				sprintf(buff, "%s %s >%s", macroname, infname, prepfname);
				if(sys(buff))
					{
					rmf(prepfname);
					break;
					}
				infname = prepfname;
				}

			if(c == 'e')
				sprintf(buff, "efl %s %s >%s", eflags, infname, fortfile);
			else
				sprintf(buff, "ratfor %s %s >%s", rflags, infname, fortfile);
			status = sys(buff);
			if(macroflag)
				rmf(infname);
			if(status)
				{
				loadflag = NO;
				rmf(fortfile);
				break;
				}

			if( ! fortonly )
				{
				infname = argv[i] = lastfield(argv[i]);
				*lastchar(infname) = 'f';
	
				if( dofort(argv[i]) )
					loadflag = NO;
				else	{
					if( nodup(t = setdoto(argv[i])) )
						*loadp++ = t;
					rmf(fortfile);
					}
				}
			break;

		case 'f':	/* Fortran file */
		case 'F':
			if( unreadable(argv[i]) )
				break;
			if( dofort(argv[i]) )
				loadflag = NO;
			else if( nodup(t=setdoto(argv[i])) )
				*loadp++ = t;
			break;

		case 'c':	/* C file */
		case 's':	/* Assembler file */
			if( unreadable(argv[i]) )
				break;
			fprintf(diagfile, "%s:\n", argv[i]);
			sprintf(buff, "cc -c %s", argv[i] );
			if( sys(buff) )
				loadflag = NO;
			else
				if( nodup(t = setdoto(argv[i])) )
					*loadp++ = t;
			break;

		case 'o':
			if( nodup(argv[i]) )
				*loadp++ = argv[i];
			break;

		default:
			if( ! strcmp(argv[i], "-o") )
				aoutname = argv[++i];
			else
				*loadp++ = argv[i];
			break;
		}

if(loadflag)
	doload(loadargs, loadp);
done(0);
return 0;
}

int
dofort(s)
char *s;
{
int retcode;
char buff[200];

infname = s;
sprintf(buff, "%s %s %s %s %s %s",
	fcom, fflags, s, asmfname, initfname, textfname);
switch( sys(buff) )
	{
	case 1:
		goto error;
	case 0:
		break;
	default:
		goto comperror;
	}

if(content(initfname) > 0)
	if( dodata() )
		goto error;
if( dopass2() )
	goto comperror;
doasm(s);
retcode = 0;

ret:
	rmf(asmfname);
	rmf(initfname);
	rmf(textfname);
	return(retcode);

error:
	fprintf(diagfile, "\nError.  No assembly.\n");
	retcode = 1;
	goto ret;

comperror:
	fprintf(diagfile, "\ncompiler error.\n");
	retcode = 2;
	goto ret;
}




int
dopass2()
{
char buff[100];

if(verbose)
	fprintf(diagfile, "PASS2.");

	sprintf(buff, "%s <%s >%s", "cat", textfname, asmpass2);
	return( sys(buff) );
}



void
doasm(s)
char *s;
{
register char *lastc;
char *obj;
char buff[200];

if(*s == '\0')
	s = objfdefault;
lastc = lastchar(s);
obj = setdoto(s);

#ifdef PASS2OPT
if(optimflag)
	{
	sprintf(buff, "%s %s %s", PASS2OPT, asmpass2, optzfname);
	if( sys(buff) )
		rmf(optzfname);
	else {
		sprintf(buff,"mv %s %s", optzfname, asmpass2);
		sys(buff);
		}
	}
#endif

if(saveasmflag)
	{
	*lastc = 's';
	sprintf(buff, "cat %s %s >%s", asmfname, asmpass2, obj);
	sys(buff);
	*lastc = 'o';
	}
else
	{
	if(verbose)
		fprintf(diagfile, "  ASM.");

	sprintf(buff, "%s -o %s %s %s", asmname, obj, asmfname, asmpass2);

	if( sys(buff) )
		fatal("assembler error");
	if(verbose)
		fprintf(diagfile, "\n");
	}

rmf(asmpass2);
}



void
doload(v0, v)
register char *v0[], *v[];
{
char **p;
int waitpid;

for(p = liblist ; *p ; *v++ = *p++)
	;

*v++ = "-o";
*v++ = aoutname;
*v = NULL;

if(verbose)
	fprintf(diagfile, "LOAD.");
if(debugflag)
	{
	for(p = v0 ; p<v ; ++p)
		fprintf(diagfile, "%s ", *p);
	fprintf(diagfile, "\n");
	}

	if( (waitpid = fork()) == 0)
		{
		enbint(SIG_DFL);
		execv(ldname, v0);
		fatal1("couldn't load %s", ldname);
		}
	await(waitpid);

if(verbose)
	fprintf(diagfile, "\n");
}

/* Process control and Shell-simulating routines */

int
sys(str)
char *str;
{
register char *s, *t;
char *argv[100], path[100];
char *inname, *outname;
int append = 0;
int waitpid;
int argc;


if(debugflag)
	fprintf(diagfile, "%s\n", str);
inname  = NULL;
outname = NULL;
argv[0] = shellname;
argc = 1;

t = str;
while( isspace(*t) )
	++t;
while(*t)
	{
	if(*t == '<')
		inname = t+1;
	else if(*t == '>')
		{
		if(t[1] == '>')
			{
			append = YES;
			outname = t+2;
			}
		else	{
			append = NO;
			outname = t+1;
			}
		}
	else
		argv[argc++] = t;
	while( !isspace(*t) && *t!='\0' )
		++t;
	if(*t)
		{
		*t++ = '\0';
		while( isspace(*t) )
			++t;
		}
	}

if(argc == 1)   /* no command */
	return(-1);
argv[argc] = 0;

s = path;
t = "/usr/bin/";
while(*t)
	*s++ = *t++;
for(t = argv[1] ; (*s++ = *t++) ; )
	;
if((waitpid = fork()) == 0)
	{
	if(inname)
		freopen(inname, "r", stdin);
	if(outname)
		freopen(outname, (append ? "a" : "w"), stdout);
	enbint(SIG_DFL);

	texec(path+9, argv);  /* command */
	texec(path+4, argv);  /*  /bin/command */
	texec(path  , argv);  /* /usr/bin/command */

	fatal1("Cannot load %s",path+9);
	}

return( await(waitpid) );
}





#include <errno.h>

/* modified version from the Shell */
void
texec(f, av)
char *f;
char **av;
{

execv(f, av+1);

if (errno==ENOEXEC)
	{
	av[1] = f;
	execv(shellname, av);
	fatal("No shell!");
	}
if (errno==ENOMEM)
	fatal1("%s: too large", f);
}






void
done(k)
int k;
{
static int recurs	= NO;

if(recurs == NO)
	{
	recurs = YES;
	rmfiles();
	}
exit(k);
}






void
enbint(k)
void (*k)(int);
{
if(sigivalue == 0)
	signal(SIGINT,k);
if(sigqvalue == 0)
	signal(SIGQUIT,k);
}



void
intrupt(int a)
{
done(2);
}


int
await(waitpid)
int waitpid;
{
int w, status;

enbint(SIG_IGN);
while ( (w = wait(&status)) != waitpid)
	if(w == -1)
		fatal("bad wait code");
enbint(intrupt);
if(status & 0377)
	{
	if(status != SIGINT)
		fprintf(diagfile, "Termination code %d", status);
	done(3);
	}
return(status>>8);
}

/* File Name and File Manipulation Routines */

int
unreadable(s)
register char *s;
{
register FILE *fp;

if((fp = fopen(s, "r")))
	{
	fclose(fp);
	return(NO);
	}

else
	{
	fprintf(diagfile, "Error: Cannot read file %s\n", s);
	loadflag = NO;
	return(YES);
	}
}



void
clf(p)
FILEP *p;
{
if(p!=NULL && *p!=NULL && *p!=stdout)
	{
	if(ferror(*p))
		fatal("writing error");
	fclose(*p);
	}
*p = NULL;
}

void
rmfiles()
{
rmf(textfname);
rmf(asmfname);
rmf(initfname);
rmf(asmpass2);
}








/* return -1 if file does not exist, 0 if it is of zero length
   and 1 if of positive length
*/
int
content(filename)
char *filename;
{
#include <sys/types.h>
#include <sys/stat.h>
struct stat buf;
if(stat(filename,&buf) < 0) 
	return(-1);
else
	return( buf.st_size > 0 );
}




void
crfnames()
{
fname(textfname, "x");
fname(asmfname, "s");
fname(asmpass2, "a");
fname(initfname, "d");
fname(sortfname, "S");
fname(objfdefault, "o");
fname(prepfname, "p");
fname(optzfname, "z");
fname(setfname, "A");
}



void
rmf(fn)
register char *fn;
{
if(!debugflag && fn!=NULL && *fn!='\0')
	unlink(fn);
}





LOCAL void fname(name, suff)
char *name, *suff;
{
sprintf(name, "fort%d.%s", pid, suff);
}



int
dotchar(s)
register char *s;
{
for( ; *s ; ++s)
	if(s[0]=='.' && s[1]!='\0' && s[2]=='\0')
		return( s[1] );
return(NO);
}



char *lastfield(s)
register char *s;
{
register char *t;
for(t = s; *s ; ++s)
	if(*s == '/')
		t = s+1;
return(t);
}



char *lastchar(s)
register char *s;
{
while(*s)
	++s;
return(s-1);
}

char *setdoto(s)
register char *s;
{
*lastchar(s) = 'o';
return( lastfield(s) );
}


void
badfile(s)
char *s;
{
fatal1("cannot open intermediate file %s", s);
}



ptr ckalloc(n)
int n;
{
ptr p;

if( (p = calloc(1, (unsigned) n) ))
	return(p);

fatal("out of memory");
/* NOTREACHED */
return NULL;
}




char *
copyn(n, s)
register int n;
register char *s;
{
register char *p, *q;

p = q = (char *) ckalloc(n);
while(n-- > 0)
	*q++ = *s++;
return(p);
}


char *
copys(s)
char *s;
{
return( copyn( strlen(s)+1 , s) );
}




int
nodup(s)
char *s;
{
register char **p;

for(p = loadargs ; p < loadp ; ++p)
	if( !strcmp(*p, s) )
		return(NO);

return(YES);
}



void fatal(t)
char *t;
{
	fatal1(t);
}




static void
fatal1(char *fmt, ...)
{
	va_list ap;

	va_start(ap, fmt);
	fprintf(diagfile, "Compiler error in file %s: ", infname);
	vfprintf(diagfile, fmt, ap);
	fprintf(diagfile, "\n");
	va_end(ap);

	if (debugflag)
		abort();
	done(1);
	exit(1);
}



void
err(s)
char *s;
{
fprintf(diagfile, "Error in file %s: %s\n", infname, s);
}

LOCAL int nch	= 0;
LOCAL FILEP asmfile;
LOCAL FILEP sortfile;

static ftnint typesize[NTYPES]
	= { 1, FSZADDR, FSZSHORT, FSZLONG, FSZLONG, 2*FSZLONG,
	    2*FSZLONG, 4*FSZLONG, FSZLONG, 1, 1, 1};
static int typealign[NTYPES]
	= { 1, ALIADDR, ALISHORT, ALILONG, ALILONG, ALIDOUBLE,
	    ALILONG, ALIDOUBLE, ALILONG, 1, 1, 1};

int
dodata()
{
char buff[50];
char varname[XL+1], ovarname[XL+1];
int status;
flag erred;
ftnint offset, vlen, type;
register ftnint ooffset, ovlen;
ftnint vchar;
int size, align;
int vargroup;
ftnint totlen;

erred = NO;
ovarname[0] = '\0';
ooffset = 0;
ovlen = 0;
totlen = 0;
nch = 0;

sprintf(buff, "sort %s >%s", initfname, sortfname);
if((status = sys( buff) ))
	fatal1("call sort status = %d", status);
if( (sortfile = fopen(sortfname, "r")) == NULL)
	badfile(sortfname);
if( (asmfile = fopen(asmfname, "a")) == NULL)
	badfile(asmfname);
pruse(asmfile, USEINIT);

while( rdname(&vargroup, varname) && rdlong(&offset) && rdlong(&vlen) && rdlong(&type) )
	{
	size = typesize[type];
	if( strcmp(varname, ovarname) )
		{
		prspace(ovlen-ooffset);
		strcpy(ovarname, varname);
		ooffset = 0;
		totlen += ovlen;
		ovlen = vlen;
		if(vargroup == 0)
			align = (type==TYCHAR ? FSZLONG : typealign[type]);
		else	align = ALIDOUBLE;
		totlen = doeven(totlen, align);
		if(vargroup == 2)
			prcomblock(asmfile, varname);
		else
			fprintf(asmfile, FLABELFMT, varname);
		}
	if(offset < ooffset)
		{
		erred = YES;
		err("overlapping initializations");
		}
	if(offset > ooffset)
		{
		prspace(offset-ooffset);
		ooffset = offset;
		}
	if(type == TYCHAR)
		{
		if( ! rdlong(&vchar) )
			fatal("bad intermediate file format");
		prch( (int) vchar );
		}
	else
		{
		putc('\t', asmfile);
		while	( putc( getc(sortfile), asmfile)  != '\n')
			;
		}
	if( (ooffset += size) > ovlen)
		{
		erred = YES;
		err("initialization out of bounds");
		}
	}

prspace(ovlen-ooffset);
totlen = doeven(totlen+ovlen, (ALIDOUBLE>FSZLONG ? ALIDOUBLE : FSZLONG) );
clf(&sortfile);
clf(&asmfile);
clf(&sortfile);
rmf(sortfname);
return(erred);
}



void
prspace(n)
register ftnint n;
{
register ftnint m;

while(nch>0 && n>0)
	{
	--n;
	prch(0);
	}
m = FSZSHORT * (n/FSZSHORT);
if(m > 0)
	prskip(asmfile, m);
for(n -= m ; n>0 ; --n)
	prch(0);
}




ftnint doeven(tot, align)
register ftnint tot;
int align;
{
ftnint new;
new = roundup(tot, align);
prspace(new - tot);
return(new);
}



int
rdname(vargroupp, name)
int *vargroupp;
register char *name;
{
register int i, c;

if( (c = getc(sortfile)) == EOF)
	return(NO);
*vargroupp = c - '0';

for(i = 0 ; i<XL ; ++i)
	{
	if( (c = getc(sortfile)) == EOF)
		return(NO);
	if(c != ' ')
		*name++ = c;
	}
*name = '\0';
return(YES);
}



int
rdlong(n)
register ftnint *n;
{
register int c;

for(c = getc(sortfile) ; c!=EOF && isspace(c) ; c = getc(sortfile) );
	;
if(c == EOF)
	return(NO);

for(*n = 0 ; isdigit(c) ; c = getc(sortfile) )
	*n = 10* (*n) + c - '0';
return(YES);
}



void
prch(c)
register int c;
{
static int buff[FSZSHORT];

buff[nch++] = c;
if(nch == FSZSHORT)
	{
	prchars(asmfile, buff);
	nch = 0;
	}
}
