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
char xxxvers[] = "\nFORTRAN 77 PASS 1, VERSION 1.16,  3 NOVEMBER 1978\n";

#include "macdefs.h"

#include "ftypes.h"
#include "defines.h"
#include "defs.h"

static FILEP opf(char *);
LOCAL void clfiles(void);

int
main(int argc, char **argv)
{
char *s;
int k, retcode;

#define DONE(c)	{ retcode = c; goto finis; }

--argc;
++argv;

while(argc>0 && argv[0][0]=='-')
	{
	for(s = argv[0]+1 ; *s ; ++s) switch(*s)
		{
		case 'w':
			if(s[1]=='6' && s[2]=='6')
				{
				ftn66flag = YES;
				s += 2;
				}
			else
				nowarnflag = YES;
			break;

		case 'U':
			shiftcase = NO;
			break;

		case 'u':
			undeftype = YES;
			break;

		case 'O':
			optimflag = YES;
			if( isdigit((int)s[1]) )
				{
				k = *++s - '0';
				if(k > MAXREGVAR)
					{
					warn1("-O%d: too many register variables", k);
					maxregvar = MAXREGVAR;
					}
				else
					maxregvar = k;
				}
			break;

		case 'd':
			debugflag = YES;
			break;

		case 'p':
			profileflag = YES;
			break;

		case 'C':
			checksubs = YES;
			break;

		case '1':
			onetripflag = YES;
			break;

		case 'I':
			if(*++s == '2')
				tyint = TYSHORT;
			else if(*s == '4')
				{
				shortsubs = NO;
				tyint = TYLONG;
				}
			else if(*s == 's')
				shortsubs = YES;
			else
				fatal1("invalid flag -I%c\n", *s);
			tylogical = tyint;
			break;

		default:
			fatal1("invalid flag %c\n", *s);
		}
	--argc;
	++argv;
	}

if(argc != 4)
	fatal1("arg count %d", argc);
asmfile  = opf(argv[1]);
initfile = opf(argv[2]);
textfile = opf(argv[3]);

initkey();
if(inilex( copys(argv[0]) ))
	DONE(1);
fprintf(diagfile, "%s:\n", argv[0]);
fileinit();
procinit();
if((k = yyparse()))
	{
	fprintf(diagfile, "Bad parse, return code %d\n", k);
	DONE(1);
	}
if(nerr > 0)
	DONE(1);
if(parstate != OUTSIDE)
	{
	warn("missing END statement");
	endproc();
	}
doext();
preven(ALIDOUBLE);
prtail();
	puteof();
DONE(0);


finis:
	done(retcode);
	return(retcode);
}


void
done(k)
int k;
{
static int recurs	= NO;

if(recurs == NO)
	{
	recurs = YES;
	clfiles();
	}
exit(k);
}


LOCAL FILEP opf(fn)
char *fn;
{
FILEP fp;
if(( fp = fopen(fn, "w") ))
	return(fp);

fatal1("cannot open intermediate file %s", fn);
/* NOTREACHED */
return 0; /* XXX GCC */
}



LOCAL void
clfiles()
{
clf(&textfile);
clf(&asmfile);
clf(&initfile);
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

