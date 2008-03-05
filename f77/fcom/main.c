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

#include <unistd.h>

#include "ftypes.h"
#include "defines.h"
#include "defs.h"

static FILEP opf(char *);
LOCAL void clfiles(void);
void mkdope(void);

int f2debug, e2debug, odebug, rdebug, b2debug, c2debug, t2debug;
int s2debug, udebug, x2debug, nflag, kflag;
int xdeljumps, xtemps, xssaflag;




int
main(int argc, char **argv)
{
	int ch;
	int k, retcode;

#define DONE(c)	{ retcode = c; goto finis; }

	while ((ch = getopt(argc, argv, "w:UuOdpC1I:Z:")) != -1)
		switch (ch) {
		case 'w':
			if(optarg[0]=='6' && optarg[1]=='6') {
				ftn66flag = YES;
			} else
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
#ifdef notdef
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
#endif
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
			if(*optarg == '2')
				tyint = TYSHORT;
			else if(*optarg == '4') {
				shortsubs = NO;
				tyint = TYLONG;
			} else if(*optarg == 's')
				shortsubs = YES;
			else
				fatal1("invalid flag -I%c\n", *optarg);
			tylogical = tyint;
			break;

		case 'Z':
			while (*optarg)
				switch (*optarg++) {
				case 'f': /* instruction matching */
					++f2debug;
					break;
				case 'e': /* print tree upon pass2 enter */
					++e2debug;
					break;
				case 'o': ++odebug; break;
				case 'r': /* register alloc/graph coloring */
					++rdebug;
					break;
				case 'b': /* basic block and SSA building */
					++b2debug;
					break;
				case 'c': /* code printout */
					++c2debug;
					break;
				case 't': ++t2debug; break;
				case 's': /* shape matching */
					++s2debug;
					break;
				case 'u': /* Sethi-Ullman debugging */
					++udebug;
					break;
				case 'x': ++x2debug; break;
				case 'n': ++nflag; break;
				default:
					fprintf(stderr, "unknown Z flag '%c'\n",
					    optarg[-1]);
					exit(1);
				}
			break;


		default:
			fatal1("invalid flag %c\n", ch);
		}
	argc -= optind;
	argv += optind;

	if(argc != 4)
		fatal1("arg count %d", argc);
	asmfile  = opf(argv[1]);
	initfile = opf(argv[2]);
	textfile = opf(argv[3]);

	mkdope();
	initkey();
	if(inilex( copys(argv[0]) ))
		DONE(1);
	fprintf(diagfile, "%s:\n", argv[0]);
	fileinit();
	procinit();
	if((k = yyparse())) {
		fprintf(diagfile, "Bad parse, return code %d\n", k);
		DONE(1);
	}
	if(nerr > 0)
		DONE(1);
	if(parstate != OUTSIDE) {
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

