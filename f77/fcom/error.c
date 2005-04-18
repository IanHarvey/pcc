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
 * notice, this list of conditions and the following disclaimer in the
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

#include "defs.h"


warn1(s,t)
char *s, *t;
{
char buff[100];
warn( sprintf(buff, s, t) );
}


warn(s)
char *s;
{
if(nowarnflag)
	return;
fprintf(diagfile, "Warning on line %d of %s: %s\n", lineno, infname, s);
++nwarn;
}



err2(s,t,u)
char *s, *t, *u;
{
char buff[100];
err( sprintf(buff, s, t, u) );
}


err1(s,t)
char *s, *t;
{
char buff[100];
err( sprintf(buff, s, t) );
}


err(s)
char *s;
{
fprintf(diagfile, "Error on line %d of %s: %s\n", lineno, infname, s);
++nerr;
}


yyerror(s)
char *s;
{ err(s); }



dclerr(s, v)
char *s;
struct nameblock *v;
{
char buff[100];

if(v)
	err( sprintf(buff, "Declaration error for %s: %s", varstr(VL, v->varname), s) );
else
	err1("Declaration error %s", s);
}



execerr(s, n)
char *s, *n;
{
char buf1[100], buf2[100];

sprintf(buf1, "Execution error %s", s);
err( sprintf(buf2, buf1, n) );
}


fatal(t)
char *t;
{
fprintf(diagfile, "Compiler error line %d of %s: %s\n", lineno, infname, t);
if(debugflag)
	abort();
done(3);
exit(3);
}




fatal1(t,d)
char *t, *d;
{
char buff[100];
fatal( sprintf(buff, t, d) );
}
