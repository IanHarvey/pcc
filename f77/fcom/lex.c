#include "defs"
#include "tokdefs"

# define BLANK	' '
# define MYQUOTE (2)
# define SEOF 0

/* card types */

# define STEOF 1
# define STINITIAL 2
# define STCONTINUE 3

/* lex states */

#define NEWSTMT	1
#define FIRSTTOKEN	2
#define OTHERTOKEN	3
#define RETEOS	4


LOCAL int stkey;
LOCAL int stno;
LOCAL long int nxtstno;
LOCAL int parlev;
LOCAL int expcom;
LOCAL int expeql;
LOCAL char *nextch;
LOCAL char *lastch;
LOCAL char *nextcd 	= NULL;
LOCAL char *endcd;
LOCAL int prevlin;
LOCAL int thislin;
LOCAL int code;
LOCAL int lexstate	= NEWSTMT;
LOCAL char s[1390];
LOCAL char *send	= s+20*66;
LOCAL int nincl	= 0;

struct inclfile
	{
	struct inclfile *inclnext;
	FILEP inclfp;
	char *inclname;
	int incllno;
	char *incllinp;
	int incllen;
	int inclcode;
	ftnint inclstno;
	} ;

LOCAL struct inclfile *inclp	=  NULL;
LOCAL struct keylist { char *keyname; int keyval; } ;
LOCAL struct punctlist { char punchar; int punval; };
LOCAL struct fmtlist { char fmtchar; int fmtval; };
LOCAL struct dotlist { char *dotname; int dotval; };
LOCAL struct keylist *keystart[26], *keyend[26];




inilex(name)
char *name;
{
nincl = 0;
inclp = NULL;
doinclude(name);
lexstate = NEWSTMT;
return(NO);
}



/* throw away the rest of the current line */
flline()
{
lexstate = RETEOS;
}



char *lexline(n)
ftnint *n;
{
*n = (lastch - nextch) + 1;
return(nextch);
}





doinclude(name)
char *name;
{
FILEP fp;
struct inclfile *t;

if(inclp)
	{
	inclp->incllno = thislin;
	inclp->inclcode = code;
	inclp->inclstno = nxtstno;
	if(nextcd)
		inclp->incllinp = copyn(inclp->incllen = endcd-nextcd , nextcd);
	else
		inclp->incllinp = 0;
	}
nextcd = NULL;

if(++nincl >= MAXINCLUDE)
	fatal("includes nested too deep");
if(name[0] == '\0')
	fp = stdin;
else
	fp = fopen(name, "r");
if( fp )
	{
	t = inclp;
	inclp = ALLOC(inclfile);
	inclp->inclnext = t;
	prevlin = thislin = 0;
	infname = inclp->inclname = name;
	infile = inclp->inclfp = fp;
	}
else
	{
	fprintf(diagfile, "Cannot open file %s", name);
	done(1);
	}
}




LOCAL popinclude()
{
struct inclfile *t;
register char *p;
register int k;

if(infile != stdin)
	clf(&infile);
free(infname);

--nincl;
t = inclp->inclnext;
free(inclp);
inclp = t;
if(inclp == NULL)
	return(NO);

infile = inclp->inclfp;
infname = inclp->inclname;
prevlin = thislin = inclp->incllno;
code = inclp->inclcode;
stno = nxtstno = inclp->inclstno;
if(inclp->incllinp)
	{
	endcd = nextcd = s;
	k = inclp->incllen;
	p = inclp->incllinp;
	while(--k >= 0)
		*endcd++ = *p++;
	free(inclp->incllinp);
	}
else
	nextcd = NULL;
return(YES);
}




yylex()
{
static int  tokno;

	switch(lexstate)
	{
case NEWSTMT :	/* need a new statement */
	if(getcds() == STEOF)
		return(SEOF);
	crunch();
	tokno = 0;
	lexstate = FIRSTTOKEN;
	yylval = stno;
	stno = nxtstno;
	toklen = 0;
	return(SLABEL);

first:
case FIRSTTOKEN :	/* first step on a statement */
	analyz();
	lexstate = OTHERTOKEN;
	tokno = 1;
	return(stkey);

case OTHERTOKEN :	/* return next token */
	if(nextch > lastch)
		goto reteos;
	++tokno;
	if((stkey==SLOGIF || stkey==SELSEIF) && parlev==0 && tokno>3) goto first;
	if(stkey==SASSIGN && tokno==3 && nextch<lastch &&
		nextch[0]=='t' && nextch[1]=='o')
			{
			nextch+=2;
			return(STO);
			}
	return(gettok());

reteos:
case RETEOS:
	lexstate = NEWSTMT;
	return(SEOS);
	}
fatal1("impossible lexstate %d", lexstate);
/* NOTREACHED */
}

LOCAL getcds()
{
register char *p, *q;

top:
	if(nextcd == NULL)
		{
		code = getcd( nextcd = s );
		stno = nxtstno;
		prevlin = thislin;
		}
	if(code == STEOF)
		if( popinclude() )
			goto top;
		else
			return(STEOF);

	if(code == STCONTINUE)
		{
		lineno = thislin;
		err("illegal continuation card ignored");
		nextcd = NULL;
		goto top;
		}

	if(nextcd > s)
		{
		q = nextcd;
		p = s;
		while(q < endcd)
			*p++ = *q++;
		endcd = p;
		}
	for(nextcd = endcd ;
		nextcd+66<=send && (code = getcd(nextcd))==STCONTINUE ;
		nextcd = endcd )
			;
	nextch = s;
	lastch = nextcd - 1;
	if(nextcd >= send)
		nextcd = NULL;
	lineno = prevlin;
	prevlin = thislin;
	return(STINITIAL);
}

LOCAL getcd(b)
register char *b;
{
register int c;
register char *p, *bend;
int speclin;
static char a[6];
static char *aend	= a+6;

top:
	endcd = b;
	bend = b+66;
	speclin = NO;

	if( (c = getc(infile)) == '&')
		{
		a[0] = BLANK;
		a[5] = 'x';
		speclin = YES;
		bend = send;
		}
	else if(c=='c' || c=='C' || c=='*')
		{
		while( (c = getc(infile)) != '\n')
			if(c == EOF)
				return(STEOF);
		++thislin;
		goto top;
		}

	else if(c != EOF)
		{
		/* a tab in columns 1-6 skips to column 7 */
		ungetc(c, infile);
		for(p=a; p<aend && (c=getc(infile)) != '\n' && c!=EOF; )
			if(c == '\t')
				{
				while(p < aend)
					*p++ = BLANK;
				speclin = YES;
				bend = send;
				}
			else
				*p++ = c;
		}
	if(c == EOF)
		return(STEOF);
	if(c == '\n')
		{
		while(p < aend)
			*p++ = BLANK;
		if( ! speclin )
			while(endcd < bend)
				*endcd++ = BLANK;
		}
	else	{	/* read body of line */
		while( endcd<bend && (c=getc(infile)) != '\n' && c!=EOF )
			*endcd++ = (c == '\t' ? BLANK : c);
		if(c == EOF)
			return(STEOF);
		if(c != '\n')
			{
			while( (c=getc(infile)) != '\n')
				if(c == EOF)
					return(STEOF);
			}

		if( ! speclin )
			while(endcd < bend)
				*endcd++ = BLANK;
		}
	++thislin;
	if(a[5]!=BLANK && a[5]!='0')
		return(STCONTINUE);
	for(p=a; p<aend; ++p)
		if(*p != BLANK) goto initline;
	for(p = b ; p<endcd ; ++p)
		if(*p != BLANK) goto initline;
	goto top;

initline:
	nxtstno = 0;
	for(p = a ; p<a+5 ; ++p)
		if(*p != BLANK)
			if(isdigit(*p))
				nxtstno = 10*nxtstno + (*p - '0');
			else	{
				lineno = thislin;
				err("nondigit in statement number field");
				nxtstno = 0;
				break;
				}
	return(STINITIAL);
}

LOCAL crunch()
{
register char *i, *j, *j0, *j1, *prvstr;
int ten, nh, quote;

/* i is the next input character to be looked at
j is the next output character */
parlev = 0;
expcom = 0;	/* exposed ','s */
expeql = 0;	/* exposed equal signs */
j = s;
prvstr = s;
for(i=s ; i<=lastch ; ++i)
	{
	if(*i == BLANK) continue;
	if(*i=='\'' ||  *i=='"')
		{
		quote = *i;
		*j = MYQUOTE; /* special marker */
		for(;;)
			{
			if(++i > lastch)
				{
				err("unbalanced quotes; closing quote supplied");
				break;
				}
			if(*i == quote)
				if(i<lastch && i[1]==quote) ++i;
				else break;
			else if(*i=='\\' && i<lastch)
				switch(*++i)
					{
					case 't':
						*i = '\t'; break;
					case 'b':
						*i = '\b'; break;
					case 'n':
						*i = '\n'; break;
					case 'f':
						*i = '\f'; break;
					case '0':
						*i = '\0'; break;
					default:
						break;
					}
			*++j = *i;
			}
		j[1] = MYQUOTE;
		j += 2;
		prvstr = j;
		}
	else if( (*i=='h' || *i=='H')  && j>prvstr)	/* test for Hollerith strings */
		{
		if( ! isdigit(j[-1])) goto copychar;
		nh = j[-1] - '0';
		ten = 10;
		j1 = prvstr - 1;
		if (j1<j-5) j1=j-5;
		for(j0=j-2 ; j0>j1; -- j0)
			{
			if( ! isdigit(*j0 ) ) break;
			nh += ten * (*j0-'0');
			ten*=10;
			}
		if(j0 <= j1) goto copychar;
/* a hollerith must be preceded by a punctuation mark.
   '*' is possible only as repetition factor in a data statement
   not, in particular, in character*2h
*/

		if( !(*j0=='*'&&s[0]=='d') && *j0!='/' && *j0!='(' &&
			*j0!=',' && *j0!='=' && *j0!='.')
				goto copychar;
		if(i+nh > lastch)
			{
			err1("%dH too big", nh);
			nh = lastch - i;
			}
		j0[1] = MYQUOTE; /* special marker */
		j = j0 + 1;
		while(nh-- > 0)
			{
			if(*++i == '\\')
				switch(*++i)
					{
					case 't':
						*i = '\t'; break;
					case 'b':
						*i = '\b'; break;
					case 'n':
						*i = '\n'; break;
					case 'f':
						*i = '\f'; break;
					case '0':
						*i = '\0'; break;
					default:
						break;
					}
			*++j = *i;
			}
		j[1] = MYQUOTE;
		j+=2;
		prvstr = j;
		}
	else	{
		if(*i == '(') ++parlev;
		else if(*i == ')') --parlev;
		else if(parlev == 0)
			if(*i == '=') expeql = 1;
			else if(*i == ',') expcom = 1;
copychar:		/*not a string of BLANK -- copy, shifting case if necessary */
		if(shiftcase && isupper(*i))
			*j++ = tolower(*i);
		else	*j++ = *i;
		}
	}
lastch = j - 1;
nextch = s;
}

LOCAL analyz()
{
register char *i;

	if(parlev != 0)
		{
		err("unbalanced parentheses, statement skipped");
		stkey = SUNKNOWN;
		return;
		}
	if(nextch+2<=lastch && nextch[0]=='i' && nextch[1]=='f' && nextch[2]=='(')
		{
/* assignment or if statement -- look at character after balancing paren */
		parlev = 1;
		for(i=nextch+3 ; i<=lastch; ++i)
			if(*i == (MYQUOTE))
				{
				while(*++i != MYQUOTE)
					;
				}
			else if(*i == '(')
				++parlev;
			else if(*i == ')')
				{
				if(--parlev == 0)
					break;
				}
		if(i >= lastch)
			stkey = SLOGIF;
		else if(i[1] == '=')
			stkey = SLET;
		else if( isdigit(i[1]) )
			stkey = SARITHIF;
		else	stkey = SLOGIF;
		if(stkey != SLET)
			nextch += 2;
		}
	else if(expeql) /* may be an assignment */
		{
		if(expcom && nextch<lastch &&
			nextch[0]=='d' && nextch[1]=='o')
				{
				stkey = SDO;
				nextch += 2;
				}
		else	stkey = SLET;
		}
/* otherwise search for keyword */
	else	{
		stkey = getkwd();
		if(stkey==SGOTO && lastch>=nextch)
			if(nextch[0]=='(')
				stkey = SCOMPGOTO;
			else if(isalpha(nextch[0]))
				stkey = SASGOTO;
		}
	parlev = 0;
}



LOCAL getkwd()
{
register char *i, *j;
register struct keylist *pk, *pend;
int k;

if(! isalpha(nextch[0]) )
	return(SUNKNOWN);
k = nextch[0] - 'a';
if(pk = keystart[k])
	for(pend = keyend[k] ; pk<=pend ; ++pk )
		{
		i = pk->keyname;
		j = nextch;
		while(*++i==*++j && *i!='\0')
			;
		if(*i == '\0')
			{
			nextch = j;
			return(pk->keyval);
			}
		}
return(SUNKNOWN);
}



initkey()
{
extern struct keylist keys[];
register struct keylist *p;
register int i,j;

for(i = 0 ; i<26 ; ++i)
	keystart[i] = NULL;

for(p = keys ; p->keyname ; ++p)
	{
	j = p->keyname[0] - 'a';
	if(keystart[j] == NULL)
		keystart[j] = p;
	keyend[j] = p;
	}
}

LOCAL gettok()
{
int havdot, havexp, havdbl;
int radix;
extern struct punctlist puncts[];
struct punctlist *pp;
extern struct fmtlist fmts[];
extern struct dotlist dots[];
struct dotlist *pd;

char *i, *j, *n1, *p;

	if(*nextch == (MYQUOTE))
		{
		++nextch;
		p = token;
		while(*nextch != MYQUOTE)
			*p++ = *nextch++;
		++nextch;
		toklen = p - token;
		*p = '\0';
		return (SHOLLERITH);
		}
/*
	if(stkey == SFORMAT)
		{
		for(pf = fmts; pf->fmtchar; ++pf)
			{
			if(*nextch == pf->fmtchar)
				{
				++nextch;
				if(pf->fmtval == SLPAR)
					++parlev;
				else if(pf->fmtval == SRPAR)
					--parlev;
				return(pf->fmtval);
				}
			}
		if( isdigit(*nextch) )
			{
			p = token;
			*p++ = *nextch++;
			while(nextch<=lastch && isdigit(*nextch) )
				*p++ = *nextch++;
			toklen = p - token;
			*p = '\0';
			if(nextch<=lastch && *nextch=='p')
				{
				++nextch;
				return(SSCALE);
				}
			else	return(SICON);
			}
		if( isalpha(*nextch) )
			{
			p = token;
			*p++ = *nextch++;
			while(nextch<=lastch &&
				(*nextch=='.' || isdigit(*nextch) || isalpha(*nextch) ))
					*p++ = *nextch++;
			toklen = p - token;
			*p = '\0';
			return(SFIELD);
			}
		goto badchar;
		}
/* Not a format statement */

if(needkwd)
	{
	needkwd = 0;
	return( getkwd() );
	}

	for(pp=puncts; pp->punchar; ++pp)
		if(*nextch == pp->punchar)
			{
			if( (*nextch=='*' || *nextch=='/') &&
				nextch<lastch && nextch[1]==nextch[0])
					{
					if(*nextch == '*')
						yylval = SPOWER;
					else	yylval = SCONCAT;
					nextch+=2;
					}
			else	{yylval=pp->punval;
					if(yylval==SLPAR)
						++parlev;
					else if(yylval==SRPAR)
						--parlev;
					++nextch;
				}
			return(yylval);
			}
	if(*nextch == '.')
		if(nextch >= lastch) goto badchar;
		else if(isdigit(nextch[1])) goto numconst;
		else	{
			for(pd=dots ; (j=pd->dotname) ; ++pd)
				{
				for(i=nextch+1 ; i<=lastch ; ++i)
					if(*i != *j) break;
					else if(*i != '.') ++j;
					else	{
						nextch = i+1;
						return(pd->dotval);
						}
				}
			goto badchar;
			}
	if( isalpha(*nextch) )
		{
		p = token;
		*p++ = *nextch++;
		while(nextch<=lastch)
			if( isalpha(*nextch) || isdigit(*nextch) )
				*p++ = *nextch++;
			else break;
		toklen = p - token;
		*p = '\0';
		if(inioctl && nextch<=lastch && *nextch=='=')
			{
			++nextch;
			return(SNAMEEQ);
			}
		if(toklen>=8 && eqn(8, token, "function") &&
			nextch<lastch && *nextch=='(')
				{
				nextch -= (toklen - 8);
				return(SFUNCTION);
				}
		if(toklen > VL)
			{
			err2("name %s too long, truncated to %d", token, VL);
			toklen = VL;
			token[6] = '\0';
			}
		if(toklen==1 && *nextch==MYQUOTE)
			{
			switch(token[0])
				{
				case 'z':  case 'Z':
				case 'x':  case 'X':
					radix = 16; break;
				case 'o':  case 'O':
					radix = 8; break;
				case 'b':  case 'B':
					radix = 2; break;
				default:
					err("bad bit identifier");
					return(SNAME);
				}
			++nextch;
			for(p = token ; *nextch!=MYQUOTE ; )
				if( hextoi(*p++ = *nextch++) >= radix)
					{
					err("invalid binary character");
					break;
					}
			++nextch;
			toklen = p - token;
			return( radix==16 ? SHEXCON : (radix==8 ? SOCTCON : SBITCON) );
			}
		return(SNAME);
		}
	if( ! isdigit(*nextch) ) goto badchar;
numconst:
	havdot = NO;
	havexp = NO;
	havdbl = NO;
	for(n1 = nextch ; nextch<=lastch ; ++nextch)
		{
		if(*nextch == '.')
			if(havdot) break;
			else if(nextch+2<=lastch && isalpha(nextch[1])
				&& isalpha(nextch[2]))
					break;
			else	havdot = YES;
		else if(*nextch=='d' || *nextch=='e')
			{
			p = nextch;
			havexp = YES;
			if(*nextch == 'd')
				havdbl = YES;
			if(nextch<lastch)
				if(nextch[1]=='+' || nextch[1]=='-')
					++nextch;
			if( ! isdigit(*++nextch) )
				{
				nextch = p;
				havdbl = havexp = NO;
				break;
				}
			for(++nextch ;
				nextch<=lastch && isdigit(*nextch);
				++nextch);
			break;
			}
		else if( ! isdigit(*nextch) )
			break;
		}
	p = token;
	i = n1;
	while(i < nextch)
		*p++ = *i++;
	toklen = p - token;
	*p = '\0';
	if(havdbl) return(SDCON);
	if(havdot || havexp) return(SRCON);
	return(SICON);
badchar:
	s[0] = *nextch++;
	return(SUNKNOWN);
}

/* KEYWORD AND SPECIAL CHARACTER TABLES
*/

struct punctlist puncts[ ] =
	{
	'(', SLPAR,
	')', SRPAR,
	'=', SEQUALS,
	',', SCOMMA,
	'+', SPLUS,
	'-', SMINUS,
	'*', SSTAR,
	'/', SSLASH,
	'$', SCURRENCY,
	':', SCOLON,
	0, 0 } ;

/*
LOCAL struct fmtlist  fmts[ ] =
	{
	'(', SLPAR,
	')', SRPAR,
	'/', SSLASH,
	',', SCOMMA,
	'-', SMINUS,
	':', SCOLON,
	0, 0 } ;
*/

LOCAL struct dotlist  dots[ ] =
	{
	"and.", SAND, 
	"or.", SOR, 
	"not.", SNOT, 
	"true.", STRUE, 
	"false.", SFALSE, 
	"eq.", SEQ, 
	"ne.", SNE, 
	"lt.", SLT, 
	"le.", SLE, 
	"gt.", SGT, 
	"ge.", SGE, 
	"neqv.", SNEQV, 
	"eqv.", SEQV, 
	0, 0 } ;

LOCAL struct keylist  keys[ ] =
	{
	"assign",  SASSIGN,
	"automatic",  SAUTOMATIC,
	"backspace",  SBACKSPACE,
	"blockdata",  SBLOCK,
	"call",  SCALL,
	"character",  SCHARACTER,
	"close",  SCLOSE,
	"common",  SCOMMON,
	"complex",  SCOMPLEX,
	"continue",  SCONTINUE,
	"data",  SDATA,
	"dimension",  SDIMENSION,
	"doubleprecision",  SDOUBLE,
	"doublecomplex", SDCOMPLEX,
	"elseif",  SELSEIF,
	"else",  SELSE,
	"endfile",  SENDFILE,
	"endif",  SENDIF,
	"end",  SEND,
	"entry",  SENTRY,
	"equivalence",  SEQUIV,
	"external",  SEXTERNAL,
	"format",  SFORMAT,
	"function",  SFUNCTION,
	"goto",  SGOTO,
	"implicit",  SIMPLICIT,
	"include",  SINCLUDE,
	"inquire",  SINQUIRE,
	"intrinsic",  SINTRINSIC,
	"integer",  SINTEGER,
	"logical",  SLOGICAL,
	"open",  SOPEN,
	"parameter",  SPARAM,
	"pause",  SPAUSE,
	"print",  SPRINT,
	"program",  SPROGRAM,
	"punch",  SPUNCH,
	"read",  SREAD,
	"real",  SREAL,
	"return",  SRETURN,
	"rewind",  SREWIND,
	"save",  SSAVE,
	"static",  SSTATIC,
	"stop",  SSTOP,
	"subroutine",  SSUBROUTINE,
	"then",  STHEN,
	"undefined", SUNDEFINED,
	"write",  SWRITE,
	0, 0 };
