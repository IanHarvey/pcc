#include "pass2.h"

# define TLL TLONGLONG|TULONGLONG
# define ANYSIGNED TINT|TLONG|TSHORT|TCHAR
# define ANYUSIGNED TUNSIGNED|TULONG|TUSHORT|TUCHAR
# define ANYFIXED ANYSIGNED|ANYUSIGNED
# define TUWORD TUNSIGNED
# define TSWORD TINT
# define TWORD TUWORD|TSWORD

struct optab table[] = {
/* First entry must be an empty entry */
{ -1, FOREFF, SANY, TANY, SANY, TANY, 0, 0, "", },
{ OPSIMP,	INAREG|FOREFF,
	SAREG|STAREG,			TWORD|TPOINT,
	SAREG|STAREG|SNAME|SOREG,	TWORD|TPOINT,
		0,	RLEFT,
		"	Ow AR, AL\n", },

{ OPLOG,	FORCC,
	SBREG|STBREG,	TWORD,
	SCON,		TWORD,
		0,	RESCC,
		"	cmp AR, AL\n", },

{ OPLTYPE,	INTBREG,
	SANY,	TANY,
	SOREG,	TWORD|TPOINT,
		NBREG,	RESC1,
		"	mov.w AL, A1\n", },	
{ ASSIGN,	FOREFF,
	SOREG,		TWORD|TPOINT,
	SCON,		TANY,
		0,	0,
		"	mov.w AR, AL\n", },
{ UCALL,	INTAREG,
	SCON,	TANY,
	SANY,	TANY,
		NAREG|NASL,	RESC1,
		"	jsr.w CL\n", },

{ FREE, FREE,   FREE,   FREE,   FREE,   FREE,   FREE,   FREE,   "help; I'm in trouble\n" },
};

int tablesize = sizeof(table)/sizeof(table[0]);

