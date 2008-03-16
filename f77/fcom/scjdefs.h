/*
 * Map pass2 nodes to pass1 dito.
 */
#include "node.h"


#define P2BAD -1
#define P2PLUS PLUS
#define P2PLUSEQ 7
#define P2MINUS MINUS
#define P2NEG UMINUS
#define P2STAR MUL
#define P2STAREQ 12
#define P2BITAND AND
#define P2BITOR OR
#define P2BITXOR ER
#define P2QUEST 21
#define P2COLON 22
#define P2ANDAND 23
#define P2OROR 24
#define P2GOTO GOTO
#define P2ASSIGN ASSIGN
#define P2COMOP 59
#define P2SLASH DIV
#define P2MOD MOD
#define P2LSHIFT LS
#define P2RSHIFT RS
#define P2CALL CALL
#define P2CALL0 UCALL

#define P2NOT 76
#define P2BITNOT 77
#define P2EQ EQ
#define P2NE NE
#define P2LE LE
#define P2LT LT
#define P2GE GE
#define P2GT GT
#define	P2CONV	SCONV

/* special operators included only for fortran's use */

#define P2INT INT

#define P2PTR PTR
