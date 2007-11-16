/*
 *  Machine-dependent code-generation strategy (pass 2).
 */

#include <assert.h>
#include <string.h>

#include "pass2.h"

/*
 * Check size of offset in OREG.  Called by oregok() to see if an
 * OREG can be generated.
 */
int
notoff(TWORD t, int r, CONSZ off, char *cp)
{
	if (cp && cp[0]) return 1;
	return !(off < 32768 && off > -32769);  /* YES */
}

/*
 * Generate instructions for an OREG.  Why is this routine MD?
 * Called by swmatch().
 */
void
offstar(NODE *p, int shape)
{
	NODE *r;

	if (isreg(p))
		return; /* Is already OREG */

	r = p->n_right;
	if( p->n_op == PLUS || p->n_op == MINUS ){
		if( r->n_op == ICON ){
			if (isreg(p->n_left) == 0)
				(void)geninsn(p->n_left, INAREG);
			/* Converted in ormake() */
			return;
		}
		/* usually for arraying indexing: */
		if (r->n_op == LS && r->n_right->n_op == ICON &&
		    r->n_right->n_lval == 2 && p->n_op == PLUS) {
			if (isreg(p->n_left) == 0)
				(void)geninsn(p->n_left, INAREG);
			if (isreg(r->n_left) == 0)
				(void)geninsn(r->n_left, INAREG);
			return;
		}
	}
	(void)geninsn(p, INAREG);
}

/*
 * It is OK to do an OREG - Modify the expression tree to be an OREG.
 */
void
myormake(NODE *q)
{
}

/*
 * Check to if the UMUL node can be converted into an OREG.
 */
int
shumul(NODE *p)
{
	/* Turns currently anything into OREG */
	return SOREG;
}

/*
 * Rewrite operations on binary operators (like +, -, etc...).
 * Called as a result of a failed table lookup.
 *
 * Return nonzero to retry table search on new tree, or zero to fail.
 */
int
setbin(NODE *p)
{
	return 0;

}

/*
 * Rewrite assignment operations.
 * Called as a result of a failed table lookup.
 *
 * Return nonzero to retry table search on new tree, or zero to fail.
 */
int
setasg(NODE *p, int cookie)
{
	return 0;
}

/*
 * Rewrite UMUL operation.
 * Called as a result of a failed table lookup.
 *
 * Return nonzero to retry table search on new tree, or zero to fail.
 */
int
setuni(NODE *p, int cookie)
{
	return 0;
}

/*
 * Special handling of some instruction register allocation.
 *
 * Called as a result of specifying NSPECIAL in the tabale.
 */
struct rspecial *
nspecial(struct optab *q)
{
	switch (q->op) {

	case MOD:
	case DIV:
		{
			static struct rspecial s[] = {
				{ NLEFT, R0 },
				{ NRIGHT, R1 },
				{ NRES, R0 },
				{ -1 }
			};
			return s;
		}
	case STASG:
		{
			static struct rspecial s[] = {
				{ NEVER, R0 },
				{ NRIGHT, R1 },
				{ NEVER, R2 },
				{ 0 } };
			return s;
		}
		break;

	default:
		break;
	}

	comperr("nspecial entry %d: %s", q - table, q->cstring);
	return 0; /* XXX gcc */
}

/*
 * Set evaluation order of a binary node ('+','-', '*', '/', etc) if it
 * differs from default.
 */
int
setorder(NODE *p)
{
	return 0;
}

/*
 * Set registers "live" at function calls (like arguments in registers).
 * This is for liveness analysis of registers.
 */
int *
livecall(NODE *p)
{
        static int r[] = { R3, R2, R1, R0, -1 };
	int num = 1;

	if (p->n_op == UCALL)
		return &r[4-0];

	p = p->n_right;
	while (p->n_op == CM && num <= 4) {
		num++;
		p = p->n_left;
	}

        return &r[4 - num];
}
