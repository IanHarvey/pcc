/*
 *  Stuff for pass1.
 */

#include <assert.h>

#include "pass1.h"
#include "pass2.h"

/*
 * Modify the alignment in the data section to become a multiple of n.
 */
void
defalign(int n)
{
	n /= SZCHAR;
	if (n == 1)
		return;
	printf("\t.align %d\n", n);
}

/*
 * Define the current location as an internal label.
 */
void
deflab(int label)
{
        printf(LABFMT ":\n", label);
}

/*
 * Define the current location in the data section to be the name p->sname
 */
void
defnam(struct symtab *p)
{
	char *c = p->sname;

#ifdef GCC_COMPAT
	c = gcc_findname(p);
#endif
	if (p->sclass == EXTDEF)
		printf("\t.global %s\n", exname(c));
	printf("%s:\n", exname(c));
}


/*
 * End-of-Function code:
 */
void
efcode()
{
	if (cftnsp->stype != STRTY+FTN && cftnsp->stype != UNIONTY+FTN)
		return;
	assert(0);
}

/*
 * Beginning-of-function code:
 *
 * 'a' is an array of indices in symtab for the arguments
 * 'n' is the number of arguments
 */
void
bfcode(struct symtab **sp, int cnt)
{
        NODE *p, *q;
        int i, n;

        if (cftnsp->stype == STRTY+FTN || cftnsp->stype == UNIONTY+FTN) {
                uerror("no struct return yet");
        }
        /* recalculate the arg offset and create TEMP moves */
        for (n = R0, i = 0; i < cnt; i++) {
                if (n + szty(sp[i]->stype) <= R4) {
			/* put stack args in temps */
			p = tempnode(0, sp[i]->stype, sp[i]->sdf, sp[i]->ssue);
			spname = sp[i];
			q = block(REG, NIL, NIL,
			    sp[i]->stype, sp[i]->sdf, sp[i]->ssue);
			q->n_rval = n;
			p = buildtree(ASSIGN, p, q);
			sp[i]->soffset = p->n_left->n_lval;
			sp[i]->sflags |= STNODE;
                       	ecomp(p);
                } else {
                        sp[i]->soffset -= SZINT * 4;
                        if (xtemps) {
                                /* put stack args in temps if optimizing */
                                spname = sp[i];
                                p = tempnode(0, sp[i]->stype,
                                    sp[i]->sdf, sp[i]->ssue);
                                p = buildtree(ASSIGN, p, buildtree(NAME, 0, 0));
                                sp[i]->soffset = p->n_left->n_lval;
                                sp[i]->sflags |= STNODE;
                                ecomp(p);
                        }
		
                }
                n += szty(sp[i]->stype);
        }
}


/*
 * Beginning-of-code: finished generating function prologue
 *
 * by now, the automatics and register variables are allocated
 */
void
bccode()
{
	SETOFF(autooff, SZINT);
}

/*
 * End-of-job: called just before final exit.
 */
void
ejobcode(int flag )
{
#define OSB(x) __STRING(x)
#define OS OSB(TARGOS)
	printf("\t.ident \"%s (%s)\"\n", PACKAGE_STRING, OS);
}

/*
 * Beginning-of-job: called before compilation starts
 *
 * Initialise data structures specific for the local machine.
 */
void
bjobcode()
{
}

/*
 * Output ascii string: print character 't' at position 'i' until 't' == -1.
 */
void
bycode(int t, int i)
{
	static int lastoctal = 0;

	/* put byte i+1 in a string */

	if (t < 0) {
		if (i != 0)
			puts("\"");
	} else {
		if (i == 0)
			printf("\t.ascii \"");
		if (t == '\\' || t == '"') {
			lastoctal = 0;
			putchar('\\');
			putchar(t);
		} else if (t < 040 || t >= 0177) {
			lastoctal++;
			printf("\\%o",t);
		} else if (lastoctal && '0' <= t && t <= '9') {
			lastoctal = 0;
			printf("\"\n\t.ascii \"%c", t);
		} else {	
			lastoctal = 0;
			putchar(t);
		}
	}
}

/*
 * Compute the alignment of object with type 't'.
 */
int
fldal(unsigned int t)
{
	uerror("illegal field type");
	return(ALINT);
}

/*
 * fix up type of field p
 */
void
fldty(struct symtab *p)
{
}

void
genswitch(int num, struct swents **p, int n)
{
	NODE *r;
	int i;

	for (i = 1; i <= n; ++i) {
		r = tempnode(num, INT, 0, MKSUE(INT));
		r = buildtree(NE, r, bcon(p[i]->sval));
		cbranch(buildtree(NOT, r, NIL), bcon(p[i]->slab));
	}
	if (p[0]->slab > 0)
		branch(p[0]->slab);
}

static int regoff[7];
static TWORD ftype;

/*
 * calculate stack size and offsets
 */
static int
offcalc(struct interpass_prolog *ipp)
{
	int i, j, addto;

#ifdef PCC_DEBUG
	if (x2debug)
		printf("offcalc: p2maxautooff=%d\n", p2maxautooff);
#endif

	addto = p2maxautooff;

	// space is always allocated on the stack to save the permanents
	for (i = ipp->ipp_regs, j = 0; i ; i >>= 1, j++) {
		if (i & 1) {
			addto += SZINT/SZCHAR;
			regoff[j] = addto;
		}
	}

#if 0
	addto += 7;
	addto &= ~7;
#endif

#ifdef PCC_DEBUG
	if (x2debug)
		printf("offcalc: addto=%d\n", addto);
#endif

	addto -= AUTOINIT / SZCHAR;

	return addto;
}

void
prologue(struct interpass_prolog *ipp)
{
	int i, j;
	int addto;

#ifdef PCC_DEBUG
	if (x2debug)
		printf("prologue: type=%d, lineno=%d, name=%s, vis=%d, ipptype=%d, regs=0x%x, autos=%d, tmpnum=%d, lblnum=%d\n",
			ipp->ipp_ip.type,
			ipp->ipp_ip.lineno,
			ipp->ipp_name,
			ipp->ipp_vis,
			ipp->ipp_type,
			ipp->ipp_regs,
			ipp->ipp_autos,
			ipp->ip_tmpnum,
			ipp->ip_lblnum);
#endif

	ftype = ipp->ipp_type;

	printf("\t.align 2\n");
	if (ipp->ipp_vis)
		printf("\t.global %s\n", exname(ipp->ipp_name));
	printf("\t.type %s,%%function\n", exname(ipp->ipp_name));
	printf("%s:\n", exname(ipp->ipp_name));

	/*
	 * We here know what register to save and how much to 
	 * add to the stack.
	 */
	addto = offcalc(ipp);

	printf("\tmov %s,%s\n", rnames[IP], rnames[SP]);
	printf("\tstmfd %s!,{%s,%s,%s,%s}\n", rnames[SP], rnames[FP],
	    rnames[IP], rnames[LR], rnames[PC]);
	printf("\tsub %s,%s,#4\n", rnames[FP], rnames[IP]);
	if (addto)
		printf("\tsub %s,%s,#%d\n", rnames[SP], rnames[SP], addto);

	for (i = ipp->ipp_regs, j = 0; i; i >>= 1, j++) {
		if (i & 1) {
			printf("\tstr %s,[%s,#-%d]\n",
			    rnames[j], rnames[FP], regoff[j]);
		}
	}

}

void
eoftn(struct interpass_prolog *ipp)
{
	int i, j;

	if (ipp->ipp_ip.ip_lbl == 0)
		return; /* no code needs to be generated */

	/* return from function code */
	for (i = ipp->ipp_regs, j = 0; i ; i >>= 1, j++) {
		if (i & 1)
			printf("\tldr %s,[%s,#-%d]\n",
			    rnames[j], rnames[FP], regoff[j]);
			
	}

	/* struct return needs special treatment */
	if (ftype == STRTY || ftype == UNIONTY) {
		assert(0);
	} else {
		printf("\tldmea %s,{%s,%s,%s}\n", rnames[FP], rnames[FP],
		    rnames[SP], rnames[PC]);
	}
	printf("\t.size %s,.-%s\n", exname(ipp->ipp_name),
	    exname(ipp->ipp_name));
}

char *rnames[] = {
	"r0", "r1", "r2", "r3","r4","r5", "r6", "r7", "r8",
	"r9", "r10", "fp", "ip", "sp", "lr", "pc",
};

static void
moveargs(NODE **n, int *regp)
{
        NODE *r = *n;
        NODE *t;
	int sz;
	int regnum;

        if (r->n_op == CM) {
                moveargs(&r->n_left, regp);
                n = &r->n_right;
                r = r->n_right;
        }

 	regnum = *regp;
	sz = szty(r->n_type);

        if (regnum + sz <= R4) {
                t = block(REG, NIL, NIL, r->n_type, r->n_df, r->n_sue);
                t->n_rval = regnum;
		t = buildtree(ASSIGN, t, r);
        } else {
                t = block(FUNARG, r, NIL, r->n_type, r->n_df, r->n_sue);
        }

        *n = t;
	*regp += sz;
}

/*
 * Called with a function call with arguments as argument.
 * This is done early in buildtree() and only done once.
 */
NODE *
funcode(NODE *p)
{
	int regnum = R0;
	moveargs(&p->n_right, &regnum);
	return p;
}
