
/*
 * Generate defines for the needed hardops.
 */
#include "pass2.h"

int chkop[MAXOP];

char *cname = "external.c";
char *hname = "external.h";

struct checks {
	int op, type;
	char *name;
} checks[] = {
	{ MUL, TLONGLONG, "SMULLL", },
	{ DIV, TLONGLONG, "SDIVLL", },
	{ MOD, TLONGLONG, "SMODLL", },
	{ PLUS, TLONGLONG, "SPLUSLL", },
	{ MINUS, TLONGLONG, "SMINUSLL", },
	{ MUL, TULONGLONG, "UMULLL", },
	{ DIV, TULONGLONG, "UDIVLL", },
	{ MOD, TULONGLONG, "UMODLL", },
	{ PLUS, TULONGLONG, "UPLUSLL", },
	{ MINUS, TULONGLONG, "UMINUSLL", },
	{ 0, 0, 0, },
};

int
main(int argc, char *argv[])
{
	struct optab *op;
	struct checks *ch;
	FILE *fc, *fh;

	for (op = table; op->op != FREE; op++) {
		if (op->op >= OPSIMP)
			continue;
		if ((op->lshape & TLONGLONG) &&
		    (op->rshape & TLONGLONG))
			chkop[op->op] |= TLONGLONG;
		if ((op->lshape & TULONGLONG) &&
		    (op->rshape & TULONGLONG))
			chkop[op->op] |= TULONGLONG;
	}
	if ((fc = fopen(cname, "w")) == NULL) {
		perror("open cfile");
		return(1);
	}
	if ((fh = fopen(hname, "w")) == NULL) {
		perror("open hfile");
		return(1);
	}
	for (ch = checks; ch->op != 0; ch++) {
		if (((chkop[ch->op] | chkop[ASG ch->op]) & ch->type) == 0)
			fprintf(fh, "#define NEED_%s\n", ch->name);
	}
	fclose(fc);
	fclose(fh);
	return 0;
}
