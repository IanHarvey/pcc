/*	$Id$	*/
/*
 * Copyright (c) 2003 Anders Magnusson (ragge@ludd.luth.se).
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 * 3. The name of the author may not be used to endorse or promote products
 *    derived from this software without specific prior written permission
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 * IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 * OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 * IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 * NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 * DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 * THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 * THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */


#include "pass1.h"

#define	LEFT_IS_LEAF		0x80000000
#define	RIGHT_IS_LEAF		0x40000000
#define	IS_LEFT_LEAF(x)		(((x) & LEFT_IS_LEAF) != 0)
#define	IS_RIGHT_LEAF(x)	(((x) & RIGHT_IS_LEAF) != 0)
#define	BITNO(x)		((x) & ~(LEFT_IS_LEAF|RIGHT_IS_LEAF))
#define	CHECKBITS		8

struct tree {
	int bitno;
	struct tree *lr[2];
};

static struct tree *first;
int nametabs, namestrlen;

#define	P_BIT(key, bit) (key[bit >> 3] >> (bit & 7)) & 1
#define	getree() permalloc(sizeof(struct tree))

/*
 * Add a name to the name stack (if its non-existing),
 * return its address.
 * This is a simple patricia implementation.
 */
char *
addname(char *key)
{
	struct tree *w, *new, *last;
	int cix, bit, fbit, svbit, ix, bitno, len;
	char *m, *k, *sm;

	/* Count full string length */
	for (k = key, len = 0; *k; k++, len++)
		;
	
	switch (nametabs) {
	case 0:
		first = (struct tree *)newstring(key, len);
		namestrlen += (len + 1);
		nametabs++;
		return (char *)first;

	case 1:
		m = (char *)first;
		break;

	default:
		w = first;
		bitno = len * CHECKBITS;
		for (;;) {
			bit = BITNO(w->bitno);
			fbit = bit > bitno ? 0 : P_BIT(key, bit);
			svbit = fbit ? IS_RIGHT_LEAF(w->bitno) :
			    IS_LEFT_LEAF(w->bitno);
			w = w->lr[fbit];
			if (svbit) {
				m = (char *)w;
				break;
			}
		}
	}

	sm = m;
	k = key;

	/* Check for correct string and return */
	for (cix = 0; *m && *k && *m == *k; m++, k++, cix += CHECKBITS)
		;
	if (*m == 0 && *k == 0)
		return sm;

	ix = *m ^ *k;
	while ((ix & 1) == 0)
		ix >>= 1, cix++;

	/* Create new node */
	new = getree();
	bit = P_BIT(key, cix);
	new->bitno = cix | (bit ? RIGHT_IS_LEAF : LEFT_IS_LEAF);
	new->lr[bit] = (struct tree *)newstring(key, len);
	namestrlen += (len + 1);

	if (nametabs++ == 1) {
		new->lr[!bit] = first;
		new->bitno |= (bit ? LEFT_IS_LEAF : RIGHT_IS_LEAF);
		first = new;
		return (char *)new->lr[bit];
	}


	w = first;
	last = NULL;
	for (;;) {
		fbit = w->bitno;
		bitno = BITNO(w->bitno);
		if (bitno == cix)
			cerror("bitno == cix");
		if (bitno > cix)
			break;
		svbit = P_BIT(key, bitno);
		last = w;
		w = w->lr[svbit];
		if (fbit & (svbit ? RIGHT_IS_LEAF : LEFT_IS_LEAF))
			break;
	}

	new->lr[!bit] = w;
	if (last == NULL) {
		first = new;
	} else {
		last->lr[svbit] = new;
		last->bitno &= ~(svbit ? RIGHT_IS_LEAF : LEFT_IS_LEAF);
	}
	if (bitno < cix)
		new->bitno |= (bit ? LEFT_IS_LEAF : RIGHT_IS_LEAF);
	return (char *)new->lr[bit];
}
