#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAXNUMS 4096

typedef struct num 
{
	int n;			
	int x, y;		
}   num;

typedef struct symb 
{
	int x, y;
	int star;
}    symb;

num nums[MAXNUMS];
symb syms[MAXNUMS];
int inum, isym;

int nDigits(int n)
{
	if (n < 10)
	{
		return 1;
	}
	return 1 + nDigits(n / 10);
}

void addNum(int n, int x, int y)
{
	nums[inum++] = (num)
	{
		.n = n,.x = x,.y = y
	};
}

void addSym(int x, int y, int w)
{
	syms[isym++] = (symb)
	{
		.x = x,.y = y,.star = w
	};
}

int neighbors(int xa, int ya, int xb, int yb)
{
	return abs(xa - xb) <= 1 && abs(ya - yb) <= 1;
}

int numNextToSym(num n, symb s)
{
	int nd, i;
	nd = nDigits(n.n);
	for (i = 0; i < nd; i++) 
	{
		if (neighbors(n.x + i, n.y, s.x, s.y))
		{
			return 1;
		}
	}
	return 0;
}

int numCompAllSyms(num n)
{
	int j;
	for (j = 0; j < isym; j++)
	{
		if (numNextToSym(n, syms[j]))
		{
			return 1;
		}
	}
	return 0;
}

int64_t isGear(symb s)
{
	int i, nneighs = 0;
	int64_t prod = 1;
	for (i = 0; i < inum; i++)
	{
		if (numNextToSym(nums[i], s))
		{
			prod *= nums[i].n;
			nneighs += 1;
		}
	}
	return (nneighs == 2) * prod;
}

int getSum()
{
	int i, acc = 0;
	for (i = 0; i < inum; i++)
	{
		if (numCompAllSyms(nums[i]))
		{
			acc += nums[i].n;
		}
	}
	return acc;
}

int64_t getSumRatios()
{
	int i;
	int64_t acc = 0;
	for (i = 0; i < isym; i++)
	{
		if (syms[i].star)
		{
			acc += isGear(syms[i]);
		}
	}
	return acc;
}

void parseLine(char *l, ssize_t linelen, int y)
{
	int i = 0, j = 0, tnum = 0, x = 0, sx = 0;
	char rnum[8] = {0};	
	while (x < linelen) 
	{
		if (isdigit(l[x])) 
		{
			if (j == 0)	
			{
				sx = x;
			}
			rnum[j++] = l[x];
		}
		else if (j > 0) 
		{	
			tnum = atoi(rnum);
			addNum(tnum, sx, y);
			j = 0;
			memset(rnum, 0, 8);
		}
		if (l[x] != '.' && ispunct(l[x]))
		{
			addSym(x, y, l[x] == '*');
		}
		x += 1;
	}
}

int main(int argc, char *argv[])
{
	char *line = NULL;
	size_t linesize = 512;
	ssize_t linelen;
	FILE *fp;
	int acc1 = 0, acc2 = 0, lnum = 0, i = 0;
	fp = fopen(argv[1], "r");
	while ((linelen = getline(&line, &linesize, fp)) != -1)
	{
		parseLine(line, linelen, lnum);
		lnum += 1;
	}
	printf("hmm1: %d\n", getSum());
	printf("hmm2: %lld\n", getSumRatios());
	free(line);
	return 0;
}