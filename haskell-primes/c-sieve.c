/* Basedo on code found at
 *   http://www.algorithmist.com/index.php/Prime_Sieve_of_Eratosthenes.c 
 * My changes are in keeping with the amazingly ugly style of the code.
 */

/* This code is in public domain. Use for whatever purpose at your own risk. */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* The code assumes, that sizeof(unsigned)==4 */

#if 0
#define MAXN  100000000  /* maximum value of N */
#define P1    1562501    /* = ceil(MAXN/64) */
#define P2    50000000   /* = ceil(MAXN/2) */
#define P3    5000       /* = ceil(ceil(sqrt(MAXN))/2) */

unsigned sieve[P1];
#endif
unsigned int MAXN, P1, P2, P3;
unsigned int *sieve;

#define GET(b) ((sieve[(b)>>5]>>((b)&31))&1)

void make()
{
    register unsigned i, j, k;
    //    memset(sieve, 0, sizeof(sieve));
    for (k = 1; k <= P3; k++)
        if (GET(k)==0) for(j=2*k+1,i=2*k*(k+1);i<P2;i+=j) sieve[i>>5]|=1<<(i&31);
}

int isprime(int p) { return p==2 || (p>2 && (p&1)==1 && (GET((p-1)>>1)==0)); }

int main(int argc, const char** argv)
{
    int i, n;
    MAXN = atoi(argv[1]);
    P1 = (MAXN + 63)/64;
    P2 = (MAXN + 1)/2;
    P3 = (ceil(sqrt(MAXN)) + 1) / 2;
    sieve = (unsigned int*) calloc(P1,sizeof(unsigned int));
    make();
    for (n = 0, i = 0; i < P2; i++)
        if (GET(i)==0) n=i;
    if (n == 0) n=2;
    else n=2*n+1;
    printf("The last prime before %d is %d.\n", MAXN, n);
    return 0;
}
