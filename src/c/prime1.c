#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

#define MAX_PRIME 31601
#define PRIMES    3400
#define RANGE     100001

void e_sieve(int *primes, int n)
{
  int k, z, t=1;
  unsigned char *sieve = (unsigned char*) malloc((n+1)*sizeof(char));
  memset(sieve, 0, (n+1)*sizeof(char));

  primes[0] = 2;
  for (k=3; k<=n; k+=2)
  {
    if (sieve[k] == 1)
      continue;
    primes[t] = k; t+=1;
    for (z=k+k; z<n; z+=k)
      sieve[z] = 1;
  }

  /* free(sieve); */
}

void print_primes(const int *primes, long int n, long int m)
{
  static unsigned char sieve[RANGE];
  long int k, z, p=0, t=m-n;
  memset(sieve, 0, t+1);

  sieve[0] = n==1;
  for (k=0; k<PRIMES; k+=1)
  {
    p = primes[k];
    z = p - n%p;
    for (z-=p; z<=t; z+=p)
    {
      if (z>=0)
        sieve[z] = z+n > p;
    }
  }

  if (n<=2)
    printf("2\n");
  for (k=(n%2==0 ? 1 : 0); k<=t; k+=2)
  {
    if (sieve[k] == 0)
      printf("%ld\n", k+n);
  }
  printf("\n");
}

int main()
{
  long int t, n, m, k;
  int *primes = (int*) malloc(PRIMES * sizeof(int));
  e_sieve(primes, MAX_PRIME);

  scanf("%ld", &t);
  for (k=0; k<t; k+=1) {
    scanf("%ld %ld", &n, &m);
    print_primes(primes, n, m);
  }
  
  /* free(primes); */
  return(0);
}
