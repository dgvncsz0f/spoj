#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#define minimum(a,b) (a<b ? a : b)
#define minimum3(a,b,c) (minimum (a,minimum (b,c)))

static
int *matrix = NULL;

int ld (const char *s, const char *t)
{
  size_t m = strlen(s) + 1;
  size_t n = strlen(t) + 1;
  size_t i, j, k;

  for (i=0; i<m; i+=1)
    matrix[i*n] = i;
  for (i=0; i<n; i+=1)
    matrix[i] = i;

  for (j=1; j<n; j+=1)
  {
    for (i=1; i<m; i+=1)
    {
      k = i*n + j;
      if (s[i-1] == t[j-1]) {
        matrix[k] = matrix[(i-1)*n + (j-1)];
      }
      else
      {
        matrix[k] = minimum3( matrix[(i-1)*n+j] + 1
                            , matrix[i*n+(j-1)] + 1
                            , matrix[(i-1)*n+(j-1)] + 1
                            );
      }
    }
  }
  
  return(matrix[(m-1)*n+(n-1)]);
}

int main()
{
  char *s0 = calloc(2000, sizeof(char));
  char *s1 = calloc(2000, sizeof(char));
  unsigned int t, k;

  matrix = calloc(2001*2001, sizeof(int));
  scanf("%d", &t);
  for (k=0; k<t; k+=1)
  {
    scanf("%s %s", s0, s1);
    printf("%d\n", ld(s0, s1));
  }
  return(0);
}
