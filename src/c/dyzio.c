#include <stdio.h>
#include <stdlib.h>

static
void _dyzio(const char *s, int *i, int *a, int *c, int *m, int l)
{ 
  if (l > *m)
  {
    *m = l;
    *a = *c;
  }

  if (s[*i]=='\0')
    return;

  if (s[*i]=='1')
  {
    *c = *c + 1;
    *i = *i + 1;
    _dyzio(s, i, a, c, m, l+1);
    _dyzio(s, i, a, c, m, l+1);
  }
  else
  {
    *i = *i + 1;
  }
}

int dyzio(const char *s)
{
  int cuts     = 0;
  int maxdepth = 0;
  int level    = 0;
  int index    = 0;
  int answer   = 0;

  _dyzio(s, &index, &answer, &cuts, &maxdepth, level);
  return(answer);
}

int main(int argc, char *argv[])
{
  char *s = (char*) malloc(sizeof(char)*20001);
  int n, i;
  for (i=0; i<10; i++)
  {
    scanf("%d", &n);
    scanf("%s", s);
    s[n] = '\0';
    printf("%d\n", dyzio(s));
  }
  free(s);
  return(0);
}
