#include <cstdio>
#include <cstring>

inline
long long poleval(const int *cn, int n, int x)
{
  long long acc = 0;
  for (int k=0; k<n; k++)
    acc = x * (acc + cn[k]);
  return(acc+cn[n]);
}

int main()
{
  int cn[1000];
  int n, k, x, i, t=0;
  while (true)
  {
    t += 1;
    
    std::scanf("%d", &n);
    if (n==-1)
      break;

    for (i=0; i<n; i++)
      std::scanf("%d", cn+i);
    std::scanf("%d", cn+n);

    std::scanf("%d", &k);
    std::printf("Case %d:\n", t);
    for (i=0; i<k; i++)
    {
      std::scanf("%d", &x);
      std::printf("%lld\n", poleval(cn,n,x));
    }
  }
  return(0);
}

