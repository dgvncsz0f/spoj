#include <string>
#include <map>
#include <cstdio>

int main() 
{
  int n,p;
  std::scanf("%d\n", &n);

  for (int i=0; i<n; i++)
  {
    std::map<std::string,int> m;
    std::scanf("%d\n", &p);
    for (int j=0; j<p; j++) 
    {
      char line[33];
      std::gets(line);
      m[line]++;
    }
    char line[33];
    std::gets(line);
    
    std::map<std::string,int>::iterator it;
    for (it=m.begin();it!=m.end();it++) {
      std::printf("%s %d\n", it->first.c_str(),it->second);
    }
    std::printf("\n");
  }

  return(0);
}
