#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>

uint32_t sqrs_in_grid(const uint8_t n)
{
  uint8_t i;
  uint32_t r = 0;

  for (i=1; i<=n; i++)
  {
    r += i*i;
  }

  return(r);
}

int main(int argc, char *argv[])
{
  unsigned int grid;
  while (1)
  {
    scanf("%u", &grid);
    if (grid==0)
      break;
    printf("%d\n", sqrs_in_grid(grid));
  }
  return(0);
}
