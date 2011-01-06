#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#define __HASHSZ 101
#ifndef bool
#  define bool char
#endif
#ifndef true
#  define true 1
#endif
#ifndef false
#  define false 0
#endif

struct hash_t
{
  char **_es;
  int _keys;
};

static
unsigned int __hashcode(const char *k)
{
  size_t i, sz=strlen(k);
  unsigned int r = 0;

  for (i=0; i<sz; i++)
    r += k[i] * (i+1);
  r *= 19;

  return(r);
}

static
void __print(const struct hash_t *ht)
{
  int i;

  printf("%d\n", ht->_keys);
  for (i=0; i<__HASHSZ; i++)
  {
    if (ht->_es[i] != NULL)
      printf("%d:%s\n", i, ht->_es[i]);
  }
}

void add(struct hash_t *h, const char *k)
{
  unsigned int i, hcode=__hashcode(k)%__HASHSZ, index=hcode;
  int index0 = -1;
  size_t sz = strlen(k);
  char *dst = NULL;
  
  for (i=0; i<20; i++)
  {
    if (i)
      index = (hcode+i*i+i*23) % __HASHSZ;
    if (h->_es[index] == NULL)
      index0 = (index0==-1 ? index : index0);
    else if (strcmp(h->_es[index], k) == 0)
      return;
  }

  if (index0 != -1)
  {
    dst = (char*) malloc(sz+1);
    if (dst != NULL)
    {
      strcpy(dst, k);
      h->_es[index0] = dst;
      h->_keys += 1;
    }
  }
}

void del(struct hash_t *h, const char *k)
{
  unsigned int i, hcode=__hashcode(k)%__HASHSZ, index=hcode;
  
  for (i=0; i<20; i++)
  {
    if (i)
      index = (hcode+i*i+i*23) % __HASHSZ;
    if (h->_es[index]!=NULL && strcmp(h->_es[index], k)==0)
    {
      free(h->_es[index]);
      h->_es[index] = NULL;
      h->_keys -= 1;
      break;
    }
  }
}

bool alloc(struct hash_t **r)
{
  int i;

  *r = (struct hash_t*)malloc(sizeof(struct hash_t));
  if (*r != NULL)
  {
    (*r)->_keys = 0;
    (*r)->_es  = (char**)malloc(__HASHSZ*sizeof(char**));
    if ((*r)->_es == NULL)
    {
      free(*r);
      *r = NULL;
    }

    for (i=0; i<__HASHSZ; i++)
      (*r)->_es[i] = NULL;
  }
  return(*r != NULL);
}

void dealloc(struct hash_t *r)
{
  int i;
  for (i=0; i<__HASHSZ; i++)
    free(r->_es[i]);
  free(r->_es);
  free(r);
}

int main()
{
  struct hash_t *ht = NULL;
  int testcases, operations, i, j;
  char tmp[20];

  scanf("%d", &testcases);
  for (i=0; i<testcases; i++)
  {
    alloc(&ht);

    scanf("%d", &operations);
    for (j=0; j<operations; j++)
    {
      scanf("%s", tmp);
      if (tmp[0] == 'A')
        add(ht, tmp+4);
      else
        del(ht, tmp+4);
    }
    __print(ht);

    dealloc(ht);
  }
  
  return(0);
}
