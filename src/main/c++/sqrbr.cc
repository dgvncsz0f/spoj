Content-Type: multipart/related; start=<op.mhtml.1239480325898.bf6cab05371a3c5c@10.10.1.36>; boundary=----------4Y5zhtGijZClE7J0HCLXeZ
Content-Location: https://www.spoj.pl/files/src/save/1597412/
MIME-Version: 1.0

------------4Y5zhtGijZClE7J0HCLXeZ
Content-Disposition: inline; filename=1597412-src.htm
Content-Type: text/html; name=1597412-src.htm
Content-Id: <op.mhtml.1239480325898.bf6cab05371a3c5c@10.10.1.36>
Content-Location: https://www.spoj.pl/files/src/save/1597412/
Content-Transfer-Encoding: 8bit

#include <stdint.h>
#include <string>
#include <list>
#include <set>
#include <map>
#include <iostream>
class bexpression
{
  public:
  enum bracket_t
  {
    RIGHT = 0,
    LEFT  = 1
  };

  bexpression(const uint8_t &n):
    _n(n*2),
    _s(0),
    _bogus(0)
  {
  }
  
  void push(const bracket_t &t)
  {
    _bogus += (t == LEFT ? 1 : -1);
    _s     += 1;
  }
  
  void pop(const bracket_t &t)
  {
    _bogus -= (t == LEFT ? 1 : -1);
    _s     -= 1;
  }

  bool complete() const
  {
    return(_s == _n);
  }

  bool bogus() const
  {
    return(_bogus < 0 || (complete() && _bogus != 0));
  }

  uint8_t state() const
  {
    return(_bogus);
  }

  uint8_t size() const
  {
    return(_s);
  }

  uint8_t index() const
  {
    return(_s - 1);
  }

  private:
  uint8_t _n;
  uint8_t _s;
  int8_t _bogus;
};

class sqrbr
{
  public:
  virtual ~sqrbr()
  {}

  void fix_lsymbol(uint8_t i)
  {
    _ls.insert(i);
  }

  virtual uint32_t valid_exprs(bexpression *b)
  {
    if (b->bogus())
      return(0);

    if (b->complete())
      return(1);
    else
    {
      uint32_t r = 0;
      
      b->push(bexpression::LEFT);
      r += valid_exprs(b);
      b->pop(bexpression::LEFT);
      
      if (_ls.find(b->size()) == _ls.end())
      {
        b->push(bexpression::RIGHT);
        r += valid_exprs(b);
        b->pop(bexpression::RIGHT);
      }

      return(r);
    }
  }

  private:
  std::set<uint8_t> _ls;
};

class sqrbr_proxy : public sqrbr
{
  public:
  virtual uint32_t valid_exprs(bexpression *b)
  {
    const uint16_t k = ((b->state()&0xFF) << 8) | (b->index()&0xFF);
    std::map<uint16_t,uint32_t>::iterator it;
    if ((it=_cache.find(k)) != _cache.end())
      return(it->second);

    uint32_t r = sqrbr::valid_exprs(b);
    _cache[k] = r;
    return(r);
  }

  private:
  std::map<uint16_t,uint32_t> _cache;
};

int main()
{
  unsigned int test_cases;

  std::cin >> test_cases;
  for (unsigned int i=0; i<test_cases; i++)
  {
    unsigned int n, k;
    std::cin >> n;
    std::cin >> k;

    sqrbr_proxy obj;
    obj.fix_lsymbol(0);
    for (unsigned int i=0; i<k; i++)
    {
      unsigned int s;
      std::cin >> s;
      obj.fix_lsymbol(s-1);
    }

    bexpression exp(n);
    std::cout << obj.valid_exprs(&exp) << std::endl;
  }

  return(0);
}

------------4Y5zhtGijZClE7J0HCLXeZ--
