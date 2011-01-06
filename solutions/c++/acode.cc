Content-Type: multipart/related; start=<op.mhtml.1239480206608.424d080c34183c5c@10.10.1.36>; boundary=----------56XFP6wST7O9CjFM7A9WLE
Content-Location: https://www.spoj.pl/files/src/save/1595781/
MIME-Version: 1.0

------------56XFP6wST7O9CjFM7A9WLE
Content-Disposition: inline; filename=1595781-src.htm
Content-Type: text/html; name=1595781-src.htm
Content-Id: <op.mhtml.1239480206608.424d080c34183c5c@10.10.1.36>
Content-Location: https://www.spoj.pl/files/src/save/1595781/
Content-Transfer-Encoding: 8bit

#include <stdint.h>
#include <string>
#include <map>
#include <iostream>

class acode
{
  public:
  virtual ~acode()
  {}

  virtual uint64_t decodings_of(const std::string &s, const uint16_t &i)
  {
    const uint16_t sz = s.length();

    if (i == sz)
      return(1);

    const char c0 = s.at(i);
    if (c0 == '0')
      return(0);

    uint64_t r = 0;
    if (i < sz-1)
    {
      const char c1 = s.at(i+1);
      if (c0 == '1')
        if (c1 >= '0' && c1 <= '9')
          r += decodings_of(s, i+2);
      if (c0 == '2')
        if (c1 >= '0' && c1 <= '6')
          r += decodings_of(s, i+2);
    }
    r += decodings_of(s, i+1);

    return(r);
  }
};

class acode_proxy : public acode
{
  public:
  virtual ~acode_proxy()
  {}

  virtual uint64_t decodings_of(const std::string &s, const uint16_t &i=0)
  {
    std::map<uint16_t,uint64_t>::iterator it;
    if ((it=_cache.find(i)) == _cache.end())
      _cache[i] = acode::decodings_of(s, i);
    return(_cache[i]);
  }

  private:
  std::map<uint16_t,uint64_t> _cache;
};

int main()
{
  std::string input;
  while (true)
  {
    acode_proxy obj;
    std::cin >> input;
    if (input == "0")
      break;
    std::cout << obj.decodings_of(input, 0) << std::endl;
  }
  return(0);
}

------------56XFP6wST7O9CjFM7A9WLE--
