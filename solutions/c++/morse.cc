Content-Type: multipart/related; start=<op.mhtml.1239480310570.bbaead003119355f@10.10.1.36>; boundary=----------KvURt9RPilGWLD8GjZvQZL
Content-Location: https://www.spoj.pl/files/src/save/1604244/
MIME-Version: 1.0

------------KvURt9RPilGWLD8GjZvQZL
Content-Disposition: inline; filename=1604244-src.htm
Content-Type: text/html; name=1604244-src.htm
Content-Id: <op.mhtml.1239480310570.bbaead003119355f@10.10.1.36>
Content-Location: https://www.spoj.pl/files/src/save/1604244/
Content-Transfer-Encoding: 8bit

#include <stdint.h>
#include <string>
#include <map>
#include <set>
#include <iostream>

#define DICTENTRY_LC '.'

class dictentry
{
  public:
  dictentry() :
    _l(0),
    _r(0),
    _leafcount(0)
  {}

  ~dictentry()
  {
    if (_l)
      delete _l;
    if (_r)
      delete _r;
  }

  dictentry *find(char c)
  {
    return(c == DICTENTRY_LC ? _l : _r);
  }

  dictentry *append(char c)
  {
    dictentry *r = 0;
    if (c == DICTENTRY_LC)
    {
      if (!_l)
        _l = new dictentry;
      r  = _l;
    }
    else
    {
      if (!_r)
        _r = new dictentry;
      r  = _r;
    }
    return(r);
  }

  dictentry *l() const
  {
    return(_l);
  }

  dictentry *r() const
  {
    return(_r);
  }

  void set_leaf()
  {
    _leafcount += 1;
  }

  uint16_t leafcount() const
  {
    return(_leafcount);
  }
  
  private:
  dictentry *_l;
  dictentry *_r;
  uint16_t _leafcount;
};

class dictionary
{
  public:
  ~dictionary()
  {
    delete _root;
  }

  dictionary() :
    _root(new dictentry),
    _curr(_root)
  {}

  void append(const std::string *mstr)
  {
    uint16_t sz = mstr->size();
    for (uint16_t i=0; i<sz; i++)
      _curr = _curr->append(mstr->at(i));
    _curr->set_leaf();
    _curr = _root;
  }

  dictionary *find(char c)
  {
    _curr = _curr->find(c);
    return(this);
  }

  dictentry *snapshot() const
  {
    return(_curr);
  }

  dictentry *root() const
  {
    return(_root);
  }

  void restore()
  {
    _curr = _root;
  }

  private:
  dictentry *_root;
  dictentry *_curr;
};

class morse
{
  public:
  static
  std::string from_ascii(const std::string *m)
  {
    uint16_t sz = m->size();
    std::string r;
    for (uint16_t i=0; i<m->size(); i++)
      r += from_ascii(m->at(i));
    return(r);
  }

  static
  std::string from_ascii(char c)
  {
    switch (c)
    {
      case 'A':
        return(std::string(".-"));
      case 'B':
        return(std::string("-..."));
      case 'C':
        return(std::string("-.-."));
      case 'D':
        return(std::string("-.."));
      case 'E':
        return(std::string("."));
      case 'F':
        return(std::string("..-."));
      case 'G':
        return(std::string("--."));
      case 'H':
        return(std::string("...."));
      case 'I':
        return(std::string(".."));
      case 'J':
        return(std::string(".---"));
      case 'K':
        return(std::string("-.-"));
      case 'L':
        return(std::string(".-.."));
      case 'M':
        return(std::string("--"));
      case 'N':
        return(std::string("-."));
      case 'O':
        return(std::string("---"));
      case 'P':
        return(std::string(".--."));
      case 'Q':
        return(std::string("--.-"));
      case 'R':
        return(std::string(".-."));
      case 'S':
        return(std::string("..."));
      case 'T':
        return(std::string("-"));
      case 'U':
        return(std::string("..-"));
      case 'V':
        return(std::string("...-"));
      case 'W':
        return(std::string(".--"));
      case 'X':
        return(std::string("-..-"));
      case 'Y':
        return(std::string("-.--"));
      case 'Z':
        return(std::string("--.."));
      default:
        return(std::string());
    }
  }

  virtual ~morse()
  {}

  uint32_t phrases_of(const std::string *mstr, dictionary *dict)
  {
    if (mstr->size())
      return(_phrases_of(mstr, 0, dict->find(mstr->at(0))));
    return(0);
  }

  protected:
  virtual uint32_t _phrases_of(const std::string *mstr, uint16_t idx, dictionary *dict)
  {
    dictentry *state = dict->snapshot();
    if (!state)
      return(0);

    uint16_t lcount = state->leafcount();
    if (idx+1 >= mstr->size())
      return(lcount);

    char c = mstr->at(idx+1);

    uint32_t r = _phrases_of(mstr, idx+1, dict->find(c));
    if (lcount)
    {
      dict->restore();
      r += (lcount ? lcount : 1) * _phrases_of(mstr, idx+1, dict->find(c));
    }

    return(r);
  }
};

class morse_proxy : public morse
{
  public:
  virtual ~morse_proxy()
  {}

  protected:
  virtual uint32_t _phrases_of(const std::string *mstr, uint16_t idx, dictionary *dict)
  {
    uint64_t k = (reinterpret_cast<uint32_t>(dict->snapshot()) << 16) | idx;

    std::map<uint64_t,uint32_t>::iterator it;
    if ((it=_cache.find(k)) == _cache.end())
    {
      uint32_t r = morse::_phrases_of(mstr, idx, dict);
      _cache[k]  = r;
      return(r);
    }
    return(_cache[k]);
  }

  private:
  std::map<uint64_t,uint32_t> _cache;
};

int main()
{
  int dsets, dsize;
  std::cin >> dsets;

  for (int i=0; i<dsets; i++)
  {
    std::string mstr;
    std::cin >> mstr;

    std::cin >> dsize;
    dictionary dict;
    for (int j=0; j<dsize; j++)
    {
      std::string word;
      std::cin >> word; 
      word = morse::from_ascii(&word);
      dict.append(&word);
    }
    
    morse_proxy m;
    std::cout << m.phrases_of(&mstr, &dict) << std::endl;
  }

  return(0);
}

------------KvURt9RPilGWLD8GjZvQZL--
