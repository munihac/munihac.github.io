# MuniHac

This is the Haskell project to build the (static) website [www.munihac.de](http://www.munihac.de).

## Building

Using [stack](https://www.haskellstack.org):

```bash
$ stack setup               # (optionally) download and use the correct ghc version
$ stack build
$ stack install hakyll
$ stack exec site rebuild
$ stack exec site watch     # now open the webpage in your browser: http://localhost:8000/
```
