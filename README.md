# MuniHac

This is the Haskell project to build the (static) website [www.munihac.de](http://www.munihac.de).

## Building

Using [Nix/NixOS](https://nixos.org/):

```
nix-build
```

Using [stack](https://www.haskellstack.org):

```bash
stack setup               # (optionally) download and use the correct ghc version
stack build
stack install hakyll
stack exec site rebuild
stack exec site watch     # now open the webpage in your browser: http://localhost:8000/
```

## Publishing to master

Changes are made on `gh-pages-source`, live version is built via hakyll and published on `master`.

```bash
# on gh-pages-source
stack build
stack exec site clean
stack exec site build
git checkout master
cp -R _site/* .
```
