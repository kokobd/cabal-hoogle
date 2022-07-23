# cabal-hoogle

![GitHub Action Badge](https://github.com/kokobd/cabal-hoogle/actions/workflows/test.yml/badge.svg?branch=main)

Like `stack hoogle`, but works for cabal projects. Generates hoogle database of your local packages and all dependencies.

- mininum GHC version: 8.10
- minimum Cabal version: 3.2

## Installation

Step 1, run `cabal update` to make sure you are using the latest hackage index

Step 2, check the `Cabal` library version used by your cabal
```
cabal --version
```
It shows my cabal is using `3.6.3.0` of Cabal library
```
cabal-install version 3.6.2.0
compiled using version 3.6.3.0 of the Cabal library 
```

Step 3, use `cabal install` to install `cabal-hoogle` from Hackage, specifying the `Cabal` library version you fetched above.
```
cabal install --constraint="Cabal == 3.6.3.0" cabal-hoogle
```

## Usage

### Prerequisite

Make sure your `~/.cabal/config` has the following entries. (DON'T remove existing entries!) [Check the official docs](https://cabal.readthedocs.io/en/3.6/installing-packages.html) if you don't know where is your cabal's global configuration file 

```
documentation: True
haddock
  hoogle: True
  html: True
```

Or, run this command in bash:
```
cabal user-config update -a "documentation: True
haddock
  hoogle: True
  html: True"
```

Then, run `cabal build all` on your project, so that cabal will build haddock and
hoogle files for your local packages and dependencies.

> Adding `--enable-documentation --haddock-hoogle --haddock-html` to your build command won't work for dependencies as I tried.

### Generate
```
cabal-hoogle generate
```

This generates the hoogle database. You will see logs very similar to `stack hoogle --rebuild`

### Run Hoogle

With `cabal-hoogle run --`, extra arguments are passed directly to `hoogle`.

You may start a server like this:
```
cabal-hoogle run -- server --local --port 9000
```

Or directly search in command line like this:
```
cabal-hoogle run -- search catMaybes
```

### More Options

Run `cabal-hoogle --help` to see more options. For example if you are using a non-default build dir (not `dist-newstyle`), or you have multiple GHC verisons on the same project.

Below is a complex example:
```
cabal-hoogle --compiler ghc-9.2.3 --platform "x86_64-linux" --builddir mydist generate
```

## FAQ

### Many packages missing documentation

If you see output like this when running `cabal-hoogle generate`
```
Packages missing documentation: ansi-terminal ansi-wl-pprint async base-orphans base16-bytestring base64-bytestring Cabal cabal-hoogle clock colour extra hashable haskell-src-exts haskell-src-meta optparse-applicative primitive regex-base regex-tdfa safe split string-interpolate syb text-conversions th-abstraction th-compat th-expand-syns th-lift th-lift-instances th-orphans th-reify-many transformers-compat typed-process unliftio-core utf8-string vector vector-stream
Found 27 warnings when processing items
```

Probably you haven't built dependencies with hoogle enabled. Check [prerequisites](#prerequisite) more carefully.

### Cabal version mismatch

Make sure to build `cabal-hoogle` with the exact same `Cabal` library as your `cabal-install`. You can use `cabal --version`
to check the version of `Cabal` library. See [Installation](#installation).
