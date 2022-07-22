# cabal-hoogle

Like `stack hoogle`, but works for cabal projects. Generates hoogle database of your local packages and all dependencies.

## Installation

Step 1, check the `Cabal` library version used by your cabal
```
cabal --version
```
It shows my cabal is using `3.6.3.0` of Cabal library
```
cabal-install version 3.6.2.0
compiled using version 3.6.3.0 of the Cabal library 
```

Step 2, use `cabal install` to install `cabal-hoogle` from Hackage, specifying the `Cabal` library version you fetched above.
```
cabal install --constraint="Cabal == 3.6.3.0" cabal-hoogle
```

## Usage

### Prerequisite

Make sure your `~/.cabal/config` has the following entries:

```
documentation: True
haddock
  hoogle: True
  html: True
```

Then, run `cabal build all` on your project, so that cabal will build haddock and
hoogle files for your local packages and dependencies

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