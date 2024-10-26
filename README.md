# cabal-hoogle

![GitHub Action Badge](https://github.com/kokobd/cabal-hoogle/actions/workflows/test.yml/badge.svg?branch=main)

Like `stack hoogle`, but works for cabal projects. Generates hoogle database of your local packages and all dependencies.

<img src="https://user-images.githubusercontent.com/16440269/180609310-643ff9a1-c1eb-479a-b9ca-0cf69d65a62a.gif" width="600"/>

## Installation

```
git clone https://github.com/kokobd/cabal-hoogle.git
cd cabal-hoogle
cabal install exe:cabal-hoogle
```

## Usage

### Generate
Within your project, run:

```
cabal-hoogle generate
```

This generates a hoogle database for all the local packages and their dependencies.

You may specify targets, like below:
```
cabal-hoogle generate exe:haskell-language-server hls-code-range-plugin
```
See [Cabal Docs](https://cabal.readthedocs.io/en/3.8/cabal-commands.html#target-forms) for target syntax

### Run Hoogle

With `cabal-hoogle run --`, extra arguments are passed directly to `hoogle`.

You may start an http server like this:
```
cabal-hoogle run -- server --local --port 9000
```

Or directly search in command line like this:
```
cabal-hoogle run -- search catMaybes
```

### More Options

Run `cabal-hoogle --help` to see more options.

## Contributing

The CI is managed by [`haskell-ci`](https://github.com/haskell-CI/haskell-ci).
After adding support of any new GHC version, run the following command using the
latest `haskell-ci` executable to regenerate a new GHA workflow file.

```haskell
haskell-ci regenerate --no-unconstrained --installed=-all
```