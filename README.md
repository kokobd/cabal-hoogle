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
```
cabal-hoogle generate
```

This generates the hoogle database. You will see logs very similar to `stack hoogle --rebuild`

You may specify targets, like:
```
cabal-hoogle generate exe:haskell-language-server
```
This is very useful if you have many local packages in your `cabal.project`,
and some of them don't build.

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

Run `cabal-hoogle --help` to see more options.
