# cabal-hoogle

![GitHub Action Badge](https://github.com/kokobd/cabal-hoogle/actions/workflows/test.yml/badge.svg?branch=main)

Like `stack hoogle`, but works for cabal projects. Generates hoogle database of your local packages and all dependencies.

<img src="https://user-images.githubusercontent.com/16440269/180609310-643ff9a1-c1eb-479a-b9ca-0cf69d65a62a.gif" width="600"/>

## Installation

Compiling from source requires a minimum GHC version of 8.10. Having `cabal-hoogle` installed, you may use it on a wider range of GHC,
as long as the `Cabal` used to build `cabal-hoogle` and `cabal-install` match.

Step 1, run `cabal update` to make sure you are using the latest hackage index

Step 2, check the `Cabal` library version used by your cabal
```
cabal --version
```
It shows my cabal is using `3.6.3.0` of Cabal library. **`cabal-hoogle` requires at least Cabal 3.2**. Update it before continue.
```
cabal-install version 3.6.2.0
compiled using version 3.6.3.0 of the Cabal library 
```

Step 3, use `cabal install` to install `cabal-hoogle` from Hackage, specifying the `Cabal` library version you fetched above.
```
cabal install --constraint="Cabal == 3.6.3.0" cabal-hoogle
```

Note that if your package uses a custom `Setup.hs`, you **MUST** set `Cabal` version to match `cabal` and `cabal-hoogle`, otherwise it won't work.
For example, add this `cabal.project.local` to your project:
```
constraints:
  Cabal == 3.6.3.0
```

## Usage

### Prerequisite

#### Install Hoogle
Make sure `hoogle` is installed in your `$PATH`. You can do this by running `cabal install hoogle`

#### Enable Hoogle for Cabal
Make sure your `~/.cabal/config` has the following entries. (DON'T remove existing entries!) [Check the official docs](https://cabal.readthedocs.io/en/3.6/installing-packages.html) if you don't know where is your cabal's global configuration file 

```
haddock
  hoogle: True
  html: True
  hyperlink-source: True
  quickjump: True
```

Or, run this command in bash:
```
cabal user-config update -a "
haddock
  hoogle: True
  html: True
  hyperlink-source: True
  quickjump: True
```

#### Build Project with Cabal

Then, run `cabal build --enable-documentation all` on your project, so that cabal will build haddock and
hoogle files for your local packages and dependencies.

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

### Core libraries missing documentation

If you are on macOS or Windows, and using `ghcup`, your `ghc` probably doesn't come with docs.
See [this](https://gitlab.haskell.org/ghc/ghc/-/issues/20903) and [this](https://github.com/haskell/haskell-language-server/issues/208#issuecomment-1162169087) for details of the upstream issue.

### Run "configure" first

If you see this message from the output, it means a package doesn't have a `setup-config` file. This could happen if your package doesn't have a `library` section, or you were building with optimization level 0 or 2 (we are reading the build products with default optimization level of library only)

## Roadmap

After Cabal 3.8 is released, I will

- [ ] Use `cabal-install` library to get the structure of `dist-newstyle` instead of guessing it.
- [ ] Automatically build haddock html and hoogle txt files when `generate` runs.
