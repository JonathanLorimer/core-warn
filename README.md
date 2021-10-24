<h1 align="center"> Core Warn </h1>
<p align="center">
<a href="https://github.com/JonathanLorimer/core-warn/actions"></a>
  <img src="https://img.shields.io/github/workflow/status/JonathanLorimer/core-warn/Haskell CI?style=flat-square" alt="CI badge" />
</a>
<a href="https://haskell.org">
  <img src="https://img.shields.io/badge/Made%20in-Haskell-%235e5086?logo=haskell&style=flat-square" alt="made with Haskell"/>
</a>
<a href="https://hackage.haskell.org/package/core-warn">
  <img src="https://img.shields.io/hackage/v/core-warn.svg?logo=haskell&label=core-warn&style=flat-square" alt="hackage link" />
</a>
</p>

## Motivation

This plugin was inspired by these two blog posts from [Well
Typed](https://well-typed.com/): [part
1](https://well-typed.com/blog/2021/08/large-records/), [part
2](https://well-typed.com/blog/2021/10/large-records-part-2/). They discuss two
scenarios where GHC's core representation deviates drastically from what one
might expect. These phenomena can be grouped into two buckets: instances where
GHC produces too many unnecessary type coercions, and instances where GHC
produces large chains of dictionary references. The first can be solved by
ensuring that all expensive instances occur in a phantom context, and the
second can be solved by ensuring that all inductive instances are balanced.

## Usage

### Stack

In your `stack.yaml` add this

```yaml
extra-deps:
- core-warn-0.1.0.0
```

and then in `package.yaml` add:

```yaml
dependencies:
- core-warn

ghc-options:
- -fplugin=CoreWarn
```

### Cabal

Add these lines to the corresponding stanza of your cabal file. For example if
you want to run `CoreWarn` on your library stanza, add the below code block as
a child of `library`.

```cabal
ghc-options:      -fplugin=CoreWarn
build-depends:    base >=4.10 && <5
                , ...
                , core-warn
```

or you can run this option from the command line for a specific component, in
this example we chose the `test` component:

```shell
$ cabal repl test --ghc-options=-fplugin=CoreWarn
```

### Options

Here is an example of how you would apply an option to this plugin. For the
sake of variety we will use a file level pragma as the example.

```haskell
{-# OPTIONS_GHC -fplugin-opt=CoreWarn:<opt-name> #-}
```

The plugin accepts four arguments, but by default opts in. For example if you
wanted to only show warning for one kind of issue mentioned above, you could
use the follwing options:
  - `warn-large-coercions`
  - `warn-deep-dicts`

If you have `CoreWarn` enabled for the entire project, you might want to disable it
in a particular file. You can do so with the following options:
  - `no-warn-large-coercions`
  - `no-warn-deep-dicts`
