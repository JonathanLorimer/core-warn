# Core Warn

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
```

### Cabal
