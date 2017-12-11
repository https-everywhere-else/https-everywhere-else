[https-everywhere-rules](https://github.com/fmap/https-everywhere-rules)
========================================================================

Haskell package providing high-level access to [HTTPS Everywhere][1]
rulesets. It aims to make it easier to specify: "use secure HTTP
connections when possible."

  [1]: https://www.eff.org/https-everywhere

```haskell
λ: :m + Data.HTTPSEverywhere.Rules Network.URI
λ: rules <- getRulesets
λ: let Just it = parseURI "http://httpsnow.org"
λ: rewriteURL rules it
Just https://httpsnow.org
```
