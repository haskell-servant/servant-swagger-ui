# servant-swagger-ui

*Provide a Swagger UI for your Servant API*

"[Swagger UI](http://swagger.io/swagger-ui/) allows anyone — be it your development team or your end consumers — to visualize and interact with the API’s resources without having any of the implementation logic in place."

[![Build Status](https://github.com/haskell-servant/servant-swagger-ui/actions/workflows/haskell-ci.yml/badge.svg?branch=master)](https://github.com/haskell-servant/servant-swagger-ui/actions)
[![Hackage](https://img.shields.io/hackage/v/servant-swagger-ui.svg)](https://hackage.haskell.org/package/servant-swagger-ui)
[![Stackage LTS](https://stackage.org/package/servant-swagger-ui/badge/lts)](https://stackage.org/lts/package/servant-swagger-ui)
[![Stackage Nightly](https://stackage.org/package/servant-swagger-ui/badge/nightly)](https://stackage.org/nightly/package/servant-swagger-ui)

## Example

![example screenshot](https://raw.githubusercontent.com/haskell-servant/servant-swagger-ui/master/screenshot.png)

Check [`servant-swagger-ui-example/src/Main.hs`](https://github.com/haskell-servant/servant-swagger-ui/blob/master/servant-swagger-ui-example/src/Main.hs) for an example.

## Development

### Updating of bundled swagger-ui version:

- Extract `dist` directory of `swagger-ui` into `swagger-dist-&lt;version&gt;`. Remove the `*.map` files.
- update `extra-source-files` in the `.cabal`
- move `index.html` into `index.html.tmpl`, do the diff, port the changes
- search replace the embedded directory (in `Servant/Swagger/UI.hs`)
- test it works
