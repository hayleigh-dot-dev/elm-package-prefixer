# elm-package-prefixer

**Basic usage**:

```
npm i -g elm-package-prefixer

elm-package-prefixer --package-name ianmackenzie/elm-units --prefix Units
```

**All arguments**:

```
  --package-name, -n   [required] The name of the package to prefix.
  --tag, -t            [optional] The version tag to pull from.
  --author, -a         [optional] The author of the generated package.
  --prefix, -p         [required] The prefix to use when generating.
  --out-dir, -d        [optional] The directory to save the generated package.

  --help                          Show this message!
  --help [argument]               Show more detailed info about a particular
                                  argument.
```

## The Problem

This package is a band-aid solution to a problem that arises when one package
depends on another, but both packages expose a module (or many) with the same
name.

The issue most recently came to light when trying to update the `elm-visualization`
package to use `elm-geometry` version 3. The core problem is that `elm-geometry` now
uses `elm-units` throughout its API, but both `elm-visualization` and `elm-units`
include an exposed `Force` module (for force-directed layouts and force the physical
quantity respectively).

Since `elm-geometry` uses `elm-units` types throughout its API, basically any
code that depends on/uses `elm-geometry` directly also has to depend on/use
`elm-units`directly. However, if `elm-visualization` depends on `elm-units`
directly, then within the `elm-visualization` source code i`mport Force` is
ambiguous - does it refer to the `elm-units` module or the `elm-visualization`
module?

## The Solution

The temporary solution we provide here is to quickly create a _prefixed_ version
of an already-published package so that it can be published and depended on where
needed. For example, we could wrap the `Force` module found in `elm-units` to get
something like:

```elm
module Units.Force exposing
    ( Force
    , Newtons
    , newtons
    , inNewtons
    , ...
    )

import Force

type alias Force =
    Force.Force

type alias Newtons =
    Force.Newtons

newtons : Float -> Force
newtons =
    Force.newtons

inNewtons : Force -> Float
inNewtons =
    Force.inNewtons
```

Now, `elm-visualization` can depend on _this_ package and not worry about the
ambiguous `Force` module name.

## Development

* Clone this repository
* run `npm install`
* run `npm run build:dev` 
* run `chmod +x index.js`
* run `./index.js --help`
