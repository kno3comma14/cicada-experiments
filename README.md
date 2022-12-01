# cicada-experiments

Project to create some crypto useful functions.

## Requirements

- Clojure 1.10.3 or superior
- lein

## Usage

### Running the tests

Run 
```Bash 
lein test
```

### Using from external projects

WIP

## Examples

### sc_reduce32

```clojure
(require '[cicada-experiments.core :as core])
(core/gc-reduce32 "f9a0e73d3cd533368f75ff63cbd97b2100beffbc339cdfa5c203c1a022d9cf11")
"0ccdf1e0217221deb8d807c1ecdf9c0c00beffbc339cdfa5c203c1a022d9cf01"
```

This is useful for the private(spend and view) keys.

### ->public-key
```clojure
(require '[cicada-experiments.core :as core])
(core/->public-key "f303de33534d6a9e46497cf177e12b7bdfaf1405b2a03b5a7074a74b0946a805")
"d76344d2c5467758f0bcbf03925bc8bf4b659e163ec68c342c7ba94b9679a125"
```

This is useful for the public(spend and view) keys.
