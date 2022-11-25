# cicada-experiments

Project to create some crypto useful functions.

## Requirements

- Clojure 1.10.3 or superior
- lein

## Usage

### Running the tests

Run 
```sh 
lein test
```

### Using from external projects

WIP

## Examples

### sc_reduce32

```clojure
(require '[cicada-experiments.core :as core])
(core/sc-reduce32 "f9a0e73d3cd533368f75ff63cbd97b2100beffbc339cdfa5c203c1a022d9cf11")
"0ccdf1e0217221deb8d807c1ecdf9c0c00beffbc339cdfa5c203c1a022d9cf01"
```
