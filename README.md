# dedupe_ml

An exercise in test driving OCaml - a Dedupe CLI tool to find duplicate files in a directory

## Build

```
dune build
```

## Tests

```
dune runtest
```

## Usage

```
# output in human readable format, duplicates detected under a given directory

    dedupe DIR

# example:
dedupe_ml % dune exec dedupe ./fixtures

./fixtures/mangled/flim/mew.jpg
./fixtures/mangled/mew.jpg

./fixtures/mangled/flam/laptop does run pretty hot.jpg
./fixtures/mangled/flim/cat.jpg

./fixtures/mangled/event-1/done deal
./fixtures/mangled/flim/done deal.jpg
```

