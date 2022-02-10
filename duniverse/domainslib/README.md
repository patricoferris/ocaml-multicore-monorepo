# domainslib - Parallel Programming over Multicore OCaml

`domainslib` provides control and data structure for parallel programming using
domains.

The supported data structures are:

* [Channels](https://github.com/ocaml-multicore/domainslib/blob/master/lib/chan.mli)
  + Channels may be shared between multiple senders and receivers.
  + Channels ensure FIFO message order. 
  + Channels come in two flavours -- bounded (fixed-buffer size (>= 0) beyond which the
    send blocks) and unbounded.
* [Task](https://github.com/ocaml-multicore/domainslib/blob/master/lib/task.mli)
  + Work-stealing task pool with async/await parallelism and parallel for loop.

See
[examples](https://github.com/ocaml-multicore/domainslib/tree/master/test)
for usage.

## Installation

The library can be installed using
[`multicore-opam`](https://github.com/ocaml-multicore/multicore-opam), an OPAM
repository of multicore specific packages.

```bash
$ opam switch create 4.12.0+domains+effects
$ opam install domainslib
```

## Development

If you are interested in hacking on the implementation, then `opam pin` this
repository:

First install the multicore compiler and dune:
```bash
$ opam switch create 4.12.0+domains+effects
$ opam install dune
```

Then, pin a clone of this repo:

```bash
$ git clone https://github.com/ocaml-multicore/domainslib
$ cd domainslib
$ opam pin add domainslib file://`pwd`
```
