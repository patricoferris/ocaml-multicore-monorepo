# An OCaml Multicore Monorepo

*Status: WIP and Experimental*

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Flow.copy_string "Multicore Monorepo!\n" (Eio.Stdenv.stdout env);;
Multicore Monorepo!
- : unit = ()
```

Whilst OCaml 5.00.0+trunk stabilises, setting up an opam switch is tricky as there are quite a few deprecated functions causing lots of packages to not build.

If you are happy to use `dune` as your build tool, this repository vendors lots of packages so you can just run `dune build <target>`. It does this using [opam-monorepo][]. Some of the packages are "hot-fixed" just to get this repository off of the ground, so your mileage may vary. To get up and running, all you need to do is:

```
$ opam update # only if you need to
$ opam switch create 5.00.0+trunk
$ opam install dune.2.9.2 
```

From there you can make sure the small `Eio_main` function works from the `README.md` by running `dune runtest`. There is an `example` directory too that can be built with `dune build`.

Then create a `src` directory for your project, build it using the vendored libraries and run `dune build`. As `5.00.0+trunk` progresses more things are likely to break so you could be unfortunate enough to get a new 5.00.0 compiler that deprecates some more libraries and causes things to break. Feel free to open an issue if that is the case!


[opam-monorepo]: https://github.com/ocamllabs/opam-monorepo
