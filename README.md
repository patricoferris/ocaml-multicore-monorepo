# An OCaml Multicore Monorepo

```ocaml
# Eio_main.run @@ fun env ->
  Eio.Flow.copy_string "Multicore Monorepo!\n" (Eio.Stdenv.stdout env);;
Multicore Monorepo!
- : unit = ()
```

Whilst OCaml 5.00.0+trunk stabilises, setting up an opam switch is tricky as there are lots of deprecated functions causing lots of packages to not install or build.

If you are happy to use dune as your build tool, this repository vendors lots of packages so you can just run 
`dune build <target>` and have things running. All you need to do is:

```
$ opam update # only if you need to
$ opam switch create 5.00.0+trunk
$ opam install dune.2.9.2 
```

Then create a `src` directory for your project, build it using the vendored libraries and run `dune build`. As `5.00.0+trunk` progresses more things are likely to break so, as far as I know, you could be unfortunate enough to get a new 5.00.0 compiler that deprecates some more libraries and causes things to break.


