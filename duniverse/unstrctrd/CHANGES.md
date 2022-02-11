### v0.3 2021-10-14 (France)

- Be resilient about invalid byte and replace them by
  `Uchar.u_rep` on `to_utf_8_string` (@dinosaure, #7)
- Upgrade the distribution with `ocamlformat` and `dune.2.0` (@dinosaure, #11)
- Handle correctly quoted string (@lyrm, @dinosaure, #10)
- Fix escaping characters (@dinosaure, #13)

### v0.2 2020-05-12 (France)

- Support `angstrom.0.14.0` on tests
- Add more tests
- Add `decode_safely` and fuzz it
- Remove fmt dependency

### v0.1 2020-01-23 (France)

- First release
