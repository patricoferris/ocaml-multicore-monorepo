# `w-stream-upload`

<br>

This example is a variant of [**`g-upload`**](../g-upload#files) that
[streams](https://aantron.github.io/dream/#streaming-uploads) uploaded files
one chunk at a time, instead of loading them completely into memory. It counts
the total size of each uploaded file:

```ocaml
let home request =
  <html>
  <body>
    <%s! Dream.form_tag ~action:"/" ~enctype:`Multipart_form_data request %>
      <input name="files" type="file" multiple>
      <button>Submit!</button>
    </form>
  </body>
  </html>

let report files =
  <html>
  <body>
%   files |> List.iter begin fun (name, size) ->
%     let name =
%       match name with
%       | None -> "None"
%       | Some name -> name
%     in
      <p><%s name %>, <%i size %> bytes</p>
%   end;
  </body>
  </html>

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get  "/" (fun request ->
      Dream.html (home request));

    Dream.post "/" (fun request ->
      let rec receive file_sizes =
        match%lwt Dream.upload request with
        | None -> Dream.html (report (List.rev file_sizes))
        | Some (_, filename, _) ->
          let rec count_size size =
            match%lwt Dream.upload_part request with
            | None -> receive ((filename, size)::file_sizes)
            | Some chunk -> count_size (size + String.length chunk)
          in
          count_size 0
      in
      receive []);

  ]
  @@ Dream.not_found
```

<pre><code><b>$ npm install esy && npx esy</b>
<b>$ npx esy start</b></code></pre>

Try it in the [playground](http://dream.as/w-upload-stream).

<br>

## Security

The report page shows one file without a name ("None"). This is, in fact, the
CSRF token generated by
[`Dream.form_tag`](https://aantron.github.io/dream/#val-form_tag) inside the
template. To keep the example simple, we didn't check the CSRF token, nor filter
out the `dream.csrf` field that it appears in. If you'd like to do so in your
code, see
[`Dream.verify_csrf_token`](https://aantron.github.io/dream/#val-verify_csrf_token).

See [OWASP File Upload Cheat
Sheet](https://cheatsheetseries.owasp.org/cheatsheets/File_Upload_Cheat_Sheet.html)
for a checklist of additional security precautions.

<br>
<br>

**See also:**

- [**`g-upload`**](../g-upload#files) shows the simplified interface for
  receiving file uploads.
- [**`w-multipart-dump`**](../w-multipart-dump#files) shows the request body
  that is parsed by
  [`Dream.upload`](https://aantron.github.io/dream/#val-upload).

<br>

[Up to the example index](../#examples)