# Writing an extension for Ocsigen server

This page describes how to extend Ocsigen's server. This can be used to create new ways to generate pages (like Apache modules), to filter and change the requests (for example, rewriting of URLS), to extend the syntax of the configuration file.

### Filtering the requests or writing a module to generate pages

You can take as example the files `staticmod.ml` or `accesscontrol.ml` from Ocsigen Server's distribution.

The type of request is [`Ocsigen.Extensions.request`](./Ocsigen-Extensions.md#type-request) (have a look at it in the interface of module [`Ocsigen.Extensions`](./Ocsigen-Extensions.md)). Its field `request_info`, of type [`Ocsigen.Request.t`](./Ocsigen-Request.md#type-t), describes the HTTP request.

Each extension loaded in the configuration file tries to handle the request and returns something of type [`Ocsigen.Extensions.answer`](./Ocsigen-Extensions.md#type-answer). If the page is not found by the extension (`Ext_next status`), the following one will try to handle the request. If the page is found, the answer is `Ext_found f`, where `f` is the function that generates the response. An extension can also modify the request before giving it to the next one (answer `Ext_continue_with (request, cookies, status)`), etc.

An extension is a function of type [`Ocsigen.Extensions.extension`](./Ocsigen-Extensions.md#type-extension). It receives `Req_not_found (status, request)` if no previous extension generated a page, and `Req_found (request, response)` otherwise. For example:

```ocaml
let my_extension = function
  | Ocsigen.Extensions.Req_not_found
      (_status, {Ocsigen.Extensions.request_info; _})
    when Ocsigen.Request.sub_path request_info = ["hello"] ->
      Lwt.return
        (Ocsigen.Extensions.Ext_found
           (fun () ->
             Lwt.return
               (Ocsigen.Response.respond_string ~status:`OK ~body:"Hello" ())))
  | Ocsigen.Extensions.Req_not_found (status, _) ->
      Lwt.return (Ocsigen.Extensions.Ext_next status)
  | Ocsigen.Extensions.Req_found _ ->
      Lwt.return Ocsigen.Extensions.Ext_do_nothing
```

### Filtering the outputs

It is also possible to create extensions that will filter the output of the server (for example to compress it). It is very similar to the previous one. Have a look at the file `deflatemod.ml` for an example.

### Extending the configuration file

#### Extending the configuration file for an extension

The parsing of Ocsigen's configuration file is using a very basic xml parser (package `xml-light`). The `fun_site` function given to [`Ocsigen.Extensions.register`](./Ocsigen-Extensions.md#val-register) receives, among other parameters, the path of the web site and the xml subtree, and returns the extension (see the type [`Ocsigen.Extensions.parse_config`](./Ocsigen-Extensions.md#type-parse_config)).

```ocaml
let parse_config _path _parse_fun = function
  | Xml.Element ("hello", _attrs, _content) ->
      (* Read the attributes and the content here *)
      my_extension
  | Xml.Element (t, _, _) ->
      raise (Ocsigen.Extensions.Bad_config_tag_for_extension t)
  | _ -> raise (Ocsigen.Extensions.Error_in_config_file "(my extension)")

let () =
  Ocsigen.Extensions.register ~name:"myextension"
    ~fun_site:(fun _userconf _hosts _config path _parse_host parse_fun ->
      parse_config path parse_fun)
    ()
```
The module [`Ocsigen.Extensions.Configuration`](./Ocsigen-Extensions-Configuration.md) helps to write such parsers. The function `Ocsigen.Parseconfig.parse_size_tag` parses sizes (in bytes, GB etc).

#### Giving parameters to an extension

Extensions may take parameters in the configuration file. Give an `init_fun` function to [`Ocsigen.Extensions.register`](./Ocsigen-Extensions.md#val-register): when the extension is loaded, it receives the xml tree between `<extension>` and `</extension>` (or `<library>` and `</library>`). Write a parser for that tree. Without `init_fun`, the extension accepts no parameter.

```ocaml
let greeting = ref "Hello"

let parse_global_config = function
  | [] -> ()
  | [Xml.Element ("greeting", [], [Xml.PCData s])] -> greeting := s
  | _ ->
      raise
        (Ocsigen.Extensions.Error_in_config_file
           "Unexpected content inside myextension config")

let () =
  Ocsigen.Extensions.register ~name:"myextension"
    ~fun_site:(fun _ _ _ path _ parse_fun -> parse_config path parse_fun)
    ~init_fun:parse_global_config ()
```

### Catching the request before it is fully read

For some extensions of the Web server, it is necessary to catch the request before it has been fully read (especially before the body of the request has been read). For example it is the case if you want to write a (reverse) proxy.

### Adding new commands for the server

If you want to add your own commands for the server command pipe, do something like:

```ocaml
let () =
  Ocsigen.Command.register_command_function ~prefix:"myextension"
    (fun _command args ->
       match args with
       | ["mycommand"] ->
           (* Do what you want here *)
           Lwt.return ()
       | _ -> raise Ocsigen.Command.Unknown_command)
```
The command to write in the pipe is then `myextension:mycommand`.
