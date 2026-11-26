(* The chain of authentication methods of Authbasic: <plain>, registered
   by Authbasic, and <always>, registered here, after it. *)

let () =
  Authbasic.register_basic_authentication_method @@ function
  | Xml.Element ("always", [], []) -> fun _ _ -> Lwt.return true
  | Xml.Element ("always", _, _) ->
      Ocsigen.Extensions.badconfig "<always> takes no attributes and no content"
  | Xml.Element (tag, _, _) -> raise (Authbasic.Bad_config_tag_for_auth tag)
  | Xml.PCData _ -> raise (Authbasic.Bad_config_tag_for_auth "#text")

let check name element user password =
  match Authbasic.get_basic_authentication_method element with
  | auth ->
      Printf.printf "%s %s/%s: %b\n" name user password
        (Lwt_main.run (auth user password))
  | exception Ocsigen.Extensions.Error_in_config_file message ->
      Printf.printf "%s: %s\n" name message

let plain attributes = Xml.Element ("plain", attributes, [])
let always = Xml.Element ("always", [], [])

let () =
  let login = "login", "me" and password = "password", "pw" in
  check "plain" (plain [login; password]) "me" "pw";
  check "plain" (plain [login; password]) "me" "other";
  check "plain password first" (plain [password; login]) "me" "pw";
  check "always" always "anyone" "anything";
  check "unknown" (Xml.Element ("ldap", [], [])) "me" "pw";
  check "plain without password" (plain [login]) "me" "pw";
  check "plain with another attribute"
    (plain [login; password; "realm", "r"])
    "me" "pw";
  check "plain with two logins"
    (plain [login; "login", "you"; password])
    "me" "pw";
  check "always with an attribute"
    (Xml.Element ("always", ["user", "me"], []))
    "me" "pw"

(* The same chain, reached from an <authbasic> element of a <host>, as the
   configuration file gives it. The init function of Authbasic registers it
   as an extension, as loading it from the configuration file does. *)

let () =
  Ocsigen_base.Loader.init_module ignore ignore false "authbasic";
  let parse =
    Ocsigen.Extensions.make_parse_config []
      (Ocsigen.Extensions.parse_config_item None []
         (Ocsigen.Extensions.default_config_info ()))
  in
  let check_config name content =
    match parse [Xml.Element ("authbasic", ["realm", "r"], content)] with
    | (_ : Ocsigen.Extensions.extension_composite) ->
        Printf.printf "<authbasic> with %s: accepted\n" name
    | exception Ocsigen.Extensions.Error_in_config_file message ->
        Printf.printf "<authbasic> with %s: %s\n" name message
  in
  check_config "<always>" [always];
  check_config "<always user=\"me\">"
    [Xml.Element ("always", ["user", "me"], [])];
  check_config "<ldap>" [Xml.Element ("ldap", [], [])]
