(* The chain of authentication methods of Authbasic: <plain>, registered
   by Authbasic, and <always>, registered here, after it. *)

let () =
  Authbasic.register_basic_authentication_method @@ function
  | Xml.Element ("always", [], []) -> fun _ _ -> Lwt.return true
  | Xml.Element (tag, _, _) -> raise (Authbasic.Bad_config_tag_for_auth tag)
  | Xml.PCData _ -> raise (Authbasic.Bad_config_tag_for_auth "#text")

let check name element user password =
  match Authbasic.get_basic_authentication_method element with
  | auth ->
      Printf.printf "%s %s/%s: %b\n" name user password
        (Lwt_main.run (auth user password))
  | exception Ocsigen.Extensions.Error_in_config_file message ->
      Printf.printf "%s: %s\n" name message

let () =
  let plain = Xml.Element ("plain", [("login", "me"); ("password", "pw")], []) in
  check "plain" plain "me" "pw";
  check "plain" plain "me" "other";
  check "always" (Xml.Element ("always", [], [])) "anyone" "anything";
  check "unknown" (Xml.Element ("ldap", [], [])) "me" "pw";
  check "bad plain" (Xml.Element ("plain", [("login", "me")], [])) "me" "pw"
