(******************************************************************)
(* Extending commands *)
exception Unknown_command

let register_command_function, get_command_function =
  let command_function = ref (fun ?prefix _ _ -> Lwt.fail Unknown_command) in
  ((fun ?prefix f ->
      let prefix' = prefix in
      let old_command_function = !command_function in
      command_function :=
        (fun ?prefix s c ->
           Lwt.catch (fun () -> old_command_function ?prefix s c)
             (function
               | Unknown_command ->
                 if prefix = prefix'
                 then f s c
                 else Lwt.fail Unknown_command
               | e -> Lwt.fail e))),
   (fun () -> !command_function))


let () =
  register_command_function
    ~prefix:"logs"
    (Ocsigen_messages.command_f Unknown_command)
