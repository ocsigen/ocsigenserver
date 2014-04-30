open Ocsigen_cookies
open Ocsigen_lib

let to_cohttp_header cookies_table header =
  let set_cookies =
    Cookies.fold
      (fun path table acc ->
         CookiesTable.fold
           (fun name value acc ->
              match value with
              | OUnset ->
                let c = Cohttp.Cookie.Set_cookie_hdr.make
                    ~expiration:(`Max_age (Int64.of_int 0))
                    ~path:(Url.string_of_url_path ~encode:true path)
                    ~secure:false
                    (name, "") in
                let (k, v) = Cohttp.Cookie.Set_cookie_hdr.serialize c
                in (k, v) :: acc
              | OSet (time, value, secure) ->
                let time = match time with | Some time -> time | None -> 0.0 in
                let c = Cohttp.Cookie.Set_cookie_hdr.make
                    ~expiration:(`Max_age (Int64.bits_of_float time))
                    ~path:(Url.string_of_url_path ~encode:true path)
                    ~secure
                    (name, value) in
                let (k, v) = Cohttp.Cookie.Set_cookie_hdr.serialize c
                in (k, v) :: acc)
           table acc)
      cookies_table
      []
  in List.fold_left
    (fun acc (key, value) -> Cohttp.Header.add acc key value)
    header set_cookies
