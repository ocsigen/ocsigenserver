(** Cast between Ocsigen Cookies and Cohttp Cookies
 * Cookies in Cohttp is directly in Cohttp.Header.t
 * and for Ocsigen, there is a both structure for
 * manipulation with Eliom *)
val to_cohttp_header :
  Ocsigen_cookies.cookie Ocsigen_cookies.CookiesTable.t Ocsigen_cookies.Cookies.t ->
    Cohttp.Header.t -> Cohttp.Header.t
