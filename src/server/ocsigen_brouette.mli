(* This module is temporary to avoid cyclical inclusion between
 * Ocsigen_extensions and Ocsigen_server *)

exception Ocsigen_Is_a_directory of (Ocsigen_request_info.request_info -> Neturl.url)
