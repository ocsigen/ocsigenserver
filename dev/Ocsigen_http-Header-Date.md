# Module `Header.Date`

Dates in header fields (RFC 9110, section 5\.6.7).

```ocaml
val parse : ?now:float -> string -> float option
```
`parse s` is the time, in seconds since the epoch, of the HTTP date `s`, in any of its three formats: the preferred one (`"Sun, 06 Nov 1994 08:49:37 GMT"`) and the obsolete RFC 850 (`"Sunday, 06-Nov-94 08:49:37 GMT"`) and asctime (`"Sun Nov  6 08:49:37 1994"`) ones. The two-digit year of the RFC 850 format is the latest year with these digits that does not put the date more than 50 years after `now` (default: the current time). `parse s` is `None` when `s`, without its surrounding whitespace, is not a valid date in the exact syntax of RFC 9110 (single spaces, except the two before a one-digit day of the asctime format).
