
# Module `Ocsigen_base`

```ocaml
module Cache : sig ... end
```
Cache. Association tables (from any kind of database) that keep the most recently used values in memory. It is also possible to set a maximum lifetime for data in the cache.

```ocaml
module Config_static : sig ... end
```
```ocaml
module Lib : sig ... end
```
This module contains some auxiliaries for the Ocsigenserver. In contrast to [`Ocsigen_lib_base`](./Ocsigen_lib_base.md), the function may also refer to libraries other than the standard library.

```ocaml
module Loader : sig ... end
```
Module `Loader`: Dynamic loading for Ocsigen.

```ocaml
module Ocsigen_stream : sig ... end
```