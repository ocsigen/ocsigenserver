
# Module `Ocsigen_extensions.Configuration`

This modules contains types and constructor for the description of XML configurations and the accordingly parsing.

```ocaml
type element
```
Specification of a XML element.

```ocaml
type attribute
```
Specification of a XML attribute.

```ocaml
val element : 
  name:string ->
  ?obligatory:bool ->
  ?init:(unit -> unit) ->
  ?elements:element list ->
  ?attributes:attribute list ->
  ?pcdata:(string -> unit) ->
  ?other_elements:(string -> (string * string) list -> Xml.xml list -> unit) ->
  ?other_attributes:(string -> string -> unit) ->
  unit ->
  element
```
Create the specification of a XML element.

parameter name Name of the XML tag
parameter init A function to be executed before processing the child elements and attributes
parameter obligatory Whether the element is obligatory (false by default)
parameter elements Specifications of the child elements
parameter attribute Specifications of the attributes
parameter pcdata Function to be applied on the pcdata (ignore\_blank\_pcdata by default)
parameter other\_elements Optional function to be applied on the content of unspecified tags
parameter other\_attributes Optional function to be applied on the unspecfied attributes
```ocaml
val attribute : 
  name:string ->
  ?obligatory:bool ->
  (string -> unit) ->
  attribute
```
`attribute ~name f` create a specification of a XML attribute.

parameter name The name of the XML attribute
parameter obligatory Whether the attribute is obligatory (false by default)
parameter f Function to be applied on the value string of the attribute
```ocaml
val process_element : 
  in_tag:string ->
  elements:element list ->
  ?pcdata:(string -> unit) ->
  ?other_elements:(string -> (string * string) list -> Xml.xml list -> unit) ->
  Xml.xml ->
  unit
```
Process an XML element by the specifications.

parameter in\_tag Name of the enclosing XML tag (just for generating error messages)
parameter elements Specifications of the child elements
parameter pcdata Function to be applied on the PCDATA (ignore\_blank\_pcdata by default)
parameter other\_elements Optional function to be applied on unexpected child elements
raises [`Error_in_config_file`](./Ocsigen_extensions.md#exception-Error_in_config_file) If an element (resp. attribute) occurs which is not specified by the element (resp. attribute) parameter and no function other\_elements (resp. other\_attributes) is provided
```ocaml
val process_elements : 
  in_tag:string ->
  elements:element list ->
  ?pcdata:(string -> unit) ->
  ?other_elements:(string -> (string * string) list -> Xml.xml list -> unit) ->
  ?init:(unit -> unit) ->
  Xml.xml list ->
  unit
```
Application of `process_element` on a list of XML elements.

```ocaml
val ignore_blank_pcdata : in_tag:string -> string -> unit
```
The specification for ignoring blank PCDATA ('\\n', '\\r', ' ', '\\t') and failing otherwise (a reasonable default).

```ocaml
val refuse_pcdata : in_tag:string -> string -> unit
```