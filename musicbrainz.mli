type artist_tags = [`Alias|`Area|`Arid|`Artist|`Artistaccent|`Begin|`Beginarea|`Comment|`Country|`End|`Endarea|`Ended|`Gender|`Ipi|`Sortname|`Tag|`Type]
type freedb_tags = [`Artist|`Cat|`Discid|`Title|`Tracks|`Year]
type tag_tags    = [`Tag]

type tags = [artist_tags|freedb_tags|tag_tags]
type _ searches =
  | Artist : artist_tags searches
  | Freedb : freedb_tags searches
  | Tag    : tag_tags searches

val tags_to_string : tags -> string
val searches_to_string : _ searches -> string

module Xml_tree : sig

  type name = string * string
  type attribute = name * string
  type tag = name * attribute list

  type t = Element of tag * t list | Data of string

  val get_xml : 'a searches -> ([< tags ] as 'a) -> ?limit:int -> ?offset:int -> string -> t Lwt.t

end
