type artist_tags = [`Alias|`Area|`Arid|`Artist|`Artistaccent|`Begin|`Beginarea|`Comment|`Country|`End|`Endarea|`Ended|`Gender|`Ipi|`Sortname|`Tag|`Type]
type freedb_tags = [`Artist|`Cat|`Discid|`Title|`Tracks|`Year]
type tag_tags    = [`Tag]

type tags = [artist_tags|freedb_tags|tag_tags]
type _ t =
  | Artist : artist_tags t
  | Freedb : freedb_tags t
  | Tag    : tag_tags t

val tags_to_string : [< tags ] -> string
val t_to_string : 'a t -> string
val make_request : ([< tags ] as 'a) t -> 'a -> ?limit:int -> ?offset:int -> string -> string
