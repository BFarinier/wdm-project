type artist_tags = [`Area|`Beginarea|`Endarea|`Arid|`Artist|`Artistaccent|`Alias|`Begin|`Comment|`Country|`End|`Ended|`Gender|`Ipi|`Sortname|`Tag|`Type]
type freedb_tags = [`Artist|`Title|`Discid|`Cat|`Year|`Tracks]
type tag_tags    = [`Tag]

type tags = [artist_tags|freedb_tags|tag_tags]

type _ t =
  | Artist : artist_tags t
  | Freedb : freedb_tags t
  | Tag    : tag_tags t

let tags_to_string : [< tags] -> string = function
  | `Area -> "area"
  | `Beginarea -> "beginarea"
  | `Endarea -> "endarea"
  | `Arid -> "arid"
  | `Artist -> "artist"
  | `Artistaccent -> "artistaccent"
  | `Alias -> "alias"
  | `Begin -> "begin"
  | `Comment -> "comment"
  | `Country -> "country"
  | `End -> "end"
  | `Ended -> "ended"
  | `Gender -> "gender"
  | `Ipi -> "ipi"
  | `Sortname -> "sortname"
  | `Tag -> "tag"
  | `Type -> "type"
  | `Title -> "title"
  | `Discid -> "discid"
  | `Cat -> "cat"
  | `Year -> "year"
  | `Tracks -> "tracks"

let t_to_string : type a. a t -> string = function
  | Artist -> "artist"
  | Freedb -> "freedb"
  | Tag -> "tag"

let make_request (index: 'a t) (tag: 'a) ?(limit=25) ?(offset=0) str : string =
  Printf.sprintf "http://musicbrainz.org/ws/2/%s/?query=%s:%s%s%s"
    (t_to_string index) (tags_to_string tag) str
    (if (limit <> 25 && limit >= 1 && limit <= 100)
     then (Printf.sprintf "&limit=%i" limit)
     else "")
    (if (offset > 0)
     then (Printf.sprintf "&offset=%i" offset)
     else "")    
