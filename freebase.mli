type artist_tags = [`Genre|`Origin|`Album|`Label|`Active_start|`Active_end|`Contribution]
type album_tags  = [`Artist|`Featured_artists|`Release_date|`Genre|`Release_type|`Album_content_type|`Releases|`Primary_release|`Compositions]
type genre_tags  = [`Parent_genre|`Subgenre|`Artists|`Albums|`Recordings]

type tags = [artist_tags|album_tags|genre_tags]

type _ searches =
  | Artist : artist_tags searches
  | Album  : album_tags searches
  | Genre  : genre_tags searches

val tags_to_string : [< tags] -> string
val searches_to_string : 'a searches -> string

val search : 'a searches -> ([< tags] as 'a) -> string -> string list Lwt.t
