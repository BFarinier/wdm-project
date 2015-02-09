open Lwt_ops

type artist_tags = [`Genre|`Origin|`Album|`Label|`Active_start|`Active_end|`Contribution]
type album_tags  = [`Artist|`Featured_artists|`Release_date|`Genre|`Release_type|`Album_content_type|`Releases|`Primary_release|`Compositions]
type genre_tags  = [`Parent_genre|`Subgenre|`Artists|`Albums|`Recordings]

type tags = [artist_tags|album_tags|genre_tags]

type _ searches =
  | Artist : artist_tags searches
  | Album  : album_tags searches
  | Genre  : genre_tags searches

type _ mid = Mid : string -> 'a mid

let tags_to_string : [< tags] -> string = function
  |`Album -> "album"
  |`Album_content_type -> "album_content_type"
  |`Albums -> "albums"
  |`Active_end -> "active_end"
  |`Active_start -> "active_start"
  |`Artist -> "artist"
  |`Artists ->"artists"
  |`Compositions -> "compositions"
  |`Contribution -> "contribution"
  |`Featured_artists -> "featured_artists"
  |`Genre -> "genre"
  |`Label -> "label"
  |`Origin -> "origin"
  |`Primary_release -> "primary_release"
  |`Recordings -> "recordings"
  |`Releases -> "releases"
  |`Release_date -> "release_date"
  |`Release_type -> "release_type"
  |`Parent_genre -> "parent_genre"
  |`Subgenre -> "subgenre"

let searches_to_string : type a. a searches -> string = function
  | Artist -> "artist"
  | Album -> "album"
  | Genre -> "genre"

let make_search_request (search: 'a searches) ?(limit=25) ?(offset=0) str : string =
  let search = searches_to_string search in
  Printf.sprintf "https://www.googleapis.com/freebase/v1/search?%s"
    (Ocsigen_lib.Url.make_encoded_parameters
       (["query", str; "filter", Printf.sprintf "(any type:/music/%s)" search] 
        @ (if (limit <> 25 && limit >= 1 && limit <= 100)
           then ["limit", string_of_int limit]
           else [])
        @ (if (offset > 0)
           then ["offset", string_of_int offset]
           else [])))

let make_query_request (search: 'a searches) (tag: 'a) ((Mid mid): 'a mid) : string =
  let search = searches_to_string search in
  let tag = tags_to_string tag in
  Printf.sprintf "https://www.googleapis.com/freebase/v1/mqlread?%s"
    (Ocsigen_lib.Url.make_encoded_parameters
       ["query", Printf.sprintf "{\"type\":\"/music/%s\", \"mid\":\"%s\", \"/music/%s/%s\":[]}" search mid search tag])

let get_string (url: string) : string Lwt.t =
  let buff = Buffer.create 256 in
  let rec aux stream =
    Ocsigen_stream.next stream >>= function
    | Ocsigen_stream.Finished None -> Lwt.return (Buffer.contents buff)
    | Ocsigen_stream.Finished (Some stream) -> aux stream
    | Ocsigen_stream.Cont (x, stream) -> Buffer.add_string buff x; aux stream
  in
  Ocsigen_http_client.get_url url >>= fun frame ->
  match frame.Ocsigen_http_frame.frame_content with
  | None -> raise Not_found
  | Some s ->
    let l = aux (Ocsigen_stream.get s) in
    lwt _ = Ocsigen_stream.finalize s `Success in l

let get_mid (search: 'a searches) str : 'a mid Lwt.t =
  make_search_request search str |> get_string
  >|= Ezjsonm.from_string >|= Ezjsonm.value
  >|= (fun v -> Ezjsonm.find v ["result"])
  >|= Ezjsonm.get_list (fun v -> Ezjsonm.find v ["mid"])
  >|= (function [] -> Ezjsonm.string "" | a::l -> a)
  >|= Ezjsonm.get_string
  >|= fun mid -> Mid mid

let get_tag_from_mid (search: 'a searches) (tag: 'a) (mid: 'a mid) : string list Lwt.t =
  make_query_request search tag mid |> get_string
  >|= Ezjsonm.from_string >|= Ezjsonm.value
  >|= (let path = Printf.sprintf "/music/%s/%s" (searches_to_string search) (tags_to_string tag) in
       fun v -> Ezjsonm.find v ["result"; path])
  >|= Ezjsonm.get_list Ezjsonm.get_string
