open Lwt_ops

type artist_tags = [`Area|`Beginarea|`Endarea|`Arid|`Artist|`Artistaccent|`Alias|`Begin|`Comment|`Country|`End|`Ended|`Gender|`Ipi|`Sortname|`Tag|`Type]
type freedb_tags = [`Artist|`Title|`Discid|`Cat|`Year|`Tracks]
type tag_tags    = [`Tag]

type tags = [artist_tags|freedb_tags|tag_tags]

type _ searches =
  | Artist : artist_tags searches
  | Freedb : freedb_tags searches
  | Tag    : tag_tags searches

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

let searches_to_string : type a. a searches -> string = function
  | Artist -> "artist"
  | Freedb -> "freedb"
  | Tag -> "tag"

let make_request (index: 'a searches) (tag: 'a) ?(limit=25) ?(offset=0) str : string =
  Printf.sprintf "http://musicbrainz.hotbeverage.org/ws/2/%s/?%s"
    (searches_to_string index)
    (Ocsigen_lib.Url.make_encoded_parameters
       (["query", (tags_to_string tag)^":"^str]
        @ (if (limit <> 25 && limit >= 1 && limit <= 100)
           then ["limit", string_of_int limit]
           else [])
        @ (if (offset > 0)
           then ["offset", string_of_int offset]
           else [])))


let get_stream (frame: Ocsigen_http_frame.t) : string Lwt_stream.t =
  let stream = ref
      (match frame.Ocsigen_http_frame.frame_content with
       | None -> raise Not_found
       | Some s -> Ocsigen_stream.get s)
  in
  let rec aux () =
    Ocsigen_stream.next (!stream) >>= function
    | Ocsigen_stream.Finished None -> Lwt.return None
    | Ocsigen_stream.Finished (Some s) -> stream := s; aux ()
    | Ocsigen_stream.Cont (x, s) -> stream := s; Lwt.return (Some x)
  in
  Lwt_stream.from aux


let read_stream ?(limit=16) (stream: string Lwt_stream.t) : unit -> int =
  Lwt_stream.on_terminate stream (fun () -> raise End_of_file);
  let length = ref 0 in
  let string = ref "" in
  let pos = ref 0 in
  let compt = ref limit in
  let rec aux = fun () ->
    if (!pos) < (!length) then
      let char = String.get (!string) (!pos) in
      (incr pos; int_of_char char)
    else
      match Lwt_stream.get_available_up_to 1 stream with
      | [s] ->
        string := s;
        length := String.length s;
        pos := 0;
        aux ()
      | _ ->
        if (!compt <= 0) then raise Not_found
        else (decr compt; aux ())
  in aux


module Xml_tree = struct

  type name = string * string
  type attribute = name * string
  type tag = name * attribute list

  type t = Element of tag * t list | Data of string

  let get_xml (index: 'a searches) (tag: 'a) ?(limit=25) ?(offset=0) str : t Lwt.t =
    let url = make_request index tag ~limit ~offset str in
    Ocsigen_http_client.get_url url >>= fun frame ->
    let stream = get_stream frame in
    let input = Xmlm.make_input (`Fun (read_stream stream)) in
    let el tag childs = Element (tag, childs)  in
    let data d = Data d in
    Lwt.return
      (try snd (Xmlm.input_doc_tree ~el ~data input) with
         Xmlm.Error (pos, error) -> raise Not_found)

end

open Xml_tree

let rec extract_tag =
  let rec extract_count = function
    | [] -> None
    | ((_, "count"), s)::_ -> (try Some (int_of_string s) with Failure "int_of_string" -> None)
    | _::l -> extract_count l
  in
  let rec extract_name = function
    | Element (((_, "name"), _), [Data s]) -> Some s
    | _ -> None
  in
  function
  | Element (((_, "tag"), c), l) ->
    let c =
      extract_count c
      |> function None -> 0 | Some c -> c
    in
    let l =
      List.map extract_name l
      |> List.fold_left (fun acc x -> match x with None -> acc | Some x -> x::acc) []
    in
    [c, List.hd l]
  | Element (_, l) -> List.flatten (List.map extract_tag l)
  | Data _ -> []

let search_artist_tags (artist:string) : (int * string) list Lwt.t =
  try
    Xml_tree.get_xml Artist `Artist ~limit:1 artist
    >|= extract_tag
  with
    Not_found -> Lwt.return []
