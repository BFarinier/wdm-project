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
  Printf.sprintf "http://musicbrainz.org/ws/2/%s/?%s"
    (searches_to_string index)
    (Ocsigen_lib.Url.make_encoded_parameters
       (["query", (tags_to_string tag)^":"^str]
        @ (if (limit <> 25 && limit >= 1 && limit <= 100)
           then ["limit", string_of_int limit]
           else [])
        @ (if (offset > 0)
           then ["offset", string_of_int offset]
           else [])))

let get_strings (url: string) : string list Lwt.t =
  let rec aux acc stream =
    Ocsigen_stream.next stream >>= function
    | Ocsigen_stream.Finished None -> Lwt.return (List.rev acc)
    | Ocsigen_stream.Finished (Some stream) -> aux acc stream
    | Ocsigen_stream.Cont (x, stream) -> aux (x::acc) stream
  in
  Ocsigen_http_client.get_url url >>= fun frame ->
  match frame.Ocsigen_http_frame.frame_content with
  | None -> raise Not_found
  | Some s ->
    let l = aux [] (Ocsigen_stream.get s) in
    lwt _ = Ocsigen_stream.finalize s `Success in l

let iter_strings (strings: string list) : unit -> int =
  let length, string =
    match strings with
    | [] -> (ref 0, ref "")
    | s::_ -> (ref (String.length s), ref s)
  in
  let strings = ref strings in
  let pos = ref 0 in
  let rec aux = fun () ->
    if (!pos) < (!length) then
      let char = String.get (!string) (!pos) in
      (incr pos; Printf.printf "%c%!" char; int_of_char char)
    else match (!strings) with
      | [] -> raise End_of_file
      | s::l -> (
          strings := l;
          string := s;
          length := String.length s;
          pos := 0;
          aux ()
        )
  in aux

module Xml_tree = struct

  type name = string * string
  type attribute = name * string
  type tag = name * attribute list

  type t = Element of tag * t list | Data of string

  let tree_of_strings (strings: string list) : t =
    let input = Xmlm.make_input (`Fun (iter_strings strings)) in
    let el tag childs = Element (tag, childs)  in
    let data d = Data d in
    try snd (Xmlm.input_doc_tree ~el ~data input) with
      Xmlm.Error (pos, error) ->
      print_endline (Printf.sprintf "(%i, %i), %s" (fst pos) (snd pos) (Xmlm.error_message error));
      assert false

  let get_xml (index: 'a searches) (tag: 'a) ?(limit=25) ?(offset=0) str : t Lwt.t =
    make_request index tag ~limit ~offset str
    |> get_strings
    >|= tree_of_strings

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
  Xml_tree.get_xml Artist `Artist ~limit:1 artist
  >|= extract_tag
