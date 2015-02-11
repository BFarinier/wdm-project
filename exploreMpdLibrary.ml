open Ctypes
open Foreign
open Batteries

module Mpd = struct
  let ptr_eq p q = ptr_compare p q = 0

  let ptr_of_opt ty = function
    | Some p -> p
    | None -> coerce (ptr void) ty null

  let opt_of_ptr ty p =
    if ptr_eq (coerce ty (ptr void) p) null then None
    else Some p

  let bool = view
      ~read:(fun x -> x <> 0)
      ~write:(fun b -> if b then 1 else 0)
      int

  let mpd_lib = Dl.dlopen
      ~filename:"libmpdclient.so"
      ~flags:[Dl.RTLD_NOW; Dl.RTLD_GLOBAL]

  module C = struct
    type connection
    let connection : connection structure typ = structure "mpd_connection"

    type entity
    let entity : entity structure typ = structure "mpd_entity"

    type song
    let song : song structure typ = structure "mpd_song"
  end

  type connection = C.connection structure ptr
  let connection = ptr C.connection

  type entity = C.entity structure ptr
  let entity = ptr C.entity

  type song = C.song structure ptr
  let song = ptr C.song

  type error =
    | Error_Success
    | Error_OOM
    | Error_Argument
    | Error_State
    | Error_Timeout
    | Error_System
    | Error_Resolver
    | Error_Malformed
    | Error_Closed
    | Error_Server

  let error = view
      ~read:(fun i -> match Unsigned.UInt.to_int i with
        | 0 -> Error_Success
        | 1 -> Error_OOM
        | 2 -> Error_Argument
        | 3 -> Error_State
        | 4 -> Error_Timeout
        | 5 -> Error_System
        | 6 -> Error_Resolver
        | 7 -> Error_Malformed
        | 8 -> Error_Closed
        | _ -> Error_Server)
      ~write:(fun e -> begin match e with
        | Error_Success -> 0
        | Error_OOM -> 1
        | Error_Argument -> 2
        | Error_State -> 3
        | Error_Timeout -> 4
        | Error_System -> 5
        | Error_Resolver -> 6
        | Error_Malformed -> 7
        | Error_Closed -> 8
        | Error_Server -> 9 end |> Unsigned.UInt.of_int)
      uint

  type entity_type =
    | Entity_Type_Unknown
    | Entity_Type_Directory
    | Entity_Type_Song
    | Entity_Type_Playlist

  let entity_type = view
      ~read:(fun i -> match Unsigned.UInt.to_int i with
        | 0 -> Entity_Type_Unknown
        | 1 -> Entity_Type_Directory
        | 2 -> Entity_Type_Song
        | _ -> Entity_Type_Playlist)
      ~write:(fun e -> begin match e with
        | Entity_Type_Unknown -> 0
        | Entity_Type_Directory -> 1
        | Entity_Type_Song -> 2
        | Entity_Type_Playlist -> 3 end |> Unsigned.UInt.of_int)
      uint

  type tag_type =
    | Tag_Unknown
    | Tag_Artist
    | Tag_Album
    | Tag_Album_Artist
    | Tag_Title
    | Tag_Track
    | Tag_Name
    | Tag_Genre
    | Tag_Date
    | Tag_Composer
    | Tag_Performer
    | Tag_Comment
    | Tag_Disc
    | Tag_Musicbrainz_Artistid
    | Tag_Musicbrainz_Albumid
    | Tag_Musicbrainz_Albumartistid
    | Tag_Musicbrainz_trackid
    | Tag_Count

  let tag_type = view
      ~read:(fun i -> match Unsigned.UInt.to_int i with
        | -1 -> Tag_Unknown
        | 0 -> Tag_Artist
        | 1 -> Tag_Album
        | 2 -> Tag_Album_Artist
        | 3 -> Tag_Title
        | 4 -> Tag_Track
        | 5 -> Tag_Name
        | 6 -> Tag_Genre
        | 7 -> Tag_Date
        | 8 -> Tag_Composer
        | 9 -> Tag_Performer
        | 10 -> Tag_Comment
        | 11 -> Tag_Disc
        | 12 -> Tag_Musicbrainz_Artistid
        | 13 -> Tag_Musicbrainz_Albumid
        | 14 -> Tag_Musicbrainz_Albumartistid
        | 15 -> Tag_Musicbrainz_trackid
        | _ -> Tag_Count)
      ~write:(fun e -> begin match e with
        | Tag_Unknown -> -1
        | Tag_Artist -> 0
        | Tag_Album -> 1
        | Tag_Album_Artist -> 2
        | Tag_Title -> 3
        | Tag_Track -> 4
        | Tag_Name -> 5
        | Tag_Genre -> 6
        | Tag_Date -> 7
        | Tag_Composer -> 8
        | Tag_Performer -> 9
        | Tag_Comment -> 10
        | Tag_Disc -> 11
        | Tag_Musicbrainz_Artistid -> 12
        | Tag_Musicbrainz_Albumid -> 13
        | Tag_Musicbrainz_Albumartistid -> 14
        | Tag_Musicbrainz_trackid -> 15
        | Tag_Count -> 16 end |> Unsigned.UInt.of_int)
      uint

  let connection_new host port timeout =
    foreign ~from:mpd_lib "mpd_connection_new"
      (string @-> uint @-> uint @-> returning connection)
      host
      (Unsigned.UInt.of_int port)
      (Unsigned.UInt.of_int timeout)

  let connection_get_error = foreign ~from:mpd_lib "mpd_connection_get_error"
      (connection @-> returning error)

  let connection_free = foreign ~from:mpd_lib "mpd_connection_free"
      (connection @-> returning void)

  let send_list_all_meta = foreign ~from:mpd_lib "mpd_send_list_all_meta"
      (connection @-> string @-> returning bool)

  let recv_entity conn =
    foreign ~from:mpd_lib "mpd_recv_entity"
      (connection @-> returning entity)
      conn
    |> opt_of_ptr entity

  let entity_free = foreign ~from:mpd_lib "mpd_entity_free"
      (entity @-> returning void)

  let entity_get_type = foreign ~from:mpd_lib "mpd_entity_get_type"
      (entity @-> returning entity_type)

  let entity_get_song = foreign ~from:mpd_lib "mpd_entity_get_song"
      (entity @-> returning song)

  let song_get_tag s typ idx =
    foreign ~from:mpd_lib "mpd_song_get_tag"
      (song @-> tag_type @-> uint @-> returning string_opt)
      s typ (Unsigned.UInt.of_int idx)
end

(* End of bindings -- *)
(* Begin: higher level functions *)

let connection_new ?(port = 6600) host =
  let conn = Mpd.connection_new host port 0 in
  match Mpd.connection_get_error conn with
  | Mpd.Error_Success -> Some conn
  | _ -> Mpd.connection_free conn; None

let connection_close = Mpd.connection_free

let get_stats conn =
  if Mpd.send_list_all_meta conn "" = false then
    None
  else begin
    let artists = Hashtbl.create 37 in
    let add_album artist album =
      try
        let albums = Hashtbl.find artists artist in
        Hashtbl.replace artists artist (Set.add album albums)
      with Not_found ->
        Hashtbl.replace artists artist (Set.singleton album)
    in
    let rec loop () =
      match Mpd.recv_entity conn with
      | None -> ()
      | Some e ->
        begin match Mpd.entity_get_type e with
          | Mpd.Entity_Type_Song ->
            let s = Mpd.entity_get_song e in
            let (artist, album) = (Mpd.song_get_tag s Mpd.Tag_Artist 0,
                                   Mpd.song_get_tag s Mpd.Tag_Album 0) in
            Mpd.entity_free e;
            (match artist, album with
             | Some artist, Some album ->
               add_album artist album
             | _ -> ());
            loop ()
          | _ ->
            Mpd.entity_free e;
            loop ()
        end
    in
    loop ();

    Hashtbl.enum artists
    |> List.of_enum
    |> Option.some
  end

let stats ?(port = 6600) host =
  match connection_new ~port host with
  | None -> None
  | Some conn ->
    let stats = get_stats conn in
    Option.may (fun _ -> connection_close conn) stats;
    stats
