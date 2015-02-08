open Batteries

let (^/) = Filename.concat

let explore_directory (dir: string) =
  let root = dir in

  let rec aux (prefixes, files) =
    match files with
    | f::fs -> f, (prefixes, fs)
    | [] ->
      begin match prefixes with
        | [] -> raise Enum.No_more_elements
        | prefix::ps ->
          begin try
              let content = Sys.readdir (root ^/ prefix) in
              Array.fold_left (fun (prefixes, files) it ->
                let it = prefix ^/ it in
                if Sys.is_directory (root ^/ it) then
                  (it :: prefixes, files)
                else
                  (prefixes, it :: files)
              ) (ps, []) content
              |> aux
            with Sys_error _ -> aux (ps, [])
          end;
      end
  in

  Enum.from_loop ([""], []) aux
  
let stats (library: string): (string * int) list =
  let artists = Hashtbl.create 37 in
  let add_album artist album =
    try
      let albums = Hashtbl.find artists artist in
      Hashtbl.replace artists artist (Set.add album albums)
    with Not_found ->
      Hashtbl.replace artists artist (Set.singleton album)
  in

  explore_directory library
  |> Enum.iter (fun song ->
    try
      let song = Taglib.File.open_file `Autodetect (library ^/ song) in
      add_album (Taglib.tag_artist song) (Taglib.tag_album song);
      Taglib.File.close_file song
    with Not_found | Taglib.File.Invalid_file ->
      ()
  );

  Hashtbl.enum artists
  |> Enum.map (Tuple2.map2 Set.cardinal)
  |> List.of_enum
  |> Option.some
