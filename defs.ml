open Batteries

type date = CalendarLib.Calendar.t

let parse_date s =
  CalendarLib.Printer.Calendar.from_fstring
    "%Y-%m-%dT%H:%M:%S"
    s

type salle = string
type ville = string

type query_lieu =
  | Ville of string
  | Departement of int
  | Salle of string
  | Region of string
  | Pays of string

let regions = [
  "Alsace";
  "Aquitaine";
  "Auvergne";
  "Basse Normandie";
  "Bourgogne";
  "Bretagne";
  "Centre";
  "Champagne Ardenne";
  "Corse";
  "Franche-Comté";
  "Haute Normandie";
  "Ile de France";
  "Languedoc Roussillon";
  "Limousin";
  "Lorraine";
  "Midi pyrénées";
  "Nord Pas de Calais";
  "Pays de la Loire";
  "Picardie";
  "Poitou Charentes";
  "Provence Alpes Côte d'Azur";
  "Rhône Alpes";
  "Outre Mer";
  "Étranger";
]

let pays = [
  "Algérie";
  "Allemagne";
  "Andorre";
  "Autriche";
  "Belgique";
  "Brésil";
  "Bulgarie";
  "Cameroun";
  "Canada";
  "Congo";
  "Croatie";
  "Danemark";
  "Espagne";
  "Estonie";
  "Etats Unis";
  "France";
  "Grèce";
  "Hongrie";
  "Irlande";
  "Islande";
  "Italie";
  "Japon";
  "Lettonie";
  "Luxembourg";
  "Malawi";
  "Mali";
  "Malte";
  "Maroc";
  "Mauritanie";
  "Monaco";
  "Montenegro";
  "Norvège";
  "Pays Bas";
  "Pologne";
  "Portugal";
  "Republique Tcheque";
  "Roumanie";
  "Royaume Uni";
  "Russie";
  "Sénégal";
  "Serbie";
  "Slovenie";
  "Suède";
  "Suisse";
  "Tunisie";
  "Turquie";
  "Ukraine";
]

type concert = {
  artiste: string;
  lieu: ville * salle;
  date: date;
}

let print_concert c =
  Printf.printf "Artiste: %s\n" c.artiste;
  Printf.printf "À: %s, %s\n" (fst c.lieu) (snd c.lieu);
  print_string "Le: "; CalendarLib.Printer.Calendar.dprint c.date; print_newline ()

type artist = string
type weight = float (* ∈ [0, 1] *)
type genres = (string, weight) Map.t
type albums = string Set.t

type music_library = {
  mutable albums_nb: int;
  table: (artist,
          albums
          * genres)
      Hashtbl.t;
}

let create_library () =
  { albums_nb = 0; table = Hashtbl.create 37 }

let library_add_infos lib (artists: (artist * albums * genres) list) =
  List.iter (fun (artist, albums, genres) ->
    try
      let (a, g) = Hashtbl.find lib.table artist in
      lib.albums_nb <- lib.albums_nb + (Set.cardinal @@ Set.diff albums a);
      Hashtbl.replace lib.table artist (
        Set.union albums a,
        Map.merge (fun genre w1 w2 ->
          match w1, w2 with
          | None, None -> None (* ? *)
          | None, Some w | Some w, None -> Some w
          | Some w1, Some w2 -> Some ((w1 +. w2) /. 2.))
          genres g
      )
    with Not_found ->
      lib.albums_nb <- lib.albums_nb + (Set.cardinal albums);
      Hashtbl.add lib.table artist (albums, genres)
  ) artists

let genres_of_taglist (tags: (string * int) list): genres =
  let tot = List.enum tags |> Enum.map snd |> Enum.fold (+) 0 in
  List.enum tags
  |> Enum.map (Tuple2.map2 (fun n -> (Float.of_int n) /. (Float.of_int tot)))
  |> Map.of_enum
