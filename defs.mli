open Batteries

type date = CalendarLib.Calendar.t

val parse_date : string -> date

type salle = string
type ville = string

type query_lieu =
  | Ville of string
  | Departement of int
  | Salle of string
  | Region of string
  | Pays of string

val regions : string list
val pays : string list

type concert = {
  artiste: string;
  lieu: ville * salle;
  date: date;
}

val print_concert : concert -> unit

type artist = string
type weight = float (* ∈ [0, 1] *)
type genres = (string, weight) Map.t
type albums = string Set.t

type music_library =
  int * (* nombre total d'albums *)
  (artist,
   int (* nombre d'albums *)
   * genres)
  Hashtbl.t

val create_library : unit -> music_library
val library_add_infos : music_library -> (artist * albums * genres) list
val genres_of_taglist : (string * int) list -> genres
