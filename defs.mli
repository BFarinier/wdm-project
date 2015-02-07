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
