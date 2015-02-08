(* ?port -> host -> liste (artiste, nombre d'albums)

   Retourne None en cas d'erreur (connexion au serveur par ex) *)

val stats : ?port:int -> string -> (string * int) list option
