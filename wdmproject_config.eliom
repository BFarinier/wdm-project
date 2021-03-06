(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

(** This file contains the configuration of your Eliom application.
    You can take some configuration options from Ocsigen server's
    configuration file, as shown below.
*)


let avatar_dir = ref []

let avatars = Ocsigen_extensions.Configuration.(
  let attributes = [
    attribute ~name:"dir" ~obligatory:true
      (fun h -> avatar_dir := Eliom_lib.String.split '/' h);
  ]
  in
  element ~name:"avatars" ~obligatory:true ~attributes ()
)

let _ = Eliom_config.parse_config [avatars]

let db_name = "library_db"
let () = Freebase.set_key "AIzaSyCDJwSCMuHNEe5Pi0nrQyK_K5AoNF9SRS0"

let client_id = "401808273311928"
let client_secret = "9c926f3785998ebd4c43a37c29bff882"
