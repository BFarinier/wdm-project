(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

open Eliom_parameter

let concert_service =
  Eliom_service.App.service
    ~path:["concert"]
    ~get_params:unit ()
