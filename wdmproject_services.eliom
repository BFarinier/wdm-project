(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

open Eliom_parameter

let concert_service =
  Eliom_service.App.service
    ~path:["concert"]
    ~get_params:unit ()

let parameter_service =
  Eliom_service.App.service
    ~path:["parameter"]
    ~get_params:unit ()

let facebook_login =
  Eliom_service.App.post_service
    ~fallback:parameter_service
    ~post_params:unit ()

let facebook_login_success =
  Eliom_service.App.service
    ~path:["facebook"]
    ~get_params:(string "code") ()

let facebook_login_failure =
  Eliom_service.App.service
    ~path:["facebook"]
    ~get_params:any ()
