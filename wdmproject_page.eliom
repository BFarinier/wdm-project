(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

{shared{
  open Eliom_content.Html5.F
  open Eliom_content.Html5
}}

module Page_config = struct
  include Eba_page.Default_config

  let title = "wdmproject"

  let css = [
    ["font-awesome.css"];
    ["jquery.Jcrop.css"];
    ["eba.css"];
    ["wdmproject.css"];
  ]

  let js = [
    ["onload.js"];
    ["jquery.js"];
    ["jquery.Jcrop.js"]
  ]

  let default_predicate _ _ = Lwt.return true

  let default_connected_predicate _ _ _ = Lwt.return true

  let default_error_page _ _ exn =
    Wdmproject_container.page None
      (if Ocsigen_config.get_debugmode ()
       then [p [pcdata (Printexc.to_string exn)]]
       else [p [pcdata "Error"]])

  let default_connected_error_page userid_o _ _ exn =
    Wdmproject_container.page userid_o
      (if Ocsigen_config.get_debugmode ()
       then [p [pcdata (Printexc.to_string exn)]]
       else [p [pcdata "Error"]])
end


include Eba_page.Make(Page_config)
