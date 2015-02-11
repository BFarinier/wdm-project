(* Copyright Vincent Balat *)
(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

{shared{
open Eliom_content.Html5
open Eliom_content.Html5.F
}}

(* Here is an example of tip.
   Call this function while generating the widget concerned by the explanation
   it contains. *)
let example_tip () =
  Eba_tips.display
    ~top:40 ~right:0 ~width:300 ~height:120
    ~arrow:(`top 300)
    ~name:"example"
    ~content:[p [pcdata "Gérer les paramètre de votre compte."];
              p [pcdata "Vous pouvez à tout instant modifier les paramètres de votre compte."]
             ]
    ()
