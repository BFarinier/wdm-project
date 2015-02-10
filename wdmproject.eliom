(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

{shared{
open Eliom_content.Html5
open Eliom_content.Html5.F
}}

let main_service_handler userid_o () () =
  Wdmproject_container.page userid_o (
    [
      p [em [pcdata "Eliom base app: Put app content here."]]
    ]
  )

let concert_handler userid_o () () =
  let construct =
    let switch = ref true in
    fun name date place ->
      switch := not (!switch);
      div
        ~a:[a_id (if (!switch) then "concert_odd" else "concert_even")]
        [h2 [pcdata name]; p [pcdata ("le "^date^" à "^place)]]
  in
  [("Allemagne", "25 mars 1957", "Berlin");
   ("Belgique", "25 mars 1957", "Bruxelles");
   ("France", "25 mars 1957", "Paris");
   ("Italie", "25 mars 1957", "Rome");
   ("Luxembourg", "25 mars 1957", "Luxembourg");
   ("Pays-Bas", "25 mars 1957", "Amsterdam")]
  |> List.fold_left (fun acc (name, date, place) -> (construct name date place)::acc) []
  |> (fun l -> [div (List.rev ((button ~button_type:`Button [pcdata "Refresh"])::l))])
  |> Wdmproject_container.page userid_o


let parameter_handler userid_o () () =
  Wdmproject_container.page userid_o [
    div [
      h2 [pcdata "Local library"];
      p [
        pcdata "Import: ";
        raw_input ~input_type:`Text ~name:"import" ();
        raw_input ~input_type:`Submit ~value:"Ok" ()
      ]];
    div [
      h2 [pcdata "MPD server"];
      p [
        pcdata "Adress: ";
        raw_input ~input_type:`Text ~name:"adress" ();
        pcdata " Port: ";
        raw_input ~input_type:`Text ~name:"port" ();
        raw_input ~input_type:`Submit ~value:"Rescan" ()
      ]];
    div [
      h2 [pcdata "Facebook"];
      p [
        pcdata "Account: ";
        raw_input ~input_type:`Text ~name:"account" ();
        raw_input ~input_type:`Submit ~value:"Update" ()
      ]]
  ]  

let () =
  Wdmproject_base.App.register
    Eba_services.main_service
    (Wdmproject_page.Opt.connected_page main_service_handler);

  Wdmproject_base.App.register
    Wdmproject_services.concert_service
    (Wdmproject_page.Opt.connected_page concert_handler);

  Wdmproject_base.App.register
    Wdmproject_services.parameter_service
    (Wdmproject_page.Opt.connected_page parameter_handler)
