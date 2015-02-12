(* This file was generated by Eliom-base-app.
   Feel free to use it, modify it, and redistribute it as you wish. *)

{shared{
open Eliom_content.Html5
open Eliom_content.Html5.F
open Eliom_lib.Lwt_ops
}}

open Lwt_ops
open Batteries
open Defs

type settings = {
  mutable lieux: query_lieu list;
  mutable mpd_server: string option;
  mutable mpd_port: int option;
}

let default_settings = {
  lieux = [Ville "toulouse"];
  mpd_server = None;
  mpd_port = None;
}

type user_data = {
  settings: settings;
  selected_concerts: concert list;
  library: music_library;
}

let db = Ocsipersist.open_table Wdmproject_config.db_name

let get_user_data userid =
  let userid = Int64.to_string userid in
  Lwt.catch
    (fun () -> Ocsipersist.find db userid)
    (fun _ ->
       let data = {
         settings = default_settings;
         selected_concerts = [];
         library = create_library ();
       } in
       Ocsipersist.add db userid data >>= fun () ->
       Lwt.return data)

let set_user_data userid data =
  Ocsipersist.add db (Int64.to_string userid) data

    {shared{
type concert_table = [
    `Processing
  | `Table of (string * (string * string) * string) list
]

let fa ?(a = []) classes =
  i ~a:(a @ [a_class ("fa" :: classes)]) []
}}

let concerts_event_h:
  (int64, concert_table Eliom_react.Down.t *
          (?step:React.step -> concert_table -> unit))
    Hashtbl.t =
  Hashtbl.create 37

let concerts_event userid =
  try Hashtbl.find concerts_event_h userid with
    Not_found ->
    let e, send_e = React.E.create () in
    let e = Eliom_react.Down.of_react e in
    Hashtbl.add concerts_event_h userid (e, send_e);
    e, send_e

let lieux_event_h = Hashtbl.create 37
let lieux_event userid =
  try Hashtbl.find lieux_event_h userid with
    Not_found ->
    let (e: (string * string) list React.E.t), send_e = React.E.create () in
    let e = Eliom_react.Down.of_react e in
    Hashtbl.add lieux_event_h userid (e, send_e);
    e, send_e

(* meh *)
let concerts_to_client : concert list -> (string * (string * string) * string) list =
  List.map (fun {artiste; lieu; date} ->
    (artiste,
     lieu,
     CalendarLib.Printer.Calendar.to_string date))

let lieux_to_client lieux =
  List.map (function
    | Ville v -> "Ville", v
    | Departement d -> "Département", (string_of_int d)
    | Salle s -> "Salle", s
    | Region r -> "Région", r
    | Pays p -> "Pays", p) lieux

module FreebaseCache = Ocsigen_cache.Make (struct
  type key = string
  type value = (int * string) list
end)

let freebase_cache = new FreebaseCache.cache
  Freebase.search_artist_tags
  500

let update_concerts userid =
  lwt user_data = get_user_data userid in
  let _, send_e = concerts_event userid in

  send_e `Processing;
  Printf.printf "~> Start. %f\n%!" (Unix.gettimeofday ());

  lwt l = Lwt_list.map_p (fun lieu -> InfoConcert.get ~lieu ())
      user_data.settings.lieux in
  let t1 = Unix.gettimeofday () in
  Printf.printf "~> InfoConcert done. %f\n%!" t1;
  lwt concerts = List.flatten l
                 |> List.sort_uniq (fun c1 c2 ->
                   let res = CalendarLib.Calendar.compare c1.date c2.date in
                   if res <> 0 then res
                   else compare c1 c2)
                 |> lwt_list_filter_map_p (fun concert ->
                   lwt tags = freebase_cache#find concert.artiste in
                   let genres = genres_of_taglist tags in
                   let ((matching_artist, score), global_score) =
                     Core.rank genres user_data.library in

                   if Core.filter_score ((matching_artist, score), global_score) then
                     {
                       artiste = Printf.sprintf "%s - (%s, %f) / %f"
                           concert.artiste matching_artist score global_score;
                       lieu = concert.lieu;
                       date = concert.date
                     } |> Option.some |> Lwt.return
                   else Lwt.return None
                 ) in
  let t2 = Unix.gettimeofday () in
  Printf.printf "~> Done. %f\n%!" t2;
  Printf.printf "~> Diff: %f\n%!" (t2 -. t1);
  send_e (`Table (concerts_to_client concerts));
  set_user_data userid {user_data with selected_concerts = concerts}

let update_library userid (infos: (artist * albums) list) =
  lwt user_data = get_user_data userid in
  lwt infos = Lwt_list.map_p (fun (artist, albums) ->
    lwt tags = freebase_cache#find artist in
    let genres = genres_of_taglist tags in
    Lwt.return (artist, albums, genres)
  ) infos in
  library_add_infos user_data.library infos;
  set_user_data userid user_data

let update_mpd_library (userid, address, port) =
  Printf.printf "update_mpd_library %s:%d\n%!" address port;
  match ExploreMpdLibrary.stats ~port address with
  | None -> Lwt.return false
  | Some infos ->
    (* it worked; gotta save it for later *)
    lwt user_data = get_user_data userid in
    user_data.settings.mpd_server <- Some address;
    user_data.settings.mpd_port <- Some port;
    lwt () = set_user_data userid user_data in
    lwt () = update_library userid infos in
    Lwt.return true

let add_lieu (userid, typ, value) =
  let lieu = match typ with
    | "Ville" -> Ville value
    | "Département" -> Departement (int_of_string value)
    | "Salle" -> Salle value
    | "Région" -> Region value
    | _ -> Pays value
  in

  lwt user_data = get_user_data userid in
  (if not (List.mem lieu user_data.settings.lieux) then
     user_data.settings.lieux <- user_data.settings.lieux @ [lieu]);
  lwt () = set_user_data userid user_data in
  let _, send = lieux_event userid in
  send (lieux_to_client user_data.settings.lieux);
  Lwt.return ()

let del_lieu (userid, n) =
  lwt user_data = get_user_data userid in
  user_data.settings.lieux <- List.remove_at n user_data.settings.lieux;
  lwt () = set_user_data userid user_data in
  let _, send = lieux_event userid in
  send (lieux_to_client user_data.settings.lieux);
  Lwt.return ()

let clear_db userid =
  lwt user_data = get_user_data userid in
  set_user_data userid {user_data with library = create_library ()}

{shared{
type update_concerts_rpc = (int64) deriving(Json)
type update_mpd_library_rpc = (int64 * string * int) deriving(Json)
type add_lieu_rpc = (int64 * string * string) deriving(Json)
type del_lieu_rpc = (int64 * int) deriving(Json)
type clear_db_rpc = (int64) deriving(Json)
}}

{client{
type meh = [ Html5_types.div_content_fun ]

let (mpd_status: meh elt React.signal), mpd_status_s = React.S.create (pcdata "")
let (lieu_select: meh elt React.signal), lieu_select_s = React.S.create (pcdata "") 
let (lieux: meh elt list React.signal), lieux_s = React.S.create []

let build_concerts_table concerts =
  let alternate =
    let switch = ref true in
    fun (artiste, lieu, date) ->
      switch := not (!switch);
      let lieu = Printf.sprintf "%s (%s)" (fst lieu) (snd lieu) in
      div
        ~a:[a_id (if (!switch) then "concert_odd" else "concert_even")]
        [h2 [pcdata artiste];
         p [pcdata ("le " ^ date ^ " à " ^ lieu)]]
  in
  match concerts with
  | `Processing ->
    p [
      fa ~a:[a_id "refresh-logo"] ["fa-refresh"; "fa-spin"]
    ]
  | `Table concerts ->
    concerts
    |> List.map alternate
    |> (fun l ->
      div [
        table (List.map (fun elt -> tr [td [elt]]) l);
      ])

let update_concerts_rpc = %(server_function Json.t<update_concerts_rpc> update_concerts)
let update_concerts userid _ =
  Lwt.async (fun () ->
    update_concerts_rpc userid
  )

let update_mpd_library_rpc = %(server_function Json.t<update_mpd_library_rpc> update_mpd_library)
let update_mpd_library userid field_host field_port _ =
  let host = field_host##value |> Js.to_string in
  let port = field_port##value |> Js.parseInt in
  Lwt.async (fun () ->
    mpd_status_s (i ~a:[a_class ["fa"; "fa-refresh"; "fa-spin"]] []);
    update_mpd_library_rpc (userid, host, port) >|= fun ok ->
    (match ok with
     | false -> mpd_status_s (pcdata "Error")
     | true -> mpd_status_s (fa ["fa-check"]));
    lwt () = Lwt_js.sleep 5. in
    mpd_status_s (pcdata "");
    Lwt.return ()
  )

let lieu_select_loop typ ville departement salle region pays =
  Lwt.async (fun () ->
    let typ_js = To_dom.of_select typ in
    Lwt_js_events.changes typ_js (fun _ _ ->
      begin match typ_js##value |> Js.to_string with
        | "Ville" -> lieu_select_s ville
        | "Département" -> lieu_select_s departement
        | "Salle" -> lieu_select_s salle
        | "Région" -> lieu_select_s region
        | _ -> lieu_select_s pays
      end; Lwt.return ()
    )
  )

let add_lieu_rpc = %(server_function Json.t<add_lieu_rpc> add_lieu)
let add_lieu userid typ _ =
  Lwt.async (fun () ->
    let get_val x = (To_dom.of_element x |> Obj.magic)##value |> Js.to_string in
    add_lieu_rpc (userid,
                  get_val typ,
                  get_val (React.S.value lieu_select))
  )

let del_lieu_rpc = %(server_function Json.t<del_lieu_rpc> del_lieu)
let del_lieu userid n _ =
  Lwt.async (fun () -> del_lieu_rpc (userid, n))


let build_lieux_table userid lieux =
  table (List.mapi (fun i (typ, value) ->
    tr [
      td [pcdata (typ ^ " : " ^ value)];
      td [
        D.button ~button_type:`Button
          ~a:[a_onclick (del_lieu userid i)]
          [fa ["fa-times"]]
      ];
    ]
  ) lieux)

let clear_db_rpc = %(server_function Json.t<clear_db_rpc> clear_db)
let clear_db userid _ =
  Lwt.async (fun () -> clear_db_rpc userid)
}}

let main_service_handler userid_o () () =
  Wdmproject_container.page userid_o (
    [
      p [em [pcdata ""]]
    ]
  )

let concert_handler userid_o () () =
  match userid_o with
  | None -> Wdmproject_container.page userid_o []
  | Some userid ->
    lwt user_data = get_user_data userid in
    let concerts_e, _ = concerts_event userid in
    let initial_concerts = `Table (user_data.selected_concerts
                                   |> concerts_to_client) in

    let btn = D.button ~button_type:`Button
        ~a:[a_onclick {{ update_concerts %userid }}]
        [pcdata "Actualiser"] in

    Wdmproject_container.page userid_o [
      btn;
      C.node {{ R.node (React.S.map build_concerts_table
                          (%concerts_e |> React.S.hold %initial_concerts))
              }};
    ]

let parameter_handler userid_o () () =
  match userid_o with
  | None -> Wdmproject_container.page userid_o []
  | Some userid ->
    lwt user_data = get_user_data userid in

    let initial_lieux = lieux_to_client user_data.settings.lieux in
    let lieux_e, _ = lieux_event userid in

    let ville_input = D.string_input ~input_type:`Text () in
    let departement_input = D.int_input ~input_type:`Number
        ~a:[a_input_min 1.] () in
    let salle_input = D.string_input ~input_type:`Text () in
    let regions_select = D.Raw.select ~a:[a_required `Required]
        (List.map (pcdata %> option) regions) in
    let pays_select = D.Raw.select ~a:[a_required `Required]
        (List.map (pcdata %> option) pays) in
    let lieu_type = D.Raw.select
        ~a:[a_required `Required]
        [
          option (pcdata "Ville");
          option (pcdata "Département");
          option (pcdata "Salle");
          option (pcdata "Région");
          option (pcdata "Pays");
        ] in

    let _ = {unit{ lieu_select_s %ville_input }} in
    let _ = {unit{ lieu_select_loop
                   %lieu_type
                   %ville_input
                   %departement_input
                   %salle_input
                   %regions_select
                   %pays_select }} in

    let lieu_button =
      button ~button_type:`Button
        ~a:[a_onclick {{ add_lieu %userid %lieu_type }}]
        [pcdata "Ajouter"] in

    let mpd_host_input = D.string_input ~input_type:`Text
        ~value:(user_data.settings.mpd_server |? "") ()
    in
    let mpd_port_input = D.int_input ~input_type:`Number
        ~a:[a_input_min 1.]
        ~value:(user_data.settings.mpd_port |? 6600)
        () in
    let mpd_button =
      button ~button_type:`Button
        ~a:[a_onclick {{ update_mpd_library %userid
                           (To_dom.of_input %mpd_host_input)
                           (To_dom.of_input %mpd_port_input) }}]
        [pcdata "Scanner"]
    in

    let artists_nb = Hashtbl.length user_data.library.table in

    Wdmproject_container.page userid_o [
      div ~a:[a_id "parametres"] [
        div [
          h2 [pcdata "Lieux"];
          C.node {{ R.node (
            React.S.map (build_lieux_table %userid)
              (React.S.hold %initial_lieux %lieux_e)
          )
          }};

          lieu_type;
          C.node {{ R.node (lieu_select) }};
          lieu_button;
        ];
      (* div [ *)
      (*   h2 [pcdata "Bibliothèque locale"]; *)
      (*   p [ *)
      (*     pcdata "Importer : "; *)
      (*     raw_input ~input_type:`Text ~name:"import" (); *)
      (*     raw_input ~input_type:`Submit ~value:"Ok" () *)
      (*   ]]; *)
        div [
          h2 [pcdata "Serveur MPD"];
          div [
            pcdata "Adresse : ";
            mpd_host_input;
            pcdata " Port : ";
            mpd_port_input;
            mpd_button;
            C.node {{ R.node (mpd_status) }};
          ]];
        div [
          h2 [pcdata "Facebook"];
          (post_form ~service:Wdmproject_services.facebook_login
             (fun () ->
                [pcdata "Extraire les musiques de mon compte Facebook : ";
                 raw_input ~input_type:`Submit ~value:"Extraire" ()]))
            (match userid_o with Some user -> Int64.to_string user | None -> "")];
        div [
          h2 [pcdata "Données collectées"];
          div [
            pcdata (Printf.sprintf "%d artists " artists_nb);
            button ~button_type:`Button ~a:[a_onclick {{ clear_db %userid }}]
              [pcdata "Effacer"];
          ]
        ]
      ]
    ]


let facebook_userid_handler userid_o userid () =
  match userid_o with
  | Some userid -> parameter_handler userid_o () ()
  | _ -> Lwt.fail (Failure "userid_o")

let facebook_login_handler userid () =
  let open Wdmproject_config in
  let redirect_uri = "http://project.hotbeverage.org/facebook" in
  Lwt.return (Facebook.log_people_in ~client_id ~redirect_uri ~userid)

let facebook_success_handler (code, userid) () =
  let open Wdmproject_config in
  let redirect_uri = "http://project.hotbeverage.org/facebook" in
  Facebook.confirme_identity ~client_id ~client_secret ~redirect_uri ~code
  >>= fun access_token ->
  (if (Str.string_match (Str.regexp "access_token*") access_token 0)
   then
     (Printf.printf "Facebook identification: success!\n%!";
      Facebook.get_user_music ~access_token >>= fun artists ->
      update_library
        (Int64.of_string userid)
        (List.map (fun s -> (s, Set.singleton "")) artists))
   else
   (Printf.printf "Facebook identification: failure...\n%!";
   Lwt.fail (Failure access_token)))
  >|= fun () -> Wdmproject_services.parameter_service

let facebook_failure_handler _ () =
  Lwt.return Wdmproject_services.parameter_service

let () =
  Wdmproject_base.App.register
    Eba_services.main_service
    (Wdmproject_page.Opt.connected_page main_service_handler);

  Wdmproject_base.App.register
    Wdmproject_services.concert_service
    (Wdmproject_page.Opt.connected_page concert_handler);

  Wdmproject_base.App.register
    Wdmproject_services.parameter_service
    (Wdmproject_page.Opt.connected_page parameter_handler);

  Eliom_registration.Redirection.register
    Wdmproject_services.facebook_login_success
    facebook_success_handler;

  Eliom_registration.Redirection.register
    Wdmproject_services.facebook_login_failure
    facebook_failure_handler;

  Eliom_registration.String_redirection.register
    Wdmproject_services.facebook_login
    facebook_login_handler;

  Wdmproject_base.App.register
    Wdmproject_services.facebook_userid
   (Wdmproject_page.Opt.connected_page facebook_userid_handler)
