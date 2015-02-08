open Batteries
open Lwt_ops
module Camomile = CamomileLibraryDefault.Camomile
module CaseMap = Camomile.CaseMap.Make (Camomile.UTF8)

open Defs

let regions_id = [
  21, "Alsace";
  19, "Aquitaine";
  4, "Auvergne";
  14, "Basse Normandie";
  13, "Bourgogne";
  15, "Bretagne";
  7, "Centre";
  9, "Champagne Ardenne";
  28, "Corse";
  25, "Etranger";
  20, "Franche-Comté";
  10, "Haute Normandie";
  3, "Ile de France";
  16, "Languedoc Roussillon";
  11, "Limousin";
  18, "Lorraine";
  6, "Midi pyrénées";
  2, "Nord Pas de Calais";
  29, "Outre Mer";
  12, "Pays de la Loire";
  5, "Picardie";
  8, "Poitou Charentes";
  17, "Provence Alpes Côte d'Azur";
  1, "Rhône Alpes";
]

let get_region_id name =
  List.find (snd %> ((=) name)) regions_id
  |> fst

let pays_id = [
  32, "Algérie";
  6, "Allemagne";
  8, "Andorre";
  25, "Autriche";
  2, "Belgique";
  30, "Brésil";
  28, "Bulgarie";
  34, "Cameroun";
  10, "Canada";
  48, "Congo";
  24, "Croatie";
  12, "Danemark";
  5, "Espagne";
  44, "Estonie";
  38, "Etats Unis";
  1, "France";
  17, "Grèce";
  14, "Hongrie";
  16, "Irlande";
  22, "Islande";
  11, "Italie";
  45, "Japon";
  47, "Lettonie";
  4, "Luxembourg";
  46, "Malawi";
  36, "Mali";
  26, "Malte";
  7, "Maroc";
  41, "Mauritanie";
  37, "Monaco";
  43, "Montenegro";
  19, "Norvège";
  9, "Pays Bas";
  42, "Pologne";
  15, "Portugal";
  29, "Republique Tcheque";
  27, "Roumanie";
  13, "Royaume Uni";
  40, "Russie";
  33, "Sénégal";
  20, "Serbie";
  23, "Slovenie";
  18, "Suède";
  3, "Suisse";
  31, "Tunisie";
  39, "Turquie";
  35, "Ukraine";
]

let get_pays_id name =
  List.find (snd %> ((=) name)) pays_id
  |> fst

let netchannel_of_stream s =
  let pipe = new Netchannels.pipe () in
  
  let rec aux s =
    Ocsigen_stream.next s >>= function
    | Ocsigen_stream.Finished None -> Lwt.return ()
    | Ocsigen_stream.Finished (Some s') -> aux s'
    | Ocsigen_stream.Cont (x, s') ->
      pipe#output_string x;
      aux s'
  in

  aux s >>= fun () ->
  pipe#close_out ();
  Lwt.return pipe

(* Custom DTD so that ocamlnet accepts <meta /> in <div> .. </div> blocks *)
let custom_dtd =
  Nethtml.html40_dtd |> List.modify "meta" (const (`Block, `Empty))

module Html_helpers = struct
  open Nethtml

  let get_elt pred (docs: document list):
    (string * (string * string) list * document list) list =
    List.filter_map (function
      | Data _ -> None
      | Element (e_name, e_attr, content) ->
        if pred e_name e_attr content then
          Some (e_name, e_attr, content)
        else
          None
    ) docs

  let get_elt_content pred : document list -> document list = fun docs ->
    get_elt pred docs |> List.map Tuple3.third |> List.flatten

  let rec find_elt pred (docs: document list):
    (string * (string * string) list * document list) list =
    List.map (function
      | Data _ -> []
      | Element (e_name, e_attr, content) ->
        if pred e_name e_attr content then
          [(e_name, e_attr, content)]
        else
          find_elt pred content
    ) docs
    |> List.flatten

  let rec find_elt_content pred : document list -> document list list =
    find_elt pred %> List.map Tuple3.third

  let p name attr_list = fun e_name e_attr _ ->
    e_name = name &&
    List.for_all (flip List.mem e_attr) attr_list

  let pa attr_list = fun _ e_attr _ ->
    List.for_all (flip List.mem e_attr) attr_list

  let collapse l = List.fold_left (^) "" l

  let rec collect_data (docs: document list): string =
    List.map (function
      | Data d -> d
      | Element (_, _, content) -> collect_data content
    ) docs
    |> collapse
    |> Str.split (Str.regexp "[ \\|\n\\|\r\\|\t]+")
    |> List.filter ((<>) "")
    |> String.join " "
end

let extract_concerts (docs: Nethtml.document list): concert list =
  let open Html_helpers in

  docs
  |> get_elt_content (p "html" [])

  |> find_elt_content (fun _ attr_list content ->
    List.mem ("itemtype", "http://data-vocabulary.org/Event") attr_list &&
    get_elt (p "meta" ["itemprop", "eventType"; "content", "Concert"]) content <> [])
    
  |> List.map (fun concert ->
    let name =
      find_elt_content (pa ["itemprop", "summary"]) concert
      |> List.first
      |> collect_data
      |> CaseMap.titlecase in

    let location_block =
      find_elt_content (pa ["itemtype", "http://data-vocabulary.org/Organization";
                            "itemprop", "location"]) concert
      |> List.first in

    let lieu =
      find_elt_content (pa ["itemprop", "locality"]) location_block
      |> List.first
      |> collect_data in

    let salle =
      find_elt_content (pa ["itemprop", "name"]) location_block
      |> List.first
      |> collect_data in

    let date =
      find_elt (p "time" ["itemprop", "startDate"]) concert
      |> List.first
      |> (fun (_, attr, _) -> List.find (fst %> ((=) "datetime")) attr |> snd)
      |> parse_date in

    {
      artiste = name;
      lieu = (lieu, salle);
      date = date
    }
  )

let get ?lieu (): concert list Lwt.t =
  let ville, departement, salle, region, pays =
    ref "", ref "", ref "", ref "", ref "" in

  begin match lieu with
  | None -> ()
  | Some (Ville v) -> ville := v
  | Some (Departement d) -> departement := (string_of_int d)
  | Some (Salle s) -> salle := s
  | Some (Region r) -> region := (get_region_id r |> string_of_int)
  | Some (Pays p) -> pays := (get_pays_id p |> string_of_int)
  end;

  let url =
    "http://www.infoconcert.com/recherche-concert-avancee.html?" ^ (
      Ocsigen_lib.Url.make_encoded_parameters [
        "motclef_ville", !ville;
        "motclef_salle", !salle;
        "departement_id", !departement;
        "region_id", !region;
        "pays_id", !pays;
      ]) in

  Ocsigen_http_client.get_url url >>= fun frame ->
  frame.Ocsigen_http_frame.frame_content
  |> Option.map_default (fun s ->
    netchannel_of_stream (Ocsigen_stream.get s) >>= fun pipe ->
    Ocsigen_stream.finalize s `Success >>= fun () ->
    Nethtml.parse ~dtd:custom_dtd (pipe :> Netchannels.in_obj_channel)
    |> extract_concerts
    |> Lwt.return
  ) (Lwt.return [])
