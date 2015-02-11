open Lwt_ops

let get_string (url: string) : string Lwt.t =
  let buff = Buffer.create 256 in
  let rec aux stream =
    Ocsigen_stream.next stream >>= function
    | Ocsigen_stream.Finished None -> Lwt.return (Buffer.contents buff)
    | Ocsigen_stream.Finished (Some stream) -> aux stream
    | Ocsigen_stream.Cont (x, stream) -> Buffer.add_string buff x; aux stream
  in
  Ocsigen_http_client.get_url url >>= fun frame ->
  match frame.Ocsigen_http_frame.frame_content with
  | None -> raise Not_found
  | Some s ->
    let l = aux (Ocsigen_stream.get s) in
    lwt _ = Ocsigen_stream.finalize s `Success in l




let log_people_in ~client_id ~redirect_uri =
  Printf.sprintf "https://www.facebook.com/dialog/oauth?%s"
    (Ocsigen_lib.Url.make_encoded_parameters
       ["client_id", client_id;
        "redirect_uri",redirect_uri;
        "response_type", "code"])

let confirme_identity ~client_id ~client_secret ~redirect_uri ~code =
  Printf.sprintf "https://graph.facebook.com/oauth/access_token?%s"
    (Ocsigen_lib.Url.make_encoded_parameters
       ["client_id", client_id;
        "client_secret", client_secret;
        "redirect_uri", redirect_uri;
        "code", code])

let confirme_identity ~client_id ~client_secret ~redirect_uri ~code =
  confirme_identity ~client_id ~client_secret ~redirect_uri ~code
  |> get_string


let client_access_token ~client_id ~client_secret =
  Printf.sprintf "https://graph.facebook.com/oauth/access_token?%s"
    (Ocsigen_lib.Url.make_encoded_parameters
       ["client_id", client_id;
        "client_secret", client_secret;
        "grant_type", "client_credentials"])

let inspect_access_token ~input_token ~access_token =
  Printf.sprintf "https://graph.facebook.com/debug_token?%s"
    (Ocsigen_lib.Url.make_encoded_parameters
       ["input_token", input_token;
        "access_token", access_token])




let get_userid (url: string) : string option Lwt.t =
  try_lwt
    get_string url
    >|= Ezjsonm.from_string >|= Ezjsonm.value
    >|= (fun v -> Ezjsonm.find v ["data"; "user_id"])
    >|= Ezjsonm.get_string
    >|= fun userid -> Some userid
  with
    Not_found | Ezjsonm.Parse_error _ -> Lwt.return None

let get_music_name ~access_token ~page_id =
  Ocsigen_lib.Url.encode ("https://graph.facebook.com/v2.2/"^access_token)
  |> get_string
  >|= Ezjsonm.from_string >|= Ezjsonm.value
  >|= (fun v -> Ezjsonm.find v ["name"])
  >|= Ezjsonm.get_string

let get_user_music ~access_token = 
  try_lwt
    Printf.sprintf "https://graph.facebook.com/v2.2/me/music/?%s"
      (Ocsigen_lib.Url.make_encoded_parameters ["access_token", access_token])
    |> get_string >|= (fun s -> Printf.printf "%s%!" s; s)
    >|= Ezjsonm.from_string >|= Ezjsonm.value
    >|= (fun v -> Ezjsonm.find v ["data"])
    >|= Ezjsonm.get_list Ezjsonm.get_string
    >>= Lwt_list.map_p (fun page_id -> get_music_name ~access_token ~page_id)
  with
    Not_found | Ezjsonm.Parse_error _ -> Lwt.return []
