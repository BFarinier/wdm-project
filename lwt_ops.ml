let (>>=) = fun t f -> Lwt.bind t f
let (>|=) = fun m f -> Lwt.map f m

let (<?>) = fun t t' -> Lwt.choose [t; t']
let (<&>) = fun t t' -> Lwt.join [t; t']

let lwt_list_filter_map_p f l =
  Lwt_list.map_p f l >>= fun l ->
  Lwt_list.filter_p (function Some x -> Lwt.return true | None -> Lwt.return false) l >>= fun l ->
  Lwt_list.map_p (function Some x -> Lwt.return x | None -> assert false) l
