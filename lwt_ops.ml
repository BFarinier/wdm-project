let (>>=) = fun t f -> Lwt.bind t f
let (>|=) = fun m f -> Lwt.map f m

let (<?>) = fun t t' -> Lwt.choose [t; t']
let (<&>) = fun t t' -> Lwt.join [t; t']
