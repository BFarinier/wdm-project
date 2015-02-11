open Batteries
open Defs

let cos_similarity (artist1: genres) (artist2: genres): float =
  let m = Map.merge (fun genre w1 w2 ->
    match w1, w2 with
    | None, _ | _, None -> None
    | Some w1, Some w2 -> Some (w1 *. w2)) artist1 artist2 in
  let sp (* scalar product *) = Map.fold (+.) m 0. in
  let norm a =
    Map.fold (fun x acc -> (x *. x) +. acc) a 0.
    |> Float.sqrt
  in

  let den = (norm artist1) *. (norm artist2) in
  if Float.equal den 0. then 0. (* idk.. *)
  else sp /. den
  
let rank (artist_req: genres) ({albums_nb; table}: music_library):
  (artist * float) (* similarité artiste <-> artiste max *)
  * float (* score de matching global avec les tags de la bibliothèque *)
  =
  let max_cos_similarity = Hashtbl.fold (fun name (_, genres) (max_name, max_similarity) ->
    let sim = cos_similarity artist_req genres in
    if Float.Compare.(sim > max_similarity) then
      (name, sim)
    else
      (max_name, max_similarity)) table ("", 0.)
  in

  let global_score =
    Hashtbl.fold (fun name (albums, genres) acc ->
      let n = Set.cardinal albums in
      let artist_weight = (Float.of_int n) /. (Float.of_int albums_nb) in
      Map.foldi (fun genre genre_weight acc ->
        let s = (Map.Exceptionless.find genre acc) |? 0. in
        Map.add genre (s +. (genre_weight *. artist_weight)) acc
      ) genres acc
    ) table Map.empty
    |> fun library_genres -> cos_similarity artist_req library_genres
  in

  (max_cos_similarity, global_score)

let filter_score ((artist, score), global_score): bool =
  if Float.equal score 0. && Float.equal global_score 0. then
    false
  else
    true
