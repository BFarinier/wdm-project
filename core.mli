open Defs

val rank: genres (* req artist *)
  -> music_library
  -> (artist * float) (* similarité max (artiste <-> artiste) *)
     * float          (* score de matching global avec les tags de la bibliothèque *)
