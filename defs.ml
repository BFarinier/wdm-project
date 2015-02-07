type date = CalendarLib.Calendar.t

let parse_date s =
  CalendarLib.Printer.Calendar.from_fstring
    "%Y-%m-%dT%H:%M:%S"
    s

type salle = string
type ville = string

type query_lieu =
  | Ville of string
  | Departement of int
  | Salle of string
  | Region of string
  | Pays of string

let regions = [
  "Alsace";
  "Aquitaine";
  "Auvergne";
  "Basse Normandie";
  "Bourgogne";
  "Bretagne";
  "Centre";
  "Champagne Ardenne";
  "Corse";
  "Franche-Comté";
  "Haute Normandie";
  "Ile de France";
  "Languedoc Roussillon";
  "Limousin";
  "Lorraine";
  "Midi pyrénées";
  "Nord Pas de Calais";
  "Pays de la Loire";
  "Picardie";
  "Poitou Charentes";
  "Provence Alpes Côte d'Azur";
  "Rhône Alpes";
  "Outre Mer";
  "Étranger";
]

let pays = [
  "Algérie";
  "Allemagne";
  "Andorre";
  "Autriche";
  "Belgique";
  "Brésil";
  "Bulgarie";
  "Cameroun";
  "Canada";
  "Congo";
  "Croatie";
  "Danemark";
  "Espagne";
  "Estonie";
  "Etats Unis";
  "France";
  "Grèce";
  "Hongrie";
  "Irlande";
  "Islande";
  "Italie";
  "Japon";
  "Lettonie";
  "Luxembourg";
  "Malawi";
  "Mali";
  "Malte";
  "Maroc";
  "Mauritanie";
  "Monaco";
  "Montenegro";
  "Norvège";
  "Pays Bas";
  "Pologne";
  "Portugal";
  "Republique Tcheque";
  "Roumanie";
  "Royaume Uni";
  "Russie";
  "Sénégal";
  "Serbie";
  "Slovenie";
  "Suède";
  "Suisse";
  "Tunisie";
  "Turquie";
  "Ukraine";
]

type concert = {
  artiste: string;
  lieu: ville * salle;
  date: date;
}
