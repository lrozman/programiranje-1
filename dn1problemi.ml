let quick_brown_fox = "THEQUICKBRWNFXJMPSOVLAZYDG"
let rot13 = "NOPQRSTUVWXYZABCDEFGHIJKLM"

let indeks c = Char.code c - Char.code 'A'
let crka i = Char.chr (i + Char.code 'A') 


let sifriraj kljuc =
  let sifriraj_znak znak =
    if 'A' <= znak && znak <= 'Z' then
      String.get kljuc (indeks znak)
    else znak
  in
  String.map sifriraj_znak   



  let inverz kljuc =
    let abeceda = "ABCDEFGHIJKLMNOPQRSTUVWXYZ" in
    let crke = String.sub abeceda 0 (String.length kljuc) in
    let invertiraj_znak znak = 
      let i_v_kljucu = String.index kljuc znak in
      String.get abeceda i_v_kljucu
    in
    String.map invertiraj_znak crke


let dodaj_zamenjavo niz (mesto, menjava) =
  let i_mesta = indeks mesto in
  let menjati i znak =
    match i, znak with
    | i, '_' when i = i_mesta -> menjava
    | _, znak -> znak 
  in
  match String.get niz i_mesta with
  | '_' -> 
    (
    match String.index_opt niz menjava with
    | Some i -> None
    | None -> Some (String.mapi menjati niz)
    )
  | a when a = menjava -> Some niz
  | _ -> None


let dodaj_zamenjave niz (mesta, menjave) =
  let indeksi = String.length mesta in 
  let niz_opt = Some niz in
  let rec aux i str_opt =
    match i, str_opt with
    | i, Some s when i < indeksi -> aux (i + 1) (dodaj_zamenjavo s (String.get mesta i, String.get menjave i))
    | i, _ when i = indeksi -> str_opt (* pršli smo do konca *)
    | _, _ -> None (* i ni vecji od 1 in uno je pol karkol, tud ce je uno none*)
  in
  aux 0 niz_opt