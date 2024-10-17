(* ========== Vaja 2: Uvod v funkcijsko programiranje  ========== *)

(*----------------------------------------------------------------------------*]
Vektorje predstavimo kot seznam števil s plavajočo vejico.
[*----------------------------------------------------------------------------*)

type vector = float list

(*----------------------------------------------------------------------------*]
Definirajte enotske vektorje `i`, `j` in `k` v treh dimenzijah.
[*----------------------------------------------------------------------------*)

let i = [1.; 0.; 0.]
let j = [0.; 1.; 0.]
let k = [0.; 0.; 1.]

(*----------------------------------------------------------------------------*]
Napišite funkcijo `razteg : float -> vector -> vector`, ki vektor, 
predstavljen s seznamom števil s plavajočo vejico, pomnoži z danim skalarjem.
[*----------------------------------------------------------------------------*)

(* Ne vem, zakaj je ta rec in *)

let razteg faktor vektor = List.map (fun x -> faktor *. x) vektor

let razteg' faktor vektor = List.map (( *. ) faktor) vektor (* delna uporaba mnozenja*)

let razteg'' faktor = List.map (( *. ) faktor) (* delna uporaba, nardiš funkcijo pač*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `sestej : vector -> vector -> vector`, ki vrne vsoto dveh 
vektorjev.
[*----------------------------------------------------------------------------*)

let rec sestej v1 v2 = List.map2 ( +. ) v1 v2

let sestej' = List.map2 ( +. ) (* Delna uporaba!! *)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `skalarni_produkt : vector -> vector -> float`, ki izračuna 
skalarni produkt dveh vektorjev
[*----------------------------------------------------------------------------*)

let rec skalarni_produkt v1 v2 = List.fold_left ( +. ) 0. (List.map2 ( *. ) v1 v2)

(* ali bi bilo z rekurzijo in match-om to bolje? Ali bi se potem lahko delno
uporabilo, če bi naredila nekako tako? Ker zdaj ne vidim, da bi se lahko,
ker je tako gnezdeno ...
V rešitvah je dovolj podobno, tkoda nima veze, očitno ne*)

(* List.fold_left2 ( +. ) 0. v1 v2 *)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `norma : vector -> float`, ki vrne evklidsko normo vektorja.
[*----------------------------------------------------------------------------*)

let norma v = sqrt (skalarni_produkt v v)

(* Ne vem, zakaj manjka klele vaja za vmesni kot in normiranje:
let vmesni_kot v1 v2 =
  acos (skalarni_produkt v1 v2 /. (norma v1 *. norma v2))
  
  let normirani v = razteg (1.0 /. norma v) v
  *)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `projeciraj : vector -> vector -> vector`, ki izračuna 
projekcijo prvega vektorja na drugega.
[*----------------------------------------------------------------------------*)

let projeciraj v1 v2 = razteg ((skalarni_produkt v1 v2) /. (norma v1)) v2

(* razteg (skalarni_produkt v1 v2) (normirani v2) *)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `ovij : string -> string -> string`, ki sprejme ime HTML 
oznake in vsebino ter vrne niz, ki predstavlja ustrezno HTML oznako.

Primer:
`ovij "h1" "Hello, world!"`

[*----------------------------------------------------------------------------*)

let ovij oznaka vsebina =
  "<" ^ oznaka ^ ">" ^ vsebina ^ "</" ^ oznaka ^ ">"

(* copy-paste-ano, ker nisem dojela, kaj naloga sploh zahteva*)

(* Printf.sprintf "<%s>%s</%s>" oznaka vsebina oznaka *)
(* Nekoč si poglej ta modul mogoče*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `zamakni : int -> string -> string`, ki sprejme število 
presledkov in niz ter vrne niz, v katerem je vsaka vrstica zamaknjena za ustrezno število presledkov.

Primer:
`zamakni 4 "Hello, world!"`

[*----------------------------------------------------------------------------*)

(* Ne vem, a je prou, ne vem, ali je optimalno, ampak ajde*)

let zamakni zamik niz =
  let dodaj k =
    String.cat (String.make k ' ')   (* d. u. da že kr mapaš to, da dodaš " "*)
  in
  String.concat "\n" (List.map (dodaj zamik) (String.split_on_char '\n' niz))
  (* let rec ponovi k s =
    if k = 0 then "" else s ^ ponovi (k - 1) s (* to bi lahko nardila s string.make*)
  in *)
  (* let rec razstavi niz =
    String.split_on_char '\n' niz
  in *) (* tudi lahko z delno uporabo je to samo split_on_char *)
  (* Vprašanje: kako je z delno uporabo, če je prva funkcija notr v oklepajih, 
   - verjetno se ne da, ane?*)

   (* Grdo sem naredila in v teh celih vajah sem pozabila, da veriženje obstaja *)
   (* "uradna": 
   let zamakni n vsebina =
  let presledki = String.make n ' ' in
  vsebina
  |> String.split_on_char '\n'
  |> List.map (String.cat presledki)
  |> String.concat "\n"
   *)

   (* Js mama OGABNO!!!! Prou men je ogabno že zdele, pol minute po branju rešitev
   napisala sem pa eno uro nazaj. Prou "fej". *)
    


(*----------------------------------------------------------------------------*]
Napišite funkcijo `ul : string list -> string`, ki sprejme seznam nizov in vrne 
niz, ki predstavlja ustrezno zamaknjen neurejeni seznam v HTML-ju:

Primer:
`ul ["ananas"; "banana"; "čokolada"]`

[*----------------------------------------------------------------------------*)

let ul sez =
  let jedro_seznama sez= 
    String.concat "\n" (List.map (fun x -> zamakni 2 (ovij "li" x)) sez)
  in
  ovij "ul" ("\n" ^ jedro_seznama sez ^ "\n")

  (* Spet, pomoje prou, pač prave ideje in vrstn red, kokr zgori.
  Sam pač veriženje mi je ušlo iz glave kot opcija at all*)
  (* uradna:
  let ul items =
  items
  |> List.map (ovij "li")
  |> String.concat "\n"
  |> zamakni 2
  |> (fun content -> "\n" ^ content ^ "\n")
  |> ovij "ul"
  *)


(*----------------------------------------------------------------------------*]
Napišite funkcijo `razdeli_vrstico : string -> string * string`, ki sprejme niz, 
ki vsebuje vejico, loči na del pred in del za njo.

Primer:
`razdeli_vrstico "mleko, 2"`

[*----------------------------------------------------------------------------*)

(*let rec razdeli_vrstico niz =
  List.map (String.trim) (String.split_on_char ',' niz)*)
(* vejica je sam ena, hočjo met urejen par *)
let razdeli_vrstico niz =
  let indeks = String.index niz ',' in (* pomagano z rešitvami, 
  pač vidla sem, da uporab String.index in da ima
  definirane indeks, prvi in drugi, nism vidla kako,
in da vrne nakonc par teh dveh trimanih *)
  let prvi = String.sub niz 0 indeks in
  let drugi = String.sub niz (indeks + 1) (String.length niz - (indeks + 1)) in
  (String.trim prvi, String.trim drugi)
(* Je pa kul, ker pač dam notr une stvari lih tko za foro,
ker itak vem, da dela alpa ne, načeloma*)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_v_seznam_parov : string -> (string * string) list`, 
ki sprejme večvrstični niz, kjer je vsaka vrstica niz oblike 
"izdelek, vrednost", in vrne seznam ustreznih parov.

Primer:
`pretvori_v_seznam_parov "mleko, 2\nkruh, 1\njabolko, 5"`

[*----------------------------------------------------------------------------*)

let pretvori_v_seznam_parov niz = 
  List.map razdeli_vrstico (String.split_on_char '\n' niz)

  (* Poleg veriženja pozabla tud trim!
  uradna:
  let pretvori_v_seznam_parov niz =
  niz
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map razdeli_vrstico
  *)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `pretvori_druge_komponente : ('a -> 'b) -> (string * 'a) list -> (string * 'b) list`,
ki dano funkcijo uporabi na vseh drugih komponentah elementov seznama.

Primer:
```ml
let seznam = [("ata", "mama"); ("teta", "stric")] in 
pretvori_druge_komponente String.length seznam
```

[*----------------------------------------------------------------------------*)

let pretvori_druge_komponente f list =
  List.map (fun (x, y) -> (x, f y)) list

  (* Spucaj funkcije nepotrebnih vrednosti!
  Pravilna:
  let pretvori_druge_komponente f =
  List.map (fun (x, y) -> (x, f y))
  *)

(*----------------------------------------------------------------------------*]
Napišite funkcijo `izracunaj_skupni_znesek : string -> string -> float`, ki 
sprejme večvrstična niza nakupovalnega seznama in cenika in izračuna skupni 
znesek nakupa.

Primer:
```ml
let nakupovalni_seznam = "mleko, 2\njabolka, 5"
and cenik = "jabolka, 0.5\nkruh, 2\nmleko, 1.5" in
izracunaj_skupni_znesek cenik nakupovalni_seznam
```

[*----------------------------------------------------------------------------*)

let izracunaj_skupni_znesek cenik seznam =
  let cenik = pretvori_druge_komponente float_of_string (pretvori_v_seznam_parov cenik) in
  let nakupovalni_seznam = pretvori_druge_komponente float_of_string (pretvori_v_seznam_parov seznam) in
  (* Kok je to grdo od ena do zelo ? *)
  (* fora je, da mi kle še nismo delal rekurzije. Zato ne vem, 
  kako it po seznamu.
  Pomoje bom kr nardila s pomožno rekurzivno pa bo ... Pa match ...
  Bom pol pogledala rešitev, pa vidla, kaj je kej*)
  let rec po_seznamu acc cene list =
    match list with
    | [] -> acc
    | x :: xs -> po_seznamu (acc +. (List.assoc (fst x) cene) *. (snd x)) cene xs
    (* acc-u prištejem zmnozek, kok je cena artikla, pa kok ga hočm*)
  in
  po_seznamu 0. cenik nakupovalni_seznam

  (* TO JE PRECEJ GRDO - VERIŽENJE OBSTAJA!!!! POZABILA SEM
  PA VEČ RAZDELJUJ IN PO VRSTICAH IN POIMENUJ IN NAREDI LEPŠE
  PA LAHKO BI NAREDILA LEPO Z MAPOM
  POGLEJ SI ŠE ENKRAT REŠITVE IN NEKI V LAJFU NAPIŠI TAKO!
  Ampak sem pa prišla v slabih dveh urcah čez in sploh ni blo tok hudo :) *)

  (* uradna:
  let izracunaj_skupni_znesek cenik seznam =
  let cenik =
    cenik
    |> pretvori_v_seznam_parov
    |> pretvori_druge_komponente float_of_string
  in
  let cena_izdelka (izdelek, kolicina) =
    let cena = List.assoc izdelek cenik in
    float_of_int kolicina *. cena
  in
  seznam
  |> pretvori_v_seznam_parov
  |> pretvori_druge_komponente int_of_string (* a mora bit kle float?*)
  |> List.map cena_izdelka
  |> vsota_seznama
  *)
  
