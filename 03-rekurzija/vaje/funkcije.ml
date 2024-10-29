(* ========== Vaja 2: Funkcijsko Programiranje  ========== *)

(*----------------------------------------------------------------------------*]
 Definirajte pomožno funkcijo za obračanje seznamov.
[*----------------------------------------------------------------------------*)

(* let reverse sez =
  (* Pofejkan iz ocaml.org, kjer sem prej vidla ta primer*)
  (* Ampak tud dobra vaja, da pač razmisl, kaksno funkcijo naloga
  zahteva od tebe in kaj je logično, da sprejme.
  Pač tko alpa drgač kle rabš pomožno*)
  let rec aux acc =
    function
    | [] -> acc
    | h :: t -> aux (h :: acc) t (* Ne pozab zavedanja, da kar si tm 
    napisala kot brez zadnga argumenta lahko vedno notr kličeš z zadnim
    oziroma itak morš. Kle mi more mal bolj kliknt tole še.*)
  in
  aux [] sez *)
(* Neki mi teži, da ma ta expression type unit, ko ga hočem pognat, 
ne vem več, kaj je foram tkoda grem direkt copy pasteat, da vidm kaj *)

let rev list =
  let rec aux acc = function
    | [] -> acc
    | h :: t -> aux (h :: acc) t
  in
  aux [] list

(* okej, don't get, kaj je bil problem*)

(*----------------------------------------------------------------------------*]
 Funkcija [repeat x n] vrne seznam [n] ponovitev vrednosti [x]. Za neprimerne
 vrednosti [n] funkcija vrne prazen seznam.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # repeat "A" 5;;
 - : string list = ["A"; "A"; "A"; "A"; "A"]
 # repeat "A" (-2);;
 - : string list = []
[*----------------------------------------------------------------------------*)

let repeat x n =
  let rec aux acc x =
    function
    | a when a < 0 || a = 0 -> acc  (* Pazi na te pogoje, 
    najprej si dala kle -> [] namest -> acc in pol je seveda skos vračal
    prazen seznam *)
    | n -> aux (x :: acc) x (n - 1)
  in
  aux [] x n   

(*----------------------------------------------------------------------------*]
 Funkcija [range] sprejme število in vrne seznam vseh celih števil od 0 do
 vključno danega števila. Za neprimerne argumente funkcija vrne prazen seznam.
 Funkcija je repno rekurzivna.
Pri tem ne smete uporabbiti vgrajene funkcije [List.init].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # range 10;;
 - : int list = [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10]
[*----------------------------------------------------------------------------*)

(* List.init si poglej *)
let range n =
  let rec aux acc n =
    match n with
    | a when a < 0 -> acc
    | n -> aux (n :: acc) (n - 1)
  in
  aux [] n 

(*----------------------------------------------------------------------------*]
 Funkcija [map f list] sprejme seznam [list] oblike [x0; x1; x2; ...] in
 funkcijo [f] ter vrne seznam preslikanih vrednosti, torej
 [f x0; f x1; f x2; ...].
 Pri tem ne smete uporabiti vgrajene funkcije [List.map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (+) 2 in
   map plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let map' f list = (* a js te stvari delam narobe oziroma neumno? *)
  let rec aux acc f list =
    match list with
    | [] -> acc
    | x :: xs -> aux (acc @ [f x]) f xs
  in
  aux [] f list

  (* Aha, apparently ne rabš še enkrat notr dajat stvari, k so
  že argumenti krovne funkcije!
  Popravljeno: *)

let map f list = 
  let rec aux acc list =
    match list with
    | [] -> acc
    | x :: xs -> aux (acc @ [f x]) xs
  in
  aux [] list

(*----------------------------------------------------------------------------*]
 Časovna zahtevnost operatorja [@] je linearna v prvem argumentu, poskušajte 
 napisati reverse tako, da bo bolj učinkovit in hkrati repno rekurziven.
 Pri tem ne smete uporabiti vgrajene funkcije [List.rev] ali [List.rev_append].
[*----------------------------------------------------------------------------*)

(* Ne vem list, kaj misljo. Zdej gledam repno rekurzijo s predavanj
in vidm, da bi navadno rekurzijo napisal tkole npr:
let rec vsota =
  function
  | [] -> 0
  | glava :: rep -> glava + vsota rep
in pol pač so šele te pomožne funkcije, k sm jih js zard ocaml.org vidla prej

Pol je pa map napisu tkole, btw:
let map' f seznam =
  let rec aux acc =
    function
    | [] -> List.rev acc
    | glava :: rep -> aux (f glava :: acc) rep
  in
  aux [] seznam
  *)
let rec reverse = ()

(* 
FK:
let obrni sez =
  let rec revers_tlrc za_obrnit kup_nalaganja =
    match za_obrnit with
    | [] -> kup_nalaganja
    | x :: xs -> reverse_tlrc xs (x :: kup_nalaganja)
  in
  revers_tlrc sez []
    *)

(*----------------------------------------------------------------------------*]
 Funkcija [map_tlrec] je repno rekurzivna različica funkcije [map].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let plus_two = (fun x -> x + 2) in
   map_tlrec plus_two [0; 1; 2; 3; 4];;
 - : int list = [2; 3; 4; 5; 6]
[*----------------------------------------------------------------------------*)

let rec map_tlrec = ()

(* VPRAŠAJ NA VAJAH TEDVA *)

(*----------------------------------------------------------------------------*]
 Funkcija [mapi] je ekvivalentna python kodi:

  def mapi(f, list):
      mapi_list = []
      index = 0
      for x in list:
          mapi_list += [f(x, index)]
          index += 1
      return mapi_list

 Pri tem ne smete uporabiti vgrajene funkcije [List.mapi].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # mapi (+) [0; 0; 0; 2; 2; 2];;
 - : int list = [0; 1; 2; 5; 6; 7]
[*----------------------------------------------------------------------------*)

let mapi f = 
  let rec aux acc i =
    function
    | [] -> acc
    | x :: xs -> aux (acc @ [f x i]) (i + 1) xs
  in
  aux [] 0

(*----------------------------------------------------------------------------*]
 Funkcija [zip] sprejme dva seznama in vrne seznam parov istoležnih
 elementov podanih seznamov. Če seznama nista enake dolžine vrne napako.
 Pri tem ne smete uporabiti vgrajene funkcije [List.combine].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # zip [1; 1; 1; 1] [0; 1; 2; 3];;
 - : (int * int) list = [(1, 0); (1, 1); (1, 2); (1, 3)]
 # zip [1; 1; 1; 1] [1; 2; 3; 4; 5];;
 Exception: Failure "Different lengths of input lists.".
[*----------------------------------------------------------------------------*)

(* VPRAŠAJ NA VAJAH: Pač seprav a se tko dela tko, s temi pomožnimi
rekurzivnimi? Ker js to vse delam isto, pa ne vem, a je to ok al ne. *)

let zip list1 list2 = 
  let rec aux acc list1 list2 =
    match (list1, list2) with
    | ([], []) -> acc
    | (x :: xs, y :: ys) -> aux (acc @ [(x, y)]) xs ys
    | (_, _) -> failwith "Different lengths of input lists."
  in
  aux [] list1 list2


(*----------------------------------------------------------------------------*]
 Funkcija [unzip] je inverz funkcije [zip], torej sprejme seznam parov
 [(x0, y0); (x1, y1); ...] in vrne par seznamov ([x0; x1; ...], [y0; y1; ...]).
 Pri tem ne smete uporabiti vgrajene funkcije [List.split].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let unzip = 
  let rec aux acc1 acc2 =
    function
    | [] -> (acc1, acc2)
    | x :: xs -> aux (acc1 @ [fst x]) (acc2 @ [snd x]) xs
  in
  aux [] []


(*----------------------------------------------------------------------------*]
 Funkcija [unzip_tlrec] je repno rekurzivna različica funkcije [unzip].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # unzip_tlrec [(0,"a"); (1,"b"); (2,"c")];;
 - : int list * string list = ([0; 1; 2], ["a"; "b"; "c"])
[*----------------------------------------------------------------------------*)

let rec unzip_tlrec = ()

(*----------------------------------------------------------------------------*]
 Funkcija [loop condition f x] naj se izvede kot python koda:

  def loop(condition, f, x):
      while condition(x):
          x = f(x)
      return x

 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # loop (fun x -> x < 10) ((+) 4) 4;;
 - : int = 12
[*----------------------------------------------------------------------------*)

let rec loop' condition f x =
  match (condition x) with
  | true -> loop' condition f (f x)
  | false -> x
 (* ali naredim samo z if nekako ? 
  Al je ubistvu if itak problem, ker morš met isti tip, 
    pa je to pač nadomestek ? *)

let rec loop condition f x =
  if (condition x) then loop condition f (f x)
  else x 
 

(*----------------------------------------------------------------------------*]
 Funkcija [fold_left_no_acc f list] sprejme seznam [x0; x1; ...; xn] in
 funkcijo dveh argumentov [f] in vrne vrednost izračuna
 f(... (f (f x0 x1) x2) ... xn).
 V primeru seznama z manj kot dvema elementoma vrne napako.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # fold_left_no_acc (^) ["F"; "I"; "C"; "U"; "S"];;
 - : string = "FICUS"
[*----------------------------------------------------------------------------*)

let rec fold_left_no_acc f list = 
  match list with
  | x :: (y :: xs) -> 
    (
      let prvi = f x y in
      let rec aux acc =
        function
        | [] -> acc
        | x :: xs -> aux (f acc x) xs
      in
      aux prvi xs
    )
  | _ -> failwith "Seznam ima premalo elementov." 


(*----------------------------------------------------------------------------*]
 Funkcija [apply_sequence f x n] vrne seznam zaporednih uporab funkcije [f] na
 vrednosti [x] do vključno [n]-te uporabe, torej
 [x; f x; f (f x); ...; (f uporabljena n-krat na x)].
 Funkcija je repno rekurzivna.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # apply_sequence (fun x -> x * x) 2 5;;
 - : int list = [2; 4; 16; 256; 65536; 4294967296]
 # apply_sequence (fun x -> x * x) 2 (-5);;
 - : int list = []
[*----------------------------------------------------------------------------*)

let apply_sequence f x n =
  let rec aux acc x n =
    match n with
    | 0 -> acc
    | a when a < 0 -> []
    | n -> aux (acc @ [f x]) (f x) (n - 1)
  in
  aux [] x n


(*----------------------------------------------------------------------------*]
 Funkcija [filter f list] vrne seznam elementov [list], pri katerih funkcija [f]
 vrne vrednost [true].
 Pri tem ne smete uporabiti vgrajene funkcije [List.filter].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # filter ((<)3) [0; 1; 2; 3; 4; 5];;
 - : int list = [4; 5]
[*----------------------------------------------------------------------------*)

let filter f list = 
  let rec aux acc =
    function
    | [] -> acc
    | x :: xs -> 
        if (f x) 
          then aux (acc @ [x]) xs
          else aux acc xs
  in
  aux [] list

  (*
    let rec aux acc =
    function
    | [] -> acc
    | x :: xs -> 
      (* match ()   (* ali je kle if bolj eleganten ?? ! *)
        vpraši ?:
let oblika_trikotnika =
  function
  | (a, b, c) ->
      if not (veljaven_trikotnik a b c) then
        "neveljaven"
      else match (a, b, c) with
      | (0, _, _) | (_, 0, _) | (_, _, 0) -> "izrojen"
      | (a, b, c) ->
          if a = b && b = c then "enakostraničen"
          else if a = b || b = c || a = c then "enakokrak"
          else "poljuben" *)
          *)
  

(*----------------------------------------------------------------------------*]
 Funkcija [exists] sprejme seznam in funkcijo, ter vrne vrednost [true] čim
 obstaja element seznama, za katerega funkcija vrne [true] in [false] sicer.
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # exists ((<) 3) [0; 1; 2; 3; 4; 5];;
 - : bool = true
 # exists ((<) 8) [0; 1; 2; 3; 4; 5];;
 - : bool = false
[*----------------------------------------------------------------------------*)

let exists f list = 
  let rec aux =
    function 
    | [] -> false
    | x :: xs ->
      if (f x)
        then true
        else aux xs  (* match ali if ???*)
  in 
  aux list  (* + a da je zahteva po repni rekurziji, je v praksi, da ma aux?*)

(*----------------------------------------------------------------------------*]
 Funkcija [first f default list] vrne prvi element seznama, za katerega
 funkcija [f] vrne [true]. Če takšnega elementa ni, vrne [default].
 Funkcija je repno rekurzivna.
 Pri tem ne smete uporabiti vgrajene funkcije [List.find] ali podobnih. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # first ((<) 3) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 5
 # first ((<) 8) 0 [1; 1; 2; 3; 5; 8];;
 - : int = 0
[*----------------------------------------------------------------------------*)

let first f default list =
  let rec aux =
    function
    | [] -> default
    | x :: xs ->
      if f x
        then x
        else aux xs   (* kakšna je kle najlepša struktura/sistem/ najbolj pametno?*)
  in
  aux list









(* ------------------------------------------------------------- *)
(* VPRAŠAJ NA VAJAH? *)

(* btw btw, tkole recimo zgleda rekurzija brez pomožne, kjer to ni smiselno:
let rec last = function 
| [] -> None 
| x::[] -> Some x
| _ :: t -> last t ;;
*)

let rec drop_while f = 
  function
  | [] -> []
  | x :: xs when f x -> drop_while f xs
  | list -> list

(* VPRAŠAJ NA VAJAH TE STVARI IZ DN:
Pač moje vprašanje je, a pač je sam men najlaži tko delat te sezname 
al se ubistvu tko (z rekurzijo) dela te sezname. In a je bolš met brez
pomoznih, če se da, al kaj (cca a nej privzamemo, da hocmo repno rekurzivne
oziroma a to sploh to pomen .... ? )*)

let take n list =
  let rec aux acc n list =
    match list, n with
    | ([], _) -> acc
    | (x :: xs, n) when n > 0 -> aux (acc @ [x]) (n - 1) xs
    | (_, _) -> acc
  in
  aux [] n list


  (* VPRAŠAJ NA VAJAH: *)
  let stevke c n =
    let rec aux acc n =
      match n with (* Ali je bolj pregledno, če pustim to, namesto da dam function ?*)
      | 0 -> acc
      | _ -> aux ((Int.rem n c) :: acc) (n / c)
    in
    aux [] n
  (* Ali je dovolj pregledno, da se da razbrati sploh ?
  Pri vseh iz tega sklopa preveri še, ali preveč kompliciraš,
  ali je lepa rešitev in ali se da narediti lepše*)

(* Btw, kle maš izomorfizem, če maš funkcije iz AxB v funkcije iz BxA,
totalno neuporabno:
let psi1 f =
  let f' a b = f b a in
  f'
  *)

(* VPRAŠAJ NA VAJAH *)
let phi3 (a, (b, c)) = ((a, b), c)

let psi3 ((a, b), c) = (a, (b, c))

(* Ali so kateri oklepaji nepotrebni? *)

let phi3' a (b, c) = ((a, b), c)

let psi3' (a, b) c = a (b, c)

(* Vem, da ni isto, ampak zanima me, ali torej naloga hoče prvo opcijo
- ki je čisto LMN in nič PROG? *)

(* VPRAŠAJ TOLE: *)
let phi7 f =
  let f' c = fst (f c) in
  let f'' c = snd (f c) in
  f', f''

let psi7 (f, g) =
  let f' c = (f c, g c) in
  f'


(* ODSTRANJEVANJE ODVEČNIH NIČEL ?
No, there's no pattern that matches against the end of a list. It's not 
an attractive structure in OCaml because it takes linear time to find 
the end of a list. OCaml pattern matching is supposed to be fast.

You can reverse your list and match the beginning of the reversed list. 
It's only a constant factor slower than finding the end of a list.
*)

(* + Ali lahko uporabljamo vgrajene funkcije ? *)

let pocisti p =
  let rec nicle =
    function
    | x :: xs when x = 0 -> nicle xs
    | a -> a
  in
  p
  |> List.rev
  |> nicle
  |> List.rev

(* in *)
let odvod p = 
  let odstrani_glavo =
    function
    | []-> []
    | x :: xs -> xs
  in
  p 
  |> List.mapi (fun i x -> i * x)
  |> odstrani_glavo

(* DEF. VERIŽENJA! Poglej si še enkrat in nej ti zares klikne (02)*)

(* Ali lahko tako pustim? *)
let ( +++ ) p1 p2 =
  let rec aux acc p1 p2 =
    match p1, p2 with
    | a, b when a = [] || b = [] -> acc @ a @ b
    | x :: xs, y :: ys -> aux (acc @ [x + y]) xs ys
  in
  pocisti (aux [] p1 p2)


(* Ali lahko še kakšno funkcijo uporabim ? 
... Samo verjetno to pomeni, da nekaj delam narobe ...
let repeat x n =

  *)

  (* množenje polinomov, ( *** ) *)
  let ( *** ) p1 p2 =
    let repeat x n =
      let rec aux acc x =
        function
        | a when a < 0 || a = 0 -> acc
        | n -> aux (x :: acc) x (n - 1)
      in
      aux [] x n 
    in 
    let rec mnozenje_s_clenom acc clen stopnja = (* p2 *)
      function
      | [] -> (repeat 0 stopnja) @ acc
      | x :: xs -> mnozenje_s_clenom (acc @ [clen * x]) clen stopnja xs
    in
    let rec aux acc i p1 p2 = (* če ne bi imela zgoraj, bi verjetno rabila "delni" ? *)
      match p1 with
      | [] -> acc
      | x :: xs -> aux (acc +++ (mnozenje_s_clenom [] x i p2)) (i + 1) xs p2
    in
  pocisti (aux [] 0 p1 p2)

  (* VPRAŠAJ NA VAJAH, KAJ SEM PREZAKOMPLICIRALA! IN PREMISLI!*)
  (* In ali je okej, da uporabljam tukaj notri +++ ? (in pocisti) 
  Lahko to delam, ne da dajem te funkcije še v ***? *)

  let vrednost p a =
    let rec aux acc p i =
      match p with
      | [] -> acc
      | x :: xs -> aux (acc + int_of_float (float_of_int a ** i) * x) xs (i +. 1.)
    in
    aux 0 p 0.

(* Blazno grdo? *)


(* IZPIS ?!?!?! Gnezdenje matchov ?? *)



(* SAMODEJNO ODVAJANJE : *)
type odvedljiva = (float -> float) * (float -> float)

let vrednost (odv : odvedljiva) = fst odv

let odvod (odv : odvedljiva) = snd odv
(* ?? *)
(* Kako mu povemo, da je to tipa odvedljiva ? *)

let konstanta a : odvedljiva = ((fun x -> a), (fun x -> 0.)) 

let identiteta : odvedljiva = ((fun x -> x), (fun x -> 1.))

(* Razloži ? *)
let ( ++. ) : odvedljiva -> odvedljiva -> odvedljiva =
  fun (f, f') (g, g') -> ((fun x -> f x *. g x), (fun x -> f' x *. g x +. f x *. g' x))

let ( //. ) : odvedljiva -> odvedljiva -> odvedljiva =
  fun (f, f') (g, g') -> ((fun x -> f x /. g x), (fun x -> (f' x *. g x -. f x *. g' x) /. (g x) ** 2.)) 


let izpis p =
  let sup n = "\u" ^ "{U+208" ^ string_of_int n ^ "}" in  (* KAKO NAREST UTF TE KODE ?*)
  let rec aux acc i =
    function
    | [] -> acc
    | x :: xs when x = 0 -> aux acc (i + 1) xs
    | x :: xs when x > 0 -> 
      if i <> 0
        then match x with
          (* A bi se dal tko nekak narest
          da se nekje vmes neki poimenuje ?? !*)
          (* | 1 -> let clen = ["+"; "x" ^ (sup i)] in
          | n -> let clen = ["+"; string_of_int n; "x" ^ (sup i)] in *)
          | 1 -> aux (["+"; "x" ^ (sup i)] @ acc) (i + 1) xs
          | n -> aux (["+"; string_of_int n; "x" ^ (sup i)] @ acc) (i + 1) xs
        else aux (["+"; string_of_int x]) (i + 1) xs
    | x :: xs when x < 0 ->
      if i <> 0
        then match x with
          | -1 -> aux (["-"; "x" ^ (sup i)] @ acc) (i + 1) xs
          | n -> aux (["-"; string_of_int (abs n); "x" ^ (sup i)] @ acc) (i + 1) xs
        else aux (["-"; string_of_int (abs x)]) (i + 1) xs
  in 
  let seznam = aux [] 0 p in
  match seznam with
  | [] -> "Polinom ni bil podan"
  | x :: xs -> let seznam = xs in
  String.concat " " seznam


  (* zasilna, z ^ :*)
let izpis' p =
  let rec aux acc i =
    function
    | [] -> acc
    | x :: xs when x = 0 -> aux acc (i + 1) xs
    | x :: xs when x > 0 -> 
      if i <> 0
        then match x with
          (* A bi se dal tko nekak narest
          da se nekje vmes neki poimenuje ?? !*)
          (* | 1 -> let clen = ["+"; "x" ^ (sup i)] in
          | n -> let clen = ["+"; string_of_int n; "x" ^ (sup i)] in *)
          | 1 -> aux (["+"; "x^" ^ string_of_int i] @ acc) (i + 1) xs
          | n -> aux (["+"; string_of_int n; "x^" ^ string_of_int i] @ acc) (i + 1) xs
        else aux (["+"; string_of_int x]) (i + 1) xs
    | x :: xs when x < 0 ->
      if i <> 0
        then match x with
          | -1 -> aux (["-"; "x^" ^ string_of_int i] @ acc) (i + 1) xs
          | n -> aux (["-"; string_of_int (abs n); "x^" ^ string_of_int i] @ acc) (i + 1) xs
        else aux (["-"; string_of_int (abs x)]) (i + 1) xs
  in 
  let seznam = aux [] 0 p in
  match seznam with
  | [] -> "Polinom ni bil podan"
  | x :: xs -> let seznam = xs in
  String.concat " " seznam

  (* Ali je neumno narejeno? ce bi lahko poimenovala člen in naredila v unem tud en
  if x > 0 ["+"] else ["-"] pa tko, bi blo lahko lepš?*)
