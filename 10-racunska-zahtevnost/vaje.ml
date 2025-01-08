(* 
Natančno definirajte pogoje, da funkcija `f` uredi seznam. 
*)

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Vstavljanjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*----------------------------------------------------------------------------*]
 Funkcija [insert y xs] vstavi [y] v že urejen seznam [xs] in vrne urejen
 seznam. 
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # insert 9 [0; 2];;
 - : int list = [0; 2; 9]
 # insert 1 [4; 5];;
 - : int list = [1; 4; 5]
 # insert 7 [];;
 - : int list = [7]
[*----------------------------------------------------------------------------*)
let insert y sez0 =
  let rec aux acc = function
    | [] -> List.rev (y :: acc)
    | x :: xs when x < y -> aux (x :: acc) xs (* dej premisli, to zna bit narobe*)
    | x :: xs -> List.concat [(List.rev (x :: y :: acc)); xs] (* To zgleda neučinkovito al kaj?*)
  in
  aux [] sez0

(*----------------------------------------------------------------------------*]
 Prazen seznam je že urejen. Funkcija [insert_sort] uredi seznam tako da
 zaporedoma vstavlja vse elemente seznama v prazen seznam.
[*----------------------------------------------------------------------------*)

let insert_sort sez = 
  let rec aux acc = function
    | [] -> acc
    | x :: xs -> aux (insert x acc) xs
  in
  aux [] sez

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri urejanju z izbiranjem na vsakem koraku ločimo dva podseznama, kjer je prvi
 že urejen, drugi pa vsebuje vse elemente, ki jih je še potrebno urediti. Nato
 zaporedoma prenašamo najmanjši element neurejenega podseznama v urejen
 podseznam, dokler ne uredimo vseh. 

 Če pričnemo z praznim urejenim podseznamom, vemo, da so na vsakem koraku vsi
 elementi neurejenega podseznama večji ali enaki elementom urejenega podseznama,
 saj vedno prenesemo najmanjšega. Tako vemo, da moramo naslednji najmanjši člen
 dodati na konec urejenega podseznama.
 (Hitreje je obrniti vrstni red seznama kot na vsakem koraku uporabiti [@].)
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)

(* čez vse v še obstoječem seznamu, najt minimum in pač grejo navrh ...*)

let choose_sort sez =
  let rec poisci_minimum_aux min preverjeni sez =
    match sez with
    | x :: xs -> 
      (if x < min then poisci_minimum_aux x (min :: preverjeni) xs
      else poisci_minimum_aux min (x :: preverjeni) xs)
    | [] -> min, preverjeni
  in
  let rec aux urejen neurejen =
    match neurejen with
    | [] -> urejen
    | x :: xs ->
      let min, preverjeni = poisci_minimum_aux x [] xs in (* To bi mogl delat al kaj? Ne da morm celo funkcijo popravljat?*)
      (* sam js rabm minimum ven iz seznama vzet*)
      aux (min :: urejen) preverjeni
  in
  aux [] sez |> List.rev
  (* to je vse neprevejreno, ker mi docker ne dela, pa zgleda precej sumljivo ....*)
    

(*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*]
 Urejanje z Izbiranjem na Tabelah
[*~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~*)

(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Pri delu z tabelami (array) namesto seznami, lahko urejanje z izbiranjem 
 naredimo "na mestu", t.j. brez uporabe vmesnih kopij (delov) vhoda. Kot prej
 tabelo ločujemo na že urejen del in še neurejen del, le da tokrat vse elemente
 hranimo v vhodni tabeli, mejo med deloma pa hranimo v spremenljivki
 [boundary_sorted]. Na vsakem koraku tako ne izvlečemo najmanjšega elementa
 neurejenga dela tabele temveč poiščemo njegov indeks in ga zamenjamo z
 elementom na meji med deloma (in s tem dodamo na konec urejenega dela).
 Postopek končamo, ko meja doseže konec tabele.
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)


(*----------------------------------------------------------------------------*]
 Funkcija [swap a i j] zamenja elementa [a.(i)] and [a.(j)]. Zamenjavo naredi
 na mestu in vrne unit.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let test = [|0; 1; 2; 3; 4|];;
 val test : int array = [|0; 1; 2; 3; 4|]
 # swap test 1 4;;
 - : unit = ()
 # test;;
 - : int array = [|0; 4; 2; 3; 1|]
[*----------------------------------------------------------------------------*)

let swap a i j =
  let x = a.(i) in
  a.(i) <- a.(j); (*let neki = neki in je podpičje, ker vrne unit ...*)
  a.(j) <- x


(*----------------------------------------------------------------------------*]
 Funkcija [index_min a lower upper] poišče indeks najmanjšega elementa tabele
 [a] med indeksoma [lower] and [upper] (oba indeksa sta vključena).
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 index_min [|0; 2; 9; 3; 6|] 2 4 = 3
[*----------------------------------------------------------------------------*)

let index_min a lower upper =
  let rec aux i_min min i =
    match i with
    | i when i > upper -> i_min
    | i -> 
      (if a.(i) < min then aux i a.(i) (i + 1)
      else aux i_min min (i + 1))
  in 
  aux lower a.(lower) lower

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort_array] implementira urejanje z izbiranjem na mestu. 
[*----------------------------------------------------------------------------*)

let selection_sort_array a =
  let n = Array.length a in
  let rec aux boundary_sorted =
    match boundary_sorted with
    | i when i = n -> ()
    | boundary_sorted -> (
      let i_min = index_min a boundary_sorted (n - 1) in
      swap a boundary_sorted i_min;
      aux (boundary_sorted + 1)
    )
  in
  aux 0

let test1 = [|1; 4; 6; 7; 2; 3|]

(*----------------------------------------------------------------------------*]
 Funkcija [min_and_rest list] vrne par [Some (z, list')] tako da je [z]
 najmanjši element v [list] in seznam [list'] enak [list] z odstranjeno prvo
 pojavitvijo elementa [z]. V primeru praznega seznama vrne [None]. 
[*----------------------------------------------------------------------------*)

let min_and_rest list = 
  match list with
  | [] -> None
  | x :: xs -> (
    let rec poisci_minimum_aux min preverjeni sez =
      match sez with
      | x :: xs -> 
        (if x < min then poisci_minimum_aux x (min :: preverjeni) xs
        else poisci_minimum_aux min (x :: preverjeni) xs)
      | [] -> min, preverjeni
    in
    Some (poisci_minimum_aux x [] xs)
  )
(* a nam je vseen, v kerem vrstnem redu je ta seznam vrnjen?*)
    

(*----------------------------------------------------------------------------*]
 Funkcija [selection_sort] je implementacija zgoraj opisanega algoritma.
 Namig: Uporabi [min_and_rest] iz prejšnje naloge.
[*----------------------------------------------------------------------------*)

(* let selection_sort sez =
  match sez with
  | [] -> failwith "Prazen seznam"
  | x :: xs -> (
    let rec aux sez =
      let opt = min_and_rest sez in
      let min, preostali = Option.get (opt) in
      List.rev (min :: (aux preostali))
    in
    aux sez
  )
    *) (* To je vse narobe. Razmisl pa nared, ker more vrnt un seznam
  in ne vem, a znam to brez akumulatorja*)



(*----------------------------------------------------------------------------*]
 Funkcija [randlist len max] generira seznam dolžine [len] z naključnimi
 celimi števili med 0 in [max].
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 # let l = randlist 10 10 ;;
 val l : int list = [0; 1; 0; 4; 0; 9; 1; 2; 5; 4]
[*----------------------------------------------------------------------------*)


(*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*]
 Sedaj lahko s pomočjo [randlist] primerjamo našo urejevalno funkcijo (imenovana
 [our_sort] v spodnjem primeru) z urejevalno funkcijo modula [List]. Prav tako
 lahko na manjšem seznamu preverimo v čem je problem.
 - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
 let test = (randlist 100 100) in (our_sort test = List.sort compare test);;
[*-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=*)