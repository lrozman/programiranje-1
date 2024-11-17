(*----------------------------------------------------------------------------*
 # Abstrakcija
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ## Naravna števila
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte signaturo `NAT`, ki določa strukturo naravnih števil. Ima osnovni
 tip, funkcijo enakosti, ničlo in enko, seštevanje, odštevanje in množenje.
 Hkrati naj vsebuje pretvorbe iz in v OCamlov `int` tip. Opomba: Funkcije za
 pretvarjanje ponavadi poimenujemo `to_int` and `of_int`, tako da skupaj z
 imenom modula dobimo ime `NAT.of_int`, ki nam pove, da pridobivamo naravno
 število iz celega števila.
[*----------------------------------------------------------------------------*)

module type NAT = sig
  type t

  val eq  : t -> t -> bool
  val zero : t
  val enka : t
  val sestevanje : t -> t -> t
  (* val odstevanje : t -> t -> t option - to bi blo lepš, ampak bo pol teži,
  tkoda bomo vračal 0, če je negativno, al neki *)
  val odstevanje : t -> t -> t
  val mnozenje : t -> t -> t
  val to_int : t -> int
  val of_int : int -> t
end

(*----------------------------------------------------------------------------*
 Napišite implementacijo modula `Nat_int`, ki zgradi modul s signaturo `NAT`,
 kjer kot osnovni tip uporablja OCamlov tip `int`. Namig: dokler ne
 implementirate vse funkcij v `Nat_int`, se bo OCaml pritoževal. Temu se lahko
 izognete tako, da funkcije, ki še niso napisane nadomestite z `failwith
 "later"`, vendar to ne deluje za konstante.
[*----------------------------------------------------------------------------*)

module Nat_int : NAT = struct

  type t = int
  let eq x y = (x = y)
  let zero = 0
  let enka = 1
  let sestevanje x y = x + y
  (* let odstevanje x y = 
    if x - y >= 0 then Some (x - y)
    else None *)
  let odstevanje x y = 
    if x - y >= 0 then x - y
    else 0
  let mnozenje x y = x * y
  let to_int x = x
  let of_int x = 
    if x >= 0 then x
    else 0

end

(*----------------------------------------------------------------------------*
 Napišite implementacijo `NAT`, ki temelji na [Peanovih
 aksiomih](https://en.wikipedia.org/wiki/Peano_axioms). Osnovni tip modula
 definirajte kot naštevni tip, ki vsebuje konstruktor za ničlo in konstruktor za
 naslednika nekega naravnega števila. Večino funkcij lahko implementirate s
 pomočjo rekurzije. Naprimer, enakost števil `k` in `l` določimo s hkratno
 rekurzijo na `k` in `l`, kjer je osnoven primer `Zero = Zero`.
[*----------------------------------------------------------------------------*)

module Nat_peano : NAT = struct

  type t = Nic | Naslednik of t
  let rec eq x y = (* delal bi tud sam let eq x y = x = y*)
    match x, y with
    | Nic, Nic -> true
    | Nic, _ -> false
    | _, Nic -> false
    | Naslednik x', Naslednik y' -> eq x' y'
  let zero = Nic
  let enka = Naslednik Nic
  let rec sestevanje x y =
    match x with
    | Nic -> y
    | Naslednik x' -> Naslednik (sestevanje x' y)
  (* let rec odstevanje x y =
    match x, y with
    | x, Nic -> Some x
    | Nic, Naslednik y' -> None
    | Naslednik x', Naslednik y' -> odstevanje x' y' *)
  let rec odstevanje x y =
    match x, y with
    | x, Nic -> x
    | Nic, Naslednik y' -> Nic
    | Naslednik x', Naslednik y' -> odstevanje x' y'
  let mnozenje x y =
    let rec aux acc = function
    | Nic -> acc (* Nat_peano.enka ? al samo enka? POGLEJ DOMA, kjer ti dela ocaml -> acc + y *)
    | Naslednik x' -> aux (sestevanje acc y) x'
    in
    aux Nic y
  let to_int x = 
    let rec aux acc =
      function
      | Nic -> acc
      | Naslednik x' -> aux (acc + 1) x'
    in
    aux 0 x
  let of_int x =
    let rec aux acc = function
      | a when a <= 0 -> acc
      | a -> aux (Naslednik acc) (a - 1)
    in
    aux Nic x

end

(*----------------------------------------------------------------------------*
 Z ukazom `let module ImeModula = ... in ...` lahko modul definiramo samo
 lokalno. To bomo uporabili za to, da bomo lahko enostavno preklapljali med
 moduloma `Nat_int` in `Nat_peano`, saj bomo enega ali drugega shranili pod ime
 `Nat`. OCaml sicer pozna tudi ustrezne abstrakcije, ki omogočijo preklapljanje
 med moduli, na primer [funktorje](https://ocaml.org/docs/functors) ali
 [prvorazredne module](https://ocaml.org/manual/5.2/firstclassmodules.html), a
 bomo uporabili preprostejšo rešitev.

 Spodnji izračun dopolnite tako, da sešteje prvih 100 naravnih števil. Ker bo
 taka vsota tipa `NAT.t`, ki je abstrakten, končni rezultat pretvorite v tip
 `int` z uporabo funkcije `Nat.to_int`. Če ste oba modula implementirali
 pravilno, bi morali dobiti enak rezultat ne glede na to, katerega poimenujete
 `Nat`.
[*----------------------------------------------------------------------------*)

let sum_nat_100 = 
  (* let module Nat = Nat_int in *)
  let module Nat = Nat_peano in
  let rec aux acc n = 
    if (Nat.eq n Nat.zero) then acc
    else aux (Nat.sestevanje acc n) (Nat.odstevanje n Nat.enka)
    (* | Nat.zero -> acc - Ni fiksna vrednost in ni konstruktor, ne morte delat matcha. Sam if pač
    | Nat.t -> aux (Nat.sestevanje acc Nat.t) (Nat.odstevanje Nat.t Nat.enka) *)
  in
  aux Nat.zero (Nat.of_int 100)  
  |> Nat.to_int
(* val sum_nat_100 : int = 5050 *)

(* Kako se to uporablja? *)
(* Lahko bi šli n*(n-1)/2, če si definiram deljenje z 2, 
lahko greš rekurzivno od 100 navzdol al od 0 navzgor prištevamo ...
uporabljaš pa sam stvari iz modula *)

(* Da se rename symbol, desn klik pa neki, btw 
ctr shift l je pa uno k Pretnar uporabla, k vse iste spremenljivke označ in loh pretipka*)
  

(*----------------------------------------------------------------------------*
 ## Kompleksna števila
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 > Once upon a time, there was a university with a peculiar tenure
 > policy. All faculty were tenured, and could only be dismissed for
 > moral turpitude. What was peculiar was the definition of moral
 > turpitude: making a false statement in class. Needless to say, the
 > university did not teach computer science. However, it had a renowned
 > department of mathematics.
 >
 > One Semester, there was such a large enrollment in complex variables
 > that two sections were scheduled. In one section, Professor Descartes
 > announced that a complex number was an ordered pair of reals, and that
 > two complex numbers were equal when their corresponding components
 > were equal. He went on to explain how to convert reals into complex
 > numbers, what "i" was, how to add, multiply, and conjugate complex
 > numbers, and how to find their magnitude.
 >
 > In the other section, Professor Bessel announced that a complex number
 > was an ordered pair of reals the first of which was nonnegative, and
 > that two complex numbers were equal if their first components were
 > equal and either the first components were zero or the second
 > components differed by a multiple of 2π. He then told an entirely
 > different story about converting reals, "i", addition, multiplication,
 > conjugation, and magnitude.
 >
 > Then, after their first classes, an unfortunate mistake in the
 > registrar's office caused the two sections to be interchanged. Despite
 > this, neither Descartes nor Bessel ever committed moral turpitude,
 > even though each was judged by the other's definitions. The reason was
 > that they both had an intuitive understanding of type. Having defined
 > complex numbers and the primitive operations upon them, thereafter
 > they spoke at a level of abstraction that encompassed both of their
 > definitions.
 >
 > The moral of this fable is that: Type structure is a syntactic
 > discipline for enforcing levels of abstraction.
 >
 > John C. Reynolds, _Types, Abstraction, and Parametric Polymorphism_, IFIP83
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Definirajte signaturo modula kompleksnih števil. Potrebujemo osnovni tip, test
 enakosti, ničlo, enko, imaginarno konstanto i, negacijo, konjugacijo,
 seštevanje in množenje.
[*----------------------------------------------------------------------------*)

module type COMPLEX = sig
  type t
  val eq : t -> t -> bool
  val zero : t
  val one : t
  val i : t
  val negacija : t -> t
  val konjugacija : t -> t
  val sestevanje : t -> t -> t
  val mnozenje : t -> t -> t
  (* Dodajte manjkajoče! *)
end

(*----------------------------------------------------------------------------*
 Napišite kartezično implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število realno in imaginarno komponento.
[*----------------------------------------------------------------------------*)

module Cartesian : COMPLEX = struct

  type t = {re : float; im : float}

  let eq x y = x.re = y.re && x.im = y.im
  let zero = {re = 0.; im = 0.}
  let one = {re = 1.; im = 0.}
  let i = {re = 0.; im = 1.}
  let negacija z = { re = -. z.re; im = -. z.im }
  let konjugacija z = {z with im = -. z.im}
  let sestevanje z w = { re = z.re +. w.re; im = z.im +. w.im }
  let mnozenje z w = 
    let realna = z.re *. w.re -. z.im *. w.im in
    let imaginarna = z.re *. w.im +. z.im *. w.re in
    {re = realna; im = imaginarna}
  (* Dodajte manjkajoče! *)

end

(*----------------------------------------------------------------------------*
 Sedaj napišite še polarno implementacijo kompleksnih števil, kjer ima vsako
 kompleksno število radij in kot (angl. magnitude in argument). Priporočilo:
 Seštevanje je v polarnih koordinatah zahtevnejše, zato si ga pustite za konec
 (lahko tudi za konec stoletja).
[*----------------------------------------------------------------------------*)

module Polar : COMPLEX = struct

  type t = {magn : float; arg : float}

  (* Pomožne funkcije za lažje življenje. *)
  let pi = 2. *. acos 0.
  let rad_of_deg deg = (deg /. 180.) *. pi
  let deg_of_rad rad = (rad /. pi) *. 180.

  (* let eq x y = 
    (x.magn = y.magn) && (Float.rem (x.arg -. y.arg) (2.*.pi) = 0.) (* Razlika kotov mora bit deljiva z 2pi*) *)
  
  let zero = {magn = 0.; arg = 0.} (* "mora" bit fiksna stvar. *)

  (* funkcija, ki standardizira kot, bo uporabna (se odšteva 2pi, dokler si nad 2pi in odšteva, dokler <0)*)
  let rec stand_kot z =
    match (z.arg) with
    | phi when phi < 0. -> stand_kot {z with arg = (z.arg +. 2. *. pi)}
    | phi when phi >= (2.*.pi) -> stand_kot {z with arg = (z.arg -. 2. *. pi)} 
    | _ -> z
  
  let eq x y =
    let x = stand_kot x in
    let y = stand_kot y in
    (x.magn = y.magn) && (x.arg = y.arg)
  
  let one = {magn = 1.; arg = 0.}
  let i = {magn = 1.; arg = 0.5 *. pi}
  
  let negacija z = stand_kot {z with arg = z.arg +. pi}
  let konjugacija z = stand_kot {z with arg = 2.*.pi -. z.arg}

  (*let sestevanje z w = zero *)
  (* magn bo diagonala paralelograma s kotom en - drug, pa stranicama magn od obeh
   arg bo pa en-drug/(neko razmerje magnitud) ? A je to sploh res?
   Al sam projiciram, pa sestejem normalno pa odprojiciram al je to bedna rešitev?
   sam je pa vsaj rešitev, tkoda grem to narest pa rešitve pogledat *)
  let sestevanje z w = 
    let zx, zy = z.magn *. cos z.arg, z.magn *. sin z.arg in (* kera python fora, kera sm ej *)
    let wx, wy = w.magn *. cos w.arg, w.magn *. sin w.arg in
    let vsota = (zx +. wx, zy +. wy) in
    (* nazaj to dat bo pain in the neck, ker bo treba matchat, v kerem kvadrantu je blo ...*)
    (* a znam iz kartezičnih dobit polarne? Bi mogla znat, ne? *)
    let kart_v_pol (x, y) =
      let r = (x ** 2. +. y ** 2.)**(0.5) in
      let kot (x, y) =
        match x, y with
        | 0., 0. -> 0. 
        | 0., y -> (y /. (abs_float y)) *. (pi /. 2.) (*ne morem delit pol*)
        | x, y when x > 0. -> atan (y /. x) 
        | x, y -> pi +. (atan (y /. x)) (* x je že <= 0 *) 
        (* a je to prou? Pač recmo *)
      in
      { magn = r; arg = kot (x, y)} |> stand_kot
    in
  kart_v_pol vsota


  let mnozenje z w =
    { magn = z.magn *. w.magn ; arg = z.arg +. w.arg } 
    |> stand_kot

end