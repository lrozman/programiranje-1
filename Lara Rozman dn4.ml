(*----------------------------------------------------------------------------*
 # 4. domača naloga
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste napisali svoj simulator Turingovih strojev. Zaradi
 preprostosti bomo za abecedo vzeli kar znake tipa `char`, za prazni znak bomo
 izbrali presledek `' '`, stanja pa bomo predstavili z nizi. Za možne premike
 zafiksiramo tip `direction`:
[*----------------------------------------------------------------------------*)

type direction = Left | Right
type state = string

(*----------------------------------------------------------------------------*
 ## Implementacija trakov
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Tape`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip v obe smeri neomejenih trakov in glavo na danem mestu;
 - `make`, ki naredi nov trak z znaki iz niza ter glavo na prvem znaku;
 - `print`, ki izpiše vsebino traku (brez presledkov na začetku in koncu) ter
 pod njim z `^` označi mesto glave;
 - `read`, ki vrne znak pod glavo;
 - `write`, ki pod glavo zapiše dani znak;
 - `move`, ki glavo premakne v dano smer.

 Zadnji dve funkciji naj vrneta nov trak, obstoječega pa naj pustita
 nespremenjenega.

 Ker je tip `t` abstrakten, si lahko privoščite poljubno implementacijo, zato
 poskrbite tako za učinkovitost kot za preglednost kode.
[*----------------------------------------------------------------------------*)

 module type TAPE = sig
  type t

  val make : string -> t
  val print : t -> unit
  val read : t -> char
  val move : direction -> t -> t
  val write : char -> t -> t
end

module Tape : TAPE = struct
  type t = Trak of char list * char * char list

  (*let char_list_to_string cl = String.concat "" (List.map (String.make 1) cl)*)
  (*Če bom še rabla to funkcijo, sem dala ven*)
  (* Pozab, to ma neko biggest loser slovenia casovno zahtevnost pomoje.*)
  let string_to_char_list niz = List.init (String.length niz) (String.get niz)
  (*Če bom rabla, dam ven iz make, pa mam kle*)
  (* A raj to nardim na roke, da je zihr O(n), sam sej te implementirane menda niso glupe?*)

  let make niz =
    let char_list = List.init (String.length niz) (String.get niz) in
    match char_list with
    | [] -> Trak ([], ' ', [])
    | x :: xs -> Trak ([], x, xs)

  let print = function
    | Trak (levi, glava, desni) -> (
      let obrni_ltrim_prestej_levi levi_char_list = 
        let rec aux_ltrim_in_prestej i flag acc sez =
          match flag, sez with
          | _, [] -> i, List.rev acc
          | false, x :: xs when x = ' ' -> aux_ltrim_in_prestej 0 false [] xs
          | false, x :: xs -> aux_ltrim_in_prestej (i + 1) true (x :: acc) xs
          | true, x :: xs -> aux_ltrim_in_prestej (i + 1) true (x :: acc) xs
        in
        List.rev levi_char_list 
        |> aux_ltrim_in_prestej 0 false []
      in 
      (* Vrne levi seznam pripravljen za print in njegovo dolžino. *)
      let rec print_char_list = function 
        | [] -> ()
        | c :: cs -> Printf.printf "%c" c ; print_char_list cs
      in
      let dolzina, levi_za_print = obrni_ltrim_prestej_levi levi in
      print_char_list levi_za_print; 
      (* Sprintan je levi del traku. *)
      Printf.printf "%c" glava;
      (* Sprintana je glava. *)
      let print_desni desni_char_list =
        let rec aux_printd_in_iskanje_konca stevec_presledkov sez = (* Sej v pomožnem so samo presledki!*)
          match sez with
          | [] -> ()
          | x :: xs when x = ' ' -> aux_printd_in_iskanje_konca (stevec_presledkov + 1) xs (* Če je presledek, prešteje, kok jih mora natisnt. *)
          (* To ma bs časovno zahtevnost, naredi raje seznam, al kaj. *)
          | x :: xs -> (
            for i=1 to stevec_presledkov do
              Printf.printf "%c" ' '
            done;
            Printf.printf "%c" x ; 
            aux_printd_in_iskanje_konca 0 xs (*Torej znak, če smo vmes shranjeval presledke, jih zdele še naprintamo*)
          )
        in
        aux_printd_in_iskanje_konca 0 desni_char_list
      in
      print_desni desni;
      (* Sprintan je desni del traku. *)
      Printf.printf "%s" "\n";
      for i=1 to dolzina do
        Printf.printf "%c" ' '
      done;
      Printf.printf "%s" "^\n"
      (* Sprintana je druga vrsta. *)
    )

  let read = function
    | Trak (_, glava, _) -> glava
  let move dir trak = 
    match dir, trak with
    | Left, Trak (levi, glava, desni) -> (
      match levi with
      | [] -> Trak ([], ' ', glava :: desni)
      | x :: xs -> Trak (xs, x, glava :: desni)
    )
    | Right, Trak (levi, glava, desni) -> (
      match desni with
      | [] -> Trak (glava :: levi, ' ', [])
      | x :: xs -> Trak (glava :: levi, x, xs)
    )
  let write c = function
    | Trak (levi, glava, desni) -> Trak (levi, c, desni)
end

let primer_trak = Tape.(
  make "ABCDE"
  |> move Left
  |> move Left
  |> move Right
  |> move Right
  |> move Right
  |> move Right
  |> write '!'
  |> print
)
(*
AB!DE
  ^
*)
(* val primer_trak : unit = () *)

(*----------------------------------------------------------------------------*
 ## Implementacija Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Napišite modul `Machine`, ki implementira spodnjo signaturo, kjer je:

 - `t` tip Turingovih strojev;
 - `make`, ki naredi nov stroj z danim začetnim stanjem in seznamom preostalih
 stanj ter prazno prehodno funkcijo;
 - `initial`, ki vrne začetno stanje stroja;
 - `add_transition`, ki prehodno funkcijo razširi s prehodom $(q, a) \mapsto
 (q', a', d)$;
 - `step`, ki za dano stanje in trak izvede en korak stroja, če je to mogoče.

 Zadnji dve funkciji naj vrneta spremenjene vrednosti, obstoječe argumente pa
 naj pustita nespremenjene. Prav tako pri zadnjih dveh funkcijah lahko
 predpostavite, da ju bomo klicali le na poprej podanih stanjih.

 Tudi tu je tip `t` abstrakten, zato poskrbite za učinkovitost in preglednost
 kode.
[*----------------------------------------------------------------------------*)

module type MACHINE = sig
  type t
  val make : state -> state list -> t
  val initial : t -> state
  val add_transition : state -> char -> state -> char -> direction -> t -> t
  val step : t -> state -> Tape.t -> (state * Tape.t) option
end

module Machine : MACHINE = struct
  type t = unit
  let make _ _ = ()
  let initial _ = ""
  let add_transition _ _ _ _ _ _ = ()
  let step _ _ _ = None
end

(*----------------------------------------------------------------------------*
 Primer stroja "Binary Increment" na <http://turingmachine.io> lahko
 implementiramo kot:
[*----------------------------------------------------------------------------*)

let binary_increment =
  Machine.(
    make "right" [ "carry"; "done" ]
    |> add_transition "right" '1' "right" '1' Right
    |> add_transition "right" '0' "right" '0' Right
    |> add_transition "right" ' ' "carry" ' ' Left
    |> add_transition "carry" '1' "carry" '0' Left
    |> add_transition "carry" '0' "done" '1' Left
    |> add_transition "carry" ' ' "done" '1' Left
  )

(* val binary_increment : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 Zapišite funkciji `slow_run` in `speed_run` tipa `Machine.t -> str -> unit`, ki
 simulirata Turingov stroj na traku, na katerem je na začetku zapisan dani niz.
 Prva naj izpiše trakove in stanja pri vseh vmesnih korakih, druga pa naj izpiše
 le končni trak. Slednjo bomo uporabljali tudi pri meritvi učinkovitosti
 izvajanja.
[*----------------------------------------------------------------------------*)

let slow_run _ _ = ()

let primer_slow_run =
  slow_run binary_increment "1011"
(*
1011
^
right
1011
  ^
right
1011
  ^
right
1011
    ^
right
1011
    ^
right
1011
    ^
carry
1010
  ^
carry
1000
  ^
carry
1100
^
done
*)
(* val primer_slow_run : unit = () *)

let speed_run _ _ = ()

let primer_speed_run =
  speed_run binary_increment "1011"
(*
1100
^
*)
(* val primer_speed_run : unit = () *)

(*----------------------------------------------------------------------------*
 ## Krajši zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Ko definiramo Turingov stroj, prehode običajno združujemo najprej po stanjih,
 nato pa še po znakih. Prav tako pri dosti prehodih samo premikamo glavo, trak
 in stanje pa pustimo pri miru. Zapišite funkcije:

 - `for_state`
 - `for_character`
 - `for_characters`
 - `move`
 - `switch_and_move`
 - `write_and_move`
 - `write_switch_and_move`

 s katerimi bi lahko zgornji primer na krajše zapisali kot spodaj.
 Implementacijo in tipe ugotovite sami.
[*----------------------------------------------------------------------------*)

(* let binary_increment' =
  Machine.make "right" ["carry"; "done"]
  |> for_state "right" [
    for_characters "01" @@ move Right;
    for_character ' ' @@ switch_and_move "carry" Left
  ]
  |> for_state "carry" [
    for_character '1' @@ write_and_move '0' Left;
    for_characters "0 " @@ write_switch_and_move '1' "done" Left
  ]   *)
(* val binary_increment' : Machine.t = <abstr> *)

(*----------------------------------------------------------------------------*
 ## Primeri Turingovih strojev
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Pri tej nalogi boste sestavljali stroje, ki bodo iz začetnega niza na traku na
 različne načine izračunali nov niz. Pri tem lahko predpostavite, da je začetni
 niz sestavljen iz ničel in enic, preostanek traku pa je prazen. Na koncu
 izvajanja naj bo glava na začetku novega niza, z izjemo tega niza pa naj bo
 trak prazen. Ni pa treba, da se izračunani niz začne na istem mestu na traku,
 kot se je začel prvotni niz.
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 ### Obračanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki začetni niz obrne na glavo.
[*----------------------------------------------------------------------------*)

let reverse = ()

let primer_reverse = speed_run reverse "0000111001"
(* 
1001110000          
^
*)
(* val primer_reverse : unit = () *)

(*----------------------------------------------------------------------------*
 ### Podvajanje niza
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki podvoji začetni niz.
[*----------------------------------------------------------------------------*)

let duplicate = ()

let primer_duplicate = speed_run duplicate "010011"
(* 
001100001111       
^
*)
(* val primer_duplicate : unit = () *)

(*----------------------------------------------------------------------------*
 ### Eniški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite Turingov stroj, ki na začetku na traku sprejme število $n$, zapisano
 v dvojiškem zapisu, na koncu pa naj bo na traku zapisanih natanko $n$ enic.
[*----------------------------------------------------------------------------*)

let to_unary = ()

let primer_to_unary = speed_run to_unary "1010"
(* 
1111111111
^
*)
(* val primer_to_unary : unit = () *)

(*----------------------------------------------------------------------------*
 ### Dvojiški zapis
[*----------------------------------------------------------------------------*)

(*----------------------------------------------------------------------------*
 Sestavite ravno obratni Turingov stroj, torej tak, ki na začetku na traku
 sprejme število $n$ enic, na koncu pa naj bo na traku zapisano število $n$ v
 dvojiškem zapisu.
[*----------------------------------------------------------------------------*)

let to_binary = ()

let primer_to_binary = speed_run to_binary (String.make 42 '1')
(* 
101010                                           
^
*)
(* val primer_to_binary : unit = () *)
