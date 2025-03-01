let string_to_char_list str = 
  let sez = ref [] in
  let f char = 
    sez := char :: !sez;
  in
  String.iter f str;
  List.rev !sez


  let for_state stanje1 sez machine =
    let ts = ref machine in
    let rec list_aux = function
      | [] -> ()
      | l :: ls -> (
        let l1 = ref l in
        let quit_loop = ref false in
        while not !quit_loop do (* Zakaj sem sploh nardila while zanko kle? Zakaj nisem sam razstavljala seznama? *)
          match !l1 with
          | [] -> quit_loop := true
          | nabor :: nabori -> ( (* Tko a karkol do zdej sploh rabm? *)
            let char1, stanje2_opt, char2, dir = nabor in
            let stanje2 = ref "" in (* Pa kle bi loh sam matchala pa pač dvarkrat napisala .add, zato pomoje nisem hotla? *)
            if Option.is_some stanje2_opt then stanje2 := Option.get stanje2_opt 
            else stanje2 := stanje1;
            (* ts := Machine.add_transition stanje1 char1 !stanje2 char2 dir (!ts); *)
            l1 := nabori (* Lol, pomoje tega absolutno ne rabm. Al kaj ?*)
          )
        done;
        list_aux ls (* Al sm to nardila, da loh iste stvari vračam? Ker je uno unit pač. Sam sej list_aux vrača unit ?? 
        Ne vem točno, kaj sem tuki pacala.
        Aja a bi pol rabla ubistvu še eno pomožno rekurzivno funkcijo, da vsak ta element, ki je seznam, pogleda. *)
        )
    in
    list_aux sez;
    !ts

    let for_character char st_ch_dir_nabor =
      let stanje2_opt, char_opt, dir = st_ch_dir_nabor in
      if Option.is_none char_opt then [(char, stanje2_opt, char, dir)]
      else [(char, stanje2_opt, Option.get char_opt, dir)]
      (* A bi blo kle povsod sam lepš matchat? Men se mal zdi da ja ...*)
  
    (* Se mi zdi, da mam pa tok referenc kle, ker pač ne morš z ifi in tem pol narest novga objekta.*)

      let for_characters niz st_ch_dir_nabor =
        let stanje2_opt, char_opt, dir = st_ch_dir_nabor in
        let char_some = ref false in
        if Option.is_some char_opt then char_some := true else char_some := false; (* Da ne preverjamo vsakič znova. *)
        let dolzina = String.length niz in
        let rec aux acc indeks = 
          match indeks with (* Kle bi verjetno loh String.iter nardila al kaj *)
          | i when i = dolzina -> acc
          | i -> 
            if !char_some then aux ((niz.[i], stanje2_opt, Option.get char_opt, dir) :: acc) (i+1)
            else aux ((niz.[i], stanje2_opt, niz.[i], dir) :: acc) (i+1)
        in
        aux [] 0 
        (* Mah, sej bi prežvel, če bi sam skos preverjal, who cares, lepš bi zgledal, k bi bli matchi, ker teli if else so grozni.
          Mogoče. Zdej pišem te komentarje, kt da vem, o čem govorim. Pojma nimam. 
        Pa a bi kle loh sam iterirala čez seznam?*)