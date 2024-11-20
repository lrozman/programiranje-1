theorem fu {A B C : Prop} : ( A ∧ B → C ) → ( A → (B → C)) :=
  by
    intros h ha hb -- gre po vseh implikacijah, dokler gre, če damo zdej še en intro, bo tactic fail.
    -- če bi dal samo dve imeni, bi samo dve pobral.
    -- Če bi dal še hc, bi probu C sprement v nekj funkcijski tip, on skos tipe čekira, tkoda bi se prtožu, ker ne more
    -- huskell ma kind, k je ena stvar nad tipom. Term : Type,  tipom pa priredite Kind (tipi tipov)
--    intro h -- predpostavl smo prvi del implikacije, "dodamo predpostavko"
--    intro ha -- predspostavljamo prvi del implikacije A -> (B -> C), hypothesis A
--    intro hb -- Ker se to pogosto dogaja, damo lahko raj intros
    apply h -- zdej dokazujemo A in B ubistvu, ker iz tega bo sledil C, kar pa trenutno hočemo dokazat
    -- apply And.intro -- konstruktor za in And, ker je za logični ali, introduction se pa temu reče, ker? in to je spet funkcija, zato apply
    -- zadevo je razbil na case-e: case left in case right, in pač za enga rabš A za enga B
    constructor -- glej spodaj
    exact ha -- točn ha je tega tipa
    -- loh bi dal pa tud apply ha, k bi spet isto reku: maš neki, k se konča z neki tipa A, a rab to kkšne predpostavke? Ne, super.
    -- Exact je samo apply za neki, kar ne rab predpostavk
    exact hb
    -- ful stvari se konstruira s konstruktorji, zato ma Lean ime za to.: taktika constructor
    -- constructor je samo okrajšava, da on pogleda, iz česa je sestavljena ta stvar, k jo hočem dokazat
    -- in apply-a constructor za to stvar al neki ?
    -- k mam js dokaz propositiona, je čist vseen, kako sm to dokazu: proof irrelevance

#print fu
