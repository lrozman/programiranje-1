-- Izomorfizmi

theorem eq1 {A B : Prop} : (A ∧ B) ↔ (B ∧ A) :=
  by
    apply Iff.intro -- če mamo eno in drugo implikacijo, lahko dobimo izomorf.
    . intro h-- da si lepš kodo razdelite, postavte piko, ko začnete posamezn primer
      -- konstruirat mormo konjunkcijo
      apply And.intro -- razbil smo si A in B na A in na B, vsacga posebi mormo zdej dokazat
      . exact h.right
      . exact h.left   -- premisli!!!
    . intro h
      apply And.intro
      . exact h.right
      . exact h.left
-- loh bi nardil na dva druga načina, poglej, bo obešeno nekam na repo.
-- to, kar mi zdej delamo, je skor mal brezveze delat s taktikami. In pol k vidš strukturo, maš un prvi drug primer, pol je pa še brez taktik.

theorem eq2 {A B : Prop} : (A ∨ B) ↔ (B ∨ A) :=
  by
    apply Iff.intro
    . intro h
      cases h with -- k je tak k match, tak "luštn", loh ne bi s takim
      | inl ha => -- leva inkluzija al neki? ha je sam ime
        apply Or.inr -- isto, k da bi konstruktor naredl
        assumption
      | inr hb =>
        apply Or.inl
        assumption
    . intro h
      cases h  -- ta drug cases
      . apply Or.inr
        assumption
      . apply Or.inl -- apply ?!?!?
        assumption
      -- prej sm vedla, da je pršlo iz enga konstruktorja, kle pa ne vem.

theorem eq3 {A B C : Prop} : (A ∧ (B ∧ C)) ↔ (B ∧ (A ∧ C)) :=
  by
    apply Iff.intro
    . intro h
      constructor
      -- . (h.right).left ????
      .
    .

theorem eq4 {A B C : Prop} : (A ∨ (B ∨ C)) ↔ (B ∨ (A ∨ C)) :=
 sorry

theorem eq5 {A B C : Prop} : A ∧ (B ∨ C) ↔ (A ∧ B) ∨ (A ∧ C) :=
  sorry

theorem eq6 {A B C : Prop} : (B ∨ C) → A ↔ (B → A) ∧ (C → A) :=
  by
    apply Iff.intro
    . intro h
      -- apply And.intro -- konstruktor za konjukcijo. Un k je ociten iz cilja, lahko recete kr constructor
      constructor -- najbolj zunanji konstruktor na cilju bo uporabu (verjetn?)
      . intro hb
        apply h
        left
        assumption -- loh bi tud exact hb, ampak to je bolj splošno: "maš v predpostavkah točno to, kar rabš"
      . intro hc
        apply h
        right
        assumption
    . intro h hbc -- če mamo zaporedne implikacije, jih lahko hkrati pobiramo ven
      -- ne vemo, kerga mamo, b al c. Bomo ločl po tem
      cases hbc
      . apply h.left-- do konjunkcije lahko kr dostopamo z h.left, h.right
        assumption -- razumi apply!!!
      . apply h.right
        assumption
        -- "ta težk del je napisat tapravo definicijo, ostalo pol za dokazovanje sam slediš nosu."
        -- "K formaliziraš analizo, se morš odločt, a boš vzel dedekindova al cauchyjeva realna stevila, k maš pol različna orodja ..."


theorem eq7 {A B C : Prop} : C → (A ∧ B) ↔ (C → A) ∧ (C → B) :=
  sorry
