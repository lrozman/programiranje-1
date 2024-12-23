set_option autoImplicit false

/------------------------------------------------------------------------------
 ## Naravna števila

 Definirajte funkcijo, ki _rekurzivno_ (torej naivno in ne direktno s formulo,
 ki jo boste morali dokazati) sešteje prvih `n` naravnih števil, ter
 dokažite, da zanjo velja znana enakost (najprej v obliki, ki ne zahteva
 deljenja, nato pa še v običajni obliki).
------------------------------------------------------------------------------/

def vsota_prvih : Nat → Nat :=
  fun n =>
    match n with
    | Nat.zero => Nat.zero
    | Nat.succ m => Nat.succ m + vsota_prvih m

theorem gauss : (n : Nat) → 2 * vsota_prvih n = n * (n + 1) :=
  by
    intro n
    induction n with
    | zero => simp [vsota_prvih]
    | succ m ih =>
      simp [vsota_prvih]
      rw [Nat.mul_add, ih]
      calc 
        2 * (m + 1) + m * (m + 1)
        _ = (m + 1) * 2 + m * (m + 1) := by rw [Nat.mul_comm]
        _ = m * (m + 1) + (m + 1) * 2 := by rw [Nat.add_comm]
        _ = (m + 1) * m + (m + 1) * 2 := by rw [Nat.mul_comm]
        _ = (m + 1) * (m + 2) := by rw [← Nat.mul_add]
        _ = (m + 1) * (m + 1 + 1) := by rfl


theorem cisto_pravi_gauss : (n : Nat) → vsota_prvih n = (n * (n + 1)) / 2 := 
  by
    intro n
    induction n with
    | zero => simp [vsota_prvih]
    | succ m ih =>
      simp [vsota_prvih]
      rw [ih]
      calc
        m + 1 + m * (m + 1) / 2
        _ = (m + 1) + m * (m + 1) / 2 := by rfl
        _ = (2 * (m + 1) + m * (m + 1)) / 2 := by 
          rw [← Nat.mul_add_div]
          apply Nat.two_pos
        _ = ((m + 1) * 2 + m * (m + 1)) / 2 := by rw [Nat.mul_comm]
        _ = (m * (m + 1) + (m + 1) * 2) / 2 := by rw [Nat.add_comm]
        _ = ((m + 1) * m + (m + 1) * 2) / 2 := by rw [Nat.mul_comm]
        _ = ((m + 1) * (m + 2)) / 2 := by rw [Nat.mul_add]
        _ = ((m + 1) * (m + 1 + 1)) / 2 := by rfl


/------------------------------------------------------------------------------
 ## Vektorji

 Definirajmo vektorje podobno kot na predavanjih, le da namesto svojih naravnih
 števil uporabimo vgrajena. Da se tipi ujamejo, funkcijo stikanja napišemo s
 pomočjo taktik.

 Napišite funkcijo `obrni`, ki vrne na glavo obrnjen vektor, ter funkciji
 `glava` in `rep`, ki varno vrneta glavo in rep _nepraznega_ seznama.
------------------------------------------------------------------------------/

inductive Vektor : Type → Nat → Type where
  | prazen : {A : Type} → Vektor A 0
  | sestavljen : {A : Type} → {n : Nat} → A → Vektor A n → Vektor A (n + 1)
deriving Repr

def stakni : {A : Type} → {m n : Nat} → Vektor A m → Vektor A n → Vektor A (m + n) :=
  fun xs ys => match xs with
  | .prazen => by rw [Nat.add_comm]; exact ys
  | .sestavljen x xs' => by rw [Nat.add_right_comm]; exact Vektor.sestavljen x (stakni xs' ys)


def obrni : {A : Type} → {n : Nat} → Vektor A n → Vektor A n :=
  fun xs => match xs with
  | .prazen => .prazen
  | .sestavljen x xs' => stakni (obrni xs') (Vektor.sestavljen x Vektor.prazen)

def glava : {A : Type} → {n : Nat} → Vektor A (Nat.succ n) → A :=
  fun xs =>
    match xs with
    | .sestavljen x _ => x

def rep : {A : Type} → {n : Nat} → Vektor A (Nat.succ n) → Vektor A n :=
  fun xs =>
    match xs with
    | .sestavljen _ xs' => xs'

/------------------------------------------------------------------------------
 ## Predikatni račun

 Dokažite spodnje tri trditve. Zadnja je _paradoks pivca_, ki pravi:
   "V vsaki neprazni gostilni obstaja gost, za katerega velja,
   da če pije on, pijejo vsi v gostilni."
 Za dokaz potrebujete klasično logiko, torej nekaj iz modula `Classical`.
------------------------------------------------------------------------------/

theorem forall_implies : {A : Type} → {P Q : A → Prop} →
  (∀ x, (P x → Q x)) → (∀ x, P x) → (∀ x, Q x) := by
  intros A P Q PimQ forallP
  intro x
  apply PimQ
  apply forallP


theorem forall_implies' : {A : Type} → {P : Prop} → {Q : A → Prop} →
  (∀ x, (P → Q x)) ↔ (P → ∀ x, Q x) := by
  intros A P Q
  apply Iff.intro
  . intros PimQ hP x
    apply PimQ
    exact hP 
  . intros PimQall x hP
    apply PimQall
    exact hP


theorem paradoks_pivca :
  {G : Type} → {P : G → Prop} →
  (g : G) →  -- (g : G) pove, da je v gostilni vsaj en gost
  ∃ (p : G), (P p → ∀ (x : G), P x) := by
  intros G P g
  rw [← Classical.not_forall_not]
  intro za_vse_nevelja
  apply za_vse_nevelja g
  intro Pg
  intro p
  have not_imp_p : ¬(P p → ∀ (x : G), P x) := by apply za_vse_nevelja
  have Pp_and_y : P p ∧ ¬ ∀ (x : G), P x := by
    have prehodna_ekvivalenca : ¬(P p → ∀ (x : G), P x) ↔ P p ∧ ¬ ∀ (x : G), P x := by apply Classical.not_imp
    cases prehodna_ekvivalenca with
    | intro lema =>
      apply lema
      exact not_imp_p
  cases Pp_and_y with
  | intro Pp =>
    exact Pp


/------------------------------------------------------------------------------
 ## Dvojiška drevesa

 Podan naj bo tip dvojiških dreves skupaj s funkcijama za zrcaljenje in izračun
 višine ter dvema funkcijama, ki obe od leve proti desni naštejeta elemente
 drevesa. Pri tem prva deluje naivno in ima časovno zahtevnost O(n log n), druga
 pa je malo bolj zapletena in deluje v času O(n). Dokažite spodnje enakosti, pri
 čemer lahko do pomožne funkcije `aux` dostopate kot `elementi'.aux`
-------------------------------------------------------------------------------/

inductive Drevo : Type → Type where
  | prazno : {A : Type} → Drevo A
  | sestavljeno : {A : Type} → Drevo A → A → Drevo A → Drevo A

def zrcali : {A : Type} → Drevo A → Drevo A :=
  fun t => match t with
  | .prazno => .prazno
  | .sestavljeno l x d => .sestavljeno (zrcali d) x (zrcali l)

def visina : {A : Type} → Drevo A → Nat :=
  fun t => match t with
  | .prazno => 0
  | .sestavljeno l _ d => 1 + max (visina l) (visina d)

def elementi : {A : Type} → Drevo A → List A :=
  fun t => match t with
  | .prazno => []
  | .sestavljeno l x d => elementi l ++ x :: elementi d

def elementi' : {A : Type} → Drevo A → List A :=
  let rec aux : {A : Type} → Drevo A → List A → List A :=
    fun t acc => match t with
    | .prazno => acc
    | .sestavljeno l x d => aux l (x :: aux d acc)
  fun t => aux t []


theorem zrcali_zrcali :
  {A : Type} → (t : Drevo A) →
  zrcali (zrcali t) = t := 
by
  intros A t
  induction t with
  | prazno => simp [zrcali]
  | sestavljeno l x d ihl ihd =>
    simp [zrcali]
    constructor
    . rw [ihl]
    . rw [ihd]

theorem visina_zrcali :
  {A : Type} → (t : Drevo A) →
  visina (zrcali t) = visina t := by
  intros A t
  induction t with
  | prazno => simp [zrcali]
  | sestavljeno l x d ihl ihd =>
    simp [zrcali, visina]
    rw [ihl, ihd]
    apply Nat.max_comm


theorem lema_elementi'_aux : {A : Type} → (t : Drevo A) → (acc : List A) → 
  elementi'.aux t [] ++ acc = elementi'.aux t acc := by
  intros A t
  induction t with
  | prazno => 
    intro acc
    simp [elementi'.aux]
  | sestavljeno l x d ihl ihd =>
    intro acc
    simp [elementi'.aux]
    rw [← ihl]
    calc
    elementi'.aux l [] ++ x :: elementi'.aux d [] ++ acc
    _ = elementi'.aux l [] ++ ([x] ++ elementi'.aux d []) ++ acc := by rfl
    _ = elementi'.aux l [] ++ [x] ++ elementi'.aux d [] ++ acc := by rw [← List.append_assoc]
    _ = (elementi'.aux l [] ++ [x]) ++ (elementi'.aux d [] ++ acc) := by rw [List.append_assoc]
    _ = (elementi'.aux l [] ++ [x]) ++ elementi'.aux d acc := by rw [ihd]
    _ = elementi'.aux l [] ++ ([x] ++ elementi'.aux d acc) := by rw [List.append_assoc]
    _ = elementi'.aux l [] ++ (x :: elementi'.aux d acc) := by rfl
    _ = elementi'.aux l (x :: elementi'.aux d acc) := by rw [ihl]

theorem elementi_elementi' :
  {A : Type} → (t : Drevo A) →
  elementi t = elementi' t := by
  intros A t
  induction t with
  | prazno => 
    simp [elementi, elementi', elementi'.aux]
  | sestavljeno l x d ihl ihd =>
    simp [elementi, elementi', elementi'.aux]
    rw [ihl, ihd]
    simp [elementi']
    rw [lema_elementi'_aux]