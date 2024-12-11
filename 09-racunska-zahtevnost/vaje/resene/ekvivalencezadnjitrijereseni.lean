-- Glej zadnje tri trditve, samo te tri so nove, ostale so od zadnjič

def concat {A : Type} : List A → List A → List A :=
  fun xs ys =>
    match xs with
    | [] => ys
    | x :: xs' => x :: concat xs' ys

#check (concat ["a", "b"] ["c", "d"])

def reverse {A : Type} : List A → List A :=
  sorry


#check (reverse ["a", "b", "c", "d"])

def length {A : Type} : List A → Nat :=
  sorry


#check (length ["a", "b", "c", "d"])

theorem trd1  {A : Type} {x : A} : reverse [x] = [x] :=
  sorry

theorem trd2 {A : Type} {xs ys : List A} : length (concat xs ys) = length xs + length ys :=
  sorry

-- Tega poznamo že iz predavanj
theorem trd3 {A : Type} {xs : List A} : concat xs [] = xs :=
  by
    induction xs with
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]

theorem trd4 {A : Type} {xs ys zs : List A} : concat (concat xs ys) zs = concat xs (concat ys zs) :=
  sorry

theorem trd5 {A : Type} {xs ys : List A} : reverse (concat xs ys) = concat (reverse ys) (reverse xs) :=
  sorry

theorem trd6 {A : Type} {xs : List A} : length (reverse xs) = length xs :=
  sorry

theorem trd7 {A : Type} {xs : List A} : reverse (reverse xs) = xs :=
  sorry


def map {A B : Type} : (A → B) → List A → List B :=
  sorry

theorem map_assoc {A B C : Type} {f : A → B} {g : B → C} {xs : List A} : map g (map f xs) = map (g ∘ f) xs :=
  sorry

theorem map_id {A : Type} {xs : List A} : map id xs = xs :=
  sorry

theorem map_concat {A B : Type} {f : A → B} {xs ys : List A} : map f (concat xs ys) = concat (map f xs) (map f ys) :=
  sorry


theorem map_reverse {A B : Type} {f : A → B} {xs : List A} : map f (reverse xs) = reverse (map f xs) :=
  sorry

inductive tree (A : Type) : Type where
  | empty : tree A
  | node : A → tree A → tree A → tree A

#check tree.rec

def tree_map {A B : Type} : (A → B) → tree A → tree B :=
  sorry

theorem tree_map_empty {A B : Type} {f : A → B} : tree_map f tree.empty = tree.empty :=
  sorry

theorem tree_map_comp {A B C : Type} {f : A → B} {g : B → C} {t : tree A} : tree_map g (tree_map f t) = tree_map (g ∘ f) t :=
  sorry

def depth {A : Type} : tree A → Nat :=
  fun t =>
    match t with
    | tree.empty => 0
    | tree.node _ l r => 1 + Nat.max (depth l) (depth r)

-- S tem se ne bomo ukvarjali
theorem max_comm {a b : Nat} : Nat.max a b = Nat.max b a :=
  sorry

def mirror {A : Type} : tree A → tree A :=
  sorry

theorem mirror_depth {A : Type} {t : tree A} : depth (mirror t) = depth t :=
  sorry

theorem mirror_mirror {A : Type} {t : tree A} : mirror (mirror t) = t :=
  sorry

def collect {A : Type} : tree A → List A :=
  fun t =>
    match t with
    | tree.empty => []
    | tree.node x l r => concat (collect l) (concat [x]  (collect r))

theorem trd8 {A : Type} {x : A} {xs ys : List A} : concat xs (x::ys) = concat (concat xs [x]) ys :=
  sorry


theorem collect_mirror {A : Type} {t : tree A} : collect (mirror t) = reverse (collect t) :=
  sorry


def size {A : Type} : tree A → Nat :=
  sorry

theorem size_mirror {A : Type} {t : tree A} : size (mirror t) = size t :=
  sorry


--- Indukcija na pomožnih funkcijah z akumulatorjem

theorem concat2 : concat xs (x :: ys) = concat (concat (xs) [x]) ys :=
  by
    induction xs with
    | nil => simp [concat]
    | cons x' xs' ih =>
      simp [concat]
      rw [ih] -- alpa sam assumption
  -- za okrevanje: Ko nimate akumulatorjev, delate tko.


-- Definirajte repno rekurzivno funkcijo, ki obrne seznam
def reverse' {A : Type} (xs : List A) : List A :=
  -- lokalna definicija v leanu:
  --let rec aux : List A → List A → List A := -- "pa oblubmo, da bomo vrnl spet seznam" :P
    -- match xs, acc with, mamo kokr match, se mi zdi?
  let rec aux : List A → List A → List A
    | [], acc => acc
    -- "js to pišem (na projekcijo), ker mislm, da ste ta reverse napisal že dostkrat v življenju, še posebi v zadnjih parih tednih" :P
    -- "in je brezveze se o tem preveč pogovarjat. Se raj o čem bolj poučnem"
    | x' :: xs', acc => aux xs' (x' :: acc)
  aux xs [] -- zgori prvo vrstico smo spremenil, da je prvi List A zdej parameter, da je kle bl jedrnato, da ni fun xs = tralala,
  -- kokr piše pretnar na predavanjih te funkcije
  -- če neki ne bo šlo, bo dodano v rešitve tko, kokr smo na predavanjih pisal. Sam lean mal drugačne stvari hoče vidt ...

-- si definiramo še enkrat z leanovo interno konkatenacijo reverse. Gre velik lepš skos.
--Mogoče si bomo pogledal še, kako se z našo nardi

def reverse'' {A : Type} : List A → List A :=
  fun xs =>
    match xs with
    | [] => []
    | x :: xs' => (reverse'' xs') ++ [x] -- v leanu je ++

-- Dokažite, da je vaša funkcija pravilna
theorem reverse_eq_reverse' {A : Type} : ∀ {xs : List A}, reverse xs = reverse' xs :=
  by
    sorry

-- bomo za reverse'' to pokazal, ker prvič delamo z akumulatorjem. Bomo ta najlažji primer nardil
theorem reverse''_eq_reverse'STAR {A : Type} : ∀ {xs : List A}, reverse'' xs = reverse' xs :=
  by
    intro xs -- če velja za vse, velja za poljubnega, gremo enga vzet in mu dat ime xs
    induction xs with -- zdej pa je indukcija po teh seznamih
    -- zdej bo indukcijska delala za vse krajše podsezname, ne sam unga enga
    | nil =>
      simp [reverse'', reverse', reverse'.aux] -- "uporabi še definicijo pomožne funkcije", tko se dostopa
    | cons x xs' ih =>
      simp [reverse'', reverse', reverse'.aux]
      sorry

-- sam smo jo zdele popravl, ker mormo pokazat, da je to isto za vse akumulatorje tud (POGLEJ SI S PREDAVANJ, ker smo pomoje nardil mal drgač)
theorem reverse''_eq_reverse'.aux {A : Type} : ∀ {xs acc : List A},
(reverse'' xs) ++ acc = reverse'.aux xs acc :=
  by
    intro xs
    induction xs with
    | nil =>
      simp [reverse'', reverse'.aux]
    | cons x xs' ih =>
      intro acc -- ubistvu se znebimo kvantifikatorja, da lahko un preoblikuje, pač kokr najprej intro xs zgori, sej je jasno.
      simp [reverse'', reverse'.aux] -- simp proba ene par stvari narest, ena izmed njih je ubistvu rw, sam če veste, da je točn rw treba narest,
      -- je to, ker bo lean nekak hitrej to naredu al neki
      -- zdej nekak rabmo sam še drug akumulator, ker to neki dela za vsak acc, mal ne vem, premisli
      rw [ih] -- zdej je sam x :: acc pač nov akumulator. Tkodaker dela za vse, dela tud za tega, k smo mu x dodal,
      -- tkoda dela indukcijska na tej točki

-- zdej pa tud to lahko uporabmo v prvotni trditvi
theorem reverse''_eq_reverse' {A : Type} : ∀ {xs : List A}, reverse'' xs = reverse' xs :=
  by
    intro xs
    induction xs with
    | nil =>
      simp [reverse'', reverse', reverse'.aux]
    | cons x xs' ih =>
      simp [reverse'', reverse', reverse'.aux]
      rw [reverse''_eq_reverse'.aux] -- kokr, da bi lemo uporabil.
      -- (mimogrede, primer spet, kako je včasih laži neki v splošnem dokazat, pa pol it na konkretno. kokr smo mel že v ocamlu s pomožnimi ... FK)
      -- lahko bi bil exact sam nakonc namest rw.


-- NAREDI DOMA ŠE ZA NAŠO. Zraven bomo mogl vlečt še definicijo concata pač.
