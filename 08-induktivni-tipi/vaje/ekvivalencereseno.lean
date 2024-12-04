def concat {A : Type} : List A → List A → List A :=
  fun xs ys =>
    match xs with
    | [] => ys
    | x :: xs' => x :: concat xs' ys

#check (concat ["a", "b"] ["c", "d"])

def reverse {A : Type} : List A → List A :=
  fun xs =>
    match xs with
    | [] => []
    | x :: xs' => concat (reverse xs') [x]


#check (reverse ["a", "b", "c", "d"])

def length {A : Type} : List A → Nat :=
  fun xs =>
    match xs with
    | [] => 0
    | _ :: xs' => 1 + length xs'


#check (length ["a", "b", "c", "d"])
-- check samo prever, če je tip taprav, #eval vam pa zračuna

-- poglej trd3 s predavanj
theorem trd1  {A : Type} {x : A} : reverse [x] = [x] :=
  by
    simp [reverse]  -- definicija tiste funkcije, ki jo iporabljamo - je omenjena v trditvi
    simp [concat] -- concat s praznim seznamom mi je še ostal
    -- simp [reverse, concat] je rešitev drgač


theorem trd2 {A : Type} {xs ys : List A} : length (concat xs ys) = length xs + length ys :=
  by
    induction xs with -- prekopirana indukcija od trd3 in samo spreminjamo mal
    | nil =>
      simp [length, concat] -- poglej si, kaj vrne samo s simp [length ]
    | cons x xs' ih => -- VSE TO PREMISLI DOMA PO KORAKIH, KLE SAM OD NJE PREPISUJEM, NAREDI ZNOVA
      simp [length] -- ih smo dobil iz konstruktorja, če ga ne bi poimenoval, bi mu sam pač neki dal
      rw [ih] -- zdej sam še asociativnost
      rw [Nat.add_assoc] -- lahko bi blo skupi v enem rw [ih, Nat.add_assoc]


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
  by
    induction xs with -- "tale simp je kr močn, lean zlo velik za vas nardi"
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih] -- naredila sama od trd4 naprej, ampak premisli še enkrat, da se ti usede v glavo

theorem trd5 {A : Type} {xs ys : List A} : reverse (concat xs ys) = concat (reverse ys) (reverse xs) :=
  by
    induction xs with
    | nil =>
      simp [concat, reverse] -- še, da je concat s praznim od desne = xs
      rw [trd3] -- za obliko enakosti je rw, za implikacije je apply
    | cons x xs' ih =>
      simp [concat, reverse]
      rw [ih]
      rw [trd4]

theorem trd6 {A : Type} {xs : List A} : length (reverse xs) = length xs :=
  by
    induction xs with
    | nil =>
      simp [reverse] -- kero kol od obeh definicij je delal
    | cons x xs' ih =>
      simp [reverse, length]
      rw [trd2]
      simp [length, Nat.add_comm]
      rw [ih] -- premisli, kako se to da lepše/krajpe/elegantneje

theorem trd7 {A : Type} {xs : List A} : reverse (reverse xs) = xs :=
  by
    induction xs with
    | nil =>
      simp [reverse]
    | cons x xs' ih =>
      simp [reverse]
      rw [trd5]
      simp [reverse, concat]
      rw [ih]


def map {A B : Type} : (A → B) → List A → List B :=
  fun f xs =>
    match xs with
    | [] => []
    | x :: xs' => (f x) :: map f xs'


theorem map_assoc {A B C : Type} {f : A → B} {g : B → C} {xs : List A} : map g (map f xs) = map (g ∘ f) xs :=
  by
    induction xs with
    | nil =>
      simp [map]
    | cons x xs' ih =>
      simp [map]
      rw [ih]


theorem map_id {A : Type} {xs : List A} : map id xs = xs :=
  by
    induction xs with
    | nil =>
      simp [map]
    | cons x xs' ih =>
      simp [map]
      rw [ih]

theorem map_concat {A B : Type} {f : A → B} {xs ys : List A} : map f (concat xs ys) = concat (map f xs) (map f ys) :=
  by
    induction xs with
    | nil =>
     simp [map, concat] -- delu je sam s concat?
    | cons x xs' ih =>
      simp [map, concat] -- premisli korake, ker pišem, ker je skos isto in ker lean vse zna, ampak ne razumem korakov!
      rw [ih]


theorem map_reverse {A B : Type} {f : A → B} {xs : List A} : map f (reverse xs) = reverse (map f xs) :=
  by
    induction xs with
    | nil =>
      simp [reverse, map]
    | cons x xs' ih =>
      simp [reverse]
      rw [map_concat]
      rw [ih]
      simp [map] -- premisli ?!?!?!?

inductive tree (A : Type) : Type where
  | empty : tree A
  | node : A → tree A → tree A → tree A

#check tree.rec

def tree_map {A B : Type} : (A → B) → tree A → tree B :=
  fun f t =>
    match t with
    | tree.empty => tree.empty
    | tree.node x l r => tree.node (f x) (tree_map f l) (tree_map f r)


theorem tree_map_empty {A B : Type} {f : A → B} : tree_map f tree.empty = tree.empty :=
  by
    simp [tree_map]


theorem tree_map_comp {A B C : Type} {f : A → B} {g : B → C} {t : tree A} : tree_map g (tree_map f t) = tree_map (g ∘ f) t :=
  by
    induction t with
    | empty =>
      simp [tree_map]
    | node t l r ihl ihr =>
      simp [tree_map]
      constructor
      . rw [ihl]
      . rw [ihr]


def depth {A : Type} : tree A → Nat :=
  fun t =>
    match t with
    | tree.empty => 0
    | tree.node _ l r => 1 + Nat.max (depth l) (depth r)


-- S tem se ne bomo ukvarjali
theorem max_comm {a b : Nat} : Nat.max a b = Nat.max b a :=
  sorry

def mirror {A : Type} : tree A → tree A :=
  fun t =>
    match t with
    | tree.empty => tree.empty
    | tree.node t l r => tree.node t (mirror r) (mirror l)


theorem mirror_depth {A : Type} {t : tree A} : depth (mirror t) = depth t :=
  by
    induction t with
    | empty =>
      simp [mirror]
    | node t l r ihl ihr =>
      simp [mirror, depth] -- poglej si po vrsti, kaj nardita simp mirror pa simp depth, ker je samo depth dovolj !
      rw [ihr, ihl, max_comm]


theorem mirror_mirror {A : Type} {t : tree A} : mirror (mirror t) = t :=
  by
    induction t with
    | empty =>
      simp [mirror]
    | node t l r ihl ihr =>
      simp [mirror]
      constructor
      . rw [ihl]
      . rw [ihr]


def collect {A : Type} : tree A → List A :=
  fun t =>
    match t with
    | tree.empty => []
    | tree.node x l r => concat (collect l) (concat [x]  (collect r))
-- PREMISLI ?!?!?!?!?!

theorem trd8 {A : Type} {x : A} {xs ys : List A} : concat xs (x::ys) = concat (concat xs [x]) ys :=
  by
    induction xs with -- na levem je def concat, tkoda se to splača!
    | nil =>
      simp [concat]
    | cons x xs' ih =>
      simp [concat]
      rw [ih]
-- tega kle ne razumeš ^, zakaj kr dela? premisli!


theorem collect_mirror {A : Type} {t : tree A} : collect (mirror t) = reverse (collect t) :=
  by
    induction t with
    | empty =>
      simp [mirror, collect, reverse]
    | node t l r ihl ihr =>
      simp [mirror, collect, concat]
      rw [trd8]
      rw [ihl]
      rw [trd5]
      rw [ihr]
      simp [reverse]
-- pač js sploh trditve nism prebrala, tok brezmogansko js zdele to delam
-- NAREDI ŠE ENKRAT, TAKO DA PREMISLIŠ!!!!! IN RAZUMEŠ
-- Okej, zdej k je do konca, sem jo prebrala, ampak ne razumem, kaj sem delala +
-- naredi tako, da je elegantneje in ne kr neki? Ker je to bolj probavanje, kot ne ...


def size {A : Type} : tree A → Nat :=
  fun t => length (collect t)

theorem size_mirror {A : Type} {t : tree A} : size (mirror t) = size t :=
  by
    induction t with
    | empty =>
      simp [mirror]
    | node t l r ihl ihr =>
      simp [mirror, size, collect, concat]
      rw [trd8]
      rw [collect_mirror]
      rw [<- trd1]
      rw [<- trd5]
      rw [trd8]
      simp [concat, reverse]
      rw [trd8, trd3]
      rw [collect_mirror]
      rw [trd2, trd6, trd2, trd2, trd2, trd6]
      simp [length]
      exact
-- to se pa 120 000 posto da narest bolš in lepš in elegantneje, prej bi mogla trd2 in 6 začet
-- sam zdej se mi mudi, tkoda grem, NAREDI ZNOVA!
