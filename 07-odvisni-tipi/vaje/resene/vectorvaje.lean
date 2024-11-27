-- Vzamemo stvari iz predavanj
set_option autoImplicit false

inductive Naravno : Type where
  | nic : Naravno
  | naslednik : Naravno → Naravno
deriving Repr -- podoben, kokr dedovanje v Pythonu
-- dedovanje v programiranju pomen, da iz enga splošnejšga tipa povlečete neke lastnosti, k pridejo prou
-- v našem primeru, da lahko povlečemo eval.


def plus : Naravno → Naravno → Naravno :=
  fun m n =>
    match m with
    | Naravno.nic => n
    | Naravno.naslednik m' =>
        Naravno.naslednik (plus m' n)

-- Vektorji

inductive Vektor : Type → Naravno → Type where
  | prazen : {A : Type} → Vektor A Naravno.nic
  | sestavljen : {A : Type} → {n : Naravno} → A → Vektor A n → Vektor A (Naravno.naslednik n)
deriving Repr

#check (Vektor.sestavljen "a" (Vektor.sestavljen "b" (Vektor.prazen)))

def stakni_vektorja : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    match xs with
    | Vektor.prazen => ys
    | Vektor.sestavljen x xs' => Vektor.sestavljen x (stakni_vektorja xs' ys)
-- Bomo nardil to s taktikami, kokr se v leanu dela.
-- lookup - vrne i-ti element vektorja

-- Sedaj lahko definiramo `lookup`, ki ne bo nikoli povzročil napake.
inductive Finite : Naravno -> Type where -- Tip naravnih števil, ki so manjša od n?
  | fzero : {n : Naravno} -> Finite (Naravno.naslednik n)
  | fsucc : {n : Naravno} -> Finite n -> Finite (Naravno.naslednik n)
-- Pride prou tud drugje v stvareh tipa ocaml in lean, zard določenih lastnosti, k jih ma,
-- ampak mi to rabmo ker ..?


def lookup {A : Type} {n : Naravno} : Vektor A n -> Finite n -> A :=
  fun xs i => -- kt da bi mel navadne sezname in navadna naravna števila najprej. Bo javljal napake, ampak bo ogrodje.
    match xs, i with
    | Vektor.sestavljen x xs', Finite.fzero => x
    | Vektor.sestavljen _ xs', Finite.fsucc i' => lookup xs' i' -- prvega preskočmo, pa delamo lookup na manj.

#eval lookup Vektor.prazen Finite.fzero -- ker smo nekje v definiciji finite-a mel naslednik, tega zdej sploh ne mormo napisat
-- ker ker je vektor prazen, dobimo Vektor Naravno.nic, iz finite pa dobimo naslednika.
-- Ne mormo napisat tazga števila tipa Finite, da bi lahko dobil prazen vektor.
-- Pač dejansko *ne mormo* napisat lookup od praznga vektorja, ker se tipi ne poklopjo
-- K uporabmo finite za to, da napišemo indeks, pač ne bo 0. Najmanjpi finite, k ga lahko nardimo, je finite od 1.
-- V lookupu gre v match cel tip, ne število.
-- Finite.fzero ne rab argumenta, ker nam vedno zna ničlo vrnt, ne rab argumenta za to. Lahko poveste, kero ničlo hočte iz tega,
-- ampak lean lahko ugane, n je implicitn argument.
-- Mi mamo namen uporabljat finite samo v teh lookupih, kjer je ta n že v zraku - dolžina tega vektorja!
-- (loh bi mel vektor že definiran s tem, namest z naravnimi. Sam bi se pol verjetno zaplezal pa v kkšne druge težave.)
-- kle pr eval. Je edino naravno število, k žvi v tem kontektsu, ta naravno.nič, "nadležno napreden software so te dokazovalniki,
-- kr kr iz konteksta vlečejo stvari", tkoda bo uganu, da bo finite.fzero. Ampak nekak ne vem še vedno, zakaj ne gre
-- neki je s tem naslednikom, sam js ne razumem, ker pač 0 ma naslednika sam ni naslednik, i don't get it, neki s konstruktorjem pa neki?
-- Bomo dal nazaj na predavanja Finite.
-- "Traja neki časa, da se prebavi, kokr neskončnost v prvem letniku"
-- Ta lookup zdej pač ne more padt ven iz seznama ...


-- Včasih enakost tipov ni takoj očitna in jo moramo izpeljati
-- Dopolnite naslednjo definicijo, vse potrebne leme pa dokažite kar s taktiko `sorry`.

def stakni_vektorja' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus n m) :=
  -- fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    -- match xs with
    -- | Vektor.prazen => ys
    -- | Vektor.sestavljen x xs' => Vektor.sestavljen x (stakni_vektorja xs' ys) -- če prekopiramo od zgori, ne dela
    -- ni mu prou, ker plus n m. Ker naravna števila, k jih mamo mi, nimajo še nobene infrastrukture zgrajene -
    -- komutativnost, asociativnost ... lean še nč ne ve od tega.
    -- zato bomo z golimi rokami dokazl, za vajo.
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) =>
    match xs with
    | Vektor.prazen => by -- prestavmo se v taktičen način dela
      -- kle pričakuje Vektor A (plus n Naravno.nic)
      -- exact ys -- ne vrjame nam še, ker pač lean ne ve, da plus n naravno.nic = n
      -- ne bomo teh lem dokazval, zato bomo sorry
        have plus_nic : plus n Naravno.nic = n := sorry -- To hočmo dokazat, loh bi, smo že prejšn tedn
        rw [plus_nic]
        exact ys
    | Vektor.sestavljen x xs' => by
        have v := Vektor.sestavljen x (stakni_vektorja' xs' ys) -- naslednika mormo premaknt pa zamanjat uno dvoje. Lemi:
        have add_succ {m n : Naravno} : plus m (Naravno.naslednik n) = Naravno.naslednik (plus m n) := sorry -- to hočmo, da naslednik gre ven.
        -- have add_comm {m n : Naravno} : plus m n = plus n m := sorry -- to smo baje tud že naredl. Natural numbers!
        rw [add_succ]  -- ubistvu nismo rabl komutativnosti
        exact v -- sam še to, da uporablja 'sorry', ga zdej mot.
-- poglej si, kako se spreminja goal in kako kontekst.

-- Uporabite samo definicijo `stakni_vektorja'` in taktike `rw` in `exact`.
def stakni_vektorja'' : {A : Type} → {m n : Naravno} → Vektor A m → Vektor A n → Vektor A (plus m n) :=
  fun {A : Type} {m n : Naravno} (xs : Vektor A m) (ys : Vektor A n) => by -- prevedemo na prejšnjo definicijo
    have v := stakni_vektorja' xs ys
    have add_comm {m n : Naravno} : plus m n = plus n m := sorry
    rw [add_comm] -- apply je, ko mate funkcijo, za enakost rewrite (i think)
    exact v
-- premisli vse tole
