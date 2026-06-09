import Mathlib

/-
Evaluating Expressions
Reference: https://anggtwu.net/2023.1-LA/fplean4-inicio.pdf
-/

-- 42 + 19
theorem myAdd_42_19 : Nat.add 42 19 = 61 := rfl

-- "A" ++ ("B" ++ "C")
theorem stringConcat_AB_C : "A" ++ ("B" ++ "C") = "ABC" := rfl

-- ("A" ++ "B") ++ "C"
theorem stringConcat_A_BC : ("A" ++ "B") ++ "C" = "ABC" := rfl

theorem concatAssociative (a b c : String) : (a ++ b) ++ c = a ++ (b ++ c) :=
  String.append_assoc

-- if 3 == 3 then 5 else 7
theorem if3eq3 : (if 3 == 3 then 5 else 7) = 5 := rfl

example (a b c: Nat) : (if a == a then b else c) = b := by simp
example (a b c: Nat) : (if a == a +1 then b else c ) = c := by simp

-- joinStringWith
def joinStringsWith (sep a b: String) : String := a ++ sep ++ b
#guard joinStringsWith ", " "one" "another" == "one, another"

example : String -> String -> String := joinStringsWith ": "

structure RectangularPrism where
  (h w l : Float)
  deriving Repr

def RectangularPrism.volume (p : RectangularPrism) : Float := p.h * p.w * p.l
#guard RectangularPrism.volume ⟨1.0, 2.0, 3.0⟩ == 6.0

structure Point where
  (x y : Float) deriving Repr

structure Segment where
  (p1 p2 : Point) deriving Repr

def Segment.length (seg : Segment) : Float :=
  let dx := seg.p2.x - seg.p1.x
  let dy := seg.p2.y - seg.p1.y
  Float.sqrt $ dx * dx + dy * dy
#guard Segment.length ⟨⟨1.0, 2.0⟩, ⟨4.0, 6.0⟩⟩ == 5.0

def add1 (n : Nat) : Nat := n + 1

theorem add1_7_eq_8 : add1 7 = 8 := rfl

-- def main : IO Unit := do
--   IO.println "Hello, World!"
--   IO.println (add1 7)

def safeHead : List a -> Option a
  |  [] => none
  | x::_ => some x

#guard safeHead [1,2,3] == some 1
#guard safeHead ([] : List Nat) == none

def safeLast : List a -> Option a
  | [] => none
  | [x] => some x
  | _::xs => safeLast xs

#guard safeLast [1,2,3] == some 3
#guard safeLast ([] : List Nat) == none

theorem safeHead_eq (xs : List a) : safeHead xs = List.head? xs := rfl

def List.findFirst? {α : Type} : List α -> (α -> Bool) -> Option α
  | [], _ => none
  | x::xs, p => if p x then some x else List.findFirst? xs p

#guard List.findFirst? [1,2,3] (fun x => x > 1) == some 2
#guard List.findFirst? ([] : List Nat) (fun x => x > 1) == none

theorem findFirst_eq {α : Type} (xs : List α) (predicate : α -> Bool) :
  List.findFirst? xs predicate = List.find? predicate xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
      simp [List.findFirst?, List.find?, ih]
      split <;> simp_all

def swap {α β : Type} : (pair : α × β) -> β × α
  | (x, y) => (y, x)
#guard swap (1, 2) == (2, 1)
theorem swap_eq {α β : Type} (pair : α × β) : swap pair = Prod.swap pair := rfl


inductive Pet
  | dog (name : String)
  | cat (name : String)
  deriving Repr, BEq

def animals : List Pet := [Pet.dog "Spot", Pet.cat "Tiger", Pet.dog "Fifi", Pet.dog "Rex", Pet.cat "Floof"]

-- abbrev PetName : Type := String × String
-- def animals : List PetName := [( "Spot", "Dog"), ("Tiger", "Cat"), ("Fifi", "Dog"), ("Rex", "Dog"), ("Floof", "Cat")]

def Pet.isDog : Pet -> Bool
  | Pet.dog _ => true
  | Pet.cat _  => false

#guard (List.filter Pet.isDog animals) == [Pet.dog "Spot", Pet.dog "Fifi", Pet.dog "Rex"]

def howManyDogs : (pets : List Pet) -> Nat
  | [] => 0
  | x::xs => x.isDog.toNat + howManyDogs xs

#guard howManyDogs animals == 3

def mapAndSum :  (f : α -> Nat) -> (xs : List α) ->  Nat
  | _, [] => 0
  | f, x::xs => f x + mapAndSum f xs

#guard mapAndSum (fun x => x) [1,2,3] == 6

def howManyDogs2 : (pets : List Pet) -> Nat :=
    mapAndSum $ Bool.toNat ∘ Pet.isDog

theorem howManyDogs_eq (pets : List Pet) :
    howManyDogs2 pets = howManyDogs pets := by
    induction pets with
        | nil => rfl
        | cons x xs ih =>
            simp_all [howManyDogs2, howManyDogs, mapAndSum]


def zip : (xs : List α) -> (ys : List β) -> List (α × β)
    | [], _ => []
    | _, [] => []
    | x::xs, y::ys => (x, y) :: zip xs ys

#guard zip [1,2,3] [4,5,6] == [(1,4), (2,5), (3,6)]
#guard zip [1,2] [4,5,6] == [(1,4), (2,5)]
#guard zip [1,2,3] [4,5] == [(1,4), (2,5)]
#guard zip ([] : List Nat) [1,2,3] == []
#guard zip [1,2,3] ([] : List Nat) == []

theorem zip_eq :  (xs : List α) -> (ys : List β) -> zip xs ys = List.zip xs ys
    | [], _ => rfl
    | x::xs, [] => rfl
    | x::xs, y::ys => by
        simp [zip, zip_eq xs ys]

    -- induction xs generalizing ys with
    -- | nil => rfl
    -- | cons x xs' ih =>
    --     cases ys with
    --     | nil => rfl
    --     | cons y ys' =>
    --         simp [zip, ih]

def take : Nat -> List α -> List α
| 0, _ => []
| _, [] => []
| n, (x::xs) => x :: take (n - 1) xs

#guard take 3 [1,2,3,4,5] == [1,2,3]
#guard take 1 [1,2,3,4,5] == [1]
#guard take 5 [1,2,3] == [1,2,3]
#guard take 3 ["bolete", "oyster"] == ["bolete", "oyster"]
#guard take 1 ["bolete", "oyster"] == ["bolete"]

theorem take_eq : (n:Nat) -> (xs : List α) -> take n xs = List.take n xs
    | 0, [] => rfl
    | 0, x::xs => rfl
    | n+1, [] => rfl
    | n+1, x::xs => by
        simp [take, take_eq n xs]

def prodSumDistrib {α β γ : Type} : α × (β ⊕ γ) -> (α × β) ⊕ (α × γ)
  | (a, Sum.inl b) => Sum.inl (a, b)
  | (a, Sum.inr c) => Sum.inr (a, c)

#guard prodSumDistrib (1, Sum.inl 2) == (Sum.inl (1, 2) : Sum (Nat × Nat) (Nat × Nat))
#guard prodSumDistrib (1, Sum.inr 3) == (Sum.inr (1, 3) : Sum (Nat × Nat) (Nat × Nat))

-----------------------------------------------------
-- Proving our custom function equals Mathlib's!
-----------------------------------------------------

example {α β γ : Type} :
  @prodSumDistrib α β γ = (Equiv.prodSumDistrib α β γ).toFun := by
  funext ⟨a, bc⟩
  cases bc <;> rfl

def main : IO Unit := do
let englishGreeting := IO.println "Hello!"
IO.println "Bonjour!"
englishGreeting
