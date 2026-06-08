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

def volume (l w h: Nat) : Nat := l * w * h
example : volume 1 2 3 = 6 := rfl


def add1 (n : Nat) : Nat := n + 1

theorem add1_7_eq_8 : add1 7 = 8 := rfl

def main : IO Unit := do
  IO.println "Hello, World!"
  IO.println (add1 7)
