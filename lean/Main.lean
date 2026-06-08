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

-- joinStringWith
def joinStringWith (sep a b: String) : String := a ++ sep ++ b

theorem joinComma : joinStringWith ", " "one" "another" = "one, another" := rfl

#guard joinStringWith ", " "one" "another" == "one, another"



-- if 3 == 4 then "equal" else "not equal"
theorem ifaeqaplus1 (a b c: Nat) : (if a == a +1 then b else c ) = c := by simp

def add1 (n : Nat) : Nat := n + 1

theorem add1_7_eq_8 : add1 7 = 8 := rfl

def main : IO Unit := do
  IO.println "Hello, World!"
  IO.println (add1 7)
