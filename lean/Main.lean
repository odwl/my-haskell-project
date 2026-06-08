/-
Evaluating Expressions
Reference: https://anggtwu.net/2023.1-LA/fplean4-inicio.pdf
-/

-- 42 + 19
theorem myAdd_42_19 : Nat.add 42 19 = 61 := rfl

-- String.append "A" (String.append "B" "C")
theorem stringConcat_AB_C : String.append "A" (String.append "B" "C") = "ABC" := rfl

-- String.append (String.append "A" "B") "C"
theorem stringConcat_A_BC : String.append (String.append "A" "B") "C" = "ABC" := rfl

theorem concatAssociative (a b c : String) : String.append a (String.append b c) = String.append (String.append a b) c :=
  String.append_assoc.symm





def myAdd (x y : Nat) : Nat := x + y -- eq to Nat.add

def add1 (n : Nat) : Nat := n + 1

theorem add1_7_eq_8 : add1 7 = 8 := rfl

def main : IO Unit := do
  IO.println "Hello, World!"
  IO.println (add1 7)
