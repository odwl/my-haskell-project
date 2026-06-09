def List.findFirst? {α : Type} : List α -> (α -> Bool) -> Option α
  | [], _ => none
  | x::xs, p => if p x then some x else List.findFirst? xs p

theorem findFirst_eq {α : Type} (xs : List α) (predicate : α -> Bool) :
  List.findFirst? xs predicate = List.find? predicate xs := by
  induction xs with
  | nil => rfl
  | cons x xs ih =>
    simp [List.findFirst?, List.find?, ih]
    split <;> simp_all
