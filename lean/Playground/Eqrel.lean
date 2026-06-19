import Mathlib.Logic.Relation
import Mathlib.Data.Setoid.Basic


/-!
# Equivalence Relations and Closures

This file contains definitions and theorems related to equivalence relations
and equivalence closures.

https://logicmatters.net/categories/SmithCat.pdf
-/

namespace Playground.Eqrel

variable {X Y Z Q : Type}
variable (R : Y → Y → Prop)
variable (k : Y → Z)

/-- The equivalence kernel of a function k. -/
def Ek (k : Y → Z) : Setoid Y where
  r y1 y2 := k y1 = k y2
  iseqv := {
    refl _ := rfl
    symm h := h.symm
    trans h1 h2 := h1.trans h2
  }

def Pfg (f g : X → Y) (y1 y2 : Y) : Prop :=
  ∃ x : X, f x = y1 ∧ g x = y2

/-- The equivalence projection of a pair of functions f and g. -/
def Efg (f g : X → Y) : Setoid Y :=
  Relation.EqvGen.setoid (Pfg f g)

/-- A function k respects R if related elements are mapped to the same value. -/
def Respects (k : Y → Z) (R : Y → Y → Prop) : Prop :=
  ∀ {y1 y2 : Y}, R y1 y2 → k y1 = k y2

theorem theorem_63 (h : Respects k R) : Respects k (Relation.EqvGen R) :=
  fun hy =>
  match hy with
  | .rel _ _ hxy => h hxy
  | .refl _ => rfl
  | .symm _ _ hxy => (theorem_63 h hxy).symm
  | .trans _ _ _ hxy hyz => (theorem_63 h hxy).trans (theorem_63 h hyz)

theorem Efg_le_Ek (h : Respects k (Pfg f g)) : Efg f g ≤ Ek k :=
  fun _ _ hrel => theorem_63 (Pfg f g) k h hrel

-- def IsScheme (R : Y → Y → Prop) (q : Y → Q) : Prop :=
--   (∀ {y1 y2 : Y}, R y1 y2 ↔ q y1 = q y2) ∧ Function.Surjective q

-- theorem equivalence_of_isScheme {R : Y → Y → Prop} {q : Y → Q} (h : IsScheme R q) : Equivalence R where
--   refl _ := h.left.mpr rfl
--   symm hrel := h.left.mpr (h.left.mp hrel).symm
--   trans h1 h2 := h.left.mpr ((h.left.mp h1).trans (h.left.mp h2))

def IsScheme (R : Setoid Y) (q : Y → Q) : Prop :=
  (∀ {y1 y2 : Y}, R.r y1 y2 ↔ q y1 = q y2) ∧ Function.Surjective q

theorem respects_of_isScheme {R : Setoid Y} {q : Y → Q} (h : IsScheme R q) : Respects q R :=
  fun hr => h.left.mp hr

theorem scheme_universal_property {Y Q Z : Type} {R : Setoid Y} {q : Y → Q} (h_scheme : IsScheme R q)
    (k : Y → Z) (hk : Respects k R) :
    ∃! u : Q → Z, ∀ y, u (q y) = k y := by
  have h_surj := h_scheme.right
  let u (qy : Q) : Z := k (Classical.choose (h_surj qy))
  use u
  constructor
  · intro y
    have h_spec := Classical.choose_spec (h_surj (q y))
    have hr : R.r (Classical.choose (h_surj (q y))) y := h_scheme.left.mpr h_spec
    exact hk hr
  · intro u' hu'
    funext qy
    obtain ⟨y, hy⟩ := h_surj qy
    rw [← hy]
    have hu_y : u (q y) = k y := by
      have h_spec := Classical.choose_spec (h_surj (q y))
      have hr : R.r (Classical.choose (h_surj (q y))) y := h_scheme.left.mpr h_spec
      exact hk hr
    rw [hu_y, hu' y]

/-- Every quotient scheme `Q` for `R` is isomorphic (equivalent) to the canonical `Quotient R`. -/
noncomputable def schemeEquivQuotient {Y Q : Type} {R : Setoid Y} {q : Y → Q} (h_scheme : IsScheme R q) :
    Q ≃ Quotient R where
  toFun qy := Quotient.mk R (Classical.choose (h_scheme.right qy))
  invFun := Quotient.lift q (fun a b hab => respects_of_isScheme h_scheme hab)
  left_inv qy := by
    dsimp
    exact Classical.choose_spec (h_scheme.right qy)
  right_inv x := by
    obtain ⟨y, hy⟩ := Quotient.exists_rep x
    rw [← hy]
    dsimp
    have h_spec := Classical.choose_spec (h_scheme.right (q y))
    have hr : R.r (Classical.choose (h_scheme.right (q y))) y := h_scheme.left.mpr h_spec
    exact Quotient.eq.mpr hr


theorem isScheme_of_universal_property {Y Q : Type} {R : Setoid Y} (q : Y → Q)
    (hq_resp : Respects q R)
    (h_univ : ∀ {Z : Type} (k : Y → Z), Respects k R → ∃! u : Q → Z, ∀ y, u (q y) = k y) :

    IsScheme R q := by
  constructor
  · intro y1 y2
    constructor
    · exact hq_resp
    · intro hqy
      have h_resp_mk : Respects (Quotient.mk R) R := by
        intro a b hab
        exact Quotient.eq.mpr hab
      rcases h_univ (Quotient.mk R) h_resp_mk with ⟨u, hu, _⟩
      have hy1 := hu y1
      have hy2 := hu y2
      have h_eq : Quotient.mk R y1 = Quotient.mk R y2 := by
        rw [← hy1, ← hy2, hqy]
      exact Quotient.eq.mp h_eq
  · intro q0
    by_contra h_notsurj
    push Not at h_notsurj
    let u1 (_ : Q) : Prop := True
    let u2 (qy : Q) : Prop := qy ≠ q0
    have hu1 (y : Y) : u1 (q y) = True := rfl
    have hu2 (y : Y) : u2 (q y) = True := propext ⟨fun _ => trivial, fun _ => h_notsurj y⟩
    have h_resp_true : Respects (fun _ : Y => True) R := fun _ => rfl
    rcases h_univ (fun _ : Y => True) h_resp_true with ⟨u, _, hu_uniq⟩
    have h_u1_u2 : u1 = u2 := (hu_uniq u1 hu1).trans (hu_uniq u2 hu2).symm
    have h_contra : True = (q0 ≠ q0) := congrFun h_u1_u2 q0
    have h_false : q0 ≠ q0 := h_contra ▸ trivial
    exact h_false rfl

end Playground.Eqrel






