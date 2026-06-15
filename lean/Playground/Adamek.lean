import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Limits.Shapes.Terminal

open CategoryTheory
open CategoryTheory.Limits

-- Define the indexing category J with two objects and a single arrow 0 ⟶ 1
inductive J where
  | zero
  | one
  deriving DecidableEq

inductive JHom : J → J → Type where
  | id_zero : JHom .zero .zero
  | id_one : JHom .one .one
  | f : JHom .zero .one

instance : Category J where
  Hom := JHom
  id X := match X with
    | .zero => .id_zero
    | .one => .id_one
  comp g h := match g, h with
    | .id_zero, .id_zero => .id_zero
    | .id_zero, .f => .f
    | .f, .id_one => .f
    | .id_one, .id_one => .id_one
  id_comp h := by cases h <;> rfl
  comp_id h := by cases h <;> rfl
  assoc g h k := by cases g <;> cases h <;> cases k <;> rfl

-- Show that 1 (one) is terminal in J
def J.one_is_terminal : IsTerminal J.one :=
  IsTerminal.ofUniqueHom
    (fun
      | .zero => .f
      | .one => .id_one)
    (fun _ m => by cases m <;> rfl)

-- Define the diagram D : J ⥤ C mapping 0 ↦ ⊥_ C and 1 ↦ F(⊥_ C)
variable {C : Type*} [Category C] [HasInitial C] (F : C ⥤ C)

noncomputable def D : J ⥤ C where
  obj X := match X with
    | .zero => ⊥_ C
    | .one => F.obj (⊥_ C)
  map {X Y} hom := match X, Y, hom with
    | .zero, .zero, .id_zero => 𝟙 (⊥_ C)
    | .one, .one, .id_one => 𝟙 (F.obj (⊥_ C))
    | .zero, .one, .f => initial.to (F.obj (⊥_ C))
  map_id X := match X with
    | .zero => rfl
    | .one => rfl
  map_comp {X Y Z} g h := match X, Y, Z, g, h with
    | .zero, .zero, .zero, .id_zero, .id_zero => by simp
    | .zero, .zero, .one, .id_zero, .f => by simp
    | .zero, .one, .one, .f, .id_one => by simp
    | .one, .one, .one, .id_one, .id_one => by simp

-- Prove that F(⊥_ C) is the colimit of D
noncomputable def D_colimit : IsColimit (coconeOfDiagramTerminal J.one_is_terminal (D F)) :=
  colimitOfDiagramTerminal J.one_is_terminal (D F)
