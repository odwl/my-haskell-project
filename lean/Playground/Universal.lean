import Mathlib.CategoryTheory.Category.Basic
import Mathlib.CategoryTheory.Limits.Shapes.Terminal
import Mathlib.CategoryTheory.Iso
import Mathlib.CategoryTheory.Types.Basic

open Functor
open CategoryTheory
open CategoryTheory.Limits
open TypeCat

/-!
# Exercise 2.2.1 from Pierce 1991

Here we define a Signature Ω, the Functor F_Ω, and prove that the
Traditional Algebra homomorphism definition is exactly equivalent
to the F-Algebra homomorphism definition.
-/
universe u v

-- 1. An F-Algebra is a carrier object in K and an evaluation morphism `a : F(carrier) ⟶ carrier`
structure FAlgebra (K : Type u) [Category.{v} K] (F : K ⥤ K) where
  carrier : K
  a : F.obj carrier ⟶ carrier

namespace C

variable {K : Type u} [Category.{v} K] {F : K ⥤ K}

@[ext]
structure FHom (A B : FAlgebra K F) where
  hom : A.carrier ⟶ B.carrier
  is_hom : F.map hom ≫ B.a = A.a ≫ hom

attribute [reassoc (attr := simp)] FHom.is_hom

instance : Category (FAlgebra K F) where
  Hom A B := FHom A B
  id A := ⟨𝟙 A.carrier, by simp⟩
  comp f g := ⟨f.hom ≫ g.hom, by simp [F.map_comp]⟩
  id_comp f := by ext; simp
  comp_id f := by ext; simp
  assoc f g h := by ext; simp

-- Lambek

lemma id_unique_of_initial {I : FAlgebra K F} (hI : IsInitial I) (f : I ⟶ I) : f = 𝟙 I :=
  hI.hom_ext f (𝟙 I)

@[ext]
theorem FAlgebra.hom_ext {A B : FAlgebra K F} (f g : A ⟶ B) (h : f.hom = g.hom) : f = g := by
  cases f; cases g; congr

@[simp]
lemma hom_eq_iff {A B : FAlgebra K F} (f g : A ⟶ B) : f = g ↔ f.hom = g.hom :=
  ⟨fun h => by rw [h], FAlgebra.hom_ext f g⟩

@[simp]
lemma id_hom (A : FAlgebra K F) : (𝟙 A : FHom A A).hom = 𝟙 A.carrier := rfl

@[simp]
lemma comp_hom {A B C : FAlgebra K F} (f : A ⟶ B) (g : B ⟶ C) : (f ≫ g : FHom A C).hom = f.hom ≫ g.hom := rfl

def FAlgebraFunctor : FAlgebra K F ⥤ FAlgebra K F where
  obj X := ⟨F.obj X.carrier, F.map X.a⟩
  map f := ⟨F.map f.hom, by simp [-CategoryTheory.Functor.map_comp, ← CategoryTheory.Functor.map_comp, f.is_hom]⟩
  map_id X := by simp [F.map_id]
  map_comp f g := by simp [F.map_comp]

def algebra_evaluation_hom (X : FAlgebra K F) : FAlgebraFunctor.obj X ⟶ X where
  hom := X.a
  is_hom := rfl

namespace Lambek

def uFA {I : FAlgebra K F} (hI : IsInitial I) : I ⟶ FAlgebraFunctor.obj I :=
  hI.to (FAlgebraFunctor.obj I)

def initial_comp_a_hom {I : FAlgebra K F} (hI : IsInitial I) : I ⟶ I :=
  uFA hI ≫ algebra_evaluation_hom I

lemma m_comp_a_eq_id {I : FAlgebra K F} (hI : IsInitial I) : uFA hI ≫ algebra_evaluation_hom I = 𝟙 I :=
  id_unique_of_initial hI (initial_comp_a_hom hI)

lemma a_comp_m_eq_id {I : FAlgebra K F} (hI : IsInitial I) :
    algebra_evaluation_hom I ≫ uFA hI = 𝟙 (FAlgebraFunctor.obj I) := by
  ext
  change I.a ≫ (uFA hI).hom = 𝟙 (F.obj I.carrier)
  have h_hom := (uFA hI).is_hom
  have h_id := congr_arg FHom.hom (m_comp_a_eq_id hI)
  change (uFA hI).hom ≫ I.a = 𝟙 I.carrier at h_id
  rw [← h_hom]
  change F.map (uFA hI).hom ≫ F.map I.a = 𝟙 (F.obj I.carrier)
  rw [← F.map_comp, h_id, F.map_id]

end Lambek

instance lambek_theorem {I : FAlgebra K F} (hI : IsInitial I) : IsIso (algebra_evaluation_hom I) where
  out := ⟨Lambek.uFA hI, Lambek.a_comp_m_eq_id hI, Lambek.m_comp_a_eq_id hI⟩

end C


namespace U

-- 2. A Signature consists of Operations, each with an Arity (which we model as an index Type)
structure Signature where
  Op : Type
  Arity : Op → Type

-- 3. The Functor F for a given signature Ω as a category-theoretic functor Type ⥤ Type
def SigFunctor (Ω : Signature) : Type ⥤ Type where
  obj S := Σ (ω : Ω.Op), (Ω.Arity ω → S)
  map f := ofHom (fun ⟨w, g⟩ => ⟨w, (f : _ → _) ∘ g⟩)
  map_id S := by ext ⟨w, g⟩; dsimp [ofHom]; rfl
  map_comp f g := by ext ⟨w, g⟩; dsimp [ofHom]; rfl

-- 4. The Universal Algebra definition of a homomorphism
def IsUniversalHom {Ω : Signature} (A B : FAlgebra Type (SigFunctor Ω)) (h : A.carrier → B.carrier) : Prop :=
  ∀ (sig : (SigFunctor Ω).obj A.carrier), h (A.a sig) = B.a ((SigFunctor Ω).map (ofHom h) sig)

end U

-- 5. Exercise 2.2.1: Prove they are equivalent!
theorem hom_equiv {Ω : U.Signature} (A B : FAlgebra Type (U.SigFunctor Ω)) (h : A.carrier → B.carrier) :
    h ∘ A.a = B.a ∘ (U.SigFunctor Ω).map (ofHom h) ↔ U.IsUniversalHom A B h := ⟨congr_fun, funext⟩

-- Connecting bundled morphisms (A ⟶ B) in Category C to U.IsUniversalHom
theorem hom_equiv_morphism {Ω : U.Signature} (A B : FAlgebra Type (U.SigFunctor Ω)) (h : A.carrier → B.carrier) :
    (∃ (f : A ⟶ B), (f.hom : A.carrier → B.carrier) = h) ↔ U.IsUniversalHom A B h := by
  constructor
  · rintro ⟨f, rfl⟩
    intro sig
    have h_val := congr_fun (congr_arg (fun (m : (U.SigFunctor Ω).obj A.carrier ⟶ B.carrier) => (m : (U.SigFunctor Ω).obj A.carrier → B.carrier)) f.is_hom) sig
    have h_ofhom : ofHom (f.hom : A.carrier → B.carrier) = f.hom := rfl
    rw [h_ofhom]
    exact h_val.symm
  · intro hh
    have h_is_hom : (U.SigFunctor Ω).map (ofHom h) ≫ B.a = A.a ≫ ofHom h := by
      ext sig
      have h_fun := (hom_equiv A B h).mpr hh
      have h_val := congr_fun h_fun sig
      exact h_val.symm
    exact ⟨⟨ofHom h, h_is_hom⟩, rfl⟩
