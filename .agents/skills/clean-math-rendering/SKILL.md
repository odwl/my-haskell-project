---
name: clean-math-rendering
description: Enforce crisp, clean Unicode formatting for mathematical symbols, equations, and multivector algebra in all chat responses.
---

# Clean Mathematical Rendering Skill

## Purpose
Ensure all mathematical discussions, Clifford algebra equations, and derivations are rendered using native Unicode symbols, clear code blocks, and markdown tables rather than raw LaTeX syntax.

## Guidelines
1. **Never use raw LaTeX (`$...$` or `$$...$$`)** in chat responses.
2. **Use Native Unicode Symbols:**
   - Powers: `x²`, `y²`, `z²`, `v²`, `e₁²`, `e₂²`, `2ⁿ`, `||v||²`, `x⁻¹`
   - Indices: `e₁`, `e₂`, `e₃`, `e₁₂`, `e₁₂₃`, `x₁`, `y₁`, `x₂`, `y₂`
   - Geometric Algebra: `*` (Geometric), `∧` (Wedge), `·` (Scalar), `∗` (Clifford Scalar), `⨼` (Left Contraction), `●` (Fat Dot), `•` (Hestenes Dot), `~A` (Reversion)
   - Relations & Logic: `⟹`, `⟺`, `→`, `↦`, `±`, `≠`, `≈`, `≤`, `≥`, `∈`, `⊕`, `⊗`
   - Sets & Fields: `ℝ`, `ℂ`, `ℍ`, `ℤ`, `ℕ`
3. **Format Step-by-Step Equations:** Use aligned code blocks (```) for derivations.
