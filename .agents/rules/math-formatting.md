---
description: Always enforce clean Unicode formatting for mathematical equations and symbols in chat prompt responses.
globs: ["*"]
---

# Mathematical Formatting Rule for Chat Responses

## Guideline:
In all visible chat responses to the user, **NEVER** output raw LaTeX `$...$` or `$$...$$` math delimiters because the chat interface markdown renderer does not process LaTeX.

## Formatting Rules:
1. **Always use Crisp Unicode Symbols:**
   - **Superscripts:** `x²`, `y²`, `z²`, `v²`, `e₁²`, `e₂²`, `2ⁿ`, `||v||²`, `x⁻¹`
   - **Subscripts:** `e₁`, `e₂`, `e₃`, `e₁₂`, `e₁₂₃`, `x₁`, `y₁`, `x₂`, `y₂`
   - **Geometric & Ring Operators:** `*` (Geometric), `∧` (Wedge), `·` (Scalar), `∗` (Clifford Scalar), `⨼` (Left Contraction), `●` (Fat Dot), `•` (Hestenes Dot), `~A` (Reversion)
   - **Standard Math Operators:** `+`, `-`, `×`, `÷`, `±`, `≠`, `≈`, `≤`, `≥`, `∈`, `∉`, `⊆`, `⊂`, `∪`, `∩`, `⊕`, `⊗`
   - **Arrows & Logic:** `⟹`, `⟺`, `→`, `↦`, `∃!`, `∀`
   - **Fractions & Roots:** `½`, `¼`, `¾`, `1/2`, `√x`
   - **Sets & Greek:** `ℝ`, `ℂ`, `ℍ`, `ℤ`, `ℕ`, `θ`, `ϕ`, `α`, `β`, `γ`, `σ`

2. **Step-by-Step Derivations:**
   - Use indented fenced code blocks (```) or markdown tables for multi-step derivations and alignments so equations are aligned and readable.

3. **Artifacts Distinction:**
   - In **HTML artifacts** with MathJax loaded: use `\(...\)` and `\[...\]` for browser rendering.
   - In **Chat text & Markdown files**: use clean Unicode and formatted code blocks.
