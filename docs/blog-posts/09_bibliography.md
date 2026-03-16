# Part 9: Bibliography

This section centralizes all the foundational papers, influential articles, and recommended reading for the concepts discussed throughout this series.

### Type Theory & Algebraic Data Types

1. **Howard, W. A. (1980).** *The formulae-as-types notion of construction*. In To H. B. Curry: Essays on Combinatory Logic, Lambda Calculus and Formalism (pp. 479-490). Academic Press. 
   *(The seminal work establishing the Curry-Howard correspondence connecting uninhabited types to logical falsity).*
2. **Maguire, S. (2018).** *[Thinking with Types](https://thinkingwithtypes.com/)*. 
   *(Features early chapters specifically focusing on the "Cardinality" and algebra of types, using structural counting to prove which functions can mathematically exist).*
3. **McBride, C. (2001).** *The derivative of a regular type is its type of one-hole contexts*. 
   *(A mind-bending proof that taking the calculus derivative of a data type's polynomial structurally generates its exact Zipper).*
4. **Swierstra, W. (2008).** *Data types à la carte*. 
   *(A seminal paper proving how to use the algebraic Sum operator over Functors to modularly compose distinct data types and interpreters).*
5. **Taylor, C. (2013).** *[The Algebra of Algebraic Data Types](https://gist.github.com/gregberns/5e9da0c95a9a8d2b6338afe69310b945)*. 
   *(A famous blog series expanding the structural analogy by directly using high-school algebra to calculate isomorphic data types).*

### Category Theory & Deep Math

6. **Bird, R. & de Moor, O. (1997).** *Algebra of Programming*. 
   *(A foundational text exploring how algebras and functor subcategories are derived systematically from building blocks like Bifunctors).*
7. **Milewski, B. (2014).** *[Category Theory for Programmers](https://bartoszmilewski.com/2014/10/28/category-theory-for-programmers-the-preface/)*. 
   *(A highly acclaimed resource that connects structural logic to Haskell, including the definitions of the Initial Object (0) and Terminal Object (1) purely structurally before ever introducing Functors and Monads).*
8. **Wadler, P. (1989).** *Theorems for free!*. 
   *(The definitive paper explaining parametricity in Category Theory and why we get free laws for our functions and data types).*

### Typeclasses, Functors, and Monads

9. **Bhargava, A.** *Functors, Applicatives, And Monads In Pictures*. 
   *(A highly recommended visual guide for grasping the basic intuition behind these three crucial typeclasses).*
10. **Moggi, E. (1991).** *Notions of computation and monads*. 
    *(The foundational paper introducing the concept of monads to programming languages to model side-effects).*
11. **Yorgey, B. (2009).** *[The Typeclassopedia](https://wiki.haskell.org/Typeclassopedia)*. The Monad Reader Issue 13. 
    *(The definitive guide to mapping out the core Haskell typeclasses, their relationships, and their mathematical laws).*

### Practical Haskell & Reasoning

12. **Danielsson, N. A., Hughes, J., Jansson, P., & Gibbons, J. (2006).** *Fast and Loose Reasoning is Morally Correct*. ACM SIGPLAN Notices, 41(1), 273-284. 
    *(A formal justification for reasoning about Haskell programs while ignoring `_|_`, widely accepted as standard practice in the Haskell community).*
13. **Diehl, S.** *[What I Wish I Knew When Learning Haskell](https://smunix.github.io/dev.stephendiehl.com/hask/tutorial.pdf)*. 
    *(A comprehensive guide to practical Haskell, covering many advanced type-level mechanics including `Void` and phantom types).*
14. **King, A. (2019).** *[Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)*. 
    *(A highly influential post demonstrating how to use the type system, including uninhabited types, to prove properties and prevent invalid states).*
15. **Leijen, D., & Meijer, E. (1999).** *Domain Specific Embedded Compilers*. ACM SIGPLAN Notices, 35(1), 109-122. 
    *(An early and influential paper showcasing the use of Phantom Types in Haskell).*
