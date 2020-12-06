module equivalence-of-cast-calculi.Observable
  (Label : Set)
  where

open import equivalence-of-cast-calculi.Type
open import equivalence-of-cast-calculi.LabelUtilities Label

data ValueDisplay : Type → Set where
  dyn : ValueDisplay *

  #t : ValueDisplay (` B)
  #f : ValueDisplay (` B)

  lam : ∀ {T1 T2}
    ---
    → ValueDisplay (` T1 ⇒ T2)
    
  cons : ∀ {T1 T2}
    → ValueDisplay (` T1 ⊗ T2)

open import equivalence-of-cast-calculi.Error

Observable : Type → Set
Observable T = Error Label×Polarity (ValueDisplay T)
