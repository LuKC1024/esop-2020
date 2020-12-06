open import equivalence-of-cast-calculi.Type
open import equivalence-of-cast-calculi.C.BlameStrategies
open import equivalence-of-cast-calculi.S.CastADT

module equivalence-of-cast-calculi.Bisimulation.BisimulationRelation
  (Label : Set)
  (BS : BlameStrategy Label)
  (CADT : CastADT Label (BlameStrategy.Injectable BS))
  where

open import equivalence-of-cast-calculi.LabelUtilities Label
  renaming (negate-label×polarity to neg)
open BlameStrategy BS using (Injectable)

open import equivalence-of-cast-calculi.Type
open import equivalence-of-cast-calculi.CC Label hiding (Cast)
open import equivalence-of-cast-calculi.Error
open import equivalence-of-cast-calculi.Observable Label

module L where
  open BlameStrategy BS using (apply-cast) public
  open import equivalence-of-cast-calculi.CC Label using (Cast) public
  open import equivalence-of-cast-calculi.C.Value Label Injectable public
  open import equivalence-of-cast-calculi.C.Machine Label BS public
  
  open import Relation.Binary.PropositionalEquality using (_≡_; refl)
  
  data CastList : Type → Type → Set where
    [] : ∀ {T}
      → CastList T T
  
    _∷_ : ∀ {T1 T2 T3}
      → Cast T1 T2
      → CastList T2 T3
      → CastList T1 T3
  
  _++_ : ∀ {T1 T2 T3} → CastList T1 T2 → CastList T2 T3 → CastList T1 T3
  [] ++ ds    = ds
  (c ∷ cs) ++ ds = c ∷ (cs ++ ds)

  ++-assoc : ∀ {T1 T2 T3 T4}
    → (xs : CastList T1 T2)
    → (ys : CastList T2 T3)
    → (zs : CastList T3 T4)
    → (xs ++ (ys ++ zs)) ≡ ((xs ++ ys) ++ zs)
  ++-assoc []       ys zs = refl
  ++-assoc (x ∷ xs) ys zs rewrite ++-assoc xs ys zs = refl


  ⟦_⟧ : ∀ {T1 T2}
    → CastList T1 T2
    → (Value T1 → Error Label×Polarity (Value T2))
  ⟦_⟧ [] = return
  ⟦_⟧ (x ∷ xs) = apply-cast x >=> ⟦_⟧ xs
  
  lem-id : ∀ {T}
    → (v : Value T)
    → ⟦_⟧ [] v ≡ return v
  lem-id v = refl

  lem-seq : ∀ {T1 T2 T3}
    → (xs : CastList T1 T2)
    → (ys : CastList T2 T3)
    → (∀ v → ⟦_⟧ (xs ++ ys) v ≡ (⟦_⟧ xs >=> ⟦_⟧ ys) v)
  lem-seq [] ys v = refl
  lem-seq (x ∷ xs) ys v with apply-cast x v
  ... | return v' = lem-seq xs ys v'
  ... | raise l = refl

open L hiding (lookup; _—→_; Cast)


module R where
  open CastADT CADT public
  open import equivalence-of-cast-calculi.S.Value Label Injectable Cast public
  open import equivalence-of-cast-calculi.S.Machine Label Injectable CADT public
  open import equivalence-of-cast-calculi.Chain using (_++_) public

open R hiding (lookup; id; _⨟_; _—→_; [□⟨_⟩]_; Cast)


data CastListRelate : {S T : Type} → CastList S T → R.Cast S T → Set where
  id : ∀ {T}
    → CastListRelate {T} {T} [] (R.id T)

  just : ∀ {T1 T2}
    → (c : L.Cast T1 T2)
    → CastListRelate (c ∷ []) R.⌈ c ⌉

  _⨟_ : ∀ {T1 T2 T3}
    → {lcs : CastList  T1 T2}
    → {rcs : R.Cast T1 T2}
    → (cs : CastListRelate lcs rcs)
    → {lds : CastList  T2 T3}
    → {rds : R.Cast T2 T3}
    → (ds : CastListRelate lds rds)
    -----
    → CastListRelate (lcs L.++ lds) (rcs R.⨟ rds)

lcast : ∀ {T1 T2 lc rc} → CastListRelate {T1} {T2} lc rc → CastList T1 T2
lcast {lc = lc} c = lc

rcast : ∀ {T1 T2 lc rc} → CastListRelate {T1} {T2} lc rc → R.Cast T1 T2
rcast {rc = rc} c = rc

data FCastList : Type → Type → Type → Type → Set where
  [] : ∀ {T11 T12}
    → FCastList T11 T12 T11 T12
  _⟪_ : ∀ {T11 T12 T21 T22 T31 T32}
    → FCastList T11 T12 T21 T22
    → L.Cast (` T21 ⇒ T22) (` T31 ⇒ T32)
    → FCastList T11 T12 T31 T32

view-lambda : ∀ {T11 T12 T21 T22}
  → L.Value (` T11 ⇒ T12)
  → FCastList T11 T12 T21 T22
  → L.Value (` T21 ⇒ T22)
view-lambda v [] = v
view-lambda v (cs ⟪ c) = view-lambda v cs ⇒⟨ c ⟩

dom : ∀ {T11 T12 T21 T22} → FCastList T11 T12 T21 T22 → CastList T21 T11
dom [] = []
dom (cs ⟪ ((` T21 ⇒ T22) ⟹[ l ] (` T31 ⇒ T32)))
  = ( T31 ⟹[ neg l ] T21 ) ∷ dom cs

cod : ∀ {T11 T12 T21 T22} → FCastList T11 T12 T21 T22 → CastList T12 T22
cod [] = []
cod (cs ⟪ ((` T21 ⇒ T22) ⟹[ l ] (` T31 ⇒ T32)))
  = (cod cs) L.++ ((T22 ⟹[ l ] T32) ∷ [])

data PCastList : Type → Type → Type → Type → Set where
  [] : ∀ {T11 T12}
    → PCastList T11 T12 T11 T12
  _⟪_ : ∀ {T11 T12 T21 T22 T31 T32}
    → PCastList T11 T12 T21 T22
    → L.Cast (` T21 ⊗ T22) (` T31 ⊗ T32)
    → PCastList T11 T12 T31 T32

view-cons : ∀ {T11 T12 T21 T22}
  → L.Value (` T11 ⊗ T12)
  → PCastList T11 T12 T21 T22
  → L.Value (` T21 ⊗ T22)
view-cons v [] = v
view-cons v (cs ⟪ c) = view-cons v cs ⊗⟨ c ⟩

lft : ∀ {T11 T12 T21 T22} → PCastList T11 T12 T21 T22 → CastList T11 T21
lft [] = []
lft (cs ⟪ ((` T21 ⊗ T22) ⟹[ l ] (` T31 ⊗ T32)))
  = lft cs L.++ ((T21 ⟹[ l ] T31) ∷ [])

rht : ∀ {T11 T12 T21 T22} → PCastList T11 T12 T21 T22 → CastList T12 T22
rht [] = []
rht (cs ⟪ ((` T21 ⊗ T22) ⟹[ l ] (` T31 ⊗ T32)))
  = rht cs L.++ ((T22 ⟹[ l ] T32) ∷ [])

data ErrorRelate {A B : Set} (A~B : A → B → Set) :
  Error Label×Polarity A → Error Label×Polarity B → Set
  where
  return : {a : A}{b : B} → (a~b : A~B a b) → ErrorRelate A~B (return a) (return b)
  raise : ∀ l → ErrorRelate A~B (raise l) (raise l)

mutual
  data EnvRelate : ∀ {Γ} → L.Env Γ → R.Env Γ → Set where
    []  : EnvRelate [] []
    _∷_ : ∀ {Γ T}
      → {v : L.Value T}{u : R.Value T}
      → ValueRelate v u
      → {E : L.Env Γ}{F : R.Env Γ}
      → EnvRelate E F
      → EnvRelate (v ∷ E) (u ∷ F)

  data ValueRelate : ∀ {T} → L.Value T → R.Value T → Set where
    dyn : ∀ {P}
      → (Pi : Injectable P)
      → {lv : L.Value (` P)}
      → {rv : R.Value (` P)}
      → ValueRelate lv rv
      ----------------
      → ValueRelate (dyn Pi lv) (dyn Pi rv)

    #t : ValueRelate #t #t
    #f : ValueRelate #f #f

    lam⟨_,_⇒_⟩ : ∀ {Γ T11 T12 T21 T22}
      → (lcs : FCastList T11 T12 T21 T22)
      → {rc1 : R.Cast T21 T11}
      → {rc2 : R.Cast T12 T22}
      → CastListRelate (dom lcs) rc1
      → CastListRelate (cod lcs) rc2
      → (e : Γ , T11 ⊢ T12)
      → {lE : L.Env Γ}
      → {rE : R.Env Γ}
      → (E : EnvRelate lE rE)
      ------
      → ValueRelate (view-lambda (lam e lE) lcs) (lam⟨ rc1 ⇒ rc2 ⟩ e rE)

    cons⟨_,_⊗_⟩ : ∀ {T1 T2 T3 T4}
      → (lcs : PCastList T1 T2 T3 T4)
      → {rc1 : R.Cast T1 T3}
      → {rc2 : R.Cast T2 T4}
      → CastListRelate (lft lcs) rc1
      → CastListRelate (rht lcs) rc2
      → {lv1 : L.Value T1}
      → {rv1 : R.Value T1}
      → (v1 : ValueRelate lv1 rv1)
      → {lv2 : L.Value T2}
      → {rv2 : R.Value T2}
      → (v2 : ValueRelate lv2 rv2)
      ----
      → ValueRelate (view-cons (cons lv1 lv2) lcs) (cons⟨ rc1 ⊗ rc2 ⟩ rv1 rv2)

lenv : ∀ {T}
  → {v : L.Env T}
  → {u : R.Env T}
  → EnvRelate v u
  → L.Env T
lenv {v = v} vr = v

renv : ∀ {T}
  → {v : L.Env T}
  → {u : R.Env T}
  → EnvRelate v u
  → R.Env T
renv {u = u} vr = u

lvalue : ∀ {T}
  → {v : L.Value T}
  → {u : R.Value T}
  → ValueRelate v u
  → L.Value T
lvalue {v = v} vr = v

rvalue : ∀ {T}
  → {v : L.Value T}
  → {u : R.Value T}
  → ValueRelate v u
  → R.Value T
rvalue {u = u} vr = u

lookup : ∀ {Γ T}
  → {lE : L.Env Γ}
  → {rE : R.Env Γ}
  → (E : EnvRelate lE rE)
  → (x : Γ ∋ T)
  → ValueRelate (L.lookup lE x) (R.lookup rE x)
lookup (c ∷ E) zero = c
lookup (c ∷ E) (suc x) = lookup E x

CastResultRelate : ∀ {T} → Error Label×Polarity (L.Value T) → Error Label×Polarity (R.Value T)
  → Set
CastResultRelate = ErrorRelate ValueRelate

data FrameRelate : ∀ {S T} → L.Frame S T → R.Frame S T → Set where
  app₁ : ∀ {Γ S T}
    → (e : Γ ⊢ S)
    → {lE : L.Env Γ}
    → {rE : R.Env Γ}
    → (E : EnvRelate lE rE)
    --------
    → FrameRelate {` S ⇒ T} {T} (app₁ e lE) (app₁ e rE)
                          
  app₂ : ∀ {S T}
    → {lv : L.Value (` S ⇒ T)}
    → {rv : R.Value (` S ⇒ T)}
    → (v : ValueRelate lv rv)
    --------
    → FrameRelate (app₂ lv) (app₂ rv)

  if₁ : ∀ {Γ T}
    → (e2 : Γ ⊢ T)
    → (e3 : Γ ⊢ T)
    → {lE : L.Env Γ}
    → {rE : R.Env Γ}
    → (E : EnvRelate lE rE)
    ---------
    → FrameRelate (if₁ e2 e3 lE) (if₁ e2 e3 rE)

  cons₁ : ∀ {Γ T1 T2}
    → (e2 : Γ ⊢ T2)
    → {lE : L.Env Γ}
    → {rE : R.Env Γ}
    → (E : EnvRelate lE rE)
    -----
    → FrameRelate {T1} (cons₁ e2 lE) (cons₁ e2 rE)

  cons₂ : ∀ {T1 T2}
    → {lv1 : L.Value T1}
    → {rv1 : R.Value T1}
    → (v1 : ValueRelate lv1 rv1)
    -----
    → FrameRelate {T2} (cons₂ lv1) (cons₂ rv1)

  fst₁ : ∀ {T1 T2} → FrameRelate {` T1 ⊗ T2} {T1} fst₁ fst₁
  snd₁ : ∀ {T1 T2} → FrameRelate {` T1 ⊗ T2} {T2} snd₁ snd₁

view-cont : ∀ {T1 T2 T3}
  → CastList T1 T2
  → L.Cont T2 T3
  → L.Cont T1 T3
view-cont [] k    = k
view-cont (c ∷ cs) k = [ □⟨ c ⟩ ] view-cont cs k

mutual
  data ContRelate : {T1 T2 : Type} → L.Cont T1 T2 → R.Cont T1 T2 → Set where
    [□⟨_⟩]_ : ∀ {T1 T2 T3 lc rc lk rk}
      → (c : CastListRelate {T1} {T2} lc rc)
      → (k : PreContRelate {T3} lk rk)
      ---
      → ContRelate (view-cont lc lk) (R.[□⟨ rc ⟩] rk)
    
  data PreContRelate {Z : Type} : ∀ {S} → L.Cont S Z → R.PreCont S Z → Set where
    □ : PreContRelate □ □

    [_] : ∀ {T1 T2}
      → {lF : L.Frame T1 T2}
      → {rF : R.Frame T1 T2}
      → (F : FrameRelate lF rF)
      → {lk : L.Cont T2 Z}
      → {rk : R.Cont T2 Z}
      → (k : ContRelate lk rk)
      ---
      → PreContRelate ([ lF ] lk) ([ rF ] rk)
  
lcont : ∀ {T1 T2}
  → {lk : L.Cont T1 T2}
  → {rk : R.Cont T1 T2}
  → (kk : ContRelate lk rk)
  ---
  → L.Cont T1 T2
lcont {lk = lk} kk = lk

rcont : ∀ {T1 T2}
  → {lk : L.Cont T1 T2}
  → {rk : R.Cont T1 T2}
  → (kk : ContRelate lk rk)
  ---
  → R.Cont T1 T2
rcont {rk = rk} kk = rk

lprecont : ∀ {T1 T2}
  → {lk : L.Cont T1 T2}
  → {rk : R.PreCont T1 T2}
  → (kk : PreContRelate lk rk)
  ---
  → L.Cont T1 T2
lprecont {lk = lk} kk = lk

rprecont : ∀ {T1 T2}
  → {lk : L.Cont T1 T2}
  → {rk : R.PreCont T1 T2}
  → (kk : PreContRelate lk rk)
  ---
  → R.PreCont T1 T2
rprecont {rk = rk} kk = rk

data OrdinaryStateRelate {Z : Type} : L.OrdinaryState Z → R.OrdinaryState Z
  → Set where
  expr : ∀ {Γ T1}
    → (e : Γ ⊢ T1)
    → {lE : L.Env Γ}
    → {rE : R.Env Γ}
    → (E : EnvRelate lE rE)
    → {lK : L.Cont T1 Z}
    → {rK : R.Cont T1 Z}
    → (K : ContRelate lK rK)
    ------------
    → OrdinaryStateRelate (expr e lE lK) (expr e rE rK)
    
  cont : ∀ {T}
    → {lv : L.Value T}
    → {rv : R.Value T}
    → (v : ValueRelate lv rv)
    → {lK : L.Cont T Z}
    → {rK : R.Cont T Z}
    → (K : ContRelate lK rK)
    ------------
    → OrdinaryStateRelate (cont lv lK) (cont rv rK)

  halt : ∀ {lv rv}
    → (v : ValueRelate lv rv)
    → OrdinaryStateRelate (halt lv) (halt rv)
    
StateRelate : ∀ {T} → L.State T → R.State T → Set
StateRelate = ErrorRelate OrdinaryStateRelate

lstate : ∀ {T}
  → {ls : L.State T}
  → {rs : R.State T}
  → (ss : StateRelate ls rs)
  ---
  → L.State T
lstate {ls = ls} ss = ls

rstate : ∀ {T}
  → {ls : L.State T}
  → {rs : R.State T}
  → (ss : StateRelate ls rs)
  ---
  → R.State T
rstate {rs = rs} ss = rs
