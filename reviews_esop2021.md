# Reviews (ESOP 2021)

This file contains reviews given when submitted to ESOP 2021

## Review 1, Before Rebuttal

Overall evaluation:	
-2: (reject)

SUMMARY

This paper introduces "hypercoercions" which is a compressed representation of casts in gradual type systems that has a point of difference to quite a few existing ones by having a mechanised proof in Agda and being generally more amenable to mechanisation. After a very basic and standard (even vs odd) introduction to the problem of coercions growing in size for every stack frame despite tail recursion and other traditional examples, the paper claims to have an improved approach compared to the current state of the art (mostly by the same authors despite other work). The link to the implementation is broken (https://github.com/LuKC1024/esop-2020 gives 404 error) and the Related Work is very hastily written.

After a lengthy background outlining the abstract machine to represent the up and down casts (the Figure 3 being the core summary), the claim is made that the existing coercions approaches are covered by this set up. Then the core contribution is in Section 4 with the main proof using (weak) bisimulation between C(B) (casts in general from Section 3) and S(C) (space efficient proposal - see Figure 8 for summary). The hypercoercions are then presented in the remaining core Section 5.

EVALUATION

The paper does not do a good job in either abstract or introduction explaining the novelty of this approach or what exactly are the reasons for mechanisation of bisimulation proofs in the existing coercions work being so hard. Even after reading the introduction with contributions it is unclear to the reader why mechanising hypercoercions in Agda is a very important or hard (as opposed to BAU) contribution. After all, the space efficiency is just a one paragraph outline of a proposal, not a core contribution overall. I would highly recommend stating what is the issue with mechanisation of the existing work in the abstract and making it clear what is the hard/main point of difference of hypercoercions in the introduction.

Finally, here is an example of recent published work on coercions that I think is of relevance to the authors (I don't think you cite this as most of your citations are naturally from your group?):

https://2020.ecoop.org/details/ecoop-2020-papers/8/Space-Efficient-Gradual-Typing-in-Coercion-Passing-Style

(They modified Grift to handle your odd/even tail recursion case efficiently.)

There are many many many grammatical issues in the paper with the main three core issues being:

(1) use of articles (e.g. "applicable to *the* other cast representations" in the abstract);

(2) use of "casted" - there is no such word (https://writingexplained.org/cast-vs-casted);

(3) use of subject and verb in the same number (https://webapps.towson.edu/ows/sub-verb.htm).

## Review 1, After Rebuttal

-1: (weak reject)
SUMMARY

This paper introduces "hypercoercions" which is a compressed representation of casts in gradual type systems that has a point of difference to quite a few existing ones by having a mechanised proof in Agda and being generally more amenable to mechanisation. After a very basic and standard (even vs odd) introduction to the problem of coercions growing in size for every stack frame despite tail recursion and other traditional examples, the paper claims to have an improved approach compared to the current state of the art (mostly by the same authors despite other work). The link to the implementation is broken (https://github.com/LuKC1024/esop-2020 gives 404 error) and the Related Work is very hastily written.

After a lengthy background outlining the abstract machine to represent the up and down casts (the Figure 3 being the core summary), the claim is made that the existing coercions approaches are covered by this set up. Then the core contribution is in Section 4 with the main proof using (weak) bisimulation between C(B) (casts in general from Section 3) and S(C) (space efficient proposal - see Figure 8 for summary). The hypercoercions are then presented in the remaining core Section 5.

EVALUATION

The paper does not do a good job in either abstract or introduction explaining the novelty of this approach or what exactly are the reasons for mechanisation of bisimulation proofs in the existing coercions work being so hard. Even after reading the introduction with contributions it is unclear to the reader why mechanising hypercoercions in Agda is a very important or hard (as opposed to BAU) contribution. After all, the space efficiency is just a one paragraph outline of a proposal, not a core contribution overall. I would highly recommend stating what is the issue with mechanisation of the existing work in the abstract and making it clear what is the hard/main point of difference of hypercoercions in the introduction.

Finally, here is an example of recent published work on coercions that I think is of relevance to the authors (I don't think you cite this as most of your citations are naturally from your group?):

https://2020.ecoop.org/details/ecoop-2020-papers/8/Space-Efficient-Gradual-Typing-in-Coercion-Passing-Style

(They modified Grift to handle your odd/even tail recursion case efficiently.)

There are many many many grammatical issues in the paper with the main three core issues being:

(1) use of articles (e.g. "applicable to *the* other cast representations" in the abstract);

(2) use of "casted" - there is no such word (https://writingexplained.org/cast-vs-casted);

(3) use of subject and verb in the same number (https://webapps.towson.edu/ows/sub-verb.htm).

AFTER REBUTTAL

Thanks for answering my question and indeed tail recursion optimisation is a bit different. However, the other concerns as shown by the other reviewers and discussion remain and we all hope that after additional work and a solid rewrite to fixed the minor issues this paper will be published somewhere!

## Review 2, Before Rebuttal

-1: (weak reject)
The authors present a proof framework, implemented in Agda, for
proving the correctness of space-efficient cast calculi for gradually
typed languages by showing them equivalent to certain standard
semantics. Then a new flavor of space efficient cast calculus dubbed
"hypercoercions" is presented and proven correct using this
framework. Hypercoercions are claimed to be closer to a realistic
low-level representation of space efficient calculi.

I certainly have mixed feelings about this work. On the one hand, due
to their complexity, it is absolutely critical that optimized
representations of casts be proven correct with respect to simpler
specifications and so providing some kind of reusable proof framework
for this is a laudable goal.

However, given the evidence presented in this paper, (1) I question
the generality of the framework and (2) I question the claim that
hypercoercions are "more suitable for implementation" than coercions
in normal form.

First, let's discuss the generality of the proof framework. The
parameter of the framework is an ADT consisting of a datatype for
Cast[S,T] for each type S,T, implementing id and composition
operations and implementing an action on values such that id is the
identity action and composition is the composition of actions. Then
the framework is based on (1) isolating all cast cases in the
operational semantics, (2) providing specific correctness criteria for
the action on values and then (3) from those correctness criteria
providing a bisimulation proof to a standard semantics. The
correctness criteria come in 2 variants: one for the Lazy D strategy
and one for the Lazy UD strategy, which differ only in blame. The
authors note that they would need to adapt the framework if they used
eager function casts, but lazy function casts are more
common. However, it looks like they missed that they have adopted lazy
*product* casts, whereas *eager* product casts are more common in the
literature and practice, for instance in Typed Racket.

By lazy product casts, I mean that a cast <S x T => S' x
T'>cons(v1,v2) is a value in this system and when fst is applied the
term reduces to <S => S'>v1 (similarly for snd). In eager product
semantics, <S x T => S' x T'>cons(v1, v2) would reduce to cons(<S =>
S'>v1, <T => T'>v2) and this would trigger the evaluation of further
casts. Eager product casts are preferred in call-by-value languages
like Typed Racket, because they preserve the safety of operations like
fst/snd whereas in lazy product semantics, projections may trigger
casts. In the language of New et al., lazy function casts are the only
ones that preserve eta for functions, but eager product casts are the
only ones that preserve eta for Call-by-value products.

I emphasize this because it calls into question how re-usable this
framework is. Is this supposed to be a framework that others are meant
to extend? If so then this is an obstacle for most
theorists/practitioners from doing so. I think ultimately adapting the
system to accommodate either product semantics is feasible, but it is
not negligible work.

Next, I question the idea that hypercoercions are significantly "more
suitable for implementation" than the coercions in normal form
are. The bit-level representation of coercions presented in Section
5.3 is a very sensible proposal for how to represent coercions at
runtime, but it doesn't seem significantly more similar to
hypercoercions than coercions. Hypercoercions simply note that the
coercions in normal form (Fig. 6) can be re-arranged to consist of an
optional projection, followed by a "middle coercion", followed by an
optional injection. The equivalence of this notion to coercions is
apparent by inspection of Figure 6. The bit level representation is
different from these in that its structure is based on (1)
distinguishing between identity and not (2) distinguishing between
base type (immediate) and more complex (heap-allocated) types. A
closer abstract representation to this in BNF would be the following
(let's call them "low-level coercions"):

llc (low-level coercion) ::= id | bc | cc
bc (base coercion) ::= < prj? , basety , bool >
cc (complex coercion) ::= < prj? , prety , bool >
prj? ::= l | *

The grammar here much more closely matches how low-level code would
distinguish between different cases and so the Agda code to implement
it would be much closer to the actual low level implementation than
that of hypercoercions. There would need to be separate cases for
composing base coercions with base coercions, base coercions with
complex coercions, two complex coercions, etc. that would reflect
cases of the implementation. It also shows that the implementation
must inspect the base type itself to determine what the projection is,
etc. It seems to me that the conceptual gap between this
representation and hypercoercions is at least as "big" as the
conceptual gap between hypercoercions and coercions in normal form,
which is why I question the claim that the hypercoercion presentation
is significantly more "low level" than normal form coercions. If
anything, I would say that "hypercoercions" are more "high-level" in
that their structure is simpler to understand. On the other hand, I
think proving the correctness of something like low-level coercions
would be a better example of the utility of the framework then
hypercoercions.

As practical advice to the authors, I would suggest improving on the
work in the following ways:

1. Implement or explain how you would adapt the framework to
accommodate, eager product semantics.
2. Prove the correctness of something more like the "low-level
coercions" I described that more closely matches your proposal for
a low-level implementation.
3. Focus more in the body of the paper on higher-level questions of
generality and adaptability of the framework rather than focusing
on all of the details of what is a fairy standard bisimulation
proof.
4. Provide a better introduction in the paper to the difference
between Lazy D and Lazy UD. For instance, it is never stated that
the only difference between the strategies is what blame label is
raised, and that the two do not differ in if errors occur.

Below I have included some smaller errors/notes:
Page 3: "10 different kinds" kinds of what? types?

Page 4: should "Lazy UD coercions in normal form" be "Lazy D
coercions in normal form"? Otherwise did you only prove the Lazy D
hypercoercions correct with respect to the Lazy D semantics?

The discussion of things being "easier to understand" is fuzzy
and a bit unscientific.


Is the only difference between Lazy D and Lazy UD blame?

2.1: comparison to Siek 2020 is pretty opaque.

Page 11: "three parts..." "function coercion" should be "function or
product" coercion or maybe more generally "connective coercion"?

Page 12: Definition 4, last bullet: "let v <- [c](v) in [d](v)"
preferable to write it as "let v' <- [c](v) in [d](v')" to avoid the
reader needing to reason about binding?

Page 16: Definition 6: same as previous comment, but additionally
you write "A Cast is ..." and I think you meant "A Cast ADT is ..."

## Review 2, After Rebuttal

-1: (weak reject)
The authors present a proof framework, implemented in Agda, for
proving the correctness of space-efficient cast calculi for gradually
typed languages by showing them equivalent to certain standard
semantics. Then a new flavor of space efficient cast calculus dubbed
"hypercoercions" is presented and proven correct using this
framework. Hypercoercions are claimed to be closer to a realistic
low-level representation of space efficient calculi.

I certainly have mixed feelings about this work. On the one hand, due
to their complexity, it is absolutely critical that optimized
representations of casts be proven correct with respect to simpler
specifications and so providing some kind of reusable proof framework
for this is a laudable goal.

However, given the evidence presented in this paper, (1) I question
the generality of the framework and (2) I question the claim that
hypercoercions are "more suitable for implementation" than coercions
in normal form.

First, let's discuss the generality of the proof framework. The
parameter of the framework is an ADT consisting of a datatype for
Cast[S,T] for each type S,T, implementing id and composition
operations and implementing an action on values such that id is the
identity action and composition is the composition of actions. Then
the framework is based on (1) isolating all cast cases in the
operational semantics, (2) providing specific correctness criteria for
the action on values and then (3) from those correctness criteria
providing a bisimulation proof to a standard semantics. The
correctness criteria come in 2 variants: one for the Lazy D strategy
and one for the Lazy UD strategy, which differ only in blame. The
authors note that they would need to adapt the framework if they used
eager function casts, but lazy function casts are more
common. However, it looks like they missed that they have adopted lazy
*product* casts, whereas *eager* product casts are more common in the
literature and practice, for instance in Typed Racket.

By lazy product casts, I mean that a cast <S x T => S' x
T'>cons(v1,v2) is a value in this system and when fst is applied the
term reduces to <S => S'>v1 (similarly for snd). In eager product
semantics, <S x T => S' x T'>cons(v1, v2) would reduce to cons(<S =>
S'>v1, <T => T'>v2) and this would trigger the evaluation of further
casts. Eager product casts are preferred in call-by-value languages
like Typed Racket, because they preserve the safety of operations like
fst/snd whereas in lazy product semantics, projections may trigger
casts. In the language of New et al., lazy function casts are the only
ones that preserve eta for functions, but eager product casts are the
only ones that preserve eta for Call-by-value products.

I emphasize this because it calls into question how re-usable this
framework is. Is this supposed to be a framework that others are meant
to extend? If so then this is an obstacle for most
theorists/practitioners from doing so. I think ultimately adapting the
system to accommodate either product semantics is feasible, but it is
not negligible work.

Next, I question the idea that hypercoercions are significantly "more
suitable for implementation" than the coercions in normal form
are. The bit-level representation of coercions presented in Section
5.3 is a very sensible proposal for how to represent coercions at
runtime, but it doesn't seem significantly more similar to
hypercoercions than coercions. Hypercoercions simply note that the
coercions in normal form (Fig. 6) can be re-arranged to consist of an
optional projection, followed by a "middle coercion", followed by an
optional injection. The equivalence of this notion to coercions is
apparent by inspection of Figure 6. The bit level representation is
different from these in that its structure is based on (1)
distinguishing between identity and not (2) distinguishing between
base type (immediate) and more complex (heap-allocated) types. A
closer abstract representation to this in BNF would be the following
(let's call them "low-level coercions"):

llc (low-level coercion) ::= id | bc | cc
bc (base coercion) ::= < prj? , basety , bool >
cc (complex coercion) ::= < prj? , prety , bool >
prj? ::= l | *

The grammar here much more closely matches how low-level code would
distinguish between different cases and so the Agda code to implement
it would be much closer to the actual low level implementation than
that of hypercoercions. There would need to be separate cases for
composing base coercions with base coercions, base coercions with
complex coercions, two complex coercions, etc. that would reflect
cases of the implementation. It also shows that the implementation
must inspect the base type itself to determine what the projection is,
etc. It seems to me that the conceptual gap between this
representation and hypercoercions is at least as "big" as the
conceptual gap between hypercoercions and coercions in normal form,
which is why I question the claim that the hypercoercion presentation
is significantly more "low level" than normal form coercions. If
anything, I would say that "hypercoercions" are more "high-level" in
that their structure is simpler to understand. On the other hand, I
think proving the correctness of something like low-level coercions
would be a better example of the utility of the framework then
hypercoercions.

As practical advice to the authors, I would suggest improving on the
work in the following ways:

1. Implement or explain how you would adapt the framework to
accommodate, eager product semantics.
2. Prove the correctness of something more like the "low-level
coercions" I described that more closely matches your proposal for
a low-level implementation.
3. Focus more in the body of the paper on higher-level questions of
generality and adaptability of the framework rather than focusing
on all of the details of what is a fairy standard bisimulation
proof.
4. Provide a better introduction in the paper to the difference
between Lazy D and Lazy UD. For instance, it is never stated that
the only difference between the strategies is what blame label is
raised, and that the two do not differ in if errors occur.

Below I have included some smaller errors/notes:
Page 3: "10 different kinds" kinds of what? types?

Page 4: should "Lazy UD coercions in normal form" be "Lazy D
coercions in normal form"? Otherwise did you only prove the Lazy D
hypercoercions correct with respect to the Lazy D semantics?

The discussion of things being "easier to understand" is fuzzy
and a bit unscientific.


Is the only difference between Lazy D and Lazy UD blame?

2.1: comparison to Siek 2020 is pretty opaque.

Page 11: "three parts..." "function coercion" should be "function or
product" coercion or maybe more generally "connective coercion"?

Page 12: Definition 4, last bullet: "let v <- [c](v) in [d](v)"
preferable to write it as "let v' <- [c](v) in [d](v')" to avoid the
reader needing to reason about binding?

Page 16: Definition 6: same as previous comment, but additionally
you write "A Cast is ..." and I think you meant "A Cast ADT is ..."


-- COMMENTS AFTER AUTHORS' RESPONSE --

Thanks to the authors for the carefully-written response. Still, this paper, while being promising, looks like it is still in its early stage and it needs more work to be publishable.

## Review 3, Before Rebuttal

0: (borderline paper)
== Summary

Motivated by bridging the gap between simple theoretical models and efficient
implementations of gradual typing, there exist a lot of works that propose
efficient cast representations. However, the proof works to verify equivalence
between the efficient casts and the original, inefficient casts have been done
individually. This paper presents a framework built on the top of Agda that
extracts an essence of cast semantics and reduces the proof burden for cast
language equivalence to implementing Cast Abstract Data Type (Cast ADT) with a
few operations enjoying some axioms. The paper further defines two variants of
the Cast ADT, Lazy D Cast ADT and Lazy UD Cast ADT, and proves that the cast
semantics implemented with each is correct with respect to the corresponding
cast semantics (Lazy D or Lazy UD). The usefulness of the framework is
demonstrated by applications to Lazy UD coercions in normal form and Lazy UD
threesomes (I cannot find them after the introduction, though). Motivated by
achieving both mechanizability and simplicity, the paper also proposes a new
cast representation, called hypercoercions, for each of Lazy D and Lazy UD,
proves its correctness through the proposed framework, and discusses their
potential for practice use.


== Evaluation

Pros:

- I really enjoy reading the paper. The attention to efficiency has been
increasingly growing in the gradual typing community. The fact that space
efficiency is one of active topics is proven by a lots of works in the
literature. Since the seminal work by Herman et al., it has been known that
coercion composition or collapsing casts is key in space efficiency, but, in
my opinion, the techniques for space efficiency make cast languages
complicated and so are not very suitable when one is interested in other
theoretical aspects of gradual typing, such as gradual guarantee and
parametricity. Thus, proving correctness of the efficient cast
representations is crucial to bridge the gap between theory and
implementation. This paper absolutely contributes to reducing its proof
burden by identifying reasonable sufficient conditions formalized by Cast ADT,
Lazy D Cast ADT, and Lazy UD Cast ADT.

- I also like hypercoercions, which are a very simple and intuitive
representation of coercions in normal form (I think the presentation of Lazy D
hypercoercions could be improved as noted below, though).

- The writing is almost clear. It provides a gentle review of the previous work
and then introduces the present work in a clear manner, except for a few
parts.


Cons:

- My major criticism is that it seems that the paper does not concretely
present the implementations of Lazy UD threesomes and Lazy UD coercions in
normal form within the proposed framework. I feel this critical because an
aim of the paper is to provide a framework for proving correctness of
efficient cast representations. Lacking the presentation makes me concerned
about whether the aim is achieved. I feel hypercoercions do not work for
this purpose, because they are not proven space efficient, yet. I hope the
paper presents an implementation of, at least, either threesomes or
coercions in normal form.

- Lacking the implementation of coercions in normal form also makes it
difficult to precisely identify the contributions of hypercoercions to
mechanization. In fact, I fail to grasp the challenges in mechanizing
coercions in normal form.

- I also have a technical concern with determinism of S(C). The semantics of
S(C) generates coercions at run time. For example, the evaluation of a state
"<\lambda x:S. e, E, K>" must infer a return type T as well as a type
environment \Gamma (the second rule in Figure 8). In order for S(C) to be
deterministic, T and \Gamma must be unique; otherwise, s_1 and s_2 in
Proposition 3 may be syntactically different. I expect T and \Gamma for
lambda abstractions could be uniquely inferred, but a similar, (perhaps) more
serious issue would happen in the function mkK. mkK tops a pre-continuation k
with an identity cast id(S). For that, the type S is inferred from the typing
of k. However, it seems that k can be of multiple types. For example, if k
is \box, I guess k can be of T \Rightarrow T for any T (I only guessed because
the inference rules of the continuation typing seem to be omitted). This
would indicate that a continuation mkK(k) may not be uniquely determined (put
differently, mkK would not be well defined as a function). As a result, I'm
concerned that S(C) might be nondeterministic because s_1 and s_2 in
Proposition 3 may include different identity coercions inserted by mkK. This
issue could be solved by annotating \box in pre-continuations, continuations,
and frames with types. Anyway, I think the paper must prove uniqueness of
each typing, including cast typing, to show determinism.


Suggestion:

I would like to see more discussions in the paper, like the followings.

- The semantics of S(C) depends on the design decision that a frame is paired
with a coercion. I think this unsuitable for practical compiler
implementations because it inevitably incurs a large overhead by a huge
amount of identity casts mkK will insert. I would like see discussions
about differences between a "space-efficient" model in the framework and a
compiler-level implementation of it.

- I also would like to see future work, e.g., handling Eager D/UD (specifically,
eager product casts) and prospects of supporting other SOTA approaches to
space efficiency.

- The paper provides a framework for correctness. I am curious about whether
it is possible to construct a framework for proving space efficiency (I
don't expect mechanization for that, because it would make the work quite
hard).


== Comments/Typos

- Are all properties in the paper (including determinism) proven on the top of
Agda? If so, please explicitly note it in the paper. If not, it would be
quite nice to spell out which is done by pencil-and-paper and to present the
proofs in Appendix.

- p1, abstract, "The proofs of correctness": It would be nicer to spell out
what correctness means in this paper.

- p3, "we apply the framework to Lazy UD coercions in normal form ... and Lazy UD threesomes:
It would be nice to explain Lazy UD here.

- p4, "a more compact memory representation": I cannot find the discussion that
claims hypercoercions are _more_ compact than coercions in normal form.
Section 5.3 just describes compactness of hypercoercions.

- Section 2: I feel discussing related works here is not effective because
C(B) and S(C) have not been introduced yet. I suggest moving it to the place,
e.g., before the conclusion.

- p5: but doesn't normalize --> does not

- p7, Figure 2: Why does not the typing rules of casts require the source and
target types consistent?

- p7, p9: by specify blame strategies --> specifying

- p12, Figure 7: It would be nicer to clarify who provides injectable types.
Done by an instance of Cast ADT or a blame strategy?

- p12, "Value typing, continuation typing, and environment typing are defined in the obvious way":
Are they completely omitted? If so, it would be helpful to put them in
Appendix.

- p12, Definition 4:

-- "indexed by two types": It would be nicer to spell out they are source and
target types of the casts in the Cast ADT.

-- Do the definitions of the operations and properties assume the indexing types
to be denoted by S and T?

-- What are the types of coercions appearing in the composition c;c and [[c]]?

-- Is id(T) only in Cast_(T,T)? If S is not equal to T, neither id(T) nor
id(S) is in Cast(T,S)?

- p13: "we are to defined" --> define ?

- p15, Definitions 5 and 6: I suppose these definitions formulate axioms (or
properties) to be satisfied by instances of Lazy D Cast ADT and Lazy UD Cast
ADT, respectively. In either case, the paper could more clearly spell out
what these definitions represent.

- p15, Definition 6, "in [[ G \Rightarrow^l \star ]] (v)" in the third clause:
It seems \lceil and \rceil are missed.

- p15, "\vdash o : T": Where has this notation been introduced?

- p17: "al possibly ways" --> possible

- p17, Lemmas 1 and 2: "and \vdash v \approx v' : S [. Then]"

- p17, the proofs of Lemmas 1 and 2: Please consider handling the case of "if no".

- p18, Lemma 3:

-- "[. T]hen either"

-- "1. s1 final and s1' final": I think this statement does not reflect the
intention of the paper because it allows the case that s1 is blame and s2 is
a value as final is a unitary predicate. The paper could say both s1 and s2
are blame or both are values.

- p19, Figure 10:

-- (\epsilon; id(P); \epsilon) --> (...; id_m(P); ...)
-- id(Bool), id(S \rightarrow T), id(S \times T) --> id_m(...)

- p19, "Prior presentations of this function": I cannot find them, and think
that this is the first instance of the translation function. Where have the
prior presentations been given?

- p20, Figure 11:

-- "id_\star ; d = d" (the first equation in the figure) --> id_\star ; c = c ?

-- "castFromDyn(l,Q)" --> castFromDyn(Q,l)

-- I think Labels appearing in the contravariant positions in the definitions of
the translation function, castFromDyn, and castToDyn should not be negated.

- I feel Section 5.2 is not as well presented as other parts in the paper. In
particular, it could explain the role and necessity of source casts
(gaps) more gently. I guess they are introduced to record the information of
t1 and h2 in the hypercoercion composition because t1 and h2 may be more
precise than ground types.

- p21, "imposes considerable difficulty in the termination proof of the composition for Lazy D Coercions in Normal Form"
As described above, I would like to see the details.

- p23: "the result is the second middle" --> the first middle?

- p23: "a gap g is good if and only if there is not P and l and Q"

- p23: "both casts become smaller in recursive calls" --> both coercions?


== Questions

1. Does the paper present implementations of Lazy UD threesomes and Lazy UD
coercions somewhere? Or, can the authors present (either of) them in the
revision easily?

2. Can the authors address my concern about determinism of S(C)?

3. Can the authors clarify the difficulty with mechanization of (Lazy D)
coercions in normal form?

## Review 3, After Rebuttal

1: (weak accept)
== Summary

Motivated by bridging the gap between simple theoretical models and efficient
implementations of gradual typing, there exist a lot of works that propose
efficient cast representations. However, the proof works to verify equivalence
between the efficient casts and the original, inefficient casts have been done
individually. This paper presents a framework built on the top of Agda that
extracts an essence of cast semantics and reduces the proof burden for cast
language equivalence to implementing Cast Abstract Data Type (Cast ADT) with a
few operations enjoying some axioms. The paper further defines two variants of
the Cast ADT, Lazy D Cast ADT and Lazy UD Cast ADT, and proves that the cast
semantics implemented with each is correct with respect to the corresponding
cast semantics (Lazy D or Lazy UD). The usefulness of the framework is
demonstrated by applications to Lazy UD coercions in normal form and Lazy UD
threesomes (I cannot find them after the introduction, though). Motivated by
achieving both mechanizability and simplicity, the paper also proposes a new
cast representation, called hypercoercions, for each of Lazy D and Lazy UD,
proves its correctness through the proposed framework, and discusses their
potential for practice use.


== Evaluation

Pros:

- I really enjoy reading the paper. The attention to efficiency has been
increasingly growing in the gradual typing community. The fact that space
efficiency is one of active topics is proven by a lots of works in the
literature. Since the seminal work by Herman et al., it has been known that
coercion composition or collapsing casts is key in space efficiency, but, in
my opinion, the techniques for space efficiency make cast languages
complicated and so are not very suitable when one is interested in other
theoretical aspects of gradual typing, such as gradual guarantee and
parametricity. Thus, proving correctness of the efficient cast
representations is crucial to bridge the gap between theory and
implementation. This paper absolutely contributes to reducing its proof
burden by identifying reasonable sufficient conditions formalized by Cast ADT,
Lazy D Cast ADT, and Lazy UD Cast ADT.

- I also like hypercoercions, which are a very simple and intuitive
representation of coercions in normal form (I think the presentation of Lazy D
hypercoercions could be improved as noted below, though).

- The writing is almost clear. It provides a gentle review of the previous work
and then introduces the present work in a clear manner, except for a few
parts.


Cons:

- My major criticism is that it seems that the paper does not concretely
present the implementations of Lazy UD threesomes and Lazy UD coercions in
normal form within the proposed framework. I feel this critical because an
aim of the paper is to provide a framework for proving correctness of
efficient cast representations. Lacking the presentation makes me concerned
about whether the aim is achieved. I feel hypercoercions do not work for
this purpose, because they are not proven space efficient, yet. I hope the
paper presents an implementation of, at least, either threesomes or
coercions in normal form.

- Lacking the implementation of coercions in normal form also makes it
difficult to precisely identify the contributions of hypercoercions to
mechanization. In fact, I fail to grasp the challenges in mechanizing
coercions in normal form.

- I also have a technical concern with determinism of S(C). The semantics of
S(C) generates coercions at run time. For example, the evaluation of a state
"<\lambda x:S. e, E, K>" must infer a return type T as well as a type
environment \Gamma (the second rule in Figure 8). In order for S(C) to be
deterministic, T and \Gamma must be unique; otherwise, s_1 and s_2 in
Proposition 3 may be syntactically different. I expect T and \Gamma for
lambda abstractions could be uniquely inferred, but a similar, (perhaps) more
serious issue would happen in the function mkK. mkK tops a pre-continuation k
with an identity cast id(S). For that, the type S is inferred from the typing
of k. However, it seems that k can be of multiple types. For example, if k
is \box, I guess k can be of T \Rightarrow T for any T (I only guessed because
the inference rules of the continuation typing seem to be omitted). This
would indicate that a continuation mkK(k) may not be uniquely determined (put
differently, mkK would not be well defined as a function). As a result, I'm
concerned that S(C) might be nondeterministic because s_1 and s_2 in
Proposition 3 may include different identity coercions inserted by mkK. This
issue could be solved by annotating \box in pre-continuations, continuations,
and frames with types. Anyway, I think the paper must prove uniqueness of
each typing, including cast typing, to show determinism.


Suggestion:

I would like to see more discussions in the paper, like the followings.

- The semantics of S(C) depends on the design decision that a frame is paired
with a coercion. I think this unsuitable for practical compiler
implementations because it inevitably incurs a large overhead by a huge
amount of identity casts mkK will insert. I would like see discussions
about differences between a "space-efficient" model in the framework and a
compiler-level implementation of it.

- I also would like to see future work, e.g., handling Eager D/UD (specifically,
eager product casts) and prospects of supporting other SOTA approaches to
space efficiency.

- The paper provides a framework for correctness. I am curious about whether
it is possible to construct a framework for proving space efficiency (I
don't expect mechanization for that, because it would make the work quite
hard).


== Comments/Typos

- Are all properties in the paper (including determinism) proven on the top of
Agda? If so, please explicitly note it in the paper. If not, it would be
quite nice to spell out which is done by pencil-and-paper and to present the
proofs in Appendix.

- p1, abstract, "The proofs of correctness": It would be nicer to spell out
what correctness means in this paper.

- p3, "we apply the framework to Lazy UD coercions in normal form ... and Lazy UD threesomes:
It would be nice to explain Lazy UD here.

- p4, "a more compact memory representation": I cannot find the discussion that
claims hypercoercions are _more_ compact than coercions in normal form.
Section 5.3 just describes compactness of hypercoercions.

- Section 2: I feel discussing related works here is not effective because
C(B) and S(C) have not been introduced yet. I suggest moving it to the place,
e.g., before the conclusion.

- p5: but doesn't normalize --> does not

- p7, Figure 2: Why does not the typing rules of casts require the source and
target types consistent?

- p7, p9: by specify blame strategies --> specifying

- p12, Figure 7: It would be nicer to clarify who provides injectable types.
Done by an instance of Cast ADT or a blame strategy?

- p12, "Value typing, continuation typing, and environment typing are defined in the obvious way":
Are they completely omitted? If so, it would be helpful to put them in
Appendix.

- p12, Definition 4:

-- "indexed by two types": It would be nicer to spell out they are source and
target types of the casts in the Cast ADT.

-- Do the definitions of the operations and properties assume the indexing types
to be denoted by S and T?

-- What are the types of coercions appearing in the composition c;c and [[c]]?

-- Is id(T) only in Cast_(T,T)? If S is not equal to T, neither id(T) nor
id(S) is in Cast(T,S)?

- p13: "we are to defined" --> define ?

- p15, Definitions 5 and 6: I suppose these definitions formulate axioms (or
properties) to be satisfied by instances of Lazy D Cast ADT and Lazy UD Cast
ADT, respectively. In either case, the paper could more clearly spell out
what these definitions represent.

- p15, Definition 6, "in [[ G \Rightarrow^l \star ]] (v)" in the third clause:
It seems \lceil and \rceil are missed.

- p15, "\vdash o : T": Where has this notation been introduced?

- p17: "al possibly ways" --> possible

- p17, Lemmas 1 and 2: "and \vdash v \approx v' : S [. Then]"

- p17, the proofs of Lemmas 1 and 2: Please consider handling the case of "if no".

- p18, Lemma 3:

-- "[. T]hen either"

-- "1. s1 final and s1' final": I think this statement does not reflect the
intention of the paper because it allows the case that s1 is blame and s2 is
a value as final is a unitary predicate. The paper could say both s1 and s2
are blame or both are values.

- p19, Figure 10:

-- (\epsilon; id(P); \epsilon) --> (...; id_m(P); ...)
-- id(Bool), id(S \rightarrow T), id(S \times T) --> id_m(...)

- p19, "Prior presentations of this function": I cannot find them, and think
that this is the first instance of the translation function. Where have the
prior presentations been given?

- p20, Figure 11:

-- "id_\star ; d = d" (the first equation in the figure) --> id_\star ; c = c ?

-- "castFromDyn(l,Q)" --> castFromDyn(Q,l)

-- I think Labels appearing in the contravariant positions in the definitions of
the translation function, castFromDyn, and castToDyn should not be negated.

- I feel Section 5.2 is not as well presented as other parts in the paper. In
particular, it could explain the role and necessity of source casts
(gaps) more gently. I guess they are introduced to record the information of
t1 and h2 in the hypercoercion composition because t1 and h2 may be more
precise than ground types.

- p21, "imposes considerable difficulty in the termination proof of the composition for Lazy D Coercions in Normal Form"
As described above, I would like to see the details.

- p23: "the result is the second middle" --> the first middle?

- p23: "a gap g is good if and only if there is not P and l and Q"

- p23: "both casts become smaller in recursive calls" --> both coercions?


== Questions

1. Does the paper present implementations of Lazy UD threesomes and Lazy UD
coercions somewhere? Or, can the authors present (either of) them in the
revision easily?

2. Can the authors address my concern about determinism of S(C)?

3. Can the authors clarify the difficulty with mechanization of (Lazy D)
coercions in normal form?


== POST-REBUTTAL

Thank you for the response. It addresses my concerns about the determinism of
S(C) and the challenge in mechanization of Lazy D coercions in normal form. So
I will up my grade.

I would also be happy if the implementation of threesomes or coercions in normal
form on the framework is found in the paper for making it self-contained.
