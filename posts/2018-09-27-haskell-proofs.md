---
title: Writing and using proofs in Haskell
tags:
  - haskell
  - dependent-types
---

This post has an accompanying [github repo][github-repo].

## Intro

Haskell's type system has long been sort of a lab rat for experiments on many
concepts relevant to type theory. Especially lately there has been quite a lot
of interest in type-level programming and dependent types.

What initially piqued my interest in trying to use Haskell to prove mathematical
theorems at the type level was a blog post called "Proving stuff in Haskell" by
Mads Buch [^mads]. It is quite fun to follow so I'm not going to go into detail,
but the gist is to use propositional equality to prove a theorem by showing that
two haskell types are actually the same type. So for example, if we wanted to
prove that $a + b = b + a$ (commutativity of addition of natural numbers) then
we'll have a function whose type reflects exactly that property, rather than
using actual values to prove it (more on that later).

Writing proofs using the type system is nothing new. What I describe in this
post is first of all a way to write readable proofs by induction using the same
steps we would use if the proof was done on paper, and then how to actually use
these proofs to "nudge" the type system to compile code that could not be
compiled otherwise.

One disclaimer: I am definitely not an expert so many things might not be
correct. This post's style is informal and more of a braindump, much like some
of my [favourite posts](https://artyom.me/lens-over-tea-1) out there. If you
find any errors or if you have any question or comment, feel free to leave a
comment below, drop me an email (`alexpeitsinis [at] gmail [dot] com`) or find
me at fpslack (`@alexpeits`). What follows is also most definitely not suitable
for "production use". However I believe it's a good application of using
dependent types and some more exotic language extensions.

[^mads]: [Proving stuff in Haskell](http://www.madsbuch.com/blog/proving-stuff-in-haskell/)

## Setting up

Unless I am completely mistaken, code should compile along the way. Here are the
required language extensions:

```haskell
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
```

I'll also try to note why and when an extension should be enabled.

## Dependent types

I've already given some spoilers, so here it is: to achieve what we want to do
I'm going to use whatever Haskell offers to simulate a dependently typed
language. To better understand the what and how of dependent types, I suggest
taking a look at any work by [Richard Eisenberg](https://cs.brynmawr.edu/~rae/),
especially the extremely well written *Stitch* functional pearl [^stitch].
Another great resource for everything type-level related is the Book of Types
[^book-of-types] by [Sandy Maguire](http://reasonablypolymorphic.com/) (consider
supporting it, it's awesome). Finally, the Idris language tutorial [^idris] is a
great resource, especially to understand what needs to be done differently in
Haskell and how.

Many of the following do not require dependent types and can probably be modeled
using e.g. type classes, but I'm going to use dependent types here.

[^stitch]: 'Stitch: The Sound Type-Indexed Type Checker' (Richard A. Eisenberg)
    <https://cs.brynmawr.edu/~rae/papers/2018/stitch/stitch.pdf>
[^book-of-types]: [Book of Types on Patreon](https://www.patreon.com/isovector)
[^idris]: <http://docs.idris-lang.org/en/latest/tutorial/typesfuns.html>

## Natural numbers

To prove theorems on natural number operations we first need a way to represent
natural numbers at the type level. The canonical way to do this is to use
[Peano numbers]( https://wiki.haskell.org/Peano_numbers):

```haskell
data Nat = Z | S Nat
```

We have the base case which is the number zero (here `Z` for brevity later), and
then we recursively define the successor (`S`). So to represent the number 1 we
would write `S Z` (at the type level), to represent the number 2 we would write
`S (S Z)` and so on. The thing is, we'd not use the **value** `Z` or `S Z`,
because they have the same type, `Nat`, which is not very useful for type-level
programming. On the other hand, because the types `Z` and `S Z` have kind `Nat`
and not `*`, they cannot have any inhabitants, meaning that there is no value
`x` so that `x :: Z` or `x :: S Z`. But there is a way to circumvent that
restriction, which will be described later.

In the above explanation we can use `Z` and `S` as types thanks to the
`DataKinds` extension. What it does is "promote" data constructors to types and
types to kinds. One thing to note is that there is no way to get a value of one
of those types, as their kinds is `Nat` and not `*`. We could also have written
something like this:

```haskell
data Z
data S a
```

What we lose from this definition is the way to unite those two types under a
kind. Our definition allows writing `Nat` in GADT and type family declarations,
which adds a layer of type safety. Otherwise we'd have to use `*`, which, in
some cases, would allow for ill-formed types.

## Type equality

There is a ridiculously simple way to convince the type system that two types
are equal using propositional equality in the module
[Data.Type.Equality][hackage-data-type-equality].

```haskell
data a :~: b where
    Refl :: a :~: a
```

What this means is that, if somewhere in our code we have the type `a :~: b` and
somehow manage to give it the value `Refl`, we have convinced the type checker
that types `a` and `b` are exactly the same, which is exactly what we want to
achieve in our proofs.

## Type-level functions

In order to actually have something to prove, we'll define addition at the type
level as per the [Peano axiom for addition][peano-axiom-addition].

$$
\begin{align}
a + 0    = a && \text{(1)}\\
a + S(b) = S (a + b) && \text{(2)}\\
\end{align}
$$

This can be achieved using a closed type family:

```haskell
type family a + b where
    a + Z   = a         -- (1)
    a + S b = S (a + b) -- (2)
```

(from now on I'll refer to those two properties as `(1)` and `(2)` respectively)

We don't need to specify a kind for this type family explicitly, as it is
deduced to be `Nat -> Nat -> Nat` since the type family is closed (no more
instances can be defined). Also, the symbol `+` does not clash with the addition
function, for the same reason the type `Z` does not clash with the value `Z` (if
there is an ambiguity in that case we can tell the type system that we're
talking about the *type* `Z` by writing `'Z`).

## First proof

Now let's try to combine this type family definition with propositional equality:

```haskell
testEquality :: (Z + Z) :~: Z
testEquality = Refl
```

This type checks because `Refl` says "ok, I need `Z + Z` to be the same as `Z`.
But when the type checker sees `Z + Z`, it uses `(1)` to say that `Z + Z = Z`,
where `a = Z`, and our equality stands. This is the first proof! The fact that
we don't need to give any values or "nudge" the type system in any way means
that the elaboration happens automatically, and this equality stands every time.
Let's also try not with concrete types, but with a type variable:

```haskell
testEquality' :: (a + Z) :~: a
testEquality' = Refl
```

This type checks, again thanks to `(1)`. Let's try `(2)`:

```haskell
testEquality'' :: (a + S b) :~: S (a + b)
testEquality'' = Refl
```

And yet again, this type checks, this time thanks to `(2)`. Now let's try
something that is not immediately apparent. With `testEquality'`, we proved the
right identity property of addition. What about left identity?

```haskell
plusLeftId :: (Z + a) :~: a
plusLeftId = Refl
```

Which, when compiled, unfortunately fails:

```text
• Couldn't match type ‘a’ with ‘'Z + a’
    ‘a’ is a rigid type variable bound by
    the type signature for:
        plusLeftId :: forall (a :: Nat). ('Z + a) :~: a
    at ...
    Expected type: ('Z + a) :~: a
    Actual type: a :~: a
• In the expression: Refl
    In an equation for ‘plusLeftId’: plusLeftId = Refl
• Relevant bindings include
    plusLeftId :: ('Z + a) :~: a
        (bound at ...)
|
70 | plusLeftId = Refl
|       ^^^^
```

However simple it seems, the type checker knows nothing about natural number
addition, so we have to convince it that this is indeed correct. To prove that
`Z + a = a` we need to do induction on `a`. By induction, I mean that we need to
prove a base case, then assume that our hypothesis holds for $a$ and prove that
it holds for $S (a)$. If we prove that, we have proven the theorem for all
natural numbers. That's because if we substitute $a$ for `Z` then our hypothesis
holds because we've already proven for `Z`. So if we manage to prove for `S Z`,
then we do the same for `S (S Z)` and so on. We could try something like this:

```haskell
baseCase :: (Z + Z) :~: Z
baseCase = Refl

induction :: (Z + S a) :~: S a
induction = ???
```

But we find a dead end. We need to be able to use one proof in order to prove
another one.

Long story short, looking in `Data.Type.Equality` again, there is a function
that seems like it does exactly that: [gcastWith][hackage-gcastWith].

```haskell
gcastWith :: (a :~: b) -> (a ~ b => r) -> r
```

In simple terms, what `gCastWith` says is: give me a `Refl` that proves `a` is
the same type as `b`, and something of type `r` that could use the knowledge
that `a` is `b`, and I will tell your type checker that `r` is well typed. This
seems like the same logic used in one step of a mathematical proof. So we could
do things like:

```haskell
helper :: (S Z + S Z) :~: S (S Z + Z)
helper = Refl

stupid :: (S Z + S Z) :~: S (S Z)
stupid = gcastWith helper Refl
```

What this says is: "I know that `S Z + S Z` is `S (S Z + Z)`, now use that".
Then the `Refl` in `stupid` says "ok, I have `S Z + S Z` and the other guy says
that this is `S (S Z + Z)`, can I use it? Ah, of course, from `(1)` I know that
`S Z + Z` is `S Z`, so what I have is `S (S Z)`, that's it!" (or as
mathematicians say, [Q.E.D.](https://en.wikipedia.org/wiki/Q.E.D.)). OK, to be
fair this is not necessary as `stupid` is proven directly: it's 2 reduction
steps of the `+` type family.

Still, we are talking about concrete values, whereas what we need to prove has a
type variable `a`. We need to somehow recurse until we reach the base case,
which means we'll need to reflect the **exact** type we have in the signature of
`plusLeftId` to a value. If you peeked at the Idris tutorial, you might say
something like `{a:Nat} -> (Z + S a) :~: S a`. There is a way to do this in
Haskell, using singletons.

## Singletons

Singletons, as their name implies, are values that have a 1-1 mapping with their
relevant types. This means that, when we have a singleton value `v` of type `T`
then we know for sure that `v` always has the type `T` (that makes sense), but
also that type `T` has only one inhabitant (only one possible value), and that
is `v`. This is the singleton definition for our `Nat`:

```haskell
data SNat :: Nat -> * where
    SZ :: SNat Z
    SS :: SNat a -> SNat (S a)
```

Now, if we ask for the type of `SZ` we get `SNat Z`, and `SS (SS (SZ))` gives
`SNat (S (S Z))`. It goes without saying that we cannot write `SS (SS (SZ)) ::
SNat Z` or something similar, because that gets rejected by the definition of
the `SNat` GADT.

## Revisiting the proof

Let's try to prove left identity again, this time using an `SNat` to help us
with the induction by allowing recursion:

```haskell
plusLeftId :: SNat a -> (Z + a) :~: a
```

The base case will be for `a ~ Z` (`~` is type equality):

```haskell
plusLeftId :: SNat a -> (Z + a) :~: a
plusLeftId SZ = Refl
```

This type checks because, according to the `Refl` value, `(Z + Z) ~ Z` which is
true from `(1)`. The reason that `a` is assumed to be `Z` is that the first
parameter is `SZ :: SNat Z`, so `a ~ Z`. That's very similar to the type
signature in Idris: `{a:Nat} -> (Z + S a) :~: S a`, only with some indirection
via the singleton type for `Nat`!

Now for the induction step. Recall that we need to now prove for every type `S
a`, assuming that the proof stands for `a`. So we'll use the singleton for the
successor, `SS`:

```haskell
plusLeftId :: SNat a -> (Z + a) :~: a
plusLeftId SZ     = Refl
plusLeftId (SS n) = Refl
```

But this fails:

```text
• Could not deduce: ('Z + n) ~ n
  from the context: m ~ 'S n
    bound by a pattern with constructor:
               SS :: forall (m :: Nat). SNat m -> SNat ('S m),
             in an equation for ‘plusLeftId’
```

(I have made the error message a bit more readable)

Which makes sense: we have to *tell* the type checker that the equality holds
for `n`. So, as described earlier, we'll use `gcastWith`:

```haskell
plusLeftId :: SNat a -> (Z + a) :~: a
plusLeftId SZ     = Refl
plusLeftId (SS n) = gcastWith ??? Refl
```

Recall that `gcastWith :: (a :~: b) -> (a ~ b => r) -> r`. Here, `Refl` is the
`r`, and, according to the error message, we need `a ~ (Z + n)` and `b ~ n`, or
even better `(Z + n) :~: n`. But that's the result type of `plusLeftId` itself,
if we called it with `n`:

```haskell
plusLeftId :: SNat a -> (Z + a) :~: a
plusLeftId SZ     = Refl
plusLeftId (SS n) = gcastWith (plusLeftId n) Refl
```

And it type checks (try calling the function with any `SNat` value)!

Let's also rewrite the proof for write identity, as well as the axioms we
automatically get from the type family, to use values:

```haskell
given1 :: SNat a -> (a + Z) :~: a
given1 _ = Refl

given2 :: SNat a -> SNat b -> (a + S b) :~: S (a + b)
given2 _ _ = Refl

plusRightId :: SNat a -> (a + Z) :~: a
plusRightId = given1
```

Of course the first two don't need anything further to be proved. The same
applies for right identity, but here I used `given1` to demonstrate the use of a
proof in another proof. This would work too:

```haskell
plusRightId' :: SNat a -> (a + Z) :~: a
plusRightId' n = gcastWith (given1 n) Refl
```

## Proofs with multiple steps

The proof for left identity was a relatively simple one. Here it is in
mathematical notation, copied from [here][nat-addition-proofs].
(I'll refer to the addition axiom as $(1)$ and $(2)$):

$$
\begin{align}
& 0 + S(a)\\
=\ & S(0 + a) && \text{by (2)}\\
=\ & S(a) && \text{by the induction hypothesis}\\
\end{align}
$$

This means that in the proof we went from `Z + (S n)` to `S (Z + n)` by using
the type family definition, and then proved what's inside the `S` inductively.
It makes sense that we want to work on `Z + n`, since the error we first got
said exactly that:

```text
• Could not deduce: ('Z + n) ~ n
  from the context: m ~ 'S n
```

## Proving the associativity of addition

Let's now try a proof that's a bit more complex: associativity of addition. So
we'll prove that $(a + b) + c = a + (b + c)$. I am going to copy the
mathematical notation for the proof here and follow that verbatim, rather than
explaining each step.

For the base case c = 0:

$$
\begin{align}
& (a + b) + 0\\
=\ & a + b && \text{by (1) for}\ a + b\\
=\ & a + (b + 0) && \text{by (1) for}\ b\\
\end{align}
$$

For the induction, assuming $(a + b) + c = a + (b + c)$:

$$
\begin{align}
& (a + b) + S(c)\\
=\ & S((a + b) + c) && \text{by (2)}\\
=\ & S(a + (b + c)) && \text{by the induction hypothesis}\\
=\ & a + S(b + c) && \text{by (2)}\\
=\ & a + (b + S(c)) && \text{by (2)}\\
\end{align}
$$

Let's try to prove that in Haskell. First I'm going to take the long path and
not assume that `(1)` and `(2)` are resolved automatically. This will help build
some groundwork for later proofs. This will not typecheck for now so I'm going
to use undefined in several places, but the intermediate steps are correct and
correspond 1-1 with the steps above. Let's also define addition for the
singleton datatype, which will be useful because its type can directly reflect
to the `+` type family:

```haskell
(!+) :: SNat n -> SNat m -> SNat (n + m)
n !+ SZ     = n
n !+ (SS m) = SS (n !+ m)
```

We'll use this function to construct, for example, the $a + b$ in the base case
of the proof given $a$ and $b$. Here it goes:

```haskell
plusAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZ =
  let
    step1 :: SNat x -> SNat y -> ((x + y) + Z) :~: (x + y)
    step1 x y = gcastWith (given1 (x !+ y)) Refl -- (1)

    step2 :: SNat x -> SNat y -> (x + y) :~: (x + (y + Z))
    step2 x y = gcastWith (given1 y) Refl -- (1)
  in undefined
```

What we do in those 2 steps is exactly what we do in the base case for $c = 0$.
I used `x` and `y` to make it clear that we're not talking about `a` and `b`,
but just defining steps based on two numbers, even though we're going to pass
`a` and `b` in those methods. In the first step, just as in the first step of
the proof, we want to say that `((x + y) + Z) ~ (x + y)`. It's the first time
we've seen a more complex argument in the proofs so far. Given that we need to
use `(1)` to prove this, we have to pass something of type `SNat n` where n is
of type `x + y`. That's why the addition for singletons (`!+`) is used here. The
resulting type of this addition, if we pass in `x :: x` and `y :: y` is `x + y`.
The second step is a bit easier to understand. The main trick here is to extract
the *part* of the expression that we want to "transform" to something else, and
then use another proof function to drive the process.

It's clear what needs to be done now. We want the type `((a + b) + Z) :~: (a +
(b + Z))`, and we have the types (I substitute `x` and `y` for `a` and `b` for
clarity) `((a + b) + Z) :~: (a + b)` and `(a + b) :~: (a + (b + Z))`.
Fortunately, this is like saying "if type x is the same as type y, and type y is
the same as type z, then is type x the same as type z"? Of course it is! Let's
prove it (I'll also use a fancy symbol to make the process clearer later):

```haskell
(==>) :: a :~: b -> b :~: c -> a :~: c
Refl ==> Refl = Refl
```

Since the above type checks, the proof stands. This is the **transitive**
property of `:~:` (propositional equality).

Let's see how this helps:

```haskell
plusAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZ =
  let
    step1 :: SNat x -> SNat y -> ((x + y) + Z) :~: (x + y)
    step1 x y = gcastWith (given1 (x !+ y)) Refl -- (1)

    step2 :: SNat x -> SNat y -> (x + y) :~: (x + (y + Z))
    step2 x y = gcastWith (given1 y) Refl -- (1)
  in step1 a b ==> step2 a b
```

Voila! Onward to the next step, the induction.

```haskell
plusAssoc
  :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZ     = ...
plusAssoc a b (SS c) =
  let

    step1
      :: SNat x -> SNat y -> SNat (S z) -> ((x + y) + S z) :~: S ((x + y) + z)
    step1 x y (SS z) = gcastWith (given2 (x !+ y) (SS z)) Refl -- (2)

    step2
      :: SNat x -> SNat y -> SNat z -> S ((x + y) + z) :~: S (x + (y + z))
    step2 x y z = gcastWith (plusAssoc x y z) Refl -- induction

    step3
      :: SNat x -> SNat y -> SNat z -> S (x + (y + z)) :~: (x + S (y + z))
    step3 x y z = gcastWith (given2 x (y !+ z)) Refl -- (2)

    step4
      :: SNat x -> SNat y -> SNat z -> (x + S (y + z)) :~: (x + (y + S z))
    step4 x y z = gcastWith (given2 y z) Refl -- (2)

  in step1 a b (SS c) ==> step2 a b c ==> step3 a b c ==> step4 a b c
```

To be fair, none of this is needed for both stages of the proof. Since all but
the induction step are inferred automatically by the `+` type family, the proof
can be written more compactly:

```haskell
plusAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZ     = Refl
plusAssoc a b (SS c) = gcastWith (plusAssoc a b c) Refl
```

In this case, only the induction step is needed to help the second step
type-check. But the long proof shows a lot on how to take a mathematical proof
and directly apply it in Haskell.

Bonus: using `ScopedTypeVariables` we can make it much prettier. We'll use a
`forall` in the `let` block to bring some fresh variables in scope, and define
the steps inside it, using the same names in the types so that we don't have to
pass variables to each step:

```haskell
plusAssoc :: SNat a -> SNat b -> SNat c -> ((a + b) + c) :~: (a + (b + c))
plusAssoc a b SZ =
  let proof :: forall x y. SNat x -> SNat y -> ((x + y) + Z) :~: (x + (y + Z))
      proof x y = step1 ==> step2
        where
          step1 :: ((x + y) + Z) :~: (x + y)
          step1  = gcastWith (given1 (x !+ y)) Refl

          step2 :: (x + y) :~: (x + (y + Z))
          step2 = gcastWith (given1 y) Refl
  in proof a b
plusAssoc a b (SS c) =
  let proof ::
        forall x y z.
        SNat x -> SNat y -> SNat z ->
        ((x + y) + S z) :~: (x + (y + S z))
      proof x y z = step1 ==> step2 ==> step3 ==> step4
        where
          step1 :: ((x + y) + S z) :~: S ((x + y) + z)
          step1 = gcastWith (given2 (x !+ y) (SS z)) Refl

          step2 :: S ((x + y) + z) :~: S (x + (y + z))
          step2 = gcastWith (plusAssoc x y z) Refl

          step3 :: S (x + (y + z)) :~: (x + S (y + z))
          step3 = gcastWith (given2 x (y !+ z)) Refl

          step4 :: (x + S (y + z)) :~: (x + (y + S z))
          step4 = gcastWith (given2 y z) Refl
  in proof a b c
```

## Commutativity

Here's the proof (in mathematical notation) for the property of commutativity of
addition. What we want to prove is $a + b = b + a$:

The base case $b = 0$ is the left identity property.

For the second base case $b = 1$:

\- First we prove it for $a = 0$, which gives $0 + 1 = 1 + 0$, the
right identity property.

\- Then we prove inductively, assuming $a + 1 = 1 + a$:

$$
\begin{align}
& S(a) + 1\\
=\ & S(a) + S(0) && \text{by definition of natural numbers}\\
=\ & S(S(a) + 0) && \text{by (2)}\\
=\ & S((a + 1) + 0) && \text{by definition of natural numbers}\\
=\ & S(a + 1) && \text{as proved by the base case for}\ b = 0 \\
=\ & S(1 + a) && \text{by the induction hypothesis}\\
=\ & 1 + S(a) && \text{by (2)}\\
\end{align}
$$

For the induction, assuming $a + b = b + a$:

$$
\begin{align}
& a + S(b)\\
=\ & a + (b + 1) && \text{by definition of natural numbers}\\
=\ & (a + b) + 1 && \text{by associativity}\\
=\ & (b + a) + 1 && \text{by the induction hypothesis}\\
=\ & b + (a + 1) && \text{by associativity}\\
=\ & b + (1 + a) && \text{by the base case for}\ b = 1 \\
=\ & (b + 1) + a && \text{by associativity}\\
=\ & S(b) + a && \text{by definition of natural numbers}\\
\end{align}
$$

The proof is left as an exercise, but using the same pattern as in the
associativity proof it should be pretty straightforward. The signature should
be:

```haskell
plusComm :: SNat a -> SNat b -> (a + b) :~: (b + a)
```

The proof (and even more proofs) can be found in the repo.

## Using the proofs

It was a bit difficult to find a use case for the proofs on addition, but I
quickly found an issue trying to append two length-indexed vectors. The vector
definition and several operations are more or less the same as the ones in the
Stitch paper:

```haskell
data Vec :: Nat -> * -> * where
  V0   :: Vec Z a
  (:>) :: a -> Vec n a -> Vec (S n) a

infixr 5 :>
```

I'm not going to go into detail here, but the most important part is that this
vector type has its length in its type as extra information, and we need to
reason about the length when manipulating it.

One common example is appending two vectors. It is apparent that if one has
length `n` and the other has length `m`, then the resulting vector will have
length `n + m`. A naive first attemt would be the following:

```haskell
append :: Vec n a -> Vec m a -> Vec (n + m) a
append V0      ys = ys
append (x:>xs) ys = undefined -- let's wait for now
```

But even the base case fails to typecheck:

```text
• Could not deduce: ('Z + m) ~ m
      from the context: n ~ 'Z
```

The error says that the only concrete information we have is `n ~ Z`, because
the first vector is the empty vector. And because we return `ys`, which has
length `m`, the resulting type `Z + m` should be the same as `m`, but the type
checker is not convinced. Let's notice that what we need is to use the left
identity property, and convince the checker:

```haskell
append :: Vec n a -> Vec m a -> Vec (n + m) a
append V0      ys = gcastWith (plusLeftId ?) ys
append (x:>xs) ys = undefined -- let's wait for now
```

There is one important part missing: the actual length. All our proofs used
singletons to help drive the checking process, but here there is no mention of a
singleton. So let's add it for now, and later we'll try to make this process
implicit. We'll explicitly pass the lengths of both vectors:

```haskell
append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0      ys = gcastWith (plusLeftId m) ys
append n  m (x:>xs) ys = undefined -- let's wait for now
```

OK, this works. Let's try to do the next case where the first vector is
nonempty. We'll make it fail just to see the error message:

```haskell
append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0      ys = gcastWith (plusLeftId m) ys
append n  m (x:>xs) ys = x :> append ? m xs ys
```

Uh-oh. How do we get the length of the first vector after we remove the first
element? Luckily, that's just `n - 1`. Let's create a helper function to get the
*predecessor* number of a singleton of `Nat`:

```haskell
spred :: SNat (S n) -> SNat n
spred (SS n) = n
```

This function is total, because there is no need for the case `spred SZ`. As a
matter of fact, it wouldn't type check because it would mean trying to do `SZ ::
SNat (S n)` which doesn't make sense. And because the first vector in the second
case of `append` is not empty, we know that `n` is not `SZ` but `SS ...`.

```haskell
append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0      ys = gcastWith (plusLeftId m) ys
append n  m (x:>xs) ys = x :> append (spred n) m xs ys
```

This time we get this error:

```text
• Could not deduce: ('S n1 + m) ~ 'S (n1 + m)
  from the context: n ~ 'S n1
```

Where `n1` is the type of the result of `spred n`. Let's construct a proof for
that:

We need to prove $S(a) + b = S(a + b)$. We can do an inductive proof which will
be quicker (in the repo I use an inductive proof), but let's try to do it in one
pass:

$$
\begin{align}
& S(a) + b\\
=\ & b + S(a) && \text{by commutativity}\\
=\ & S(b + a) && \text{by (2)}\\
=\ & S(a + b) && \text{by commutativity}\\
\end{align}
$$

And in Haskell:

```haskell
append :: SNat n -> SNat m -> Vec n a -> Vec m a -> Vec (n + m) a
append SZ m V0 ys = gcastWith (plusIdenL m) ys
append n m (x:>xs) ys = gcastWith (proof pn m) $ x :> app (spred n) m xs ys
  where
    pn = spred n
    proof :: forall x y. SNat x -> SNat y -> (S x + y) :~: S (x + y)
    proof x y = step1 ==> step2 ==> step3
      where
        step1 :: (S x + y) :~: (y + S x)
        step1 = gcastWith (plusComm (SS x) y) Refl

        step2 :: (y + S x) :~: S (y + x)
        step2 = gcastWith (given2 y (SS x)) Refl

        step3 :: S (y + x) :~: S (x + y)
        step3 = gcastWith (plusComm y x) Refl
```

And that's the proof. Now appending two vectors should work. First, here's an
instance of `Show` for `Vec`:

```haskell
instance (Show a) => Show (Vec n a) where
  show v = "[" ++ go v
    where go :: (Show a') => Vec n' a' -> String
          go v = case v of
            V0        -> "]"
            (x :> xs) -> show x ++ sep ++ go xs
              where sep = case xs of
                      V0   -> ""
                      _    -> ", "
```

And two example vectors:

```haskell
x = 1 :> 2 :> 3 :> 4 :> V0
lengthX = SS (SS (SS (SS SZ)))

y = 5 :> 6 :> 7 :> 8 :> 9 :> V0
lengthY = SS (SS (SS (SS (SS SZ))))
```

```text
> append lengthX lengthY x y
[1, 2, 3, 4, 5, 6, 7, 8, 9]
> :t append lengthX lengthY x y
append lengthX lengthY x y
  :: Vec ('S ('S ('S ('S ('S ('S ('S ('S ('S 'Z))))))))) Integer
```

We can also remove the need to have the length in a variable beforehand. To do
that, we have to resort to type classes (again, this is described in the Stitch
paper). We will construct a type class with a single method that can magically
give an `SNat` depending on the instance we are using:

```haskell
class IsNat (n :: Nat) where nat :: SNat n

instance            IsNat Z     where nat = SZ
instance IsNat n => IsNat (S n) where nat = SS nat
```

Then we can just use `nat` to get the length. The instance of `IsNat` to use is
resolved thanks to the fact that the `n` in the constraint `IsNat` is the same
`n` that is the vector length:

```haskell
vlength :: IsNat n => Vec n a -> SNat n
vlength _ = nat
```

```text
> append (vlength x) (vlength y) x y
[1, 2, 3, 4, 5, 6, 7, 8, 9]
```

But I promised that we can have the length passed implicitly. Again, we'll use
the typeclass with some help from `TypeApplications` and `ScopedTypeVariables`:

```haskell
(+++) :: forall n m a. (IsNat n, IsNat m) => Vec n a -> Vec m a -> Vec (n + m) a
(+++) = append (nat @n) (nat @m)
```

```text
> x +++ y
[1, 2, 3, 4, 5, 6, 7, 8, 9]
```

Magic!

## Conclusion

This was a lengthy blog post, and the ideas presented fall into the obscure side
of things. Should there be a need to resort to dependent types, the above might
prove useful. Nevertheless, it's a fun way to explore both dependent types and
translating mathematical proofs to Haskell. After all, as someone said in the
[functional programming slack](https://fpchat-invite.herokuapp.com/),

> A relatively large number of people making zygosynchroid semi-applicative
> plesiofunctors, one madman pushing the type system to its limits, and like
> three people concerning themselves with practical software engineering using
> Haskell.


[github-repo]: <https://github.com/alexpeits/haskell-proofs>
[hackage-data-type-equality]: <http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Equality.html>
[peano-axiom-addition]: <https://en.wikipedia.org/wiki/Peano_axioms#Addition>
[hackage-gcastWith]: <http://hackage.haskell.org/package/base-4.12.0.0/docs/Data-Type-Equality.html#v:gcastWith>
[nat-addition-proofs]: <https://en.wikipedia.org/wiki/Proofs_involving_the_addition_of_natural_numbers>
