# Functions: The First Tools

*(or: why purity matters more than you might think)*

---

## The Premise

Functions are the atoms of functional programming. Get them right, and everything else follows naturally.

If you've written code in any language, you've written functions. But in Coalton (and Haskell), functions work a bit differently than what you might be used to. The key difference isn't syntax—it's **purity**.

A pure function:
- Always returns the same output for the same input
- Has no side effects (no printing, no mutation, no randomness)
- Depends only on its arguments

This might sound limiting at first. "No side effects? How do I get anything done?"

The answer: you separate *what* you want to compute from *how* you perform effects. Pure functions handle the "what," and the type system ensures you're explicit about the "how."

Let me show you what this looks like in practice.

---

## A First Glimpse: Your First Pure Function

In Haskell, a simple pure function looks like this:

```haskell
-- Haskell
double :: Int -> Int
double x = x * 2
```

The type signature `Int -> Int` says: "give me an `Int`, I'll give you back an `Int`."

Here's the Coalton equivalent. Create a file named `double.smt`:

```lisp
#!/usr/bin/env smt run

;; A pure function: same input always gives same output
(declare double (Integer -> Integer))
(define (double x)
  (* 2 x))

(define main
  (progn
    (println "double 21 =>")
    (println (show (double 21)))
    (println "double 100 =>")
    (println (show (double 100)))))
```

Run it:

```bash
$ smelter run double.smt
double 21 =>
42
double 100 =>
200
```

Notice the structure:
1. We **declare** the type signature first
2. We **define** the implementation
3. We have a `main` entry point that handles I/O

💡 **The `declare` line isn't optional.** While Coalton can infer types in many cases, explicit declarations make your code clearer and help catch errors earlier. Think of them as documentation that the compiler verifies.

---

## Type Safety in Action

Now let's see what happens when we make a mistake. Change `main` to:

```lisp
(define main
  (double "not a number"))
```

Run it:

```bash
$ smelter run double.smt
Error running script: error: Type mismatch
  --> <macroexpansion>:8:12
  |
8 | (DOUBLE "not a number"))
  |         ^^^^^^^^^^^^^^ Expected type 'INTEGER' but got 'STRING'
```

The compiler caught this **before running any code**. In a dynamically-typed language, this would blow up at runtime. In Coalton, it never gets that far.

This is the first benefit of pure functions with type signatures: *the compiler becomes your assistant*, catching whole categories of bugs before they reach production.

---

## A More Meaningful Example: Pure vs Impure

Let's look at a more realistic example that shows why purity matters.

Here's an impure function in most languages—it prints as a side effect:

```python
# Python (impure)
def process_and_print(x):
    result = x * 2
    print(f"Result: {result}")  # Side effect!
    return result
```

You can't tell from the signature whether this function prints, writes to a file, or launches missiles. You have to read the implementation.

In Coalton, we separate computation from effects. Create `pure-computation.smt`:

```lisp
#!/usr/bin/env smt run

;; Pure computation: just transforms data
(declare process (Integer -> Integer))
(define (process x)
  (* 2 x))

;; Pure computation: checks if even
(declare is-even (Integer -> Boolean))
(define (is-even n)
  (== (mod n 2) 0))

;; Helper to show boolean
(declare show-bool (Boolean -> String))
(define (show-bool b)
  (if b "yes" "no"))

;; Main handles ALL the I/O in one place
(define main
  (progn
    (println "Processing numbers...")
    (let ((result (process 21)))
      (progn
        (println (show result))
        (println (<> "Is " (<> (show result) (<> " even? " (show-bool (is-even result))))))))))
```

Run it:

```bash
$ smelter run pure-computation.smt
Processing numbers...
42
Is 42 even? yes
```

**Why this matters:**

The pure functions `process` and `is-even` are:
- **Testable**: You can test them without mocking I/O
- **Composable**: You can combine them in any order
- **Predictable**: Same input → same output, always
- **Parallelizable**: Safe to run concurrently (no shared state)

All the messiness (printing, formatting) lives in `main`, clearly separated.

---

## Function Composition: Building Bigger from Smaller

Pure functions shine when you start composing them. Each function does one thing well, and you chain them together.

Create `composition.smt`:

```lisp
#!/usr/bin/env smt run

;; Simple transformations
(declare add-one (Integer -> Integer))
(define (add-one x)
  (+ x 1))

(declare double (Integer -> Integer))
(define (double x)
  (* 2 x))

(declare square (Integer -> Integer))
(define (square x)
  (* x x))

;; Compose them!
(declare transform (Integer -> Integer))
(define (transform x)
  (square (double (add-one x))))

;; Break down the steps
(declare show-steps (Integer -> Unit))
(define (show-steps x)
  (let ((step1 (add-one x))
        (step2 (double (add-one x)))
        (step3 (square (double (add-one x)))))
    (progn
      (println (<> "Start with: " (show x)))
      (println (<> "After add-one: " (show step1)))
      (println (<> "After double: " (show step2)))
      (println (<> "After square: " (show step3))))))

(define main
  (progn
    (println "=== Direct transformation ===")
    (println (show (transform 5)))
    (println "")
    (println "=== Step by step ===")
    (show-steps 5)))
```

Run it:

```bash
$ smelter run composition.smt
=== Direct transformation ===
144
=== Step by step ===
Start with: 5
After add-one: 6
After double: 12
After square: 144
```

The transformation pipeline: 5 → 6 → 12 → 144

Each function is small, simple, and pure. You can:
- Test each piece independently
- Swap out `square` for `cube` without touching anything else
- Trace exactly what happens at each step
- Reuse these functions in other pipelines

---

## Why This Matters

Pure functions with type signatures give you:

- **Fearless refactoring**: If it compiles, it probably works
- **Built-in documentation**: The signature tells you what goes in and out
- **Easier reasoning**: No hidden state or side effects to track
- **Compiler assistance**: Type errors caught before runtime
- **Confident composition**: Small functions combine into bigger ones safely
- **Trivial testing**: No setup, no teardown, just input → output

This is why functional programmers talk about "reasoning with types." The type signature is a contract that the compiler enforces.

---

## Try It Yourself (Smelter Ready)

Let's practice writing pure functions. Create `exercises.smt`:

```lisp
#!/usr/bin/env smt run

;; Exercise 1: Write a function that triples a number
(declare triple (Integer -> Integer))
(define (triple x)
  ;; Your implementation here
  (* 3 x))

;; Exercise 2: Write a function that subtracts 10
(declare subtract-ten (Integer -> Integer))
(define (subtract-ten x)
  ;; Your implementation here
  (- x 10))

;; Exercise 3: Compose them - triple first, then subtract 10
(declare triple-then-subtract (Integer -> Integer))
(define (triple-then-subtract x)
  ;; Your implementation here
  (subtract-ten (triple x)))

(define main
  (progn
    (println "triple 7 =>")
    (println (show (triple 7)))
    (println "subtract-ten 20 =>")
    (println (show (subtract-ten 20)))
    (println "triple-then-subtract 10 =>")
    (println (show (triple-then-subtract 10)))))
```

**Expected output:**
```
triple 7 =>
21
subtract-ten 20 =>
10
triple-then-subtract 10 =>
20
```

**Challenge**: What happens if you try to pass a `String` to `triple`? Try it and read the error message carefully.

**Bonus**: Can you write a function that composes in the opposite order: `subtract-then-triple`?

---

## Closing Thought

Pure functions are the foundation of everything in Coalton. They're predictable, composable, and safe by default.

The type signatures aren't red tape—they're guardrails that let you move fast without breaking things. Once you get comfortable thinking in terms of "data in, data out," you'll find yourself writing clearer, more maintainable code.

Next time, we'll look at how to work with collections of data using lists, and you'll see how purity makes recursion natural and safe.

---

**(As someone smarter than me once said) A pure function is a computation you can trust.**

---

**Stay tuned for more ...**
