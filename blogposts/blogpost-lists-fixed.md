
# Coalton via Haskell: Lists and Recursion

*(being an exploration of structural computation)*

---

## The Premise

I've been thinking about how we learn to process collections. In most languages, we reach for loops first – `for`, `while`, the familiar iteration patterns. But functional programming flips this: recursion comes first, loops are just optimization details.

This isn't just academic preference. When you process lists recursively, you're forced to think about structure: what's the base case? What's the recursive case? How does data decompose? These questions lead to clearer, more correct code.

In Coalton, like in Haskell, lists are the canonical recursive data structure. They teach us pattern matching, structural recursion, and how to think about computation as transformation rather than mutation.

---

## A First Glimpse: Building and Deconstructing

In Haskell, lists are everywhere. They're so fundamental that the language gives them special syntax:

```haskell
-- Haskell
myList = [1, 2, 3, 4, 5]
doubled = map (*2) myList  -- [2, 4, 6, 8, 10]

-- Pattern matching on lists
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs
```

💡 **Coalton has lists too, but with Lisp syntax.** The concepts map directly: cons cells, pattern matching, recursive processing.

Let's see the same ideas in Coalton. Create `lists-intro.smt`:

```lisp
#!/usr/bin/env smt run

;; Helper to show a list
(declare show-list-helper (coalton:List Integer -> String))
(define (show-list-helper lst)
  (match lst
    ((Nil) "")
    ((Cons x xs)
     (match xs
       ((Nil) (show x))
       ((Cons _ _) (<> (show x) (<> " " (show-list-helper xs))))))))

(declare show-list (coalton:List Integer -> String))
(define (show-list lst)
  (<> "#(" (<> (show-list-helper lst) ")")))

;; Sum a list recursively
(declare sum-list (coalton:List Integer -> Integer))
(define (sum-list lst)
  (match lst
    ((Nil) 0)
    ((Cons x xs) (+ x (sum-list xs)))))

;; Double each element
(declare double-all (coalton:List Integer -> coalton:List Integer))
(define (double-all lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs) (Cons (* 2 x) (double-all xs)))))

(define main
  (let ((numbers (make-list 1 2 3 4 5)))
    (progn
      (println "Original list: 1 2 3 4 5")
      (println (<> "Sum: " (show (sum-list numbers))))
      (println "Doubled: ")
      (println (show-list (double-all numbers))))))
```

Run it:

```bash
$ smelter run lists-intro.smt
Original list: 1 2 3 4 5
Sum: 15
Doubled: 
#(2 4 6 8 10)
```

Now let's see what happens when we mix types. Change the list creation to:

```lisp
(define main
  (let ((numbers (make-list 1 "oops" 3 4 5)))  ;; Type error!
    ...
```

The compiler catches it immediately:

```
Error: Type mismatch
  --> <macroexpansion>
  |
  | (make-list 1 "oops" 3 4 5)
  |              ^^^^^^ Expected type 'INTEGER' but got 'STRING'
```

If you're coming from Common Lisp where lists are heterogeneous, this is the key difference: *Coalton lists are homogeneous by design.*

---

## A More Meaningful Example: List Comprehension via Recursion

Haskell's list comprehensions are syntactic sugar over recursive patterns. While Coalton doesn't have comprehension syntax, we can build the same behavior explicitly.

💡 **Understanding the recursion behind comprehensions makes you a better functional programmer.** It's not magic, it's pattern matching all the way down.

Create `list-operations.smt`:

```lisp
#!/usr/bin/env smt run

;; Helper to show a list
(declare show-list-helper (coalton:List Integer -> String))
(define (show-list-helper lst)
  (match lst
    ((Nil) "")
    ((Cons x xs)
     (match xs
       ((Nil) (show x))
       ((Cons _ _) (<> (show x) (<> " " (show-list-helper xs))))))))

(declare show-list (coalton:List Integer -> String))
(define (show-list lst)
  (<> "#(" (<> (show-list-helper lst) ")")))

;; Filter elements that satisfy a predicate
(declare filter-list ((Integer -> Boolean) -> coalton:List Integer -> coalton:List Integer))
(define (filter-list pred lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs)
     (if (pred x)
         (Cons x (filter-list pred xs))
         (filter-list pred xs)))))

;; Map a function over a list
(declare map-list ((Integer -> Integer) -> coalton:List Integer -> coalton:List Integer))
(define (map-list f lst)
  (match lst
    ((Nil) Nil)
    ((Cons x xs) (Cons (f x) (map-list f xs)))))

;; Fold (reduce) a list from the left
(declare fold-left ((Integer -> Integer -> Integer) -> Integer -> coalton:List Integer -> Integer))
(define (fold-left f acc lst)
  (match lst
    ((Nil) acc)
    ((Cons x xs) (fold-left f (f acc x) xs))))

;; Helper predicates and functions
(declare is-even (Integer -> Boolean))
(define (is-even n)
  (== (mod n 2) 0))

(declare triple (Integer -> Integer))
(define (triple x)
  (* 3 x))

(define main
  (let ((numbers (make-list 1 2 3 4 5 6 7 8 9 10))
        (evens (filter-list is-even numbers))
        (tripled (map-list triple evens))
        (sum (fold-left + 0 tripled)))
    (progn
      (println "Starting with: 1..10")
      (println (<> "After filtering evens: " (show-list evens)))
      (println (<> "After tripling: " (show-list tripled)))
      (println (<> "Sum of result: " (show sum))))))
```

Run it:

```bash
$ smelter run list-operations.smt
Starting with: 1..10
After filtering evens: #(2 4 6 8 10)
After tripling: #(6 12 18 24 30)
Sum of result: 90
```

**Why this example matters:**

- **Composability**: Each function does one thing well
- **No mutation**: Original list never changes
- **Type safety**: Can't accidentally mix operations on different types
- **Clear data flow**: Input → filter → map → reduce
- **Recursion patterns**: Each function follows the same match/recurse structure

---

## Recursion Patterns

Both Haskell and Coalton share fundamental recursion patterns for lists:

**The Universal Pattern:**
```haskell
-- Haskell
process [] = baseCase
process (x:xs) = combine x (process xs)
```

```lisp
;; Coalton
(match lst
  ((Nil) base-case)
  ((Cons x xs) (combine x (process xs))))
```

**Common Variations:**

1. **Accumulator pattern** (for tail recursion):
```lisp
(declare sum-tail ((List Integer) -> Integer))
(define (sum-tail lst)
  (sum-tail-helper 0 lst))

(declare sum-tail-helper (Integer -> (List Integer) -> Integer))
(define (sum-tail-helper acc lst)
  (match lst
    ((Nil) acc)
    ((Cons x xs) (sum-tail-helper (+ acc x) xs))))
```

2. **Build-as-you-go pattern** (for transformations):
```lisp
(declare take-n (Integer -> (List Integer) -> (List Integer)))
(define (take-n n lst)
  (if (<= n 0)
      Nil
      (match lst
        ((Nil) Nil)
        ((Cons x xs) (Cons x (take-n (- n 1) xs))))))
```

3. **Multiple base cases** (for more complex logic):
```lisp
(declare zip-lists ((List Integer) -> (List Integer) -> (List (Tuple Integer Integer))))
(define (zip-lists lst1 lst2)
  (match (Tuple lst1 lst2)
    ((Tuple Nil _) Nil)
    ((Tuple _ Nil) Nil)
    ((Tuple (Cons x xs) (Cons y ys))
     (Cons (Tuple x y) (zip-lists xs ys)))))
```

---

## List Construction: Haskell vs Coalton

In Haskell, you have multiple ways to build lists:
```haskell
-- Literal syntax
[1, 2, 3]
-- Cons operator
1 : 2 : 3 : []
-- Ranges
[1..10]
-- Comprehensions
[x*2 | x <- [1..5], x > 2]
```

In Coalton, construction is more explicit:
```lisp
;; Literal construction
(make-list 1 2 3)
;; Explicit cons
(Cons 1 (Cons 2 (Cons 3 Nil)))
;; Build recursively
(define (range start end)
  (if (> start end)
      Nil
      (Cons start (range (+ start 1) end))))
```

The lack of syntactic sugar in Coalton actually helps learning – you see exactly how lists are built from cons cells.

---

## Try It Yourself (Smelter Ready)

Let's implement a classic: reversing a list. Here's the challenge with a starter template:

Create `list-reverse-exercise.smt`:

```lisp
#!/usr/bin/env smt run

;; Helper to show a list
(declare show-list-helper (coalton:List Integer -> String))
(define (show-list-helper lst)
  (match lst
    ((Nil) "")
    ((Cons x xs)
     (match xs
       ((Nil) (show x))
       ((Cons _ _) (<> (show x) (<> " " (show-list-helper xs))))))))

(declare show-list (coalton:List Integer -> String))
(define (show-list lst)
  (<> "#(" (<> (show-list-helper lst) ")")))

;; TODO: Implement reverse-list
;; Hint: Use an accumulator for efficiency
(declare reverse-list (coalton:List Integer -> coalton:List Integer))
(define (reverse-list lst)
  ;; Your implementation here
  ;; Try both naive recursion and accumulator style
  Nil)  ;; Replace this

;; Test helper
(declare list-equal (coalton:List Integer -> coalton:List Integer -> Boolean))
(define (list-equal lst1 lst2)
  (match lst1
    ((Nil)
     (match lst2
       ((Nil) True)
       ((Cons _ _) False)))
    ((Cons x xs)
     (match lst2
       ((Nil) False)
       ((Cons y ys) (and (== x y) (list-equal xs ys)))))))

(define main
  (let ((original (make-list 1 2 3 4 5))
        (reversed (reverse-list original))
        (expected (make-list 5 4 3 2 1)))
    (progn
      (println (<> "Original: " (show-list original)))
      (println (<> "Reversed: " (show-list reversed)))
      (println (<> "Correct? "
                  (if (list-equal reversed expected)
                      "yes"
                      "no"))))))
```

**Solution hints:**
- Naive approach: `reverse [] = []`, `reverse (x:xs) = reverse xs ++ [x]`
- Efficient approach: Use accumulator that builds result as you traverse
- Think about how cons adds to the *front* of a list

---

## Why Recursion Before Iteration?

I used to think teaching recursion first was just functional programming dogma. But after writing enough Coalton, I see the wisdom:

1. **Recursion matches the data structure** – Lists are recursive, so recursive processing is natural
2. **Base cases force completeness** – You can't forget edge cases when pattern matching
3. **Immutability is the default** – No index variables to accidentally mutate
4. **Composition emerges naturally** – Recursive functions compose better than loops

The compiler becomes your training partner, catching incomplete patterns and type mismatches before they become bugs.

---

**Stay tuned for pattern matching...**

---

