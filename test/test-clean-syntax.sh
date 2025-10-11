#!/bin/bash
# Test clean Coalton syntax support
set -e

SMT="./smt"

echo "Testing clean Coalton syntax..."
echo

# Test 1: Basic arithmetic with correct function names
cat > /tmp/test-clean1.coal << 'EOF'
(declare add (Integer -> Integer -> Integer))
(define (add x y) (+ x y))

(define main
  (io-println (show-int (add 2 3))))
EOF

echo "Test 1: Basic arithmetic with io-println and show-int"
if $SMT run /tmp/test-clean1.coal 2>&1 | grep -q "5"; then
    echo "✓ Test 1 passed"
else
    echo "✗ Test 1 failed"
    $SMT run /tmp/test-clean1.coal
    exit 1
fi

# Test 2: With leading whitespace in shebang
cat > /tmp/test-clean2.coal << 'EOF'
  #!/usr/bin/env smt run
  (declare greet (String -> String))
  (define (greet name)
    (<> "Hello, " name))

  (define main
    (println (greet "World")))
EOF

echo "Test 2: Script with leading whitespace"
if $SMT run /tmp/test-clean2.coal 2>&1 | grep -q "Hello, World"; then
    echo "✓ Test 2 passed"
else
    echo "✗ Test 2 failed"
    $SMT run /tmp/test-clean2.coal
    exit 1
fi

# Test 3: Boolean with show (currently only supports Integer)
cat > /tmp/test-clean3.coal << 'EOF'
(declare double (Integer -> Integer))
(define (double n)
  (* n 2))

(define main
  (println (show (double 21))))
EOF

echo "Test 3: Show function with double"
if $SMT run /tmp/test-clean3.coal 2>&1 | grep -q "42"; then
    echo "✓ Test 3 passed"
else
    echo "✗ Test 3 failed"
    $SMT run /tmp/test-clean3.coal
    exit 1
fi

echo
echo "All clean syntax tests passed!"
