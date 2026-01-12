#!/bin/bash
# CL Mode Tests for Smelter
set -e

SMT="${SMT:-./smt}"
PASS=0
FAIL=0

pass() { echo -e "\033[0;32m[PASS]\033[0m $1"; ((PASS++)) || true; }
fail() { echo -e "\033[0;31m[FAIL]\033[0m $1"; ((FAIL++)) || true; }

echo "========================================"
echo "    Smelter CL Mode Tests"
echo "========================================"
echo ""

# Test 1: CL help
echo "Testing: CL help"
if $SMT cl --help 2>&1 | grep -q "CL Mode"; then
    pass "CL help shows CL Mode"
else
    fail "CL help missing CL Mode"
fi

# Test 2: Basic CL eval
echo "Testing: Basic CL eval"
result=$($SMT cl eval '(+ 1 2 3)')
if [ "$result" = "6" ]; then
    pass "CL eval basic arithmetic"
else
    fail "CL eval got '$result' expected '6'"
fi

# Test 3: CL string formatting
echo "Testing: CL string formatting"
result=$($SMT cl eval '(format nil "Hello ~A" "World")')
if [ "$result" = "Hello World" ]; then
    pass "CL format string"
else
    fail "CL format got '$result'"
fi

# Test 4: CL list operations
echo "Testing: CL list operations"
result=$($SMT cl eval '(reduce (function +) (list 1 2 3 4 5))')
if [ "$result" = "15" ]; then
    pass "CL list reduce"
else
    fail "CL list reduce got '$result'"
fi

# Test 5: CL lambda
echo "Testing: CL lambda"
result=$($SMT cl eval '(funcall (lambda (x) (* x x)) 7)')
if [ "$result" = "49" ]; then
    pass "CL lambda"
else
    fail "CL lambda got '$result'"
fi

# Test 6: CL defun in script
echo "Testing: CL script execution"
cat > /tmp/test-cl.lisp << 'EOF'
(defun square (x) (* x x))
(format t "~A~%" (square 5))
EOF
result=$($SMT cl run /tmp/test-cl.lisp)
if [ "$result" = "25" ]; then
    pass "CL script with defun"
else
    fail "CL script got '$result'"
fi

# Test 7: CL with shebang stripped
echo "Testing: CL shebang stripping"
cat > /tmp/test-cl-shebang.lisp << 'EOF'
#!/usr/bin/env smt-cl
(format t "Shebang stripped~%")
EOF
result=$($SMT cl run /tmp/test-cl-shebang.lisp)
if [ "$result" = "Shebang stripped" ]; then
    pass "CL shebang stripped correctly"
else
    fail "CL shebang got '$result'"
fi

# Test 7b: smt-cl direct file execution (simulates shebang)
echo "Testing: smt-cl direct file execution"
SMT_CL="${SMT_CL:-./smt-cl}"
if [ -x "$SMT_CL" ] || [ -L "$SMT_CL" ]; then
    result=$($SMT_CL /tmp/test-cl-shebang.lisp)
    if [ "$result" = "Shebang stripped" ]; then
        pass "smt-cl direct file execution"
    else
        fail "smt-cl direct got '$result'"
    fi
else
    echo "[SKIP] smt-cl symlink not found"
fi

# Test 8: CL macro usage
echo "Testing: CL macros"
result=$($SMT cl eval '(let ((x 5) (y 3)) (+ x y))')
if [ "$result" = "8" ]; then
    pass "CL let macro"
else
    fail "CL let macro got '$result'"
fi

# Test 9: CL cons/car/cdr
echo "Testing: CL list primitives"
result=$($SMT cl eval '(car (cons 1 (cons 2 nil)))')
if [ "$result" = "1" ]; then
    pass "CL list primitives"
else
    fail "CL list primitives got '$result'"
fi

# Test 10: CL missing file error
echo "Testing: CL missing file error"
if $SMT cl run /nonexistent/file.lisp 2>&1 | grep -q "not found\|Error"; then
    pass "CL missing file error"
else
    fail "CL missing file should error"
fi

# Test 11: CL with loop
echo "Testing: CL loop"
result=$($SMT cl eval '(loop for i from 1 to 5 sum i)')
if [ "$result" = "15" ]; then
    pass "CL loop sum"
else
    fail "CL loop got '$result'"
fi

# Cleanup
rm -f /tmp/test-cl.lisp /tmp/test-cl-shebang.lisp

echo ""
echo "========================================"
echo "    CL Mode Test Results"
echo "========================================"
echo "Passed: $PASS"
echo "Failed: $FAIL"
echo ""

if [ $FAIL -gt 0 ]; then
    echo -e "\033[0;31mSome tests failed!\033[0m"
    exit 1
else
    echo -e "\033[0;32mAll CL mode tests passed!\033[0m"
    exit 0
fi
