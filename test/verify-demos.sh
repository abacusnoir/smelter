#!/bin/bash
set -e

echo "=== DEMO VERIFICATION ==="
echo

total=0
passed=0
failed=0

for demo in examples/showcase/*.coal; do
    [ -f "$demo" ] || continue
    total=$((total + 1))

    echo "Testing: $(basename "$demo")"

    # Check it runs (no timeout on macOS, rely on script finishing quickly)
    # Use 'smt' from PATH (for CI) or './smt' (for local dev)
    SMT_CMD="smt"
    if [ ! -x "$(command -v smt)" ] && [ -x "./smt" ]; then
        SMT_CMD="./smt"
    fi

    if $SMT_CMD run "$demo" > /tmp/demo-output.txt 2>&1; then
        echo "  ✓ Runs successfully"

        # Check it's under 50 lines
        lines=$(wc -l < "$demo" | tr -d ' ')
        if [ "$lines" -lt 50 ]; then
            echo "  ✓ Under 50 lines ($lines)"
        else
            echo "  ⚠ Warning: $lines lines (target: <50)"
        fi

        # Check for type safety demonstration
        if grep -q "Result\|Optional\|match\|Type" "$demo"; then
            echo "  ✓ Demonstrates type safety"
        fi

        # Check for helpful comments
        if grep -q "Type-safe\|type safety" "$demo"; then
            echo "  ✓ Has educational comments"
        fi

        passed=$((passed + 1))
        echo "  ✅ PASSED"
    else
        echo "  ✗ Failed to run"
        echo "  Error output:"
        cat /tmp/demo-output.txt | head -5 | sed 's/^/    /'
        failed=$((failed + 1))
        echo "  ❌ FAILED"
    fi
    echo
done

echo "=== SUMMARY ==="
echo "Total demos: $total"
echo "Passed: $passed"
echo "Failed: $failed"
echo

if [ "$failed" -eq 0 ]; then
    echo "✅ All demos verified successfully!"
    exit 0
else
    echo "❌ Some demos failed verification"
    exit 1
fi
