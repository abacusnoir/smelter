#!/bin/bash
# performance-diagnosis.sh - Complete performance analysis for Smelter

set -e

echo "================================================"
echo "    SMELTER PERFORMANCE DIAGNOSIS SUITE"
echo "================================================"
echo

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Create results directory
mkdir -p performance-results
RESULTS_DIR="performance-results/$(date +%Y%m%d-%H%M%S)"
mkdir -p "$RESULTS_DIR"

echo "Results will be saved to: $RESULTS_DIR"
echo

# =============================================================================
# PART 1: BASELINE MEASUREMENTS
# =============================================================================

echo -e "${YELLOW}=== PART 1: BASELINE MEASUREMENTS ===${NC}"
echo

# Function to measure with high precision
measure_precise() {
    local cmd="$1"
    local name="$2"
    local iterations="${3:-10}"
    
    echo -n "$name: "
    
    # Warm up
    eval "$cmd" > /dev/null 2>&1 || true
    
    # Measure
    local times=()
    for i in $(seq 1 $iterations); do
        # Use perl for microsecond precision
        start=$(perl -MTime::HiRes=time -e 'printf "%.6f\n", time')
        eval "$cmd" > /dev/null 2>&1
        end=$(perl -MTime::HiRes=time -e 'printf "%.6f\n", time')
        
        duration=$(echo "$end - $start" | bc)
        times+=($duration)
    done
    
    # Calculate statistics
    total=0
    min=${times[0]}
    max=${times[0]}
    
    for t in "${times[@]}"; do
        total=$(echo "$total + $t" | bc)
        if (( $(echo "$t < $min" | bc -l) )); then
            min=$t
        fi
        if (( $(echo "$t > $max" | bc -l) )); then
            max=$t
        fi
    done
    
    avg=$(echo "scale=3; $total / $iterations" | bc)
    avg_ms=$(echo "scale=1; $avg * 1000" | bc)
    min_ms=$(echo "scale=1; $min * 1000" | bc)
    max_ms=$(echo "scale=1; $max * 1000" | bc)
    
    echo -e "avg: ${GREEN}${avg_ms}ms${NC} (min: ${min_ms}ms, max: ${max_ms}ms)"
    echo "$name,$avg_ms,$min_ms,$max_ms" >> "$RESULTS_DIR/measurements.csv"
}

# Baseline measurements
echo "Measuring baseline operations (10 iterations each)..."
echo "name,avg_ms,min_ms,max_ms" > "$RESULTS_DIR/measurements.csv"

measure_precise "true" "Shell true command"
measure_precise "echo test" "Shell echo"
measure_precise "sbcl --noinform --no-userinit --no-sysinit --eval '(quit)' --quit" "SBCL minimal"
measure_precise "sbcl --noinform --no-userinit --eval '(quit)' --quit" "SBCL with sysinit"
measure_precise "sbcl --noinform --eval '(quit)' --quit" "SBCL with userinit"
measure_precise "./smt eval 'nil' 2>/dev/null" "Smelter nil"
measure_precise "./smt eval '(+ 1 2)' 2>/dev/null" "Smelter arithmetic"
measure_precise "python3 -c 'exit()'" "Python3 baseline"
measure_precise "ruby -e 'exit'" "Ruby baseline"
measure_precise "node -e 'process.exit()'" "Node.js baseline"

echo

# =============================================================================
# PART 2: DETAILED PROFILING
# =============================================================================

echo -e "${YELLOW}=== PART 2: DETAILED PROFILING ===${NC}"
echo

# System call analysis (macOS uses dtruss instead of strace)
echo "Analyzing system calls..."
if command -v dtruss >/dev/null 2>&1; then
    sudo dtruss -c ./smt eval 'nil' 2>&1 | head -20 > "$RESULTS_DIR/syscall-summary.txt" || echo "Could not run dtruss (needs sudo)"
elif command -v strace >/dev/null 2>&1; then
    strace -c -o "$RESULTS_DIR/syscall-summary.txt" ./smt eval 'nil' 2>/dev/null || true
fi

echo
echo "Analyzing file operations..."
if command -v dtruss >/dev/null 2>&1; then
    sudo dtruss -t open ./smt eval 'nil' 2>&1 | head -50 > "$RESULTS_DIR/file-operations.txt" || echo "Could not run dtruss (needs sudo)"
    open_count=$(sudo dtruss -t open ./smt eval 'nil' 2>&1 | grep -c "open" 2>/dev/null || echo "0")
elif command -v strace >/dev/null 2>&1; then
    strace -e trace=open,openat,read,stat,fstat ./smt eval 'nil' 2>&1 | \
        grep -v "ENOENT" | \
        head -50 > "$RESULTS_DIR/file-operations.txt"
    open_count=$(strace -e trace=open,openat ./smt eval 'nil' 2>&1 | grep -c "open" || echo "0")
else
    echo "No system call tracing tool available"
    open_count="unknown"
fi

echo -e "Files opened during startup: ${RED}${open_count}${NC}"

echo

# =============================================================================
# PART 3: BINARY ANALYSIS
# =============================================================================

echo -e "${YELLOW}=== PART 3: BINARY ANALYSIS ===${NC}"
echo

echo "Binary information:"
ls -lh smt
file smt
echo

echo "Checking for debug symbols..."
if nm smt 2>/dev/null | grep -q "debug"; then
    echo -e "${RED}WARNING: Debug symbols found!${NC}"
    debug_size=$(nm smt 2>/dev/null | wc -l)
    echo "Number of symbols: $debug_size"
else
    echo -e "${GREEN}No debug symbols detected${NC}"
fi

echo
echo "Checking for embedded files..."
strings smt | grep -E "\.(lisp|fasl|asd)" | head -20 > "$RESULTS_DIR/embedded-files.txt"
embedded_count=$(strings smt | grep -E "\.(lisp|fasl|asd)" | wc -l)
echo -e "Embedded Lisp files references: ${YELLOW}${embedded_count}${NC}"

echo
echo "Checking for Quicklisp..."
if strings smt | grep -q "quicklisp"; then
    echo -e "${RED}WARNING: Quicklisp references found!${NC}"
    strings smt | grep "quicklisp" | head -5
else
    echo -e "${GREEN}No Quicklisp references found${NC}"
fi

echo

# =============================================================================
# PART 4: COMPARISON TABLE
# =============================================================================

echo -e "${YELLOW}=== PART 4: PERFORMANCE COMPARISON ===${NC}"
echo

# Create comparison table
printf "%-13s | %-12s | %-11s | %-9s | %s\n" \
       "Tool" "Startup Time" "Binary Size" "Type Safe" "REPL"
printf "%-13s | %-12s | %-11s | %-9s | %s\n" \
       "-------------" "------------" "-----------" "---------" "----"

# Measure each tool
for tool in "Smelter|./smt eval '(+ 1 2)'|smt" \
            "Python|python3 -c 'print(1+2)'|python3" \
            "Ruby|ruby -e 'puts 1+2'|ruby" \
            "Node.js|node -e 'console.log(1+2)'|node" \
            "Bash|bash -c 'echo \$((1+2))'|bash"; do
    
    IFS='|' read -r name cmd binary <<< "$tool"
    
    # Measure startup
    if command -v $binary &> /dev/null || [ -f "$binary" ]; then
        start=$(perl -MTime::HiRes=time -e 'printf "%.6f\n", time')
        eval "$cmd" > /dev/null 2>&1
        end=$(perl -MTime::HiRes=time -e 'printf "%.6f\n", time')
        duration=$(echo "($end - $start) * 1000" | bc)
        startup="${duration}ms"
        
        # Get binary size
        if [ "$binary" = "smt" ]; then
            size=$(ls -lh $binary | awk '{print $5}')
        else
            size=$(ls -lh $(which $binary) 2>/dev/null | awk '{print $5}' || echo "N/A")
        fi
    else
        startup="N/A"
        size="N/A"
    fi
    
    # Type safety and REPL
    case $name in
        Smelter) type_safe="Yes"; repl="Yes" ;;
        Python) type_safe="No"; repl="Yes" ;;
        Ruby) type_safe="No"; repl="Yes" ;;
        Node.js) type_safe="No"; repl="Yes" ;;
        Bash) type_safe="No"; repl="Yes" ;;
    esac
    
    printf "%-13s | %-12s | %-11s | %-9s | %s\n" \
           "$name" "$startup" "$size" "$type_safe" "$repl"
done

echo

# =============================================================================
# PART 5: RECOMMENDATIONS
# =============================================================================

echo -e "${YELLOW}=== PART 5: PERFORMANCE FIX RECOMMENDATIONS ===${NC}"
echo

# Analyze results and provide recommendations
current_time=$(grep "Smelter arithmetic" "$RESULTS_DIR/measurements.csv" | cut -d',' -f2)
target_time=66

echo "Current startup time: ${current_time}ms"
echo "Target startup time: ${target_time}ms"
echo -e "Gap: ${RED}$(echo "$current_time - $target_time" | bc)ms${NC}"
echo

echo "TOP PRIORITY FIXES:"
echo "==================="

if [ "$open_count" != "unknown" ] && (( $(echo "$open_count > 100" | bc -l) )); then
    echo -e "${RED}1. Excessive file operations (${open_count} files opened)${NC}"
    echo "   - Pre-compile all .lisp to .fasl"
    echo "   - Embed compiled code in binary"
    echo "   - Expected improvement: 50-100ms"
    echo
fi

if strings smt | grep -q "quicklisp"; then
    echo -e "${RED}2. Quicklisp is being loaded${NC}"
    echo "   - Remove ALL quicklisp references"
    echo "   - Pre-load required systems"
    echo "   - Expected improvement: 100-150ms"
    echo
fi

if file smt | grep -q "not stripped"; then
    echo -e "${YELLOW}3. Binary not stripped${NC}"
    echo "   - Run: strip smt"
    echo "   - Expected improvement: Smaller binary, slightly faster"
    echo
fi

if ! file smt | grep -q "UPX"; then
    echo -e "${YELLOW}4. Binary not compressed${NC}"
    echo "   - Run: upx --best smt"
    echo "   - Expected improvement: 50-70% size reduction"
    echo
fi

echo
echo -e "${GREEN}Full results saved to: $RESULTS_DIR${NC}"
echo