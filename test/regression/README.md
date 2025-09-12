# Smelter Regression Test Framework

Comprehensive test suite for validating Smelter functionality and documenting current capabilities and limitations.

## Overview

This regression test framework focuses on:
- **Working functionality**: Tests features that currently work correctly
- **Expected limitations**: Documents known issues that will be addressed in future versions  
- **Performance benchmarks**: Ensures startup time and binary size remain reasonable
- **Comprehensive reporting**: Clear visual feedback on test results

## Current Status

### ✅ Working Features
- Custom stdlib I/O functions (`smelter.stdlib.io:io-println`, `smelter.stdlib.io:io-print`)
- Custom stdlib system functions (`smelter.stdlib.system:current-time-millis`, `smelter.stdlib.system:sleep`)
- Pure Coalton translator functionality
- Script execution framework 
- REPL functionality
- CLI command structure

### ❌ Known Limitations (Expected Failures)
- Coalton prelude functions (`+`, `>`, `<`, `==`, etc.) - not yet available in Coalton namespace
- Higher-order functions (`map`, `filter`, `fold`) - requires prelude integration
- Arithmetic operations - awaiting prelude implementation
- Comparison operators - awaiting prelude implementation

## Quick Start

### Run All Tests
```bash
# Full regression test suite (includes clean build)
make test-regression

# Quick tests (skip rebuild)
make test-regression-quick
```

### Test Categories
```bash
# Basic functionality tests (version, help)
./test/regression/run-regression-tests.sh --basic

# All available tests
./test/regression/run-regression-tests.sh
```

## Test Framework Structure

```
test/regression/
├── run-regression-tests.sh    # Main test runner
├── test-specs.yaml           # Test specifications (YAML format)  
├── scripts/
│   └── simple-stdlib.coal    # Test script using working stdlib
└── README.md                # This documentation
```

## Test Runner Features

### Color-Coded Output
- 🟢 **Green [PASS]**: Test passed as expected
- 🔴 **Red [FAIL]**: Unexpected test failure (needs investigation)
- 🟣 **Purple [EXPECTED FAIL]**: Known limitation (documented expected failure)
- 🔵 **Blue [INFO]**: Status information
- 🟡 **Yellow [WARN]**: Warnings

### Test Types
- **Command tests**: Validate CLI commands (`--version`, `--help`)
- **Eval tests**: Test expression evaluation with expected outputs/failures
- **Script tests**: Execute .coal files and validate results
- **REPL tests**: Interactive REPL session validation
- **Performance tests**: Startup time and binary size benchmarks

### Environment Variables
```bash
SKIP_BUILD=1    # Skip clean build (use existing binary)
```

## Adding New Tests

### 1. Update test-specs.yaml
Add test specifications in YAML format:
```yaml
new_test_category:
  - name: "test_name"
    type: "eval"  # or "command", "script", "repl", "performance"
    input: "(test-expression)"
    expected_stdout: "expected output"
    expected_failure: true  # for known limitations
    note: "Description of what this tests"
```

### 2. Update run-regression-tests.sh
Add test execution in the `run_all_tests()` function:
```bash
log_info "=== New Test Category ==="
test_eval "test_name" "input" "expected_output" "expected_failure"
```

### 3. Create Test Scripts
Add .coal files to `scripts/` directory for script execution tests.

## Test Result Interpretation

### Success Rate Calculation
Success rate includes both **PASS** and **EXPECTED FAIL** results:
```
Success Rate = (PASSED + EXPECTED_FAILURES) * 100 / TOTAL_TESTS
```

This reflects that expected failures are documented limitations, not actual bugs.

### Sample Output
```
========================================
    Smelter Regression Test Report  
========================================
Date: 2024-01-15 10:30:00
Binary: ./smt (15M)

Test Results:
  Total tests:      25
  Passed:           18
  Failed:           0  
  Expected failures: 7
  Success rate:     100%

Current Status:
  ✅ Custom stdlib (I/O, system) working perfectly
  ✅ Pure Coalton translator functional
  ✅ Script execution framework in place
  ❌ Coalton prelude (arithmetic, higher-order functions) not yet available

🎉 All non-expected tests passed!
   Ready for next phase: Coalton prelude integration
========================================
```

## Integration with Make

The test framework integrates with the project's Makefile:

```bash
make test-regression         # Run comprehensive regression tests
make test-regression-quick   # Skip rebuild, run tests only
make test-all               # Run smoke tests + regression tests
```

## Performance Benchmarks

### Current Targets
- **Startup time**: < 2000ms for `smt --version`
- **Binary size**: < 100MB

### Performance Test Types
- Command execution speed
- Binary size validation
- Memory usage patterns (future)

## Extending the Framework

### Adding New Test Types
1. Create new test function in `run-regression-tests.sh`
2. Add corresponding YAML specification format
3. Update help and documentation

### Custom Test Runners
The framework supports custom test runners by:
- Following the existing function patterns (`test_*`)
- Using the logging functions (`log_success`, `log_failure`, etc.)
- Updating counters (`TOTAL_TESTS`, `PASSED_TESTS`, etc.)

## Troubleshooting

### Common Issues
1. **Binary not found**: Run `make build` first
2. **Permission denied**: Run `chmod +x test/regression/run-regression-tests.sh`
3. **Build failures**: Check SBCL and Quicklisp installation

### Debug Mode
Set additional verbosity:
```bash
# Enable debug output
DEBUG=1 ./test/regression/run-regression-tests.sh
```

## Contributing

When adding new features to Smelter:
1. Add corresponding regression tests
2. Update test specifications in YAML format
3. Ensure tests properly categorize as working vs. expected limitations
4. Update documentation to reflect new capabilities

## Future Enhancements

Planned improvements for the test framework:
- [ ] Parallel test execution
- [ ] Test result caching 
- [ ] Integration with CI/CD pipelines
- [ ] Memory usage benchmarking
- [ ] Cross-platform test validation
- [ ] Automated performance regression detection