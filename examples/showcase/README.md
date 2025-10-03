# Smelter Showcase Examples

Real-world examples demonstrating Smelter's type-safe scripting capabilities.

## Running the Demos

```bash
# Run any demo
./smt run examples/showcase/config-validator.coal

# Verify all demos work
./test/verify-demos.sh
```

## Featured Demos

### config-validator.coal (19 lines)
**Problem**: Configuration validation that fails at runtime in other languages
**Solution**: Type-safe port validation with compile-time guarantees

```bash
./smt run examples/showcase/config-validator.coal
```

### error-handling.coal (27 lines)
**Problem**: Uncaught exceptions and null pointer errors in production
**Solution**: Result types force you to handle all error cases at compile time

```bash
./smt run examples/showcase/error-handling.coal
```

### type-safety.coal (29 lines)
**Problem**: Runtime type errors in dynamic languages
**Solution**: Compile-time type checking catches errors before deployment

```bash
./smt run examples/showcase/type-safety.coal
```

### rosetta.coal (31 lines)
**Problem**: Choosing between type safety and developer productivity
**Solution**: ML-style type inference gives you both

```bash
./smt run examples/showcase/rosetta.coal
```

### build-pipeline.coal (33 lines)
**Problem**: Fragile bash scripts with undefined variables
**Solution**: Type-safe build orchestration that can't fail silently

```bash
./smt run examples/showcase/build-pipeline.coal
```

### data-transform.coal (33 lines)
**Problem**: Data pipelines that fail on unexpected inputs
**Solution**: Type-safe transformations with guaranteed correctness

```bash
./smt run examples/showcase/data-transform.coal
```

## Key Takeaways

- **Same brevity as Python/Ruby** - Concise, readable code
- **Compile-time type safety** - Catch errors before runtime
- **No null pointer exceptions** - Optional and Result types
- **Pattern matching** - Exhaustive case handling
- **Fast startup** - ~43ms cold start
- **Zero dependencies** - Single 9.3MB binary

## For Your HN Post

These examples show that Smelter isn't just another scripting language - it's a practical solution to real pain points developers face daily:

1. Config errors caught at compile time, not in production
2. Error handling that can't be ignored
3. Data pipelines that guarantee type safety
4. Build scripts that won't fail silently

Try them yourself:
```bash
git clone https://github.com/yourusername/smelter
cd smelter
make build
./test/verify-demos.sh
```
