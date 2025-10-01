# Process Adapter Improvements Theory - 2025-10-01

## Theorizing Improvements for Process Adapter Limitations

Based on the current Process adapter limitations, here are practical improvement strategies:

### Current Limitations

1. **String commands require manual construction (no automatic quoting)**
2. **No list-based API `(run (list "ls" "-la"))`**
3. **Limited process management (no background processes, PIDs, signals)**
4. **Platform-dependent shell behavior**
5. **Timeout requires GNU timeout command**

---

## Improvement Strategies

### 1. Safe Command Construction with List-Based API

**Problem**: String concatenation is error-prone; need automatic escaping

**Solution A: String Accumulation Bridge**
```coalton
;; Convert Coalton list to string via accumulation
(declare build-command (String -> (List String) -> String))
(define (build-command cmd args)
  "Build escaped command string from command and arguments"
  (fold (fn (arg acc)
          (concat acc " " (escape-shell-arg arg)))
        cmd
        args))

(declare escape-shell-arg (String -> String))
(define (escape-shell-arg arg)
  "Escape special characters for shell safety"
  (lisp String (arg)
    ;; Wrap in single quotes, escape embedded quotes
    (lisp:format lisp:nil "'~A'"
                 (lisp:substitute-if-not #\'
                   (lisp:lambda (c) (lisp:char/= c #\'))
                   arg))))

;; New list-based API
(declare run-args (String -> (List String) -> (Result ProcessError ProcessResult)))
(define (run-args cmd args)
  "Run command with list of arguments (safer than strings)"
  (run (build-command cmd args)))

;; Usage
(run-args "grep" (list "-r" "pattern with spaces" "/path/to/search"))
;; Internally becomes: grep -r 'pattern with spaces' '/path/to/search'
```

**Feasibility**: ✅ **Can implement immediately** - uses only current Coalton features

### 2. Command Builder DSL

**Problem**: Even with lists, complex commands are hard to construct

**Solution: Fluent Builder Pattern**
```coalton
(define-type CommandBuilder
  (CommandBuilder String (List String) (Optional String)))  ;; cmd, args, stdin

(declare cmd (String -> CommandBuilder))
(define (cmd command)
  "Start building a command"
  (CommandBuilder command (list) None))

(declare arg (CommandBuilder -> String -> CommandBuilder))
(define (arg builder argument)
  "Add argument to command"
  (match builder
    ((CommandBuilder c args stdin)
     (CommandBuilder c (append args (list argument)) stdin))))

(declare with-stdin (CommandBuilder -> String -> CommandBuilder))
(define (with-stdin builder input)
  "Set stdin for command"
  (match builder
    ((CommandBuilder c args _)
     (CommandBuilder c args (Some input)))))

(declare execute (CommandBuilder -> (Result ProcessError ProcessResult)))
(define (execute builder)
  "Execute the built command"
  (match builder
    ((CommandBuilder c args None)
     (run-args c args))
    ((CommandBuilder c args (Some input))
     (let ((command-str (build-command c args)))
       (shell-with-input command-str input)))))

;; Usage - much more readable!
(execute
  (arg (arg (arg (cmd "grep") "-r") "pattern") "/path"))
```

**Feasibility**: ✅ **Can implement now** - just syntactic sugar

### 3. Background Process Management via Filesystem

**Problem**: No way to track background processes without FFI handles

**Solution: PID File Tracking Pattern**
```coalton
(define-type ProcessHandle
  (ProcessHandle
    String   ;; PID file path
    String   ;; Command that was run
    Integer))  ;; Start timestamp

(declare run-background (String -> (Result ProcessError ProcessHandle)))
(define (run-background command)
  "Run command in background, return handle for tracking"
  (let ((timestamp (current-timestamp))
        (pid-file (concat "/tmp/smelter-pid-" (show timestamp))))
    (match (shell (concat "(" command ") & echo $! > " pid-file))
      ((Ok _)
       (match (fs:read-file pid-file)
         ((Ok pid-str)
          (Ok (ProcessHandle pid-file command timestamp)))
         ((Err e)
          (Err (ProcessFailed "Failed to read PID file")))))
      ((Err e) (Err e)))))
```

**Feasibility**: ✅ **Can implement now** - uses shell + filesystem

### 4. Platform-Independent Shell Abstraction

**Problem**: Shell commands vary across platforms

**Solution: OS Detection and Command Mapping**
```coalton
(define-type OS
  Linux
  MacOS
  Windows
  BSD
  Unknown)

(declare detect-os (Unit -> OS))
(define (detect-os)
  "Detect operating system"
  (match (run "uname -s")
    ((Ok (ProcessResult stdout _ _))
     (cond
       ((contains? "Linux" stdout) Linux)
       ((contains? "Darwin" stdout) MacOS)
       ((contains? "BSD" stdout) BSD)
       (True Unknown)))
    ((Err _)
     ;; Try Windows detection
     (match (run "ver")
       ((Ok _) Windows)
       ((Err _) Unknown)))))
```

**Feasibility**: ✅ **Can implement immediately**

### 5. Built-in Timeout Without External Command

**Option A: Pure Shell Timeout (Most Portable)**
```coalton
(declare run-with-timeout-portable (Integer -> String -> (Result ProcessError ProcessResult)))
(define (run-with-timeout-portable seconds command)
  "Portable timeout using shell job control"
  (shell (concat
    "{ "
    command " & "
    "CMD_PID=$!; "
    "(sleep " (show seconds) "; kill $CMD_PID 2>/dev/null) & "
    "TIMER_PID=$!; "
    "wait $CMD_PID; "
    "EXIT_CODE=$?; "
    "kill $TIMER_PID 2>/dev/null; "
    "exit $EXIT_CODE; "
    "}")))
```

**Feasibility**: ✅ **Can implement now** - pure shell solution

### 6. Process Signals Support

**Solution: Shell-Based Signal API**
```coalton
(define-type Signal
  SIGTERM   ;; Graceful termination
  SIGKILL   ;; Force kill
  SIGINT    ;; Interrupt (Ctrl+C)
  SIGHUP)   ;; Hangup

(declare send-signal (ProcessHandle -> Signal -> (Result ProcessError Unit)))
(define (send-signal handle signal)
  "Send signal to process"
  (match handle
    ((ProcessHandle pid-file _ _)
     (match (fs:read-file pid-file)
       ((Ok pid)
        (let ((sig-num (signal-number signal)))
          (match (run (concat "kill -" (show sig-num) " " pid))
            ((Ok _) (Ok Unit))
            ((Err e) (Err e)))))
       ((Err _) (Err (ProcessFailed "Cannot read PID")))))))
```

**Feasibility**: ✅ **Can implement now** - Unix signals via shell

---

## Implementation Phases

### Phase 1: Core Improvements (Immediate Value) ⭐
1. ✅ **Command Builder** - Safe string construction with `escape-shell-arg`
2. ✅ **List-Based API** - `run-args` wrapper avoiding manual string construction
3. ✅ **Platform Detection** - OS-aware command execution

**Effort**: 1-2 days | **Impact**: High - makes API safer and more ergonomic

### Phase 2: Process Management (High Value)
4. ✅ **Background Processes** - PID file tracking
5. ✅ **Process Signals** - SIGTERM, SIGKILL, etc.
6. ✅ **Portable Timeout** - Shell-based solution

**Effort**: 2-3 days | **Impact**: High - enables real process management

### Phase 3: Advanced Features (Nice to Have)
7. ✅ **Pipeline Builder** - Fluent pipe construction
8. ✅ **Process Groups** - Bulk operations
9. ⚠️ **Native Timeout** - Threading (needs research)

**Effort**: 3-5 days | **Impact**: Medium - improves ergonomics

### Phase 4: Research Projects (Long Term)
10. ⚠️ **Direct FFI to fork/exec** - When Coalton supports it
11. ⚠️ **Event-driven process monitoring** - inotify/kqueue
12. ⚠️ **Async process API** - When Coalton has async

**Effort**: Unknown | **Impact**: High - but depends on Coalton evolution

---

## Recommended File Structure

```
src/adapters/
  process.lisp           # Core (current implementation)
  process/
    builder.lisp         # Command building helpers (Phase 1)
    platform.lisp        # Cross-platform abstractions (Phase 1)
    background.lisp      # Background process management (Phase 2)
    signals.lisp         # Signal support (Phase 2)
    pipeline.lisp        # Pipeline construction (Phase 3)
```

---

## Key Insights

### What Works Now
- String accumulation to build commands from lists
- Shell escaping via single-quote wrapping
- PID file tracking for background processes
- Platform detection via uname/ver

### What to Avoid
- Direct Coalton list-to-CL-list conversion
- Complex FFI handles (Coalton limitations)
- Platform-specific APIs without abstraction

### Design Philosophy
> Start simple, add complexity only when needed. Working code beats perfect design.

All Phase 1 improvements can be implemented **without changing the core adapter**, maintaining 100% backward compatibility while adding power features.

---

## Next Steps

**Recommended**: Start with Phase 1 (Command Builder + List API + Platform Detection)
- Immediate value with minimal risk
- Foundation for later phases
- Maintains backward compatibility
- Addresses most painful limitations
