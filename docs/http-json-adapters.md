# HTTP and JSON Adapters for Smelter

## Overview
This document describes the HTTP and JSON adapters added to Smelter, providing network and data serialization capabilities for Coalton scripts.

## HTTP Adapter (`src/stdlib/smelter-http.lisp`)

### Features
- Simple HTTP client functionality using drakma
- Support for GET, POST, PUT, DELETE methods
- Proper error handling with Result types

### API
```coalton
;; Error types
(define-type HttpError
  (NetworkError String)
  (TimeoutError)
  (HttpStatus Integer String))

;; Core functions
http-get : String -> (Result HttpError String)
http-post : String -> String -> (Result HttpError String)
http-put : String -> String -> (Result HttpError String)
http-delete : String -> (Result HttpError String)
```

### Implementation Details
- Uses drakma HTTP client library (loaded via Quicklisp)
- Automatically handles response body encoding (octets to string conversion)
- Returns successful responses (2xx status codes) as Ok
- Maps all other responses and errors to appropriate HttpError variants

## JSON Adapter (`src/stdlib/smelter-json.lisp`)

### Features
- Basic JSON parsing and stringification
- Type-safe JSON representation using Coalton types
- Currently supports primitive types (null, boolean, number, string)

### API
```coalton
;; JSON value representation
(define-type JsonValue
  JsonNull
  (JsonBool Boolean)
  (JsonNumber Integer)
  (JsonString String)
  (JsonArray (List JsonValue))    ;; Placeholder
  (JsonObject (List (Tuple String JsonValue))))  ;; Placeholder

;; Error types
(define-type JsonError
  (ParseError String)
  (TypeError String))

;; Core functions
parse-json : String -> (Result JsonError JsonValue)
stringify-json : JsonValue -> String
```

### Implementation Details
- Uses st-json library for parsing (loaded via Quicklisp)
- Simplified implementation focusing on primitive types
- Array and Object support defined but not fully implemented
- Stringify produces valid JSON for primitive types

## Build System Integration

### Dependencies Added
- `drakma` - HTTP client library
- `st-json` - JSON parsing library
- `flexi-streams` - For handling response encoding

### Load Order
1. Core Smelter stdlib modules
2. `smelter-http.lisp` - HTTP adapter
3. `smelter-json.lisp` - JSON adapter

## Usage Examples

### HTTP GET Request
```coalton
(define (fetch-data url)
  (let ((response (smelter/http:http-get url)))
    (match response
      ((Ok data) (println data))
      ((Err (smelter/http:NetworkError msg)) 
       (println (concat "Network error: " msg)))
      ((Err (smelter/http:HttpStatus code msg))
       (println (concat "HTTP " (concat (into code) (concat ": " msg)))))
      ((Err smelter/http:TimeoutError)
       (println "Request timed out")))))
```

### JSON Parsing
```coalton
(define (parse-simple-json)
  (let ((json-str "42")
        (result (smelter/json:parse-json json-str)))
    (match result
      ((Ok (smelter/json:JsonNumber n))
       (println (concat "Got number: " (into n))))
      ((Ok _) (println "Got other JSON value"))
      ((Err (smelter/json:ParseError msg))
       (println (concat "Parse error: " msg))))))
```

## Current Limitations

### HTTP Adapter
- No custom headers support
- No timeout configuration
- No streaming/chunked responses
- Basic error messages

### JSON Adapter
- Arrays return placeholder values
- Objects return placeholder values
- No JSON path/query support
- Limited to primitive types for full functionality

## Future Enhancements

### HTTP
- Add request configuration (headers, timeouts, auth)
- Support streaming responses
- Add connection pooling
- HTTP/2 support

### JSON
- Full array and object parsing/manipulation
- JSON path queries
- Schema validation
- Pretty printing options

## Testing
The adapters are integrated into the Smelter build and can be tested using the REPL or script files. Example scripts are provided in the `examples/` directory.

## Dependencies
Both adapters rely on established Common Lisp libraries loaded through Quicklisp, ensuring stability and compatibility.