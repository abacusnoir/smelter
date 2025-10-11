# Smelter CSV Library

The Smelter CSV library provides type-safe CSV parsing and generation capabilities for Coalton scripts. It offers a clean, functional API for working with comma-separated value data.

## Overview

The `smelter/csv` module provides functions to parse CSV strings, read CSV files, stringify CSV data structures, and write CSV files. All functions use Coalton's Result type for proper error handling.

## Types

### CSV
```coalton
(define-type (CSV)
  "A representation of CSV data as a list of rows, where each row is a list of strings."
  (List (List String)))
```

A CSV is represented as a list of rows, where each row is a list of strings representing the individual cell values.

### ParseError
```coalton
(define-type ParseError
  "An error that can occur during CSV parsing."
  (IOError String)
  (MalformedRow String))
```

Errors that can occur during CSV operations:
- `IOError`: File system related errors (file not found, permission denied, etc.)
- `MalformedRow`: CSV parsing errors (malformed data, quote mismatches, etc.)

## Functions

### parse-csv
```coalton
(declare parse-csv (String -> (Result ParseError CSV)))
```

Parse a string containing CSV data into a CSV structure.

**Parameters:**
- `content`: String containing CSV data

**Returns:** 
- `Ok CSV` on successful parsing
- `Err MalformedRow` if the CSV data is invalid

**Example:**
```coalton
(match (parse-csv "name,age,city\nAlice,30,New York\nBob,25,Boston")
  ((Ok csv-data) 
   ;; csv-data is now a list of rows: 
   ;; (("name" "age" "city") ("Alice" "30" "New York") ("Bob" "25" "Boston"))
   (println "Parsed successfully"))
  ((Err error) 
   (println "Parse error occurred")))
```

### read-csv
```coalton
(declare read-csv (String -> (Result ParseError CSV)))
```

Read and parse a CSV file from a given path.

**Parameters:**
- `filepath`: Path to the CSV file

**Returns:**
- `Ok CSV` on successful read and parse
- `Err IOError` if the file cannot be read
- `Err MalformedRow` if the file content is not valid CSV

**Example:**
```coalton
(match (read-csv "/path/to/data.csv")
  ((Ok csv-data) (process-csv-data csv-data))
  ((Err (IOError msg)) (println (mconcat (make-list "File error: " msg))))
  ((Err (MalformedRow msg)) (println (mconcat (make-list "Parse error: " msg)))))
```

### stringify-csv
```coalton
(declare stringify-csv (CSV -> String))
```

Convert a CSV data structure back into a valid CSV formatted string.

**Parameters:**
- `csv`: CSV data structure to stringify

**Returns:** String containing properly formatted CSV data

**Example:**
```coalton
(let ((data (make-list 
             (make-list "Product" "Price")
             (make-list "Apple" "1.99")
             (make-list "Banana" "0.59"))))
  (let ((csv-string (stringify-csv data)))
    (println csv-string))) ; Outputs: Product,Price\nApple,1.99\nBanana,0.59\n
```

### write-csv
```coalton
(declare write-csv (String -> CSV -> (Result fs:FSError Unit)))
```

Write a CSV data structure to a file, overwriting it if it exists.

**Parameters:**
- `filepath`: Path where to write the CSV file
- `csv`: CSV data structure to write

**Returns:**
- `Ok Unit` on successful write
- `Err FSError` if the file cannot be written

**Example:**
```coalton
(let ((data (make-list (make-list "id" "name") (make-list "1" "Alice"))))
  (match (write-csv "/tmp/output.csv" data)
    ((Ok _) (println "File written successfully"))
    ((Err error) (println "Write failed"))))
```

## Usage Patterns

### Reading and Processing CSV Data

```coalton
(define process-csv-file
  (fn (filepath)
    (match (read-csv filepath)
      ((Ok (Cons header rows))
       (progn
         (println (mconcat (make-list "Headers: " (show header))))
         (println (mconcat (make-list "Row count: " (show (length rows)))))
         (map (fn (row) (println (show row))) rows)))
      ((Ok Nil) 
       (println "Empty CSV file"))
      ((Err error) 
       (println (mconcat (make-list "Error: " (show error))))))))
```

### Creating and Writing CSV Data

```coalton
(define create-report
  (fn ()
    (let ((header (make-list "Name" "Score" "Grade"))
          (data (make-list
                 (make-list "Alice" "95" "A")
                 (make-list "Bob" "87" "B")
                 (make-list "Carol" "92" "A")))
          (csv (cons header data)))
      (write-csv "report.csv" csv))))
```

### Handling Quoted Fields

The library automatically handles quoted fields containing commas, newlines, or quotes:

```coalton
;; This CSV string will be parsed correctly:
(parse-csv "name,description\n\"John Doe\",\"Software Engineer, Lead Developer\"")
;; Results in: (("name" "description") ("John Doe" "Software Engineer, Lead Developer"))
```

## Error Handling

Always use pattern matching to handle potential errors:

```coalton
(define safe-csv-operation
  (fn (filepath)
    (match (read-csv filepath)
      ((Ok data) 
       ;; Process successful result
       (process-data data))
      ((Err (IOError msg))
       ;; Handle file system errors
       (println (mconcat (make-list "File error: " msg))))
      ((Err (MalformedRow msg))
       ;; Handle CSV parsing errors  
       (println (mconcat (make-list "CSV format error: " msg)))))))
```

## Complete Example: Sales Report Generator

See `examples/csv-report-generator.coal` for a complete example that:
1. Reads sales data from a CSV file
2. Parses each row into a structured Sale type
3. Calculates totals by product
4. Generates a summary report
5. Writes the report to a new CSV file

To run the example:
```bash
make test-csv-example
```

## Integration with Other Smelter Modules

The CSV library integrates seamlessly with other Smelter modules:

- **File System**: Uses `smelter/adapters/fs` for file operations
- **CLI**: Combine with `smelter/adapters/cli` for command-line tools
- **JSON**: Convert between CSV and JSON formats for data processing

## Testing

Run the CSV library tests with:
```bash
make test-csv
```

The test suite covers:
- Basic CSV parsing
- Quoted field handling
- Error conditions
- File I/O operations
- Round-trip conversions

## Best Practices

1. **Always handle errors**: Use pattern matching on Result types
2. **Validate data**: Parse strings into appropriate types after CSV parsing
3. **Use type-safe structures**: Define custom types for your CSV data
4. **Handle empty files**: Check for empty CSV data before processing
5. **Quote when necessary**: The library handles quoting automatically during stringify operations

## Dependencies

The CSV library requires:
- `cl-csv`: Common Lisp CSV parsing library (automatically loaded)
- `smelter/adapters/fs`: File system operations
- `coalton-prelude`: Core Coalton functionality

These dependencies are automatically available when using Smelter.