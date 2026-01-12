# gptel-agent Tests

This directory contains the test suite for gptel-agent modules.

## Running Tests

### Run All Tests

```bash
# From the gptel-agent directory
emacs -batch -l test/run-tests.el
```

### Run Specific Test File

```bash
# Load and run a specific test file
emacs -batch -L . -L test -l test/gptel-agent-permissions-test.el \
  -f ert-run-tests-batch-and-exit
```

### Interactive Testing

For interactive testing and debugging:

```elisp
;; In Emacs, from the gptel-agent directory
(add-to-list 'load-path default-directory)
(add-to-list 'load-path (expand-file-name "test"))

;; Load the test file
(load-file "test/gptel-agent-permissions-test.el")

;; Run all tests
(ert t)

;; Run tests matching a pattern
(ert "gptel-agent-permissions-test-pattern-*")

;; Run a specific test
(ert 'gptel-agent-permissions-test-cache-basic)
```

## Test Organization

### gptel-agent-permissions-test.el

Comprehensive tests for the permissions module covering:

- **Pattern Matching**: Glob-style pattern matching for tool calls
- **Permission Resolution**: Hierarchy of permission rules (tool-specific → universal → fallback)
- **Configuration Loading**: Loading and parsing `.gptel-agent.el` files
- **Cache Management**: Permission caching and invalidation
- **Tool Name Normalization**: Case-insensitive tool name handling
- **Integration**: End-to-end permission checking scenarios

### Test Structure

Tests follow ERT conventions:

```elisp
(ert-deftest test-name ()
  "Test description."
  (should (equal expected actual))
  (should-not (null value))
  (should-error (function-that-errors)))
```

### Test Helpers

The test suite includes helpers for:

- Creating temporary project directories
- Writing test configuration files
- Automatic cleanup of resources
- Cache invalidation between tests

## Writing New Tests

When adding new tests:

1. Use descriptive test names: `module-test-feature-scenario`
2. Document what the test verifies in the docstring
3. Use test helpers for setup/teardown
4. Clean up resources (files, cache, state)
5. Test both success and error cases

Example test structure:

```elisp
(ert-deftest gptel-agent-module-test-feature ()
  "Test that feature works correctly."
  (gptel-agent-permissions-test--with-cleanup
    ;; Setup
    (let ((dir (gptel-agent-permissions-test--make-temp-project)))
      ;; Test body
      (should (equal expected actual)))))
```

## Dependencies

Tests require:

- Emacs 29.1+
- ERT (built-in)
- gptel-agent modules under test

## Continuous Integration

Tests are designed to run in batch mode for CI/CD pipelines:

```bash
#!/bin/bash
# CI test script
emacs -batch -L . -L test -l test/run-tests.el 2>&1 | tee test-results.log
exit ${PIPESTATUS[0]}
```

## Debugging Test Failures

For detailed test output:

```bash
# Verbose output
emacs -batch -L . -L test -l test/gptel-agent-permissions-test.el \
  --eval "(setq ert-batch-print-level 10 ert-batch-print-length 100)" \
  -f ert-run-tests-batch-and-exit
```

For interactive debugging:

```elisp
;; Load test file interactively
(load-file "test/gptel-agent-permissions-test.el")

;; Run test with debugger
(setq debug-on-error t)
(ert 'failing-test-name)

;; Step through test
(debug-on-entry 'function-being-tested)
```
