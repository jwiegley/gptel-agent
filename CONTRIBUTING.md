# Contributing to gptel-agent

Thank you for your interest in contributing to gptel-agent! This document provides guidelines for contributions.

## Development Setup

### Prerequisites

- Emacs 29.1 or later
- [Eask](https://github.com/emacs-eask/cli) for project management
- gptel 0.9.9 or later

### Getting Started

1. Clone the repository:
   ```bash
   git clone https://github.com/jwiegley/dot-emacs.git
   cd dot-emacs/lisp/gptel-agent
   ```

2. Install dependencies:
   ```bash
   eask install-deps
   ```

3. Run tests to verify setup:
   ```bash
   eask test ert test/*.el
   ```

## Code Style

### General Guidelines

- Use lexical binding in all files:
  ```elisp
  ;;; filename.el --- Description -*- lexical-binding: t; -*-
  ```

- Follow Emacs Lisp conventions for naming:
  - Public functions: `gptel-agent-function-name`
  - Private functions: `gptel-agent--private-function`
  - Predicates: `gptel-agent-foo-p`
  - Buffer-local variables: `gptel-agent--variable-name`

- Keep lines under 80 characters when possible

- Use `cl-lib` functions, not deprecated `cl` package

### Docstrings

All public functions must have comprehensive docstrings:

```elisp
(defun gptel-agent-example-function (arg1 &optional arg2)
  "Brief one-line description.

Detailed explanation of what the function does.

ARG1 is the primary input.
ARG2 is an optional secondary input (default: nil).

Returns the processed result or nil on failure."
  ...)
```

### defcustom Requirements

All `defcustom` variables must include:
- `:type` declaration
- `:group` assignment
- Comprehensive docstring

```elisp
(defcustom gptel-agent-example-option t
  "Description of what this option controls.

When non-nil, enables the feature.  When nil, disables it."
  :type 'boolean
  :group 'gptel-agent)
```

## Testing

### Writing Tests

- Create tests in `test/` directory
- Name test files `gptel-agent-*-test.el`
- Use ERT (Emacs Lisp Regression Testing)

```elisp
(ert-deftest gptel-agent-example-test ()
  "Test description."
  (should (equal expected actual)))
```

### Test Helpers

Use the shared test utilities in `test/test-helper.el`:

```elisp
(require 'test-helper)

(ert-deftest my-test ()
  (gptel-agent-test--with-temp-env
    ;; Test code with isolated environment
    ))
```

### Running Tests

```bash
# All tests
eask test ert test/*.el

# Specific test file
eask test ert test/gptel-agent-permissions-test.el

# Interactive testing
emacs -Q -l test/run-tests.el
```

### Test Coverage Expectations

- All public functions should have tests
- Edge cases and error conditions should be tested
- SQLite-dependent tests should use `skip-unless`

## Pull Request Process

### Before Submitting

1. **Run all tests**: Ensure no regressions
   ```bash
   eask test ert test/*.el
   ```

2. **Byte-compile**: Check for warnings
   ```bash
   eask compile
   ```

3. **Lint**: Check package conventions
   ```bash
   eask lint package
   ```

4. **Update documentation**: If adding features, update:
   - Docstrings
   - README.org (if user-facing)
   - CHANGELOG.md

### Commit Messages

Follow conventional commit format:

```
type: Brief description (50 chars max)

Longer explanation if needed. Wrap at 72 characters.

- Bullet points for multiple changes
- Reference issues: Fixes #123
```

Types:
- `feat`: New feature
- `fix`: Bug fix
- `docs`: Documentation only
- `refactor`: Code change that neither fixes bug nor adds feature
- `test`: Adding or updating tests
- `chore`: Build process or auxiliary tool changes

### PR Description

Include:
- What the change does
- Why it's needed
- How to test it
- Any breaking changes

## Module Guidelines

### Adding New Modules

1. Create `gptel-agent-modulename.el` with proper header
2. Create corresponding `test/gptel-agent-modulename-test.el`
3. Update `Eask` to include the new file
4. Update `test/run-tests.el` to load the tests
5. Document in `docs/architecture.md`

### Module Header Template

```elisp
;;; gptel-agent-modulename.el --- Brief description -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <email@example.com>
;; Maintainer: Your Name <email@example.com>
;; Keywords: tools convenience ai llm
;; Homepage: https://github.com/jwiegley/dot-emacs/tree/master/lisp/gptel-agent

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Detailed description of what this module provides.

;;; Code:

(require 'cl-lib)
;; Other requires...

;;; Customization

(defgroup gptel-agent-modulename nil
  "Module description."
  :group 'gptel-agent
  :prefix "gptel-agent-modulename-")

;;; Implementation

;; Your code here...

(provide 'gptel-agent-modulename)
;;; gptel-agent-modulename.el ends here
```

## Reporting Issues

When reporting bugs, please include:

1. Emacs version (`M-x emacs-version`)
2. gptel-agent version
3. Steps to reproduce
4. Expected vs actual behavior
5. Relevant `*gptel-log*` output

## Questions?

- Check existing issues and documentation first
- Open an issue for questions about contributing
- Tag maintainers for urgent matters

Thank you for contributing!
