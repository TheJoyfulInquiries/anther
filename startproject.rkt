#lang racket

#|
# How `startproject <name> <parent?> <template?> will work:
1. Make sure <template?> exists and is compliant
  1a. If template is supplied, make sure it exists and is compliant
  1b. If template is not supplied, fallback to `<anther-home>/.anther.toml` default template;
      make sure that exists and is compliant
1. Make sure <parent?>/<name> isn't provisioned
  1a. If parent is supplied, make sure the parent path exists
  1b. If parent is not supplied, fallback to `<anther-home>/.anther.toml` default path
  2. Ensure <parent>/<name> doesn't exist or exists but is empty


# Anther Template Compliance:
- .anther_template.toml
  - "initial project toml": Path
  - 
|#


;; +------------------------------------------+
;; | THE LIBRARY                              |
;; +------------------------------------------+

;; +------------------------------------------+
;; | THE TEST SUITE                           |
;; +------------------------------------------+

(module+ test


  )

;; +------------------------------------------+
;; | THE EXECUTABLE                           |
;; +------------------------------------------+