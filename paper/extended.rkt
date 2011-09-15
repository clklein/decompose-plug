#lang racket/base
(provide extended-version?)
(define extended-version? (getenv "EXTENDED_VERSION"))
