#lang racket

;Basic shell that uses Racket as its scripting language

(require "shell.rkt")

(define namespace (module->namespace "shell.rkt"))

(eval (read) namespace)
