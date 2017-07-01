#|
MIT License
Copyright (c) 2017 Patrick Lafferty
Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:
The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.
THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
|#
#lang scribble/manual
@(require scribble/core
          scribble/manual)

@margin-note{This page was last updated on June 29/2017}
@title[#:tag "top" #:version "" ]{LuSH - The Lu Shell}

@author["Patrick Lafferty"]


Lush is a command-line shell that uses Racket as its scripting language.
It currently features:

@itemlist[@item{a basic command language that supports job control, piping, and standard stream redirects}
        @item{a Racket interpreter}]

@table-of-contents[]

@; ------------------------------------------------------------------------
@include-section["getting-started.scrbl"]
@include-section["shell.scrbl"]
@include-section["sh-lang.scrbl"]
@include-section["interpreter.scrbl"]
@include-section["filesystem.scrbl"]

@index-section[]