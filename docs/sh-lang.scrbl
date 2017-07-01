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

@title[#:tag "sh-lang"]{Sh Lang}

Sh lang is the mini language that is used inside any curly braces {}. This page
goes into the details of how it works.

@section{Parsing}

We will use the following example:

@nested[#:style 'code-inset]{{ls | grep rkt | sort -r > sorted.txt}}

First since Racket uses pairs of |'s to quote names, we replace | with 'pipe before
Racket gets a chance to parse it. Then we get a list with the following elements:

@nested[#:style 'code-inset]{
(ls
    'pipe
    grep
    rkt
    'pipe
    sort
    -replace
    >
    sorted.txt)}

Then we split the list into a list of lists separated by 'pipes (we'll call them run-lists), collecting any redirects into their own list.
For each run-list, if the first element is an executable in the current path then we escape the list to:

@nested[#:style 'code-inset]{(run (symbol->string 'the-first-element) (escape-args 'rest-of-the-list))}

where escape-args wraps values with (!!local-or-string x), so the interpreter will
check to see if x is a Racket definition or form, or otherwise treat it as a string.

So the grep run-list would look like:

@nested[#:style 'code-inset]{(run "grep" (!!local-or-string rkt))}

Once thats done for each run-list we collect them all into one list, add any redirects we found,
and wrap it with a pipe call which is responsible for launching subprocesses and setting up file streams:

@nested[#:style 'code-inset]{
(pipe 
    '(
        (run "ls") 
        (run "grep" (!!local-or-string rkt)) 
        (run "sort" (!!local-or-string -r)))
    '(!!local-or-string sorted.txt))}


@section{Running}

When interpreting sh-lang, the interpreter looks for any (!!local-or-string x) pairs. 
Depending on x, one of the following will happen:

@itemlist[@item{if x refers to a variable then its value is subsituted}
@item{if x refers to a procedure then it is applied}
@item{if x refers to a Racket form like if, cond etc then it gets interpreted}
@item{otherwise x gets treated as a string}]