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

@title[#:tag "shell" #:version ""]{Shell}

@section{Running programs}

@section{Piping}

Piping takes the output from one program and uses it as the input for the next
without having to create a temporary file to store the intermediate results. So
instead of having to do:

@nested[#:style 'code-inset]{{ls > ls_results}

    {grep rkt ls_results > grep_results}

    {sort grep_results}
}

you can do:

@nested[#:style 'code-inset]{{ls | grep rkt | sort}}

@section{Redirection}

Programs have three standard streams, input (stdin), output (stdout) and error (stderr).
Normally when you run a program in the terminal stdin comes from the keyboard, and
stdout and stderr get displayed on the screen. With redirection you can change that,
for instance you can redirect stdin to read from a file, or stdout to output to a file.
The second example is useful when you want to save the resulting output of a program
to read again later. To redirectin Lush:
changed 
@itemlist[@item{@exec{ < in-file } redirects stdin}
    @item{@exec{ > out-file } redirects stdout}
    @item{@exec{ ^ err-file } redirects stderr}
]

Examples:

@nested[#:style 'code-inset]{{ls > results.txt}}
@nested[#:style 'code-inset]{{cat < results.txt | sort -r > sorted.txt}}
@nested[#:style 'code-inset]{{wget invalid://add.re/ss ^ err.txt}}