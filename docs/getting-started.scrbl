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

@title[#:version ""]{Getting started}

This chapter shows you how to get Lush and introduces some basic features. For more in-depth resources
read on to the next chapters.

@section{Installation}

@(require "../version.rkt")

Lush requires a Unix environment. It currently is only tested on the Windows Subsystem for Linux, but should work for any *nix.
If you want to use it with WSL, make sure you have the creators update as it substantially improved WSL.

Open bash and get the latest release from @(hyperlink "https://github.com/patrick-lafferty/lush/releases" "github"):

@exec{wget https://github.com/patrick-lafferty/lush/releases/@(version)/lush.tar.xz}

Then extract lush some place you want. To put lush in your home dir:

@exec{tar xf lush.tar.xz ~/lush}

Now you can run Lush:

@exec{lush/lush}

@section{Command line}

After starting Lush you are in the command line. By default anything you enter will be interpreted as Racket code.
As soon as Lush can parse something (ie once the brackets are balanced) it will interpret the code. Try it out:

@racketblock[(displayln "hello, world")]

Lush supports multi-line editing. For example to write a multi-line function, simply omit the closing 
bracket and press enter. You can keep entering new lines and once Lush detects the last closing bracket, it joins
all the lines you entered and interprets it.

@racketblock[(define (greet name)

(printf "hello, ~a" name))]

Lush won't create the greet function until the last ). If you want to cancel input simply press Ctrl+D.

@section[#:tag "getting-started-shell"]{Shell}

You can run commands similar to a Unix shell like Bash. To do so, wrap your commands inside curly braces like so:

@nested[#:style 'code-inset]{{ls}}

Lush supports redirects:

@nested[#:style 'code-inset]{{ls > output.txt}}

as well as piping:

@nested[#:style 'code-inset]{{ls | grep rkt | sort | uniq -c}}

For more information on the shell see @secref["shell"].

