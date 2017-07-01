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

@title[#:tag "filesystem" #:version ""]{Filesystem}

@section{Detecting changes to files}

Its sometimes useful to get notified when certain files change so you can
perform an action. As an example, I wanted to automatically rebuild the Scribble documentation
for Lush whenever I changed the docs. To do this, you can use the watch procedure. 

@defproc[(watch [path string?] [func proc?]) void]{
    Runs @racket[func] whenever @racket[path] is created/modified/deleted
    
    @racket[path] can either be a directory or a file. If @racket[path] is a directory
    then @racket[func] will run whenever any file directly below @racket[path] is created/deleted/modified

    @racket[watch] returns immediately. @racket[func] runs in its own thread.
    
    Note: it wont recurse to sub-directories.
    }

Example:

@racketblock[(watch "/home/pat/projects/lush/docs" 
                (lambda (x) {scribble --htmls --dest docs docs/lush.scrbl}))]

To stop watching use the stop-watching proc:

@defproc[(stop-watching [path string?]) void]{
    Removes the watch thread for @racket[path].
}

To stop the watch for the above example:

@racket[(stop-watching "/home/pat/projects/lush/docs")]