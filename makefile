all:
	~/racket/racket/bin/raco exe -o lush main.rkt
	rm release -rf
	mkdir release
	mkdir release/lush
	~/racket/racket/bin/raco distribute release/lush lush
	cp libnotify.so release/lush/lib/plt/lush/collects
	cp libsignals.so release/lush/lib/plt/lush/collects
	cp libterminal.so release/lush/lib/plt/lush/collects
	
scribble-docs:
	rm docs -rf
	~/racket/racket/bin/scribble --htmls --dest scribble-docs documentation/lush.scrbl
	mv scribble-docs/lush docs
	rmdir scribble-docs

package:
	cd release;	tar cfJ lush.tar.xz lush
