all: clean
	cd src; ~/racket/racket/bin/raco exe -o lush main.rkt
	mkdir release
	mkdir release/lush
	~/racket/racket/bin/raco distribute release/lush src/lush
	
	g++ -shared -o libnotify.so -fPIC src/notify.cpp -std=c++11
	cp libnotify.so release/lush/lib/plt/lush/collects

	gcc -shared -o libsignals.so -fPIC src/signals.c
	cp libsignals.so release/lush/lib/plt/lush/collects
	
	gcc -shared -o libterminal.so -fPIC src/terminal.c
	cp libterminal.so release/lush/lib/plt/lush/collects

clean:
	$(RM) release -rf
	$(RM) *.so
	$(RM) src/lush
	
scribble-docs:
	rm docs -rf
	~/racket/racket/bin/scribble --htmls --dest scribble-docs documentation/lush.scrbl
	mv scribble-docs/lush docs
	rmdir scribble-docs

package:
	cd release;	tar cfJ lush.tar.xz lush
