all: clean
	cd src; ~/racket/racket/bin/raco exe -o rain main.rkt
	mkdir release
	mkdir release/rain
	~/racket/racket/bin/raco distribute release/rain src/rain
	
	g++ -shared -o libnotify.so -fPIC src/notify.cpp -std=c++11
	cp libnotify.so release/rain/lib/plt/rain/collects

	gcc -shared -o libsignals.so -fPIC src/signals.c
	cp libsignals.so release/rain/lib/plt/rain/collects
	
	gcc -shared -o libterminal.so -fPIC src/terminal.c
	cp libterminal.so release/rain/lib/plt/rain/collects

clean:
	$(RM) release -rf
	$(RM) *.so
	$(RM) src/rain
	
scribble-docs:
	rm docs -rf
	~/racket/racket/bin/scribble --htmls --dest scribble-docs documentation/rain.scrbl
	mv scribble-docs/rain docs
	rmdir scribble-docs

package:
	cd release;	tar cfJ rain.tar.xz rain
