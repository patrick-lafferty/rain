all:
	raco exe -o lush main.rkt
	raco distribute release lush
	cp libnotify.so release/lib/plt/lush/collects
	cp libsignals.so release/lib/plt/lush/collects
	
docs:
	scribble --htmls --dest scribble-docs documentation/lush.scrbl
	mv scribble-docs/lush docs
	rmdir scribble-docs