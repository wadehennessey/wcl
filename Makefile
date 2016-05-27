
all:	cl

cl:
	rm -rf dist-bin dist-lib
	cp -r bin dist-bin
	cp -r lib dist-lib
	(cd src/build; ./compile-all)

# clx currently has a bug preventing it from working
clx:
	(cd src/build; ../../bin/wcl -m 24000 < compile-clx-script.lisp)

# this currently doesn't work properly when done repeatedly. Use "cl" target
# instead.
rebuild:
	rm -rf dist-bin dist-lib
	mv bin dist-bin
	mv lib dist-lib
	mkdir bin lib
	(cd src/build; ./compile-all)

clean:
	find . -name "*.o" -exec rm -f {} \;
	find . -name "*[~,#]" -exec rm -f {} \;
	rm -f src/cl/functions/*.wcl
	rm -f src/cl/decls/*.wcl
	rm -f TAGS
	find src/compiler -name "*.wcl" -exec rm -f {} \;
	find src/clx -name "*.wcl" -exec rm -f {} \;
	find src/main -name "*.wcl" -exec rm -f {} \;
	find src/misc -name "*.wcl" -exec rm -f {} \;

install:
	cp bin/wcl /usr/local/bin
	cp lib/* /usr/local/lib64
	cp include/lisp.h /usr/local/include
	/sbin/ldconfig

dist:	
	make cl WCL_CONFIGURATION=`pwd`/src/build/DIST_CONFIGURATION
	rm -rf dist-bin
	cp ./src/build/USER_CONFIGURATION CONFIGURATION

distbins:
	rm -rf dist-lib dist-bin
	make clean
	(cd ..; tar -cf wcl/wcl-3.0-bin.tar wcl/bin wcl/lib \
                                         wcl/doc wcl/README \
                                         wcl/INSTALL wcl/Makefile)
	(cd ..; tar -cf wcl/wcl-3.0-src.tar wcl/CONFIGURATION \
					    wcl/emacs \
	 				    wcl/include \
					    wcl/src)
	gzip -f wcl-3.0-bin.tar
	gzip -f wcl-3.0-src.tar

tags:
	etags `find ./src -name "*.lisp" -print`  \
              `find ./src/cl/c-src -name "*.[c,h]" -print`
