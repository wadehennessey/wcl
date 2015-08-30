
all:	cl clx pprint logical-pathnames pcl 

dist:	
	make cl WCL_CONFIGURATION=`pwd`/src/build/DIST_CONFIGURATION
	rm -rf dist-bin
	cp ./src/build/USER_CONFIGURATION CONFIGURATION

cl:
	rm -rf dist-bin dist-lib
	cp -r bin dist-bin
	cp -r lib dist-lib
	(cd src/build; ./compile-all)

clx:
	(cd src/build; ../../bin/wcl -m 24000 < compile-clx-script.lisp)

pprint:
	(cd src/cl/pprint; make install)

pcl:
	(cd src/pcl; make install)

logical-pathnames:
	(cd src/misc; make install)

rebuild:
	rm -rf dist-bin dist-lib
	mv bin dist-bin
	mv lib dist-lib
	mkdir bin lib
	(cd src/build; ./compile-all)

clean:
	find . -name "*.*bin*" -exec rm -f {} \;
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
	cp bin/wcl /usr/bin
	cp lib/* /usr/lib64
	/sbin/ldconfig

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
