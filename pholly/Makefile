a: nbody.php
	../src/_build/default/bin/main.exe nbody.php > _test.php
	sed -e "s/#__C__ //g" _test.php > _test.c
	gcc -O3 -Wno-incompatible-pointer-types _test.c -I/usr/include/mysql -I/usr/include/mysql/mysql -I. -lmysqlclient -lm `pkg-config --cflags --libs glib-2.0`
