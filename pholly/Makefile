all: nbody.php
	../src/_build/default/bin/main.exe nbody.php > _test.c
	gcc _test.c -I/usr/include/mysql -I/usr/include/mysql/mysql -lmysqlclient `pkg-config --cflags --libs glib-2.0`
