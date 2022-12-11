all: array5.c
	sed -e "s/#__C__ //g" array5.c > _test.c
	sed -e "s/\/\*SED EATS THE AMPERSAND\*\/\&//g" _test.c > __test.c
	gcc -std=c11 __test.c -I/usr/include/mysql -I/usr/include/mysql/mysql -I/usr/include/glib-2.0/glib -lmysqlclient `pkg-config --cflags --libs glib-2.0`
