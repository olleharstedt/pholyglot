all: test.c
	sed -e "s/#__C__ //g" test.php > _oop.c
	gcc _oop.c -I/usr/include/mysql -I/usr/include/mysql/mysql -lmysqlclient `pkg-config --cflags --libs glib-2.0`
