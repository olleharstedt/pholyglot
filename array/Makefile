all: oop.c
	sed -e "s/#__C__ //g" array.c > _array.c
	gcc _array.c -I/usr/include/mysql -I/usr/include/mysql/mysql -lmysqlclient `pkg-config --cflags --libs glib-2.0`
