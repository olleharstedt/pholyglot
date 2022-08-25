all: array2.c
	sed -e "s/#__C__ //g" array2.c > _array.c
	gcc _array.c -o array -I/usr/include/mysql -I/usr/include/mysql/mysql -I/usr/include/glib-2.0/glib -lmysqlclient `pkg-config --cflags --libs glib-2.0`
