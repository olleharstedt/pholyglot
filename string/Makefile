all: string2.c
	sed -e "s/#__C__ //g" string2.c > _string.c
	gcc _string.c -o string -I/usr/include/mysql -I/usr/include/mysql/mysql -I/usr/include/glib-2.0/glib -lmysqlclient `pkg-config --cflags --libs glib-2.0`
