all: oop.c
	sed -e "s/#__C__ //g" oop.c > _oop.c
	gcc _oop.c -o oop -I/usr/include/mysql -I/usr/include/mysql/mysql -lmysqlclient `pkg-config --cflags --libs glib-2.0`
