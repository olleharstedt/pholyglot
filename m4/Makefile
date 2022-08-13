all: _m4.c
	gcc _m4.c -o m4 -I/usr/include/mysql -I/usr/include/mysql/mysql -lmysqlclient `pkg-config --cflags --libs glib-2.0`

_m4.c: m4.c
	m4 m4.c > _m4.c
