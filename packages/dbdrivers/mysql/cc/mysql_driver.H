%% This file is generated on-the-fly by mysql_driver_config.P
%% Do not edit--any changes will be written over.
%% XSB config:    i686-pc-linux-gnu
%% XSB CC:        gcc
%% CC:       gcc
%% CC Opts:   -O4 -fomit-frame-pointer -finline-functions -Wall  -I /usr/include/mysql -I /home/saikat/XSB/config/i686-pc-linux-gnu -I /home/saikat/XSB/packages/dbdrivers
:- export driverMySQL_register/0.
:- export driverMySQL_initialise/0.
:- ldoption(' -Wl,-rpath /home/saikat/XSB/config/i686-pc-linux-gnu/lib -Wl,-rpath /usr/lib/mysql -L/usr/lib/mysql -lmysqlclient -ldl /home/saikat/XSB/config/i686-pc-linux-gnu/lib/driver_manager.so /home/saikat/XSB/config/i686-pc-linux-gnu/lib/mysql_driver.so').
