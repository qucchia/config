#!/bin/sh

exec dbus-launch --exit-with-session emacs -q -l ~/Projects/config/Emacs/init.el -mm --debug-init
