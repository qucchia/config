#!/bin/sh

exec dbus-launch --exit-with-session emacs -q -l ~/Projects/config/emacs/init.el -mm --debug-init
