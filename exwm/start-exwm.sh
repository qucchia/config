#!/bin/sh

xkbcomp ~/Documents/config/layout/.Xkeymap $DISPLAY
exec dbus-launch --exit-with-session emacs -mm --debug-init -l ~/Documents/config/exwm/desktop.el
