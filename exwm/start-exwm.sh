#!/bin/sh

export CONFIG_DIR=$HOME/Documents/config
export SCHOOL_MAIL=timothydavid.skipper@alumnat.ins-mediterrania.cat

# Start Emacs
exec dbus-launch --exit-with-session emacs -mm --debug-init -l $CONFIG_DIR/exwm/desktop.el
