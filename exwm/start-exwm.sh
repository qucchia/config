#!/bin/sh

export CONFIG_DIR=$HOME/Documents/config

# Set keyboard layout
xmodmap $CONFIG_DIR/layout/.Xmodmap

# Start Tor service
sudo systemctl start tor@default.service

# Start Emacs
exec dbus-launch --exit-with-session emacs -mm --debug-init -l $CONFIG_DIR/exwm/desktop.el
