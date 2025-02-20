#!/bin/bash

source "$CONFIG_DIR/colors.sh"

sketchybar --add item calendar right \
           --set calendar icon=􀉉 \
           update_freq=30 \
           label.color=$PEACH \
           icon.color=$PEACH \
           script="$PLUGIN_DIR/calendar.sh"
