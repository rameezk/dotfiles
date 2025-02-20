#!/bin/bash

source "$CONFIG_DIR/colors.sh"

sketchybar --add item battery right \
           --set battery update_freq=120 \
           script="$PLUGIN_DIR/battery.sh" \
           label.color=$MAUVE \
           icon.color=$MAUVE \
           --subscribe battery system_woke power_source_change
