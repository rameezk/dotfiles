#!/bin/bash

source "$CONFIG_DIR/colors.sh"

sketchybar --add item cpu right \
           --set cpu  update_freq=2 \
                      icon=􀧓  \
                      label.color=$GREEN \
                      icon.color=$GREEN \
                      script="$PLUGIN_DIR/cpu.sh"
