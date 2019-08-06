#!/bin/bash

#doubleDigit=$(pmset -g batt | grep [0-9][0-9]% | awk 'NR==1{print $3}' | cut -c 1-3)
doubleDigit=$(upower -i $(upower -e | grep BAT) | grep --color=never -E "state|to\ full|to\ empty|percentage" | grep -E "percentage:" | awk '{print $2}' | sed "s/%//")

echo "$doubleDigit"

#if [ ${#doubleDigit} -gt 1 ]; then
#            echo "$doubleDigit"
#    else
#                doubleDigit=$(pmset -g batt | grep [0-9]% | awk 'NR==1{print $3}' | cut -c 1-2)
#                    echo "$doubleDigit"
#fi
