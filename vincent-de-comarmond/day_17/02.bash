#!/usr/bin/env bash

declare -i a b c instruction operand combo pointer=0
declare -a program output

a=$(sed -n 1p <"$1" | cut -d " " -f 3)
b=$(sed -n 2p <"$1" | cut -d " " -f 3)
c=$(sed -n 2p <"$1" | cut -d " " -f 3)
readarray -d , -t program < <(tail -n1 "$1" | cut -d " " -f2)




