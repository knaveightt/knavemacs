#!/bin/bash

r_flag=0
g_flag=0

# r flag not used yet, example of my usage for neovim testing
#while getopts "r" opt; do
#	case $opt in
#		r)
#			rm -rf ~/.config/nvim-test
#			rm -rf ~/.local/share/nvim-test
#			r_flag=1
#	esac
#done

while getopts "g" opt; do
    case $opt in
        g)
            g_flag=1
    esac
done
    

if [[ "$g_flag" -eq 0 ]]; then
    emacs -nw --init-directory=./
else
    emacs --init-directory=./
fi
