#!/bin/bash

while getopts "r" opt; do
	case $opt in
		r)
			rm -rf ~/.config/emacs
	esac
done

mkdir -p ~/.config/emacs
cp -r ./external ~/.config/emacs
cp -r ./internal ~/.config/emacs
cp -r ./platform ~/.config/emacs
cp -r ./snippets ~/.config/emacs
cp ./init.el ~/.config/emacs/
