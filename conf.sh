#!/bin/bash

binary_dir=~/.local/bin

command_exists () {
    type "$1" &> /dev/null ;
}

if ! command_exists cask ; then
    echo "Installing Cask..."

    if [ ! -d $binary_dir ]; then
        echo "Folder '${binary_dir}' does not exist, creating folder..."
        mkdir -p $binary_dir
    fi

    git clone https://github.com/cask/cask
    make -C cask install
    rm -rf ./cask
fi
