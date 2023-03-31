#!/bin/bash

binary_dir=~/.local/bin
binary_dir_absolute=$(readlink -f $binary_dir)

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

    if ! echo $PATH | grep -q $binary_dir_absolute ; then
        echo "Adding $binary_dir_absolute to PATH..."
        echo "export PATH=$binary_dir_absolute:\$PATH" >> ~/.bashrc
        bash
    fi
    cask
fi

echo "Installing cask dependencies..."
cask install
