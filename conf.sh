#!/bin/bash

command_exists () {
    type "$1" &> /dev/null ;
}

if ! command_exists cask ; then
    echo "Installing Cask..."

    git clone https://github.com/cask/cask
    make -C cask install
    rm -rf ./cask
fi
