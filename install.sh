#!/bin/sh

echo "this script moves the rix stdlib files into a ~/.rix directory, so they can be accessed from anywhere"

mkdir $HOME/.rix

copylib () {
    echo "moving file $1"
    cp std/$1 $HOME/.rix/$1
}

copylib seq.rix
copylib macs.rix
