#!/bin/bash

if diff "$1" <(./compile.sh "$1"); then
    echo "$1 is a metacompiler!"
else
    echo "$1 is not a metacompiler. See diff above."
fi
