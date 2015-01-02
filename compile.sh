#!/usr/bin/env bash
rm -r build
mkdir build
elm-make Main.elm --output build/index.html