elm -m --set-runtime=elm-runtime.js Main.elm
cp $HOME/.cabal/share/Elm-0.13/elm-runtime.js ./build
mv ./build/Main.html ./build/index.html