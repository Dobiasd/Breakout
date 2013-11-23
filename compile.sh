elm -m --runtime=elm-runtime.js Main.elm
cp $HOME/.cabal/share/Elm-0.10/elm-runtime.js ./build
mv ./build/Main.html ./build/index.html