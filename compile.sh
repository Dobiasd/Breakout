elm Main.elm
mkdir -p release
perl -pe 's/src=.*elm-runtime.js/src="elm-runtime.js/g' < ./build/Main.html > ./release/index.html
cp $HOME/.cabal/share/Elm-0.10/elm-runtime.js ./release
