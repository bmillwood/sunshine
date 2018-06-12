elm.js:
	elm-make Main.elm --output "$@"

# Leave it up to elm-make to decide when to rebuild
.PHONY: elm.js
