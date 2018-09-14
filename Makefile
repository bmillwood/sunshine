elm.js:
	elm make src/Main.elm --output "$@"

histo.html:
	elm make src/Histogram.elm --output "$@"

# Leave it up to elm-make to decide when to rebuild
.PHONY: elm.js histo.html
