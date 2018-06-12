elm.js:
	elm-make Main.elm --output "$@"

histo.html:
	elm-make Histogram.elm --output "$@"

# Leave it up to elm-make to decide when to rebuild
.PHONY: elm.js histo.html
