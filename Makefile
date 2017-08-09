all: main

main: Main.elm
	elm make Main.elm --output=index.html
