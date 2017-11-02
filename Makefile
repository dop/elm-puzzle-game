all: main.js

main.js: src/*.elm src/Puzzle/*.elm
	elm-make src/Main.elm --output=main.js

.PHONE: live clean

live:
	elm-live -- src/Main.elm --output=main.js

clean:
	rm main.js

distclean:
	make clean
	rm -r elm-stuff/
