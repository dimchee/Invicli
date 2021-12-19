# Maybe use -fllvm for faster code?
app: Src/*
	ghc -hidir .Build -odir .Build Src/*.hs -o app 

run: app
	./app

.PHONY: run
