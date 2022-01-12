# Maybe use -fllvm for faster code?
app: Src/*
	ghc -hidir .Build -odir .Build Src/*.hs -o app 

run: app
	./app

test: app
	./app search "test" | fzf | awk '{print $$1}' | xargs -I {} ./app play "{}"

.PHONY: run
