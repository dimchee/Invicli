# Maybe use -fllvm for faster code?
invicli: Src/*
	ghc -hidir .Build -odir .Build Src/*.hs -o invicli 

run: invicli
	./invicli

test: invicli
	./invicli search "test" | fzf | awk '{print $$1}' | xargs -I {} ./invicli play "{}"

.PHONY: run
