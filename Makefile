# Maybe use -fllvm for faster code?
invicli: Src/*
	ghc -hidir .Build -odir .Build Src/*.hs -o invicli 

run: invicli
	./invicli

test: invicli
	./invicli search "test" | fzf | ./invicli play

.PHONY: run
