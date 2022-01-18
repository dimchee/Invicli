# Simple invidious cli client

## Install

This repository contains nix flake, so you can install it as any other nix flake (google it)

## Usage:

Next example uses mpv to play video:
```sh
$ invicli search eminem | fzf | invicli play 
```
Use whatever selector you want, and whatever player
```sh
$ invicli search 'why use haskell' | dmenu | invicli getlink
```
You can also download using invicli
```sh
$ invicli search '3b1b' | wofi --dmenu | invicli download
```