# Simple invidious cli client

## Install

This repository contains nix flake, so you can install it as any other nix flake (google it),
using binary (comming soon), or by manualy building it (shouldn't be too hard :))

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
First first word of first line is used as videoId, so you can
```sh
$ invicli search 'something' | invicli play
```
