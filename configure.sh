#!/bin/sh

mkdir -p ~/.config

WD=$(cd $(dirname "$0"); pwd -P)
ln -s "$WD"/zsh "$HOME"/.zsh
ln -s "$WD"/zsh/rc.zsh "$HOME"/.zshrc

# package update
DISTRO=$(uname -s)
case $DISTRO in
    Linux)
        sudo apt-get update
        sudo apt-get install m4 zip bubblewrap curl build-essential --yes
        ;;
    Darwin)
        /bin/sh -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"
        export PATH=$PATH:/opt/homebrew/bin
        brew update
        brew install gpatch git
        brew tap d12prosted/emacs-plus
        ;;
    *)
        echo "$DISTRO: Not Supported"
        exit 2
        ;;
esac

# z here
git clone --recursive https://github.com/rupa/z.git ~/.config/z

# cargo
curl https://sh.rustup.rs -sSf | sh
. ~/.cargo/env

# ocaml
bash -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
opam init
eval $(opam env)
opam update
opam switch create kernel ocaml-base-compiler.4.12.0
eval $(opam env --switch=kernel)
opam install ocamlfind
# ready to go
