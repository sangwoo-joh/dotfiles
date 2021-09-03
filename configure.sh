#!/bin/sh

WD=$(cd $(dirname "$0"); pwd -P)
ln -s "$WD"/zsh "$HOME"/.zsh

# package update
DISTRO=$(uname -s)
case $DISTRO in
    Linux)
        sudo apt-get update
        sudo apt-get install m4 zip bubblewrap git --yes
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

# cargo
curl https://sh.rustup.rs -sSf | sh
source ~/.cargo/env

# ocaml
/bin/sh -c "sh <(curl -fsSL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)"
opam init
eval $(opam env)
opam update
opam switch create kernel ocaml-base-compiler.4.12.0

# ready to go
