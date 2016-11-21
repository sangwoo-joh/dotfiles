#!/bin/bash
set -e -u

FLAG=false

while [[ $# -gt 0 ]]
do
  key="$1"

  case $key in
    -d | --dots)
    FLAG=true
    shift
    ;;
    *)
      #unknown option
    ;;
esac
shift #past argument or value
done

if $FLAG ; then
  echo '=====Only Dotfiles====='
  echo ''
else
  echo '=====Install All====='
  echo ''
fi

if ! $FLAG; then
  # install my file list
  sudo apt-get install opam
  opam init
  # neovim
  sudo add-apt-repository ppa:neovim-ppa/unstable
  # typora
  sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA300B7755AFCFAE
  sudo add-apt-repository 'deb https://typora.io ./linux/'

  sudo apt-get update

  sudo apt-get install neovim
  sudo apt-get install typora
fi

# dotfiles setting
mkdir ~/.config -p
mkdir ~/.config/nvim -p

cp ./nvim/init.vim ~/.config/nvim/
cp ./tmux/.tmux.conf ~/.tmux.conf

cat ./bash/alias >> ~/.bashrc

