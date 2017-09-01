#!/bin/bash
set -e -u -x

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
#shift #past argument or value
done

if $FLAG ; then
  echo '=====Only Dotfiles====='
  echo ''
else
  echo '=====Install All====='
  echo ''
fi

if ! $FLAG; then
  # neovim
  sudo add-apt-repository ppa:neovim-ppa/unstable
  # typora
  sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys BA300B7755AFCFAE
  sudo add-apt-repository 'deb https://typora.io ./linux/'

  sudo apt-get update

  # install my file list
  sudo apt-get install m4 -y
  sudo apt-get install opam -y
  opam init

  sudo apt-get install silversearcher-ag tmux emacs texlive-full ko.tex-base graphviz \
       filezilla plank python-pip neovim typora thunderbird thunderbird-locale-ko -y
  sudo pip install neovim

  # download fonts
  mkdir ./fonts -p
  wget https://github.com/naver/d2codingfont/releases/download/VER1.21/D2Coding-1.2.zip -P ./fonts

  # download themes
  mkdir ./themes -p
  wget https://github.com/LinxGem33/OSX-Arc-White/releases/download/v1.4.3/osx-arc-collection_1.4.3_amd64.deb -P ./themes

fi

git submodule init
git submodule update

pushd . > /dev/null
trap "popd > /dev/null" EXIT

chmod +x z/z.sh
echo ". $(pwd)/z.sh" >> ~/.bashrc

# dotfiles setting
mkdir ~/.config -p
mkdir ~/.config/nvim -p

cp ./nvim/init.vim ~/.config/nvim/
cp ./tmux/.tmux.conf ~/.tmux.conf

mkdir ~/.ssh -p
cp ./ssh/config ~/.ssh/

mkdir ~/.emacs.d -p
mkdir ~/.emacs.d/themes -p
cp ./emacs/dracula-theme.el ~/.emacs.d/themes/
cp ./emacs/init.el ~/.emacs.d/

cat ./bash/alias >> ~/.bashrc
cat ./bash/rc >> ~/.bashrc

# global nautilus setting
gsettings set org.gnome.nautilus.preferences default-folder-viewer 'list-view'
gsettings set org.gnome.nautilus.preferences default-sort-order 'type'
gsettings set org.gnome.nautilus.preferences show-hidden-files false
