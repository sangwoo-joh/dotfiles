#!/bin/bash
set -e -u -x

function help
{
  cat <<EOF
Usage: $_ [option]

Options
  help              Print this message
  -d|--dot-only     Set dotfiles only
  -i|--install-only Install my setup only
  -a|--all          Run all config(default)
EOF
}

ALL=yes
DOT_ONLY=
INSTALL_ONLY=

while [[ $# -gt 0 ]]
do
  key="$1"

  case $key in
    -d|--dot-only)
      ALL=no
      DOT_ONLY=yes
      shift
      ;;
    -i|--install-only)
      ALL=no
      INSTALL_ONLY=yes
      shift
      ;;
    -a|--all)
      ALL=yes
      shift
      ;;
    -h|--help|help|h|--h|-help)
      help
      exit 0
      ;;
    *)
      #unknown option
      help
      exit 2
  esac
  #shift #past argument or value
done

function install_only {
  # neovim
  sudo add-apt-repository ppa:neovim-ppa/unstable

  # emacs >= 25
  sudo add-apt-repository ppa:kelleyk/emacs

  sudo apt-get update

  # install my file list
  sudo apt-get install m4 -y
  sudo apt-get install opam -y
  opam init

  sudo apt-get install emacs25 silversearcher-ag \
       tmux texlive-full ko.tex-base graphviz \
       python-pip neovim thunderbird thunderbird-locale-ko \
       ruby ruby-dev htop \
       cargo tree -y
  sudo pip install neovim

  git submodule init
  git submodule update

  # I don't know what this means
#  pushd . > /dev/null
#  trap "popd > /dev/null" EXIT
}

function dot_only {
  # dotfiles setting
  mkdir ~/.config -p
  mkdir ~/.config/nvim -p

  cp ./nvim/init.vim ~/.config/nvim/
  cp ./tmux/.tmux.conf ~/.tmux.conf

  mkdir ~/.ssh -p
  cp ./ssh/config ~/.ssh/

  # emacs setting
  mkdir ~/.emacs.d -p
  mkdir ~/.emacs.d/themes -p
  mkdir ~/.emacs.d/config -p
  mkdir ~/.emacs.d/snippets/c++-mode -p

  cp ./emacs/.editorconfig ~/
  cp ./emacs/dracula-theme.el ~/.emacs.d/themes/
  cp ./emacs/init.el ~/.emacs.d/
  cp ./emacs/config/* ~/.emacs.d/config/
  cp ./emacs/snippets/c++-mode/* ~/.emacs.d/snippets/c++-mode/

  cat ./bash/alias >> ~/.bashrc
  cat ./bash/rc >> ~/.bashrc

  # global nautilus setting
  gsettings set org.gnome.nautilus.preferences default-folder-viewer 'list-view'
  gsettings set org.gnome.nautilus.preferences default-sort-order 'type'
  gsettings set org.gnome.nautilus.preferences show-hidden-files false

  # global keyboard speed setting
  gsettings set org.gnome.desktop.peripherals.keyboard repeat-interval 30
  gsettings set org.gnome.desktop.peripherals.keyboard delay 210
}

# check all first
if [ "$ALL" = "yes" ]; then
  install_only
  dot_only
  exit 0
fi


# if both option is on respectively, check install first
if [ "$INSTALL_ONLY" = "yes" ]; then
  install_only
fi

if [ "$DOT_ONLY" = "yes" ]; then
  dot_only
fi
