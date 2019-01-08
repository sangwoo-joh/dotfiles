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
  -r|--rtags        Setup rtags env
  -z|--z            Setup z env
  -a|--all          Run all config(default)
EOF
}

ALL=yes
DOT_ONLY=
INSTALL_ONLY=
Z=
RTAGS=

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
    -r|--rtags)
      ALL=no
      RTAGS=yes
      shift
      ;;
    -z|--z)
      ALL=no
      Z=yes
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

function install_opam_packages {
  opam install -y merlin tuareg ocp-indent ocamlformat
}

function install_opam_2.0 {
  # require sudoer
  sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
  opam init

  opam switch create local 4.05.0 -j6
}

function install_anaconda {
  sh <(curl -sL https://repo.anaconda.com/archive/Anaconda3-5.3.1-Linux-x86_64.sh)
}

function install_only {
  # neovim
  sudo add-apt-repository ppa:neovim-ppa/unstable

  # emacs >= 25
  sudo add-apt-repository ppa:kelleyk/emacs

  sudo apt-get update

  # install my file list
  sudo apt-get install m4 emacs25 silversearcher-ag \
       tmux texlive-full ko.tex-base graphviz \
       neovim thunderbird thunderbird-locale-ko \
       ruby ruby-dev htop openssh-server \
       neofetch \
       cargo tree -y

  git submodule init
  git submodule update

  install_opam_2.0
  install_anaconda
  pip install neovim
}

function setup_z {
  # z setup
  # https://github.com/rupa/z.git
  git clone --recursive https://github.com/rupa/z.git
  mkdir ~/.config -p
  mv z ~/.config/
  echo ". \$HOME/.config/z/z.sh" >> ~/.bashrc
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

  # flake8 config for emacs
  cp ./flake8/flake8 ~/.config/flake8
}

function rtags {
  git clone --recursive https://github.com/Andersbakken/rtags.git
  pushd rtags
  cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
  make -j4
  mkdir -p ~/bin/rtags
  cp bin/* ~/bin/rtags/
  echo "export PATH=\$PATH:\$HOME/bin/rtags" >> ~/.bashrc
  popd
}

# check all first
if [ "$ALL" = "yes" ]; then
  install_only
  dot_only
  setup_z
  rtags
  exit 0
fi


# if both option is on respectively, check install first
if [ "$INSTALL_ONLY" = "yes" ]; then
  install_only
fi

if [ "$DOT_ONLY" = "yes" ]; then
  dot_only
fi

if [ "$Z" = "yes" ]; then
  setup_z
fi

if [ "$RTAGS" = "yes" ]; then
  rtags
fi
