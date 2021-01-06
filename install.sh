#!/bin/bash
set -e -u -x

function help
{
  cat <<EOF
Usage: $_ [option]

Options
  help              Print this message
  caml              Install OCaml-related things
  emacs             Install Emacs settings
  conda             Install Anaconda
  fonts             Install d2coding, powerline fonts
  dot               Install dotfiles
  pkg               Install packages
  zsh               Install zsh
  all               Install all above (default)
EOF
}

ALL=yes
CONDA=
CAML=
EMACS=
FONTS=
DOT=
PKG=
ZSH=

PWD=$(cd $(dirname "$0"); pwd -P)

while [[ $# -gt 0 ]]
do
  key="$1"

  case $key in
    caml)
      ALL=no
      CAML=yes
      shift
      ;;
    emacs)
      ALL=no
      EMACS=yes
      shift
      ;;
    conda)
      ALL=no
      CONDA=yes
      shift
      ;;
    fonts)
      ALL=no
      FONTS=yes
      shift
      ;;
    dot)
      ALL=no
      DOT=yes
      shift
      ;;
    pkg)
      ALL=no
      PKG=yes
      shift
      ;;
    zsh)
      ALL=no
      ZSH=yes
      shift
      ;;
    all)
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

function _install_opam_packages {
  opam install -y merlin tuareg ocp-indent ocamlformat depext base dune core utop
}

function _install_opam_2.0 {
  # opam dependencies
  # bubblewrap is only available after Ubuntu 18.04.
  # if your system is less than or 16.04, just manually install via `dpkg -i` http://security.ubuntu.com/ubuntu/pool/main/b/bubblewrap/<binary-for-your-machine>
  sudo apt-get install m4 zip bubblewrap -y

  # require sudoer
  sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
  opam init

  opam switch create kernel ocaml-base-compiler.4.11.1
}

function install_caml {
  _install_opam_2.0
  _install_opam_packages
}

function install_emacs {
  # emacs >= 25
  sudo add-apt-repository ppa:kelleyk/emacs --yes

  sudo apt-get install emacs-27

  # emacs setting - use symbolic links
  ln -s "$PWD"/emacs ~/.emacs.d
}

function install_conda {
  wget https://repo.anaconda.com/archive/Anaconda3-2020.02-Linux-x86_64.sh -O conda.sh
  chmod +x conda.sh
  ./conda.sh -b
  rm ./conda.sh
  echo "source ~/anaconda3/etc/profile.d/conda.sh" >> ~/.bashrc
  source ~/anaconda3/etc/profile.d/conda.sh
}

function _install_my_neofetch {
  git clone https://github.com/skicombinator/neofetch
  pushd "$PWD"/neofetch
  sudo make install
  popd

  # always print neofetch with memory usage %
  echo "neofetch --memory_percent on" >> ~/.bashrc
}

function _install_rustup {
  curl https://sh.rustup.rs -sSf | sh
  echo "source ~/.cargo/env" >> ~/.bashrc
  source ~/.cargo/env
}

function _install_rust_packages {
  cargo install dutree loc bat exa eva hexyl hyperfine mdcat titlecase bb
  cat "alias ls=exa" >> ~/.zshrc
  cat "alias ls=exa" >> ~/.bashrc
  ## Got hint from https://www.wezm.net/technical/2019/10/useful-command-line-tools/
  # dutree: fast du + tree
  # loc: fast line counter (order of magnitude faster than cloc)
  # bat: cat + syntax highlight
  # exa: evolved ls
  # eva: calculator
  # hexyl: hex viewer + highlight
  # hyperfine: command line benchmarking tool
  # mdcat: terminal markdown renderer
  # titlecase: make pharagraph as title (capitalize)
  # bb: evolved top (system monitor)
}

function _install_z {
  # z setup
  # https://github.com/rupa/z.git
  git clone --recursive https://github.com/rupa/z.git
  mkdir ~/.config -p
  mv z ~/.config/
  echo ". \$HOME/.config/z/z.sh" >> ~/.bashrc
  echo ". \$HOME/.config/z/z.sh" >> ~/.zshrc
}


function install_pkg {
  # neovim
  sudo add-apt-repository ppa:neovim-ppa/unstable --yes

  sudo apt-get update

  # install my file list
  sudo apt-get install --yes m4 silversearcher-ag \
       tmux texlive-full ko.tex-base graphviz \
       neovim thunderbird thunderbird-locale-ko \
       ruby ruby-dev htop openssh-server curl rsync \
       tree etckeeper gcc make cmake moreutils libtext-multimarkdown-perl \
       tar bzip2 gzip zip autoconf \
       gnome-shell-extensions chrome-gnome-shell gnome-tweaks

  git submodule init
  git submodule update

  _install_my_neofetch
  _install_rustup
  _install_rust_packages
  _install_z
}

function install_zsh {
  sudo apt-get install zsh cmake --yes
  curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh

  git clone https://github.com/romkatv/powerlevel10k ~/.oh-my-zsh/custom/themes/powerlevel10k
  sed 's/^ZSH_THEME=\"\(.*\)\"/ZSH_THEME=\"powerlevel10k\/powerlevel10k\"/g' -i ~/.zshrc

  cat "$PWD"/zsh/alias >> ~/.zshrc
  cat "$PWD"/zsh/rc >> ~/.zshrc

  # oh-my-zsh git package is too slow for big repo...
  git config --global oh-my-zsh.hide-dirty 1
  git config --global oh-my-zsh.hide-status 1

  # turn off auto-less
  # -F: quit if the content is less than one screen
  # -X: do not reset screen
  git config --global --replace-all core.pager "less -F -X"

  # change zsh as default shell
  chsh -s $(which zsh)
}

function install_fonts {
  # prerequirements
  mkdir -p ~/.local/share/fonts
  sudo apt-get install zip --yes
  git submodule init
  git submodule update

  # d2coding
  pushd "$PWD"/d2codingfont
  # 1.3.2 is latest
  unzip D2Coding-Ver1.3.2-20180524.zip
  cp D2Coding/*.ttf ~/.local/share/fonts/
  popd

  # powerline
  pushd "$PWD"/powerline
  ./install.sh
  popd

  # load fonts
  fc-cache -vf ~/.local/share/fonts/
}

function install_dot {
  # dotfiles setting
  mkdir ~/.config -p

  # use symbolic link
  ln -s "$PWD"/nvim ~/.config/nvim

  ln -s "$PWD"/tmux/.tmux.conf ~/.tmux.conf
  ln -s "$PWD"/tmux/.tmux.color.conf ~/.tmux.color.conf

  cat "$PWD"/bash/alias >> ~/.bashrc
  cat "$PWD"/bash/rc >> ~/.bashrc

  # python settings
  ln -s "$PWD"/python/flake8 ~/.config/flake8
  ln -s "$PWD"/python/pylintrc ~/.config/pylintrc

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
  #sudo apt-get install git  # git must included
  install_pkg
  install_zsh
  install_fonts
  install_caml
  install_dot
  install_conda
  exit 0
fi


# if both option is on respectively, check install first
if [ "$EMACS" = "yes" ]; then
  install_emacs
fi

if [ "$CAML" = "yes" ]; then
  install_caml
fi

if [ "$CONDA" = "yes" ]; then
  install_conda
fi

if [ "$FONTS" = "yes" ]; then
  install_fonts
fi

if [ "$DOT" = "yes" ]; then
  install_dot
fi

if [ "$PKG" = "yes" ]; then
  install_pkg
fi

if [ "$ZSH" = "yes" ]; then
  install_zsh
fi
