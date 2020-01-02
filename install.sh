#!/bin/bash
set -e -u -x

function help
{
  cat <<EOF
Usage: $_ [option]

Options
  help              Print this message
  caml              Install OCaml-related things
  conda             Install Anaconda
  fonts             Install d2coding, powerline fonts
  dot               Install dotfiles
  pkg               Install packages
  rtags             Install rtags env
  rust              Install rust env
  zsh               Install zsh
  z                 Install z env
  all               Install all above (default)
EOF
}

ALL=yes
CONDA=
CAML=
FONTS=
DOT=
PKG=
ZSH=
Z=
RTAGS=
RUST=

while [[ $# -gt 0 ]]
do
  key="$1"

  case $key in
    caml)
      ALL=no
      CAML=yes
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
    rtags)
      ALL=no
      RTAGS=yes
      shift
      ;;
    rust)
      ALL=no
      RUST=yes
      shift
      ;;
    zsh)
      ALL=no
      ZSH=yes
      shift
      ;;
    z)
      ALL=no
      Z=yes
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

function install_opam_packages {
  opam install -y merlin tuareg ocp-indent ocamlformat
}

function install_opam_2.0 {
  # opam dependencies
  sudo apt-get install m4 zip bubblewrap -y # bubblewrap is only available on Ubuntu 18.04~

  # require sudoer
  sh <(curl -sL https://raw.githubusercontent.com/ocaml/opam/master/shell/install.sh)
  opam init

  opam switch create local 4.08.0 -j6

  # setup ocamlinit
  cat ./ocaml/ocamlinit >> ~/.ocamlinit
}

function install_anaconda {
  wget https://repo.anaconda.com/archive/Anaconda3-2019.03-Linux-x86_64.sh -O conda.sh
  chmod +x conda.sh
  ./conda.sh -b
  echo "source ~/anaconda3/etc/profile.d/conda.sh" >> ~/.bashrc
  source ~/anaconda3/etc/profile.d/conda.sh
}

function install_my_neofetch {
  git clone https://github.com/skicombinator/neofetch
  pushd neofetch
  sudo make install
  popd

  # always print neofetch with memory usage %
  echo "neofetch --memory_percent on" >> ~/.bashrc
}

function install_rustup {
  curl https://sh.rustup.rs -sSf | sh
  echo "source ~/.cargo/env" >> ~/.bashrc
  source ~/.cargo/env
}

function install_rust_packages {
  cargo install dutree loc bat exa eva hexyl hyperfine mdcat titlecase bb
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


function install_packages {
  # neovim
  sudo add-apt-repository ppa:neovim-ppa/unstable --yes

  # emacs >= 25
  sudo add-apt-repository ppa:kelleyk/emacs --yes

  sudo apt-get update

  # install my file list
  sudo apt-get install --yes m4 emacs25 silversearcher-ag \
       tmux texlive-full ko.tex-base graphviz \
       neovim thunderbird thunderbird-locale-ko \
       ruby ruby-dev htop openssh-server curl rsync \
       tree etckeeper gcc make cmake moreutils libtext-multimarkdown-perl

  git submodule init
  git submodule update

  install_my_neofetch
}

function install_zsh {
  sudo apt-get install zsh cmake --yes
  curl -L https://raw.github.com/robbyrussell/oh-my-zsh/master/tools/install.sh | sh

  git clone https://github.com/bhilburn/powerlevel9k ~/.oh-my-zsh/custom/themes/powerlevel9k
  sed 's/^ZSH_THEME=\"\(.*\)\"/ZSH_THEME=\"powerlevel9k\/powerlevel9k\"/g' -i ~/.zshrc

  cat ./zsh/alias >> ~/.zshrc
  cat ./zsh/rc >> ~/.zshrc

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

function install_z {
  # z setup
  # https://github.com/rupa/z.git
  git clone --recursive https://github.com/rupa/z.git
  mkdir ~/.config -p
  mv z ~/.config/
  echo ". \$HOME/.config/z/z.sh" >> ~/.bashrc
  echo ". \$HOME/.config/z/z.sh" >> ~/.zshrc
}

function install_fonts {
  # prerequirements
  mkdir -p ~/.local/share/fonts
  sudo apt-get install zip --yes
  git submodule init
  git submodule update

  # d2coding
  pushd d2codingfont
  # 1.3.2 is latest
  unzip D2Coding-Ver1.3.2-20180524.zip
  cp D2Coding/*.ttf ~/.local/share/fonts/
  popd

  # powerline
  pushd powerline
  ./install.sh
  popd

  # load fonts
  fc-cache -vf ~/.local/share/fonts/
}

function install_dot {
  # dotfiles setting
  mkdir ~/.config -p
  mkdir ~/.config/nvim -p

  cp ./nvim/init.vim ~/.config/nvim/
  cp ./tmux/.tmux.conf ~/.tmux.conf

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

function install_rtags {
  git clone --recursive https://github.com/Andersbakken/rtags.git
  pushd rtags
  cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 .
  make -j4
  mkdir -p ~/bin/rtags
  cp bin/* ~/bin/rtags/
  echo "export PATH=\$PATH:\$HOME/bin/rtags" >> ~/.bashrc
  echo "export PATH=\$PATH:\$HOME/bin/rtags" >> ~/.zshenv
  popd
}

# check all first
if [ "$ALL" = "yes" ]; then
  #sudo apt-get install git  # git must included
  install_packages
  install_dot
  install_zsh
  install_z
  # install_rtags
  install_rustup
  install_fonts
  install_opam_2.0
  install_anaconda
  exit 0
fi


# if both option is on respectively, check install first
if [ "$PKG" = "yes" ]; then
  install_packages
fi

if [ "$DOT" = "yes" ]; then
  install_dot
fi

if [ "$FONTS" = "yes" ]; then
  install_fonts
fi

if [ "$ZSH" = "yes" ]; then
  install_zsh
fi

if [ "$Z" = "yes" ]; then
  install_z
fi

if [ "$RTAGS" = "yes" ]; then
  install_rtags
fi

if [ "$CAML" = "yes" ]; then
  install_opam_2.0
  install_opam_packages
fi

if [ "$CONDA" = "yes" ]; then
  install_anaconda
fi

if [ "$RUST" = "yes" ]; then
  # install rustup instead of debian cargo package
  install_rustup
  # install useful cargo package
  install_rust_packages
  cat "alias ls=exa" >> ~/.zshrc
fi
