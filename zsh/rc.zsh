source ~/.zsh/antigen.zsh

antigen use oh-my-zsh
antigen bundle git
antigen bundle pip
antigen bundle docker
antigen bundle command-not-found
antigen bundle zsh-users/zsh-syntax-highlighting
antigen theme romkatv/powerlevel10k
antigen apply

HISTSIZE=10000000
HISTFILESIZE=10000000

h=()
if [[ -r ~/.ssh/config ]]; then
   h=($h ${${${(@M)${(f)"$(cat ~/.ssh/config)"}:#Host *}#Host }:#*[*?]*})
fi
if [[ $#h -gt 0 ]]; then
   zstyle ':completion:*:ssh:*' hosts $h
fi

setopt nonomatch

setopt NO_HUP

source ~/.cargo/env

if [ $(uname -s) = Darwin ]; then
    eval $(/opt/homebrew/bin/brew shellenv)
else
    source /etc/zsh_command_not_found
fi


[[ ! -f ~/.zsh/p10k.zsh ]] || source ~/.zsh/p10k.zsh
[[ ! -f ~/.zsh/ocaml.zsh ]] || source ~/.zsh/ocaml.zsh
[[ ! -f ~/.zsh/alias.zsh ]] || source ~/.zsh/alias.zsh
[[ ! -f ~/.config/z/z.sh ]] || source ~/.config/z/z.sh
[[ ! -f ~/.fzf.zsh ]] || source ~/.fzf.zsh

# conditional rtags
if [ -d ~/.dotfiles/rtags ]; then
    export PATH=$PATH:~/.dotfiles/rtags/bin
fi
