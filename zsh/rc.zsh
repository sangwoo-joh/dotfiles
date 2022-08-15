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

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/sangwoo-joh/opt/anaconda3/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/sangwoo-joh/opt/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/sangwoo-joh/opt/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/sangwoo-joh/opt/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<
export PATH="/opt/homebrew/opt/llvm/bin:$PATH"
export LDFLAGS="-L/opt/homebrew/opt/llvm/lib"
export CPPFLAGS="-I/opt/homebrew/opt/llvm/include"
export CXXFLAGS="-I/opt/homebrew/opt/llvm/include"
eval "$(rbenv init - zsh)"
alias ls=ls
source /opt/homebrew/opt/chruby/share/chruby/chruby.sh
alias ls=exa
source /opt/homebrew/opt/chruby/share/chruby/auto.sh
chruby ruby-3.1.1
