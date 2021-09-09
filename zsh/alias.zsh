alias count='wc -l'
alias dist="cat /etc/*-release | grep PRETTY | awk -F = '{print $2}'"
alias vi='nvim'
alias grep='grep --color=auto'
alias png='dot -Tpng -O'
alias dotclear='rm -rf *.dot *.png'
alias diff='diff -u --color=auto'
alias ssh-hosts="grep -P \"^Host ([^*]+)$\" $HOME/.ssh/config | sed 's/Host //'"
alias jsonify='python -m json.tool'
alias ls='exa'
