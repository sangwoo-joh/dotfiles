set nocompatible
set enc=utf-8

" the basics "
set nu 
set hlsearch
set autoindent
set tabstop=2
set shiftwidth=2
set smarttab
set softtabstop=2
set expandtab
set showmatch
set autowrite
set cursorline
set showcmd
set colorcolumn=120
set textwidth=120
set linebreak

"status line"
set ls=2
set sc
set smartindent
set wrap
set magic
set sm

"syntax"
syntax enable
set history=256
set ruler
set incsearch
set noimd
set visualbell

"auto complete
set completeopt=longest,menuone
set wildmode=list:longest,full
set wildmenu
set wildignore=*.o,*.obj,*~
set wildignore+=*.png,*.jpg,*.gif
set wildignore+=*.cm*,*.o,*.native
set wildignore+=*.so,*.swp,*.zip,*.pdf

"auto correction for typo-prone words"
ab fucntion function
ab calss class
ab erturn return
ab retrun return
ab pirnt print
ab prnit print
ab popluation population

"load file with last cursor position"
au BufReadPost *
\ if line("'\"") > 0 && line("'\"") <= line("$") |
\ exe "norm g`\"" |
\ endif

"show current point(x,y) of cursor"
set statusline=\ %<%l:%v\ [%P]%=%a\ %h%m%r\ %F\

"for OCaml"
"ocaml global setting"
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')

filetype indent on
filetype plugin on
au BufRead,BufNewFile *.ml,*.mli compiler ocaml 
au BufEnter *.ml,*.mli setf ocaml

"OCaml merlin"
execute "set rtp+=" . g:opamshare . "/merlin/vim"
let g:syntastic_ocaml_checkers = ['merlin']

"vim/nvim root"
if has('nvim')
  let s:editor_root=expand("~/.config/nvim")
else
  let s:editor_root=expand("~/.vim")
endif

"ocp-indent"
execute "set rtp+=" . s:editor_root . "/ocp-indent-vim"

"Vundle"
"setting up"
let vundle_installed=1
let vundle_readme=s:editor_root . '/Vundle.vim/README.md'
if !filereadable(vundle_readme)
  echo "Installing Vundle..."
  echo ""
  silent call mkdir(s:editor_root , "p")
  silent execute "!git clone https://github.com/gmarik/vundle " . s:editor_root . "/Vundle.vim"
  let vundle_installed=0
endif

let &rtp = &rtp . ',' . s:editor_root . '/Vundle.vim/'
call vundle#begin()
call vundle#rc(s:editor_root)

Plugin 'VundleVim/Vundle.vim'
Plugin 'bling/vim-airline'
Plugin 'The-NERD-Tree'
Plugin 'altercation/vim-colors-solarized'

" Syntax, Text
Plugin 'tpope/vim-surround'
Plugin 'tpope/vim-repeat'
Plugin 'scrooloose/syntastic'
Plugin 'let-def/ocp-indent-vim'
Plugin 'terryma/vim-multiple-cursors'
" not familiar with this yet
"Plugin 'Valloric/YouCompleteMe'

"Plugin 'tpope/vim-fugitive' "conflict with ocp-indent.. you should use git in

" Fuzzy search
Plugin 'junegunn/fzf'
Plugin 'junegunn/fzf.vim'
Plugin 'Shougo/unite.vim'
Plugin 'Shougo/unite-outline'
Plugin 'Shougo/neomru.vim'

if vundle_installed==0
  echo "Installing Plugins, please ignore key map error messages..."
  echo ""
  :PluginInstall
endif

call vundle#end()            " required
filetype plugin indent on    " required

"color scheme"
set background=light
let g:solarized_termcolors=256
colorscheme solarized

"Key Mappings"
"NERDTree key shortcut"
let NERDTreeQuitOnOpen=1
map <F2> <ESC>:NERDTree<CR>
"nvim terminal"
map <F3> <ESC>:vs<CR>:terminal<CR>

" increment
nnoremap + <C-a>
nnoremap - <C-x>

" in normal mode
nnoremap <C-a> ^
nnoremap <C-e> $
nnoremap <C-f> :Ag
nnoremap <C-p> :History<CR>

nnoremap <C-b> :YcmCompleter GoTo<CR>

" in command mode
tnoremap <C-a> <home>
tnoremap <C-e> <end>

" traverse jumplist + realign screen
nnoremap <C-o> <C-o>zzzv
nnoremap <C-i> <C-i>zzzv

"save
map <C-s> <ESC>:w!<CR>

" movement in terminal, split
tnoremap <Esc> <C-\><C-n>
tnoremap <C-s> <C-\><Esc>
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l


"file types"
autocmd Filetype html setlocal ts=2 sts=2 sw=2 omnifunc=htmlcomplete#CompleteTags
autocmd Filetype xml set omnifunc=xmlcomplete#CompleteTags
autocmd Filetype python setl et ts=2 sw=2 sts=2


" grep
if executable('ack-grep')
  let g:unite_source_grep_command = 'ack-grep'
  let g:unite_source_grep_default_opts = '-i --no-heading --no-color -a -H'
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ack')
  let g:unite_source_grep_command = 'ack'
  let g:unite_source_grep_default_opts = '-i --no-heading --no-color -a -H'
  let g:unite_source_grep_recursive_opt = ''
elseif executable('ag')
  let g:unite_source_grep_command = 'ag'
  let g:unite_source_grep_default_opts = '-i --vimgrep --hidden --ignore ''.hg'' --ignore ''.svn'' --ignore ''.git'' --ignore ''.bzr'''
  let g:unite_source_grep_recursive_opt = ''
endif


" ycm

"" not familiar with this yet...
"let g:ycm_confirm_extra_conf = 0
"let g:EclimCompletionMethod = 'omnifunc'
"let g:ycm_filetype_blacklist = {
"  \ 'notes' : 1,
"  \ 'markdown' : 1,
"  \ 'text' : 1,
"  \ 'unite' : 1
"  \}


" airline
let g:airline_powerline_fonts = 1
let g:airline_extensions = ['quickfix', 'syntastic']
"let g:airline_section_c = airline#section#create_left(['%{getcwd()}','file'])

