"encoding"
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
set nocompatible

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
"set statusline=\ %<%l:%v\ [%P]%=%a\ %h%m%r\ %F\

"for OCaml"
filetype indent on
filetype plugin on
au BufRead,BufNewFile *.ml,*.mli compiler ocaml 

"OCaml merlin"
let g:opamshare = substitute(system('opam config var share'),'\n$','','''')
execute "set rtp+=" . g:opamshare . "/merlin/vim"
let g:syntastic_ocaml_checkers = ['merlin']

"vim/nvim root"
if has('nvim')
  let s:editor_root=expand("~/.config/nvim")
else
  let s:editor_root=expand("~/.vim")
endif

"Vundle"
"setting up"
let vundle_installed=1
let vundle_readme=s:editor_root . '/bundle/Vundle.vim/README.md'
if !filereadable(vundle_readme)
  echo "Installing Vundle..."
  echo ""
  silent call mkdir(s:editor_root , "p")
  silent execute "!git clone https://github.com/gmarik/vundle " . s:editor_root . "/bundle/Vundle.vim"
  let vundle_installed=0
endif

let &rtp = &rtp . ',' . s:editor_root . '/bundle/Vundle.vim/'
"call vundle#begin()
call vundle#rc(s:editor_root)

Plugin 'VundleVim/Vundle.vim'
Plugin 'tpope/vim-fugitive'
Plugin 'tpope/vim-surround'
Plugin 'scrooloose/syntastic'
Plugin 'bling/vim-airline'
Plugin 'The-NERD-Tree'
Plugin 'altercation/vim-colors-solarized'

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
tnoremap <Esc> <C-\><C-n>
tnoremap <C-h> <C-\><C-n><C-w>h
tnoremap <C-j> <C-\><C-n><C-w>j
tnoremap <C-k> <C-\><C-n><C-w>k
tnoremap <C-l> <C-\><C-n><C-w>l
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
"rad map"
map <C-a> ggVG


"file types"
autocmd Filetype html setlocal ts=2 sts=2 sw=2 omnifunc=htmlcomplete#CompleteTags
autocmd Filetype xml set omnifunc=xmlcomplete#CompleteTags
autocmd Filetype python setl et ts=2 sw=2 sts=2
autocmd Filetype css setlocal ts=2 noet sw=2 omnifunc=csscomplete#CompleteCSS

