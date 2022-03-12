" == General configuration ==

set nocompatible            " disable compatibility to old-time Vi
set showmatch               " show matching brackets
set ignorecase              " case insensitive matching
set hlsearch                " highlight search results
set autoindent              " indent a new line the same amount as the line just typed
set number                  " add line numbers
set wildmode=longest,list   " get bash-like tab completions
set cc=80                   " show columns for good coding style
set cursorline              " highlight cursor line
set ttyfast                 " speed up scrolling

filetype plugin on
filetype plugin indent on   " allow auto-indenting depending on file type

" Tabs
set tabstop=2               " set number of columns occupied by a tab character
set shiftwidth=2            " width for auto-indents
set softtabstop=2           " see multiple spaces as tabstops so <BS> deletes them together
set expandtab               " convert tabs to white space

" == Keybindings ==

" alt+j/k: move line or visually selected block
:nnoremap <A-j> <Esc>:m .+1<CR>
:nnoremap <A-k> <Esc>:m .-2<CR>
:inoremap <A-j> <Esc>:m .+1<CR>==gi
:inoremap <A-k> <Esc>:m .-2<CR>==gi
:vnoremap <A-j> :m '>+1<CR>gv=gv
:vnoremap <A-k> :m '<-2<CR>gv=gv

" == Mode mappings ==

" map leader
let g:mapleader = ','

" toggle spelling
" note: when invoking a command from a map, `<CR>` is added at the end
" note: using `:set inv<option>` toggles <option>
nnoremap <leader>s :set invspell<CR>

" insert file contents
nnoremap <leader>day :read ~/.config/nvim/_misc/everyday.txt<CR>

" datetime stamp
nnoremap <leader>o <C-R>=strftime("%Y-%m-%dT%H:%M")<CR>

" == Plugins ==

" specify directory for plugins
call plug#begin('~/.config/nvim/plugged')

Plug 'dracula/vim'                              " dracula theme
Plug 'preservim/nerdcommenter'                  " easy way to comment out lines
Plug 'scrooloose/nerdtree'                      " file explorer
Plug 'ryanoasis/vim-devicons'                   " icons for nerdtree
Plug 'SirVer/ultisnips'                         " snippets engine
Plug 'honza/vim-snippets'                       " snippets collection
Plug 'mhinz/vim-startify'                       " customisable start page
Plug 'neoclide/coc.nvim', {'branch': 'release'} " fast code completion engine
Plug 'vimwiki/vimwiki'

" initialize plugin system
call plug#end()

" == Colours ==

if (has("termguicolors"))
    set termguicolors
endif

syntax on
syntax enable
colorscheme dracula

" == Extras ==

" open new slipt panes to right and below
set splitright
set splitbelow

" vimwiki 
let g:vimwiki_list = [{'path': '~/doc/wiki/', 'path_html': '~/doc/wiki/html/'}]

