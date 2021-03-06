""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Font
:set guifont=Source\ Code\ Pro:h18
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
:set laststatus=0
" Hide pointless junk at the bottom, doesn't work in .vimrc for some reason?
:set noshowmode "don't show --INSERT--
:set noruler "don't show line numbers/column/% junk
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Keep the error column always visible (jumpy when linter runs on input)
:set signcolumn=yes
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" map Shift + F to Ack.vim
nmap <S-F> :Ack<space>
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Random color schemes!
function RandomColorScheme()
let mycolors = split(globpath(&rtp,"**/colors/*.vim"),"\n")
exe 'so ' . mycolors[localtime() % len(mycolors)]
unlet mycolors
endfunction
:command NewColor call RandomColorScheme()
function RandomBase16()
let mycolors = split(globpath(&rtp,"**/colors/base16*.vim"),"\n")
exe 'so ' . mycolors[localtime() % len(mycolors)]
unlet mycolors
endfunction
:command C call RandomBase16()
Start new windows with a random color scheme
call RandomBase16()
:colorscheme base16-materia
nnoremap ,, :C<CR>:colorscheme<CR>

" hide sidebar
:set guioptions-=r
" :set guioptions-=L

" syntax on
" :set background=dark
" colorscheme solarized

set rtp+=/usr/local/opt/fzf
nnoremap <silent> <C-p> :Files<CR>
