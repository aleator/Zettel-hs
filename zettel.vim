
function! PasteQuote()
    let quote = system("pbpaste")
    let formatted = system("pandoc -fmarkdown -tmarkdown","> " . l:quote)
    execute "normal! i" . l:formatted . "\<Esc>"
endfunction

function! ZCreate(title)
    let g:zettel_start_buffer = bufnr('%')
    call termopen("Zettel create --dolink --title " . a:title . " | xargs nvr -o",{'on_exit':'MyExitFunction'})
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction

command! -nargs=1 Zcre  call ZCreate(<q-args>)

function! ZettelSplit(current)
    let l:title = input("Split title> ")
    normal! gv"zx
    call append(line("."),"[" . l:title . "]")
    execute ':write'
    let l:cmd = "Zettel create --title " . shellescape(l:title) 
                \ . " --origin " . shellescape(a:current) 
                \ . " --ref-id " . shellescape(l:title) 
                \ . " --initial "
    echomsg(l:cmd)
    let l:result = system(l:cmd, @z)
    edit
    call ZPopSplit(l:result)
    "execute ":sp " . (l:result)
endfunction

function! ZExtend(origin,title)
    let l:cmd =  "Zettel create --origin " . a:origin . " --title " . shellescape(a:title)
    let l:name = system(l:cmd)
    echomsg(cmd,l:name)
    edit
    call ZPopSplit(l:name)
endfunction

"function! ZExtend(title,...)
"	if a:0 > 0 
"    let l:name = system("Zettel extend --origin " . expand("%:t") . " --title " . a:title . " --ref-id " . a:1)
"	else
"    let l:name = system("Zettel extend --origin " . expand("%:t") . " --title " . a:title )
"	end
"    edit
"    execute ":sp " . l:name
"endfunction

function! ZFindWikiLink(origin, ...)
 execute 'normal!"zyi['
 let g:zettel_start_buffer = bufnr('%')
 new
	if a:0 > 0 
        echomsg("Zettel link --origin " . a:origin . " --search " . a:1 . " --reference " . @z)
        call termopen("Zettel link --origin " . a:origin . " --search " . a:1 . " --reference " . @z ,{'on_exit':'MyExitFunction'})
    else
        echomsg("Zettel link --origin " . a:origin . " --reference " . shellescape(@z))
        call termopen("Zettel link --origin " . a:origin . " --reference " . shellescape(@z), {'on_exit':'MyExitFunction'})
    end
 let g:zettel_buffer = bufnr('%')
 call feedkeys("i")
endfunction

function! ZPopSplit(name)
    execute ":sp " . fnameescape(trim(a:name))
endfunction

function! ZResolve()
 execute 'normal!"zyi['
 echomsg("Zettel resolve --create --origin " . expand("%:t") . " -r '" . shellescape(@z) . "'")
 let l:name = system("Zettel resolve --create --origin " . expand("%:t") . " -r " . shellescape(@z) ) "TODO: USE SYSTEMLIST to open all files
 edit
 echo
    call ZPopSplit(l:name)
endfunction


function! ZettelNeighbourhood(origin)
    let g:zettel_start_buffer = bufnr('%')
    new
    call termopen("Zettel neighbourhood --neighbourhood " . a:origin . " | fzf -d '-' --with-nth 6.. --preview 'zettel body --origin {}' | xargs nvr -o",{'on_exit':'MyExitFunction'})
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction

function! ZettelFind(origin, kw)
    let g:zettel_start_buffer = bufnr('%')
    new
    call termopen("Zettel find --origin " . a:origin . " " . a:kw . " | xargs nvr -o",{'on_exit':'MyExitFunction'})
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction

function! ZettelFindVisualSelection()
 normal gv"zy
 let args = split(@z)
 let list = []
 for i in l:args
   call add(list,'+' . i)
 endfor
 call ZettelFullFind('',join(l:list,' '))
endfunction

function! ZettelFullFind(x,...)
    let g:zettel_start_buffer = bufnr('%')
    new
    if a:0 > 0
        call termopen("Zettel find -q" . shellescape(a:1) . g:fzf_xargs_vim,{'on_exit':'MyExitFunction'})
    else 
        let searchTerm = input("Search> ")
        call termopen("Zettel find -q" . shellescape(l:searchTerm) . g:fzf_xargs_vim,{'on_exit':'MyExitFunction'})
    endif
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction

function! MyExitFunction(a,b,c)   
 echomsg('zb' . g:zettel_buffer)
 let currwin=winnr()
 windo edit
 execute currwin . 'wincmd w'
 execute 'bd! ' . g:zettel_buffer 
endfunction 

let g:fzf_xargs_vim = "| fzf -d '-' --with-nth 6.. --multi --preview 'zettel body --origin {}' | xargs nvr -o"

function! ZettelThread(origin)
    new
    call termopen("Zettel neighbourhood --thread " . a:origin . g:fzf_xargs_vim , {'on_exit':'MyExitFunction'})
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction

function! ZettelLink(origin,...)
    new
	if a:0 > 0 
        call termopen("Zettel link --origin " . a:origin . " --search " . a:1,{'on_exit':'MyExitFunction'})
    else
        call termopen("Zettel link --origin " . a:origin,{'on_exit':'MyExitFunction'})
    end
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction


command! -nargs=0 ZFill :!zettel auto-fill --target %:t
command! -nargs=1 Zlnk call ZettelLink(expand("%:t"),<q-args>)
command! -nargs=1 Zf call ZettelFullFind(<q-args>)
command! -nargs=1 Zext call ZExtend(expand("%:t"),<q-args>)

nmap <localleader>zr :call ZResolve()<CR>
nmap <localleader>ze :call ZExtend(expand("%:t"),input('Note title> '))<CR>
vmap <localleader>zs :<c-u>call ZettelSplit(expand('%:t'))<CR>
nmap <localleader>zf :call ZettelFind(expand('%:t'),'')<CR>
nmap <localleader>zFF :call ZettelFullFind('',expand('<cword>')<CR>
vmap <localleader>zF :<c-u>call ZettelFindVisualSelection()<CR>
nmap <localleader>zF :call ZettelFullFind('')<CR>
nmap <localleader>zg :call ZettelFind(expand('%:t'),expand('<cword>'))<CR>
nmap <localleader>zn :call ZettelNeighbourhood(expand('%:t'))<CR>
nmap <localleader>zt :call ZettelThread(expand('%:t'))<CR>
nmap <localleader>zl :call ZettelLink(expand("%:t"))<CR>
nmap <localleader>zw :call ZFindWikiLink(expand("%:t"))<CR>
nmap <localleader>zp :call PasteQuote()<CR>

autocmd BufRead */zettel/* syn keyword Todo QUESTION TODO
autocmd BufRead */zettel/* syn keyword Keyword Tags Links 
autocmd BufRead */zettel/* highlight ZInlineCode guifg=green
" Match a zettelkasten wikilink
autocmd BufRead */zettel/* syn match Comment /\[.\{-}\]/ 
autocmd BufRead */zettel/* syn match Comment /\*.*\*/ 
autocmd BufRead */zettel/* syn match Keyword /\`.\{-}\`/ 
autocmd BufRead */zettel/* syn match Keyword /External references/ 
autocmd BufRead */zettel/* syn match Comment /─────.....................──────────────────────────────────────────────────────/
autocmd BufRead */zettel/* syn match Comment /-----.....................------------------------------------------------------/
autocmd BufRead */zettel/* setlocal cc=81
" autocmd BufRead */zettel/* match Comment /........-....-....-....-............-.*/

