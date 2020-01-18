function! ZCreate(title)
    call termopen("Zettel create --dolink --title " . a:title . " | xargs nvr -o",{'on_exit':'MyExitFunction'})
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction

command! -nargs=1 Zcre  call ZCreate(<q-args>)

function! ZExtend(title,...)
	if a:0 > 0 
    let l:name = system("Zettel extend --origin " . expand("%:t") . " --title " . a:title . " --ref-id " . a:1)
	else
    let l:name = system("Zettel extend --origin " . expand("%:t") . " --title " . a:title )
	end
    edit
    execute ":sp " . l:name
endfunction

function! ZFindWikiLink(origin, ...)
 execute 'normal!"zyi['
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

function! ZResolve()
 execute 'normal!"zyi['
 echomsg("Zettel resolve --create --origin " . expand("%:t") . " -r '" . shellescape(@z) . "'")
 let l:name = system("Zettel resolve --create --origin " . expand("%:t") . " -r " . shellescape(@z) ) "TODO: USE SYSTEMLIST to open all files
 edit
 echo
    execute ":sp " . l:name
endfunction
nmap <localleader>zr :call ZResolve()<CR>

command! -nargs=1 Zext !Zettel extend --origin %:t --title <args>

function! ZettelFind(kw)
    new
    call termopen("Zettel find " . a:kw . " | xargs nvr -o",{'on_exit':'MyExitFunction'})
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction

function! ZettelFullFind(kw)
    new
    call termopen("Zettel find -q" . shellescape(a:kw) . " | fzf | xargs nvr -o",{'on_exit':'MyExitFunction'})
    let g:zettel_buffer = bufnr('%')
    call feedkeys("i")
endfunction

function! MyExitFunction(a,b,c)   
 echomsg('zb' . g:zettel_buffer)
 execute 'bd! ' .g:zettel_buffer 
 edit
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


command! -nargs=1 Zlnk call ZettelLink(expand("%:t"),<q-args>)
command! -nargs=1 Zf call ZettelFullFind(<q-args>)
nmap <localleader>zf :call ZettelFind('')<CR>
nmap <localleader>zg :call ZettelFind(expand('<cword>'))<CR>
nmap <localleader>zl :call ZettelLink(expand("%:t"))<CR>
nmap <localleader>zw :call ZFindWikiLink(expand("%:t"))<CR>

