function! mappings#transparency#Toggle_transparent()
    if g:is_transparent == 0
        execute "hi backup guibg =" synIDattr(hlID("Normal"), "bg")
        hi Normal guibg=NONE ctermbg=NONE
        let g:is_transparent = 1
    else
        execute "hi Normal guibg =" synIDattr(hlID("backup"), "bg")
        let g:is_transparent = 0
    endif
endfunction
