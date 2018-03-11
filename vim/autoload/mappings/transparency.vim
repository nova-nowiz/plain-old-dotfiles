function! mappings#transparency#Toggle_transparent()
    if g:is_transparent == 0
        hi Normal guibg=NONE ctermbg=NONE
        let g:is_transparent = 1
    else
        hi Normal guibg=#282c34
        let g:is_transparent = 0
    endif
endfunction
