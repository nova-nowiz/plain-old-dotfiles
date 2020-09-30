# Enable Powerlevel10k instant prompt. Should stay close to the top of ~/.zshrc.
# Initialization code that may require console input (password prompts, [y/n]
# confirmations, etc.) must go above this block, everything else may go below.
if [[ -r "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh" ]]; then
  source "${XDG_CACHE_HOME:-$HOME/.cache}/p10k-instant-prompt-${(%):-%n}.zsh"
fi

# Path to your oh-my-zsh installation.
export ZSH=$HOME/.oh-my-zsh
export ZSH_CUSTOM=$HOME/.oh-my-zsh-custom

# If you come from bash you might have to change your $PATH.
export PATH="/usr/lib/ccache/bin/:$PATH:/home/narice/.vim/bundle/vim-live-latex-preview/bin/"
export LIBRARY_PATH="/nix/store/7p1v1b6ys9fydg5kdqvr5mpr8svhwf4p-glibc-2.31/lib:$LIBRARY_PATH"
export MAKEFLAGS="-j5 -l4"

export DEFAULT_USER="narice"
export LANG="en_US.UTF-8"
export EDITOR="emacs"
export VISUAL="emacs"
export NIX_BUILD_SHELL="zsh"

export FZF_DEFAULT_COMMAND='fd --type f --hidden --follow --exclude .git'
export FZF_DEFAULT_OPTS="--preview 'bat --style=numbers --color=always --line-range :500 {}'"
export FZF_BASE="`fzf-share`"

ZSH_THEME="powerlevel10k/powerlevel10k"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"
# /!\ do not use with zsh-autosuggestions

HYPHEN_INSENSITIVE="true" # To make _ and - the same in completions

plugins=(
    git 
    tig 
    gitfast
    colorize 
    command-not-found 
    cp 
    dirhistory 
    sudo 
    zsh-vim-mode
    web-search
    fzf
    nix-shell
    zsh-syntax-highlighting 
    zsh-autosuggestions
)
# /!\ zsh-syntax-highlighting and then zsh-autosuggestions must be at the end

ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern cursor)

typeset -gA ZSH_HIGHLIGHT_STYLES

ZSH_HIGHLIGHT_STYLES[cursor]='bold'
ZSH_HIGHLIGHT_STYLES[alias]='fg=green,bold'
ZSH_HIGHLIGHT_STYLES[suffix-alias]='fg=green,bold'
ZSH_HIGHLIGHT_STYLES[builtin]='fg=green,bold'
ZSH_HIGHLIGHT_STYLES[function]='fg=green,bold'
ZSH_HIGHLIGHT_STYLES[command]='fg=green,bold'
ZSH_HIGHLIGHT_STYLES[precommand]='fg=green,bold'
ZSH_HIGHLIGHT_STYLES[hashed-command]='fg=green,bold'

source $ZSH/oh-my-zsh.sh #This has to be set AFTER everything that is NOT user config

# User config

function chpwd() {
    emulate -L zsh
    #ls -la
    exa -hg@ --git --icons --color-scale -la
}

function rule () {
	print -Pn '%F{blue}'
	local columns=$(tput cols)
	for ((i=1; i<=columns; i++)); do
	   printf "\u2588"
	done
	print -P '%f'
}

function _my_clear() {
	echo
	rule
	zle clear-screen
}
zle -N _my_clear
bindkey '^l' _my_clear

if [[ $TERM == xterm-termite ]]; then
  . /etc/profile.d/vte.sh
  __vte_osc7
fi

# Ctrl-O opens zsh at the current location, and on exit, cd into ranger's last location.
function ranger-cd() {
	tempfile=$(mktemp)
	ranger --choosedir="$tempfile" "${@:-$(pwd)}" < $TTY
	test -f "$tempfile" &&
	if [ "$(cat -- "$tempfile")" != "$(echo -n `pwd`)" ]; then
	cd -- "$(cat "$tempfile")"
	fi
	rm -f -- "$tempfile"
	# hacky way of transferring over previous command and updating the screen
	VISUAL=true zle edit-command-line
}
zle -N ranger-cd
bindkey '^o' ranger-cd

# mkcd function
function mkcd () {
  mkdir "$1"
  cd "$1"
}

# emacs function to multiplex emacs
function emacs {
    if [[ $# -eq 0 ]]; then
        /usr/bin/env emacs # "emacs" is function, will cause recursion
        return
    fi
    args=($*)
    for ((i=0; i <= ${#args}; i++)); do
        local a=${args[i]}
        # NOTE: -c for creating new frame
        if [[ ${a:0:1} == '-' && ${a} != '-c' && ${a} != '--' ]]; then
            /usr/bin/env emacs ${args[*]}
            return
        fi
    done
    setsid emacsclient -n -a /usr/bin/env emacs ${args[*]}
}

# start typing + [Up-Arrow] - fuzzy find history forward
if [[ "${terminfo[kcuu1]}" != "" ]]; then
  autoload -U up-line-or-beginning-search
  zle -N up-line-or-beginning-search
  bindkey "${terminfo[kcuu1]}" up-line-or-beginning-search
fi
# start typing + [Down-Arrow] - fuzzy find history backward
if [[ "${terminfo[kcud1]}" != "" ]]; then
  autoload -U down-line-or-beginning-search
  zle -N down-line-or-beginning-search
  bindkey "${terminfo[kcud1]}" down-line-or-beginning-search
fi

# pacman aliases
alias pac='sudo pacman -S'   # install
alias pacu='sudo pacman -Syu'    # update, add 'a' to the list of letters to update AUR packages if you use yaourt
alias pacr='sudo pacman -Rs'   # remove
alias pacs='pacman -Ss'      # search
alias paci='sudo pacman -Si'      # info
alias paclo='sudo pacman -Qdt'    # list orphans
alias pacro='paclo && sudo pacman -Rns $(pacman -Qtdq)' # remove orphans
alias pacc='sudo pacman -Scc'    # clean cache
alias paclf='sudo pacman -Ql' # list files

# nixos aliases
alias os-upgrade='nix-channel --update && nix-env -f "<nixpkgs>" -u && nix-env -f "<home-manager>" -u && home-manager switch && sudo nixos-rebuild switch --upgrade' # full system upgrade
alias os-clean='sudo nix-env --delete-generations old --profile /nix/var/nix/profiles/system && nix-env --delete-generations old && home-manager expire-generations "now" && nix-collect-garbage'

# haxelib aliases
alias haxel='haxelib install'
alias haxels='haxelib search'
alias haxelu='haxelib update'
alias haxelr='haxelib run'

#nvim alias
alias vim='nvim'
alias youtube-dla='youtube-dl -x --audio-format vorbis --audio-quality 192'
alias scrotclip='scrot -s ~/foo.png && xclip ~/foo.png && rm ~/foo.png'
alias dcmus='screen -q -r -D cmus || screen -S cmus $(which cmus)'
alias config='/usr/bin/env git --git-dir=$HOME/dotfiles --work-tree=$HOME'
alias la='exa -hg@ --git --icons --color-scale -la'

# opam configuration
test -r /home/narice/.opam/opam-init/init.zsh && . /home/narice/.opam/opam-init/init.zsh > /dev/null 2> /dev/null || true

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh
