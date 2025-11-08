set fish_greeting ""

set -gx TERM xterm-256color

# theme
set -g theme_color_scheme terminal-dark
set -g fish_prompt_pwd_dir_length 1
set -g theme_display_user yes
set -g theme_hide_hostname no
set -g theme_hostname always

# alias
alias ls "ls -p -G"
alias la "ls -A"
alias ll "ls -l"
alias lla "ll -A"
command -qv nvim && alias vim nvim

alias pip "pip3"

set -gx EDITOR "emacsclient -c -a emacs"
set -gx LANG en_US.UTF-8

set -gx PATH /opt/homebrew/sbin $PATH
set -gx PATH (brew --prefix ruby)/bin $PATH
set -gx PATH ~/.emacs.d/bin $PATH # doom command
set -gx PATH ~/.cargo/bin $PATH # rust
set -gx PATH ~/bin $PATH # system
set -gx PATH (yarn global bin) $PATH
set -gx PATH /usr/local/opt/make/libexec/gunbin $PATH
set -gx PATH /Applications/WezTerm.app/Contents/MacOS $PATH
set -g fish_user_paths "/usr/local/opt/luajit-openresty/bin" $fish_user_paths

set -e LIBRARY_PATH
set -e CFLAGS
set -e LDFLAGS

set -l brew_prefix (brew --prefix)
set -l libgccjit_prefix (brew --prefix libgccjit)
set -l libmps_prefix (brew --prefix libmps)

set -gx LIBRARY_PATH "$libgccjit_prefix/lib/gcc/15" "$libgccjit_prefix/lib/gcc/current" "$brew_prefix/lib"
set -gx CPPFLAGS "-I$libgccjit_prefix/include -I$libmps_prefix/include"
set -gx LDFLAGS "-L$libgccjit_prefix/lib/gcc/15 -L$libgccjit_prefix/lib/gcc/current -L$libmps_prefix/lib"

set -gx PKG_CONFIG_PATH "$brew_prefix/lib/pkgconfig"


set -gx NODE_OPTIONS "--max-old-space-size=10240"

[ -f /opt/homebrew/Cellar/autojump/22.5.3_3/share/autojump/autojump.fish ]; and source /opt/homebrew/Cellar/autojump/22.5.3_3/share/autojump/autojump.fish

# alias cnpm "npm --registry=https://registry.npmmirror.com --cache=$HOME/.npm/.cache/cnpm --disturl=https://npmmirror.com/mirrors/node --userconfig=$HOME/.cnpmrc"
alias tnpm "npm --registry=https://registry.npmmirror.com --cache=$HOME/.npm/.cache/cnpm --disturl=https://npmmirror.com/mirrors/node --userconfig=$HOME/.cnpmrc"

# NVM
function __check_rvm --on-variable PWD --description 'Do nvm stuff'
  status --is-command-substitution; and return

  if test -f .nvmrc; and test -r .nvmrc;
     nvm use
  else
  end
end

# exa
if type -q exa
  alias ll "exa -l -g --icons"
  alias lla "ll -a"
end

# status is-interactive; and pyenv init --path | source

function vterm_printf;
    if begin; [  -n "$TMUX" ]  ; and  string match -q -r "screen|tmux" "$TERM"; end
        # tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$argv"
    else if string match -q -- "screen*" "$TERM"
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$argv"
    else
        printf "\e]%s\e\\" "$argv"
    end
end

function vterm_prompt_end;
    vterm_printf '51;A'(whoami)'@'(hostname)':'(pwd)
end
functions --copy fish_prompt vterm_old_fish_prompt
function fish_prompt --description 'Write out the prompt; do not replace this. Instead, put this at end of your file.'
    # Remove the trailing newline from the original prompt. This is done
    # using the string builtin from fish, but to make sure any escape codes
    # are correctly interpreted, use %b for printf.
    printf "%b" (string join "\n" (vterm_old_fish_prompt))
    vterm_prompt_end
end

# bun
set --export BUN_INSTALL "$HOME/.bun"
set --export PATH $BUN_INSTALL/bin $PATH

# pnpm
set -gx PNPM_HOME "/Users/zhangyuqiang/Library/pnpm"
if not string match -q -- $PNPM_HOME $PATH
  set -gx PATH "$PNPM_HOME" $PATH
end
# pnpm end

# Created by `pipx` on 2024-08-16 03:00:29
set PATH $PATH /Users/zhangyuqiang/.local/bin

# if test -d "/Applications/Emacs.app/Contents/MacOS/bin"
#     set -x PATH "/Applications/Emacs.app/Contents/MacOS/bin" $PATH
# end

# Added by OrbStack: command-line tools and integration
# This won't be added again if you remove it.
source ~/.orbstack/shell/init2.fish 2>/dev/null || :
