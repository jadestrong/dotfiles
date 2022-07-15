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

set -gx EDITOR "emacsclient -c -a emacs"
set -gx LANG en_US.UTF-8

set -gx PATH ~/.emacs.d/bin $PATH # doom command
set -gx PATH ~/.cargo/bin $PATH # rust
set -gx PATH /Applications/Firefox.app/Contents/MacOS $PATH
set -gx PATH ~/bin $PATH # system
set -gx PATH /usr/local/opt/make/libexec/gunbin $PATH
set -gx PATH /usr/local/Cellar/universal-ctags/HEAD-c436bca/bin $PATH # must below nix.sh, it need override ctags from nix emacs
set -g fish_user_paths "/usr/local/opt/luajit-openresty/bin" $fish_user_paths

[ -f /usr/local/share/autojump/autojump.fish ]; and source /usr/local/share/autojump/autojump.fish
# starship init fish | source

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

status is-interactive; and pyenv init --path | source

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
