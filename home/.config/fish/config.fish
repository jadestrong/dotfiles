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

set -gx EDITOR "emacsclient -c -a emacs"
set -gx LANG en_US.UTF-8

set -gx PATH ~/.emacs.d/bin $PATH # doom command
set -gx PATH ~/.cargo/bin $PATH # rust
set -gx PATH /Applications/Firefox.app/Contents/MacOS $PATH
set -gx PATH /run/current-system/sw/bin $PATH # All nix commond
set -gx PATH ~/bin $PATH # system
set -gx PATH /usr/local/opt/make/libexec/gunbin $PATH
fenv source '$HOME/.nix-profile/etc/profile.d/nix.sh'
set -gx PATH /usr/local/Cellar/universal-ctags/HEAD-c436bca/bin $PATH # must below nix.sh, it need override ctags from nix emacs
set -gx NIX_PATH darwin-config=$HOME/.nixpkgs/darwin-configuration.nix
set -gx NIX_PATH $HOME/.nix-defexpr/channels $NIX_PATH

set -g fish_user_paths "/usr/local/opt/luajit-openresty/bin" $fish_user_paths

[ -f /usr/local/share/autojump/autojump.fish ]; and source /usr/local/share/autojump/autojump.fish
starship init fish | source

# export PATH="$HOME/.emacs.d/bin:$HOME/.cargo/bin:/run/current-system/sw/bin:$HOME/bin:/usr/local/opt/make/libexec/gnubin:$PATH"
# export LANG=en_US.UTF-8


# function cheat.sh
#     curl cheat.sh/$argv
# end

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
