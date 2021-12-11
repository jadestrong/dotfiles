{ config, pkgs, ... }:

{
  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;

  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "jadestrong";
  home.homeDirectory = "/Users/jadestrong";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "20.09";
  programs.neovim = {
    enable = true;
    vimAlias = true;
    extraConfig = builtins.readFile ./home/extraConfig.vim;
    plugins = with pkgs.vimPlugins; [
      # Syntax / Language Support ###########
      vim-nix
      # vim-fish
      rust-vim # rust
      nvim-lspconfig 
      nvim-treesitter
      # nvim-compe
      # completion-nvim
      packer-nvim
      nvim-cmp
      cmp-nvim-lsp
      cmp_luasnip
      luasnip
      rust-tools-nvim
      # yajs.vim # JS syntax
      # es.next.syntax.vim # ES7 syntax

      # UI ##################################
      one-nvim 
      vim-gitgutter
      vim-airline

      # Editor Features #####################
      vim-surround # cs"'
      vim-repeat # cs"'...
      vim-commentary # gcap
      vim-indent-object # >aI
      vim-easy-align # vipg
      vim-eunuch # :Rename foo.rb
      vim-sneak
      # supertab
      ale # linting
      nerdtree
      auto-pairs

      # Buffer / Pane / File Manager ########
      # fzf-vim # all the things
      telescope-nvim

      # Panes / Larger featuresa ############
      tagbar # <leader>5
      vim-fugitive # Gblame
    ];
  };
}
