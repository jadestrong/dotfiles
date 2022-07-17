if !exists('g:loaded_telescope') | finish | endif

nnoremap <silent> <Space>pf <cmd>lua require('telescope.builtin').find_files()<cr>
nnoremap <silent> <Space>sp <cmd>lua require('telescope.builtin').live_grep()<cr>
nnoremap <silent> <Space>fr <cmd>lua require('telescope.builtin').buffers()<cr>
nnoremap <silent> <Space>pp <cmd>lua require('telescope').extensions.project.project{}<cr>
nnoremap <silent> ;t <cmd>lua require('telescope.builtin').help_tags()<cr>
nnoremap <silent> ;; <cmd>lua require('telescope.builtin').resume()<cr>
nnoremap <silent> ;e <cmd>lua require('telescope.builtin').diagnostics()<cr>

lua << EOF
function telescope_buffer_dir()
  return vim.fn.expand('%:p:h')
end

local telescope = require('telescope')
local actions = require('telescope.actions')
require('telescope').load_extension('project')

telescope.setup{
  defaults = {
    mappings = {
      n = {
        ["q"] = actions.close
      },
    },
  }
}
EOF


