-- Editor settings
vim.o.confirm = true
vim.o.colorcolumn = "80"
vim.o.signcolumn = "yes"
vim.o.number = true

-- Keybinds
-- clear search hl on esc
vim.keymap.set('n', '<Esc>', '<cmd>nohlsearch<CR>')
-- move inside wrapped line
vim.keymap.set('n', 'j', 'gj')
vim.keymap.set('n', 'k', 'gk')
-- C-i to insert single char
vim.keymap.set('n', '<C-i>', 'i_<Esc>r')
