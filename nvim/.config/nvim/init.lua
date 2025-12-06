local lazypath = vim.fn.stdpath("data") .. "/lazy/lazy.nvim"
if not vim.loop.fs_stat(lazypath) then
    vim.fn.system({
        "git",
        "clone",
        "--filter=blob:none",
        "https://github.com/folke/lazy.nvim.git",
        "--branch=stable",
        lazypath
    })
end
vim.opt.rtp:prepend(lazypath)

vim.g.mapleader = " "
vim.g.maplocalleader = "\\"

require("lazy").setup("plugins", {
	ui = {
    		icons = {
    			cmd = "",
    		  	config = "",
    		  	event = "",
    		  	ft = "",
    		  	init = "",
    		  	keys = "",
    		  	plugin = "",
    		  	runtime = "",
    		  	require = "",
    		  	source = "",
    		  	start = "",
    		  	task = "",
    		  	lazy = "",
    		},
	},
})

require("config.lsp")
require("config.settings")
