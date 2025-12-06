require("mason").setup()
require("mason-lspconfig").setup({
	ensure_installed = { "clangd", "tinymist", "jdtls" },
})

vim.lsp.config["tinymist"] = {
	cmd = {"tinymist"},
	filetypes = {"typst"},
	settings = {
		formatterMode = "typstyle",
		exportPdf = "onType",
		semanticTokens = "disable",
		--lint.enabled = true
	},
},

vim.lsp.config["jdtls"]

vim.lsp.enable({
	"tinymist","clangd","jdtls"
})
