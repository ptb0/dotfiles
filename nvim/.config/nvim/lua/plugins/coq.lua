return {
	{ 
		"ms-jpq/coq_nvim",
		init = function()
			vim.g.coq_settings = {
				auto_start = 'shut-up',
				keymap = {
					recommended = true,
					pre_select = false,
					bigger_preview = "<C-k>",
				},
				display = {
					pum = {
						y_max_len = 8,
					},
					preview = {
						border = "double",
					},
					icons = {
						mode = "none",
					},
				},
				clients = {
					buffers = {
						match_syms = false,
					},
				},
			}
		end,
	},

	{
		"ms-jpq/coq.artifacts"
	},

	{
		"ms-jpq/coq.thirdparty"
	},
}
