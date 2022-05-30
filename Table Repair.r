library(readr)
# setwd("E:/Users/Jim/My Documents/OCC/@Reviews/@OCAT R Scripts/OCC-OCAT_Shiny/OCAT Data")

read_TABS	=	function(NAME, HEAD = NULL)	{
	NAME.pos	=	NULL
	LIST		=	list.files(pattern = "*.html")
	if (any(endsWith(LIST, NAME)))	NAME.pos	=	which(endsWith(LIST, NAME))
	
	if (is.null(NAME.pos))	return(NULL)
	return(paste0(HEAD, read_file(LIST[NAME.pos])))
}

# FILE	=	file.choose()

for (FILE in list.files(pattern = "*.RData$"))	{
	DATA	=	readRDS(FILE)

	DATA$specsDESK	=	read_TABS("Specs_Desktop.html")
	# DATA$specsTEST	=	read_TABS("Specs_Test.html")
	# DATA$configOPTI	=	read_TABS("Options.html",		"<h4>Options</h4>")
	# DATA$configPRES	=	read_TABS("Presets.html",		"<h4>Presets</h4>")
	# DATA$config60FP	=	read_TABS("60 FPS Target.html",	"<h4>60 FPS Target</h4>")

	saveRDS(DATA, FILE, compress = "xz")
}
