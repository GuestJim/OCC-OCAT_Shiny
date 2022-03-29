#	with saveRDS we can save an R object directly, complete with its formatting that is lost in a CSV
#	by being an R object as a file, it can be loaded faster an the formatting does not need to be done
library(readr)

useSHORT	=	FALSE
relPath	=	paste0(unlist(strsplit(getwd(), "OCAT Data"))[1], "OCAT Data")

txtFIND	=	function(TXT, rel.Path = relPath)	{
	if (file.exists(TXT))	{
		return(readLines(TXT, warn = FALSE))
	}	else	{
		locFILE	=	paste0(rel.Path, "/", TXT)
		if (file.exists(locFILE))	return(readLines(locFILE, warn = FALSE))
	}
	return(NULL)
}


listGPU		=	c(
"RX 580",
"RX Vega 64",
"GTX 770",
"GTX 980",
"GTX 1070",
"GTX 1080",
"RTX 2060",
"RTX 2080"
)

listQUA		=	txtFIND("Qualities.txt")

listLOC		=	txtFIND("Locations.txt")
shortLOC	=	txtFIND("Locations Short.txt")
levsLOC		=	listLOC
if	(useSHORT	&	!is.null(shortLOC))	levsLOC	=	shortLOC

listAPI		=	txtFIND("APIs.txt")
shortAPI	=	txtFIND("APIs Short.txt")
levsAPI		=	listAPI
if	(useSHORT	&	!is.null(shortAPI))	levsAPI	=	shortAPI

FILE		=	list.files(pattern = "*.csv")
FILEname	=	unlist(strsplit(FILE, ".", fixed = TRUE))[1]
if (length(FILE) > 1)	{
	FILE		=	file.choose()
	FILEname	=	unlist(strsplit(FILE, "\\\\"))
	FILEname	=	unlist(strsplit(	FILEname[length(FILEname)], ".", fixed = TRUE))[1]
}


noCOL	=	c("ProcessID", "SwapChainAddress", "SyncInterval", "PresentFlags", "AllowsTearing", "PresentMode", "DwmNotified")
resultsFull	=	read_csv(FILE, lazy = TRUE, guess_max = 10, show_col_types = FALSE, col_select = !all_of(noCOL))

resultsFull$GPU		=	ordered(resultsFull$GPU,		levels = listGPU)
resultsFull$Quality	=	ordered(resultsFull$Quality)
if	(!is.null(listQUA))	resultsFull$Quality		=	ordered(resultsFull$Quality,	levels = listQUA)

resultsFull$Location	=	ordered(resultsFull$Location)
if	(!is.null(listLOC)) resultsFull$Location	=	ordered(resultsFull$Location,	levels = listLOC)

resultsFull$API		=	ordered(resultsFull$API)
if	(!is.null(listAPI)) resultsFull$API			=	ordered(resultsFull$API,		levels = listAPI)

# saveRDS(resultsFull, paste0(FILEname, ".RData"))
saveRDS(resultsFull, paste0(FILEname, ".RData"), compress = "bzip2")
