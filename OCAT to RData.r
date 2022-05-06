#	with saveRDS we can save an R object directly, complete with its formatting that is lost in a CSV
#	by being an R object as a file, it can be loaded faster an the formatting does not need to be done
library(readr)
# setwd("C:/Users/Jim/Documents/Respond/R-Shiny/R data saving")

DATA	=	new.env()

useSHORT	=	FALSE
relPath	=	paste0(unlist(strsplit(getwd(), "OCAT Data"))[1], "OCAT Data")

FILE		=	list.files(pattern = "*.csv")
FILEname	=	unlist(strsplit(FILE, ".", fixed = TRUE))[1]
if (length(FILE) > 1)	{
	FILE		=	file.choose()
	FILEname	=	unlist(strsplit(FILE, "\\\\"))
	FILEname	=	unlist(strsplit(	FILEname[length(FILEname)], ".", fixed = TRUE))[1]
}
noCOL	=	c("ProcessID", "SwapChainAddress", "SyncInterval", "PresentFlags", "AllowsTearing", "PresentMode", "DwmNotified")
resultsFull	=	read_csv(FILE, lazy = TRUE, guess_max = 10, show_col_types = FALSE, col_select = !all_of(noCOL))

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


resultsFull$GPU		=	ordered(resultsFull$GPU,		levels = listGPU)
resultsFull$Quality	=	ordered(resultsFull$Quality)
if	(!is.null(listQUA))	resultsFull$Quality		=	ordered(resultsFull$Quality,	levels = listQUA)

resultsFull$Location	=	ordered(resultsFull$Location)
if	(!is.null(listLOC)) resultsFull$Location	=	ordered(resultsFull$Location,	levels = listLOC)

resultsFull$API		=	ordered(resultsFull$API)
if	(!is.null(listAPI)) resultsFull$API			=	ordered(resultsFull$API,		levels = listAPI)

DATA$GROUPS	=	list(GPU = resultsFull$GPU,	Quality = resultsFull$Quality, API = resultsFull$API,	Location = resultsFull$Location)
DATA$results	=	resultsFull

LEVs	=	function(IN)	{
	if (length(levels(IN)) == 0)	return(NULL)
	return(levels(IN))
}
DATA$GPUs	=	LEVs(DATA$results$GPU)
DATA$QUAs	=	LEVs(DATA$results$Quality)
DATA$LOCs	=	LEVs(DATA$results$Location)
DATA$APIs	=	LEVs(DATA$results$API)

DATA$checkAPI	=	TRUE
if (is.na(unique(DATA$GROUPS$API)))		DATA$checkAPI	=	FALSE
if (!DATA$checkAPI)	DATA$GROUPS$API		=	NULL

sepCOL	=	function(aggOUT)	{
	matCOL	=	sapply(aggOUT, is.matrix)
	out	=	cbind(aggOUT[, !matCOL], as.data.frame(aggOUT[, matCOL]))
	return(out)
}

summMS	=	function(IN)	setNames(c(
		mean(IN),	median(IN),	quantile(IN, c(0.1, 1, 99, 99.9)/100)),
	c(	"Mean", 	"Median",	"0.1%", "1%", "99%", "99.9")	)

ecdfFPS	=	function(IN, listFPS = c(60, 30))	setNames(100 * (1 - ecdf(IN)(1000 / listFPS)),	paste0(listFPS, " FPS"))

#	ECDF Table
funcECDF	=	function()	{
	out	=	sepCOL(aggregate(resultsFull$MsBetweenPresents, DATA$GROUPS, ecdfFPS))
	out$Unit	=	"%"
	colDATA	=	sapply(out, is.numeric)

	return(out[, c(which(!colDATA), which(colDATA))])
}
DATA$tableECDF	=	funcECDF()

#	Summary Table
funcSUMM	=	function()	{
	out	=	sepCOL(aggregate(resultsFull$MsBetweenPresents, DATA$GROUPS, summMS))

	out$Unit	=	"ms"
	colDATA	=	sapply(out, is.numeric)

	outFPS	=	out[out$Unit == "ms", ]
	outFPS$Unit	=	"FPS"
	outFPS[, colDATA]	=	1000/outFPS[, colDATA]

	out	=	rbind(outFPS, out)
	return(out[, c(which(!colDATA), which(colDATA))])
}
DATA$tableSUMM	=	funcSUMM()

# saveRDS(DATA, paste0(FILEname, ".RData"), compress = FALSE)
# saveRDS(DATA, paste0(FILEname, ".RData"), compress = "gzip")	#	by default compresses using this method
# saveRDS(DATA, paste0(FILEname, ".RData"), compress = "bzip2")
saveRDS(DATA, paste0(FILEname, ".RData"), compress = "xz")		#	faster and better than bzip2, in one test, but gzip fastest
# library(fst)
# write_fst(DATA$results, paste0(FILEname, ".Rfst"))
#	fst has "faster" saving and loading, but the file size is larger and it is only for data frames