LEVs	=	function(IN)	{
	if (length(levels(IN)) == 0)	return(NULL)
	return(levels(IN))
}

sepCOL	=	function(aggOUT)	{
	matCOL	=	sapply(aggOUT, is.matrix)
	out	=	cbind(aggOUT[, !matCOL], as.data.frame(aggOUT[, matCOL]))
	return(out)
}

meanGEO	=	function(IN, na.rm = TRUE)	{
	if (na.rm)	IN	=	na.omit(IN)
	out			=	exp(mean(log(IN)))
	# names(out)	=	"ms"
	return(out)
}

summMS	=	function(IN, listPERC = NULL)	{
	default	=	c(0.1, 1, 99, 99.9)/100
	if (!is.null(listPERC))	if	(max(listPERC) > 1)		listPERC	=	listPERC/100
	listPERC	=	unique(sort(c(default, listPERC), decreasing = FALSE))
	if (any(listPERC > 1))	listPERC	<-	listPERC[-which(listPERC > 1)]
	if (any(listPERC < 0))	listPERC	<-	listPERC[-which(listPERC < 0)]

	setNames(c(mean(IN, na.rm = TRUE), median(IN, na.rm = TRUE), meanGEO(IN, na.rm = TRUE), quantile(IN, listPERC, na.rm = TRUE)), c("Mean", "Median", "Geo. Mean", paste0(listPERC * 100, "%")))
}

deviMS	=	function(IN)	{
	c(
		"SD"	=	sd(IN,	na.rm = TRUE),
		"SE"	=	sd(IN,	na.rm = TRUE)/sqrt(length(IN)),
		"MAD"	=	mad(IN,	na.rm = TRUE)
	)
}

ecdfFPS	=	function(IN, listFPS = NULL)	{
	# default		=	c(60, 50, 30, 20, 15)
	default		=	c(60, 30)
	listFPS		=	unique(sort(c(default, listFPS), decreasing = TRUE))
	
	setNames(100 * (1 - ecdf(IN)(1000 / listFPS)),	paste0(listFPS, " FPS"))
}

namePERC	=	function(listPERC = NULL)	{
	default	=	c(0.1, 1, 99, 99.9)/100
	if (!is.null(listPERC))	if	(max(listPERC) > 1)		listPERC	=	listPERC/100
	listPERC	=	unique(sort(c(default, listPERC), decreasing = FALSE))

	DATA$namePERC	=	paste0(listPERC * 100, "%")
}
nameECDF	=	function(listFPS = NULL)	{
	default		=	c(60, 30)
	listFPS		=	unique(sort(c(default, listFPS), decreasing = TRUE))

	DATA$nameECDF	=	paste0(listFPS, " FPS")
}
DATA$nameMEAN	=	c("Mean", "Median")
if (VIEW$GEO)	DATA$nameMEAN	=	c(DATA$nameMEAN, "Geo. Mean")
DATA$nameDEVI	=	names(deviMS(NULL))
DATA$namePERC	=	namePERC()
DATA$nameECDF	=	nameECDF()
nameDEFs	=	c("Mean", "1%", "99%", "60 FPS")

to.NUM	=	function(IN)	{
	if (length(IN) == 1) if (IN == "")	return(NULL)
	IN	=	gsub("[%]|[FPS]|[fps]", "", IN)
	IN	=	unlist(strsplit(IN, "[, ]"))
	out	=	as.numeric(IN)
	out	=	out[!is.na(out)]
	return(out)
}
tableROUND	=	function(TAB, r)	{
	numCOL	=	sapply(TAB, is.numeric)
	TAB[, numCOL]	=	round(TAB[, numCOL], r)
	return(TAB)
}

library(tableHTML)
OCCHTML	=	function(IN)	{
	tableHTML(IN, rownames = FALSE, class="OCC") %>%
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') %>%
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_header_\\d\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE) %>%
	replace_html(' id=\"tableHTML_column_\\d\\d\"', '', replace_all = TRUE)
}

#	Graph stuff below

# yrates	=	c(c(120, 60, 30, 20, 15, 12, 10), yratesEXT)
yrates	=	c(c(120, 60, 30, 20, 15, 12, 10))
yrates	=	sort(c(yrates,-yrates))
ytimes	=	sort(1000/yrates)
ybreaks	=	sort(c(round(ytimes, 2), 0))
ms2FPS	=	function(IN, r = 0)	round(1000/IN, r)
app.BREAK	=	TRUE
labelBreak	=	function(breaks, SEC = FALSE)	{
	if (!app.BREAK)	return(breaks)
	BREAK	=	c("", "\n")
	if	(is.numeric(breaks)	&	0 %in% breaks)	if	((which(breaks %in% 0) %% 2) == 0)	BREAK	=	rev(BREAK)
	if	(!SEC)	return(	paste0(rep(BREAK, length.out = length(breaks)),	breaks)	)
	if	(SEC)	return(	paste0(breaks, rep(BREAK, length.out = length(breaks)))	)
}
#	can be disabled by setting app.BREAK to FALSE

# labelRound	=	function(breaks)	sprintf("%.1f", breaks)
labelRound	=	function(breaks)	round(breaks, 1)
labelRoundB	=	function(breaks)	labelBreak(labelRound(breaks))
ms2FPS.lab	=	function(breaks)	labelBreak(ms2FPS(breaks), SEC = TRUE)
labelBreakQQ=	function(breaks)	labelBreak(paste0(signif(pnorm(breaks) * 100), "%"))
labelDisp	=	function(breaks)	round(breaks * 60/1000, 1)
labelDispB	=	function(breaks)	labelBreak(labelDisp(breaks))

BoxPerc	=	function (IN)	{
	out			=	quantile(IN, c(0.001, 0.01, 0.5, 0.99, 0.999))
	names(out)	=	c("ymin", "lower", "middle", "upper", "ymax")
	return(out)
}

qqslope	=	function (IN, r = 2, quan = c(1, 99)/100)	{
	y		=	quantile(IN, quan)
	x		=	100 * quan
	#	to make this be in percentile instead of Z-score
	slope	=	diff(y)/diff(x)
	return(round(slope, r))
}

statGRAPH	=	function(IN, ...)	{
	out			=	c(mean(IN), median(IN), median(diff(IN)), qqslope(IN, ...), quantile(IN, c(0.1, 1, 99, 99.9)/100))
	names(out)	=	c("Mean", "Median", "DiffMedian", "Slope", "0.1", "1", "99", "99.9")
	return(out)
}

diff.CONS	=	function(IN, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(IN, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(IN, lag = lag)))
}