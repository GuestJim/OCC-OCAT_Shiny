# if (!require(data.table))	install.packages("data.table")	;	library(data.table)
if (!require(tibble))	install.packages("tibble")			;	library(tibble)
#	turns out it is making results a tibble, not a data.table that accelerates things
#		handy as it removes the need for data.table or dyplyr
if (!require(tidyr))	install.packages("tidyr")			;	library(tidyr)
if (!require(dplyr))	install.packages("dplyr")			;	library(dplyr)
#	notice dtplyr is not being loaded, as it cannot be at the moment
#		I have not found its lazy_dt to be reliable for my uses and the reframe feature of dplyr 1.1.0 is not supported yet
#		it appears this means the outputs are tibbles instad of data.tables, but the performance is still superior, so I'm good with this

LEVs	=	function(IN)	{
	if (length(levels(IN)) == 0)	return(NULL)
	return(levels(IN))
}

cutWithin	=	function(IN, LIMS, ..., INCLUDE = TRUE)	!is.na(cut(IN,	LIMS,	labels = FALSE,	include.lowest = INCLUDE, ...))

PERCdefa	<-	function(LIST = NULL, PERC = defs$PERCs)	{
	if (!is.null(LIST))	if	(max(LIST) > 1)		LIST	=	LIST/100
	out	<-	unique(sort(c(PERC, LIST), decreasing = FALSE))
	out	<-	out[out >= 0 & out <= 1]

	setNames(out, paste0(out * 100, "%"))
}

ECDFdefa	<-	function(LIST = NULL, ECDF = defs$ECDFs)	{
	if (!is.null(LIST))	LIST[LIST < 0]	<-	0
	out	<-	unique(sort(c(ECDF, LIST), decreasing = FALSE))
	out	<-	out[out != 0]

	setNames(out, paste0(out, " FPS"))
}

SUMMfunc	<-	function(IN, COL = "MsBetweenPresents", GEO = FALSE)	{
	out	<-	IN	|>	reframe(
		"Mean"		=	mean(.data[[COL]],	na.rm = TRUE),
		"Median"	=	median(.data[[COL]],	na.rm = TRUE),
		if (GEO)	"Geo. Mean"	=	exp(mean(log(.data[[COL]]),	na.rm = TRUE))
	)
	out$Unit	<-	"ms"
	out	|>	relocate(Unit, .before = Mean)
}
# SUMMfunc(DATA$results)

PERCfunc	<-	function(IN, PERC = defs$PERCs, COL = "MsBetweenPresents")	{
	PERC	<-	sapply(PERC, function(IN) ifelse(IN < 1, IN, IN / 100)) |> unique() |> sort()
	out	<-	IN	|>	reframe("x" = quantile(.data[[COL]], PERC), "val" = PERC*100) |>
	pivot_wider(names_from = "val",	names_glue = "{val}%", values_from="x")
	
	out$Unit	<-	"ms"
	out	|>	relocate(Unit, .before = "0.1%")
}

ECDFfunc	<-	function(IN, ECDF = defs$ECDFs, COL = "MsBetweenPresents")	{
	ECDF	<-	ECDF |> unique() |> sort(decreasing = TRUE)
	IN	|>	reframe("x" = 100 * (1 - ecdf(.data[[COL]])(1000 / ECDF)), "val" = ECDF) |>
	pivot_wider(names_from = "val",	names_glue = "{val} FPS", values_from="x")
}

SUMMzoom	<-	function(IN, PERC = NULL)	{
	PERC	<-	PERC	|>	append(defs$PERCs)	|>	sapply(function(IN) ifelse(IN < 1, IN, IN / 100)) |> unique() |> sort()
	c(	Mean	=	mean(IN,	na.rm = TRUE),
		Median	=	median(IN,	na.rm = TRUE),
					quantile(IN,	PERC,	na.rm = TRUE)
	)
}

ECDFzoom	<-	function(IN, ECDF = NULL)	{
	ECDF	<-	ECDF	|>	append(defs$ECDFs)	|> unique() |> sort(decreasing = TRUE)
	setNames(100 * (1 - ecdf(IN)(1000 / ECDF)), paste0(ECDF, " FPS"))
}

DEVIfunc	<-	function(IN, COL = "MsBetweenPresents")	{
	out	<-	IN	|>	reframe(
		SD			=	sd(.data[[COL]],	na.rm = TRUE),
		SE			=	sd(.data[[COL]],	na.rm = TRUE) / sqrt(n()),
		"CoV (%)"	=	sd(.data[[COL]],	na.rm = TRUE) / mean(.data[[COL]],	na.rm = TRUE) * 100, 
		MAD 		=	mad(.data[[COL]],	na.rm = TRUE)
	)
	out$Unit	<-	"ms"
	out	|>	relocate(Unit, .before = SD)
}

FPSconv	<-	function(IN)	IN	|>	mutate(across(where(is.numeric), ~ 1000/.x))	|>	mutate(Unit = "FPS")

meanGEO	=	function(IN, na.rm = TRUE)	{
	if (na.rm)	IN	=	na.omit(IN)
	out			=	exp(mean(log(IN)))
	# names(out)	=	"ms"
	return(out)
}

DATA$nameMEAN	=	c("Mean", "Median")
if (VIEW$GEO)	DATA$nameMEAN	=	c(DATA$nameMEAN, "Geo. Mean")
DATA$namePERC	=	names(PERCdefa())
DATA$nameECDF	=	names(ECDFdefa())
DATA$nameDEVI	=	c("SD", "SE", "MAD")
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
	TAB	|>	mutate(across(where(is.numeric), round(r)))
	# numCOL	=	sapply(TAB, is.numeric)
	# TAB[, numCOL]	=	round(TAB[, numCOL], r)
	# return(TAB)
}

library(tableHTML)
OCCHTML	=	function(IN)	{
	tableHTML(IN, rownames = FALSE, class="OCC") |>
	replace_html('style="border-collapse:collapse;" class=OCC border=1', 'align="center" border="1" cellpadding="1" cellspacing="1" style="width: 90%;"') |>
	replace_html(' id=\"tableHTML_header_\\d\"', '', replace_all = TRUE) |>
	replace_html(' id=\"tableHTML_header_\\d\\d\"', '', replace_all = TRUE) |>
	replace_html(' id=\"tableHTML_column_\\d\"', '', replace_all = TRUE) |>
	replace_html(' id=\"tableHTML_column_\\d\\d\"', '', replace_all = TRUE)
}

#	Graph stuff below

# yrates	=	c(c(120, 60, 30, 20, 15, 12, 10), yratesEXT)
yrates	=	c(c(120, 60, 30, 20, 15, 12, 10, 8, 6, 5))
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
	IN	|>	quantile(c(0.001, 0.01, 0.5, 0.99, 0.999))	|>	setNames(c("ymin", "lower", "middle", "upper", "ymax"))
}

qqslope	=	function (IN, r = 2, quan = c(1, 99)/100)	{
	y		=	quantile(IN, quan)
	x		=	100 * quan
	#	to make this be in percentile instead of Z-score
	slope	=	diff(y)/diff(x)
	return(round(slope, r))
}

statGRAPH	=	function(IN, ...)	{
	c(mean(IN), median(IN), median(diff(IN)), qqslope(IN, ...), quantile(IN, c(0.1, 1, 99, 99.9)/100))	|>	setNames(c("Mean", "Median", "DiffMedian", "Slope", "0.1", "1", "99", "99.9"))
}

diff.CONS	=	function(IN, DIR = "Forward", lag = 1)	{
	if	(DIR == "Forward")	return(c(diff(IN, lag = lag), rep(0, lag)))
	if	(DIR == "Backward")	return(c(rep(0, lag), diff(IN, lag = lag)))
}