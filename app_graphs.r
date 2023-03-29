if (!require(ggplot2))	install.packages("ggplot2")
library(ggplot2)


StatQuan	<-	ggproto("StatQuan", Stat,
	required_aes	=	c("sample"),
	compute_group	=	function(data, scales, Q)	{
		Q	<-	sort(unique(Q))
		data.frame(
			xmin = -Inf,		ymin = -Inf,
			xmax = qnorm(Q),	ymax = quantile(data$sample, Q)
			)
	}
)

geom_qq_rect	<-	function(mapping = NULL, data = NULL, geom = "rect", position = "identity", na.rm = FALSE, show.legend = NA,  inherit.aes = TRUE, ...)	{
	layer(stat = StatQuan, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}

StatQuanSlope	<-	ggproto("StatQuanSlope", Stat,
	required_aes	=	c("sample"),
	compute_group	=	function(data, scales, Q = c(0.01, 0.99), r = 2)	{
		data.frame(
			x	=	Inf,	y	=	-Inf,
			label	=	paste0("Slope: ", round(diff(quantile(data$sample, Q))/diff(100 * Q), r))
		)
	}
)

geom_qq_label	<-	function(mapping = NULL, data = NULL, geom = "label", position = "identity", na.rm = FALSE, show.legend = NA,  inherit.aes = TRUE, ...)	{
	layer(stat = StatQuanSlope, data = data, mapping = mapping, geom = geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm, ...))
}

# GRAPH$STATS	=	reactiveVal()
# GRAPH$STATS	=	reactiveVal(sepCOL(aggregate(DATA$results[, "MsBetweenPresents"], DATA$GROUPS, statGRAPH, quan=c(1, 99)/100)))

GRAPH$FILT	<-	eventReactive(input$filtSEL,	{
	req(DATA$results)
	# for (x in c("filtGPU", "filtQUA", "filtAPI", "filtLOC"))	assign(x, 1:nrow(DATA$results))
	filtGPU	<-	1:nrow(DATA$results)
	filtAPI	<-	1:nrow(DATA$results)
	filtQUA	<-	1:nrow(DATA$results)
	filtLOC	<-	1:nrow(DATA$results)

	if (isTruthy(input$filtGPU))	filtGPU		<-	which(DATA$results$GPU		%in%	input$filtGPU)
	if (isTruthy(input$filtQUA))	filtQUA		<-	which(DATA$results$Quality	%in%	input$filtQUA)
	if (isTruthy(input$filtAPI))	filtAPI		<-	which(DATA$results$API		%in%	input$filtAPI)
	if (isTruthy(input$filtLOC))	filtLOC		<-	which(DATA$results$Location	%in%	input$filtLOC)

	Reduce(intersect, list(filtGPU, filtQUA, filtAPI, filtLOC))
},	ignoreNULL	=	FALSE)
#	ignoreNULL is necessary here for GRAPH$FILT to have a value without hitting the button

# GRAPH$STATS	<-	reactive({
	# req(DATA$results)
	# GROUPS	<-	list(
		# GPU			=	DATA$results[GRAPH$FILT(), ]$GPU,
		# Quality		=	DATA$results[GRAPH$FILT(), ]$Quality,
		# API			=	DATA$results[GRAPH$FILT(), ]$API,
		# Location	=	DATA$results[GRAPH$FILT(), ]$Location
	# )

	# sepCOL(aggregate(DATA$results[GRAPH$FILT(), input$datatypeG], GROUPS[input$listFACETS], statGRAPH, quan=c(1, 99)/100))
# })

GRAPH$QUAN	=	reactiveVal(c(1, 99)/100)
observeEvent(input$QUANrefresh, {
	LOW		=	input$QUANlow
	HIGH	=	input$QUANhigh

	if (LOW>= 1 | HIGH >= 1)	{
		LOW		=	LOW/100
		HIGH	=	HIGH/100
	}

	GRAPH$QUAN(c(LOW, HIGH))
},	label	=	"QUANrefreshValues"	)

observeEvent(input$FtimeLimitFPS, {
	updateNumericInput(inputId	=	"FtimeLimitMS",
		value		=	round(1000/input$FtimeLimitFPS, 2)
	)
})
observeEvent(input$FtimeLimitMS, {
	updateNumericInput(inputId	=	"FtimeLimitFPS",
		value		=	round(1000/input$FtimeLimitMS, 0)
	)
},	ignoreInit	=	TRUE)

GRAPH$FtimeLimitMS	=	reactiveVal(1000/15)
observeEvent(input$FtimeLimitRefresh,	{
	GRAPH$FtimeLimitMS(input$FtimeLimitMS)
},	ignoreNULL	=	FALSE,	label	=	"FtimeLimitRefresh")

GRAPH$diffLim	=	reactiveVal(20)
GRAPH$diffLim	=	eventReactive(input$diffLimRefresh,	{
	as.numeric(input$diffLim)
},	ignoreNULL	=	FALSE,	label	=	"diffLimRefresh")

GRAPH$diffPERCLim	=	reactiveVal(1.1)
GRAPH$diffPERCLim	=	eventReactive(input$diffPERCLimRefresh,	{
	as.numeric(input$diffPERCLim)/100
},	ignoreNULL	=	FALSE,	label	=	"diffLimRefresh")


observeEvent(list(input$dataInput, DATA$LOAD), {
	FAC	=	c("GPU", "API", "Quality", "Location")
	if (exists("checkAPI", envir = DATA))	if (!DATA$checkAPI)	FAC	=	c("GPU", "Quality", "Location")

	updateCheckboxGroupInput(
		inputId	=	"listFACETS",
		choices		=	FAC,	selected	=	FAC
	)
})


GRAPH$facWID	=	25
observeEvent(input$facWID, GRAPH$facWID	<-	input$facWID, ignoreInit = TRUE)
facWRAP	=	labeller(	Location	=	label_wrap_gen(GRAPH$facWID),
						API			=	label_wrap_gen(GRAPH$facWID),
						Quality		=	label_wrap_gen(GRAPH$facWID),
						GPU			=	label_wrap_gen(GRAPH$facWID)	)

GRAPH$flipFACETS	=	reactiveValues(
	SUMM	=	FALSE,	COURSE	=	FALSE,	FREQ	=	FALSE,	QQ	=	FALSE,	DIFF	=	FALSE
)

observeEvent(input$flipFACETS, {
	req(isTruthy(VIEW$FACflip))
	if (GRAPH$flipFACETS$SUMM	!=	"SUMM"		%in% input$flipFACETS)	GRAPH$flipFACETS$SUMM	<-	!GRAPH$flipFACETS$SUMM
	if (GRAPH$flipFACETS$COURSE	!=	"COURSE"	%in% input$flipFACETS)	GRAPH$flipFACETS$COURSE	<-	!GRAPH$flipFACETS$COURSE
	if (GRAPH$flipFACETS$FREQ	!=	"FREQ"		%in% input$flipFACETS)	GRAPH$flipFACETS$FREQ	<-	!GRAPH$flipFACETS$FREQ
	if (GRAPH$flipFACETS$QQ		!=	"QQ"		%in% input$flipFACETS)	GRAPH$flipFACETS$QQ		<-	!GRAPH$flipFACETS$QQ
	if (GRAPH$flipFACETS$DIFF	!=	"DIFF"		%in% input$flipFACETS)	GRAPH$flipFACETS$DIFF	<-	!GRAPH$flipFACETS$DIFF
	#	this way the specific element is only changed if the UI for it changed
	#	does take advantage of the fact it can only have one of two values, so it blindly flips the value when the UI flips
})

GRAPH$FONTsize	<-	reactive(input$graphFont)	|>	debounce(500)
FONT	=	function(SIZE = 12)	theme(text = element_text(size = SIZE))

FACET	=	function(graphtype, IN = c("Location", "Quality", "API", "GPU"), Fflip = FALSE)	{
	FACS	=	c(
		GPU			=	"GPU"		%in%	IN,
		Location	=	"Location"	%in%	IN,
		API			=	"API" 		%in%	IN,
		Quality		=	"Quality"	%in%	IN
		)
	FACETselect	=	function(IN2)	paste0(names(FACS[IN2])[FACS[IN2]], collapse = ", ")
	#	this will return only the names that are present in FACS and are desired, as set below

	if	(graphtype == "graphMEANS")		FACSsel	=	c(
		FACETselect(c("Quality", "API")),
		FACETselect(c("Location")))
	if	(graphtype == "graphCOURSE")	if (!is.null(DATA$APIs))	{
		FACSsel	=	c("Location, Quality, API",
					"GPU")
		}	else	{
		FACSsel	=	c("Location, Quality",
					"GPU")
		}
	if	(graphtype	%in%	c("graphFREQ", "graphQQ", "graphDIFF"))	FACSsel	=	c(
		FACETselect(c("Location", "Quality", "API")),
		FACETselect(c("GPU")))

	if (VIEW$FACflip & Fflip)	{
		if (graphtype	==	"graphMEANS")	FACSsel	=	rev(FACSsel)
		if (graphtype	==	"graphCOURSE")	FACSsel	=	rev(FACSsel)
		if (graphtype	==	"graphFREQ")	FACSsel	=	rev(FACSsel)
		if (graphtype	==	"graphQQ")		FACSsel	=	rev(FACSsel)
		if (graphtype	==	"graphDIFF")	FACSsel	=	rev(FACSsel)
	}
	ROWS	=	FACSsel[1]	;	COLS	=	FACSsel[2]

	outROW	=	NULL	;	outCOL	=	NULL
	if (ROWS != "")	outROW	=	paste0("rows = vars(", ROWS, ")")
	if (COLS != "")	outCOL	=	paste0("cols = vars(", COLS, ")")

	out	=	paste0("facet_grid(", paste(outROW, outCOL, sep = ", "), ", switch = 'y', labeller = facWRAP)")
	return(eval(parse(text = out)))
}

scaleY	=	function(graphtype, datatype){
	nameYP	=	"ms"
	nameYS	=	"FPS"
	labelYP	=	labelRound
	labelYS	=	ms2FPS

	if	(datatype == "MsBetweenPresents")		{
		nameYP	=	"Frame Time (ms)"
		nameYS	=	"Frame Rate (FPS)"
		labelYP	=	labelRound
		labelYS	=	ms2FPS
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		nameYP	=	"Refresh Cycles Later (1/60 s)"
		nameYS	=	"Display Time (ms)"
		labelYP	=	labelDisp
		labelYS	=	labelRound
	}
	if	(datatype == "MsUntilRenderComplete")	{
		nameYP	=	"Render Time (ms)"
		nameYS	=	"Render Rate (FPS)"
		labelYP	=	labelRound
		labelYS	=	ms2FPS
	}
	if	(datatype == "MsEstimatedDriverLag")	{
		nameYP	=	"Estimated Driver Lag (ms)"
		nameYS	=	""
		labelYP	=	labelRound
		labelYS	=	NULL
	}

	if	(graphtype %in% c("graphMEANS", "graphCOURSE", "graphQQ"))	{	return(
		scale_y_continuous(
			name		=	nameYP,
			breaks		=	ybreaks,	labels	=	labelYP,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	nameYS,
				labels	=	labelYS
				)
			)
		)	}
	if	(graphtype %in% c("graphDIFF"))	{	return(
		scale_y_continuous(
			name		=	nameYP,
			breaks		=	ybreaks,	labels	=	labelYP,	expand	=	c(0.02, 0),
			)
		)	}
	return(NULL)	#	fallback to returning nothing
}

scaleX	=	function(graphtype, datatype){
	nameXP	=	"ms"
	nameXS	=	"FPS"
	labelXP	=	labelRoundB
	labelXS	=	ms2FPS.lab

	if	(datatype == "MsBetweenPresents")	{
		nameXP	=	"Frame Time (ms)"
		nameXS	=	"Frame Rate (FPS)"
		labelXP	=	labelRoundB
		labelXS	=	ms2FPS.lab
	}
	if	(datatype == "MsBetweenDisplayChange")	{
		nameXP	=	"Refresh Cycles Later (1/60 s)"
		nameXS	=	"Display Time (ms)"
		labelXP	=	labelDispB
		labelXS	=	labelBreak
	}
	if	(datatype == "MsUntilRenderComplete")	{
		nameXP	=	"Render Time (ms)"
		nameXS	=	"Render Rate (FPS)"
		labelXP	=	labelRoundB
		labelXS	=	ms2FPS.lab
	}
	if	(datatype == "MsEstimatedDriverLag")	{
		nameXP	=	"Estimated Driver Lag (ms)"
		nameXS	=	""
		labelXP	=	labelRoundB
		labelXS	=	NULL
	}

	if	(graphtype %in% c("graphFREQ"))	{	return(
		scale_x_continuous(
			name		=	nameXP,
			breaks		=	ybreaks,	labels	=	labelXP,	expand	=	c(0.02, 0),
			sec.axis	=	dup_axis(
				name	=	nameXS,
				labels	=	labelXS
				)
			)
		)	}
	return(NULL)	#	fallback to returning nothing
}

graphSUMM	=	function(FILT = TRUE, GEO = FALSE)	{
	MEAN	=	list(geom_bar(aes(fill = GPU), stat = "summary", fun = mean))
	if (GEO)	MEAN	=	list(geom_bar(aes(fill = GPU), stat = "summary", fun = meanGEO))
	GPU_fill	=	scale_fill_manual(values = scales::hue_pal()(length(levels(DATA$results$GPU))),	breaks = levels(DATA$results$GPU))

	ggplot(data = DATA$results[FILT, ], aes(x = GPU, y = get(input$datatypeG))) +
	ggtitle(DATA$game, subtitle = paste0(input$datatypeG, " - Means, Medians, and Percentiles")) + #input$listGPU +
	geom_hline(yintercept = 1000/60, color = "red") +
	# geom_boxplot(outlier.alpha = 0) +
	stat_summary(fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
	# MEAN + scale_fill_hue(drop = FALSE) +
	MEAN + GPU_fill +
	stat_summary(fun.data = BoxPerc, geom = "boxplot", alpha = 0.25, width = 0.6) +
	# geom_boxplot(alpha = 0.50, outlier.alpha = 0) +
	# FACET("graphMEANS") +
	# scale_x_discrete(labels = labelBreak, drop = !(length(unique(DATA$results$GPU))>1)) +
	scale_x_discrete(labels = labelBreak, drop = TRUE) +
	scaleY("graphMEANS", input$datatypeG) +
	coord_cartesian(ylim = c(0, GRAPH$FtimeLimitMS())) +
	guides(fill = guide_legend(nrow = 1)) + theme(legend.position = "bottom", plot.title.position = "plot")
}

output$graphMEANfacet	=	renderPlot({
	req(DATA$results)
	graphSUMM(GRAPH$FILT(), isTruthy(input$graphGEO)) + FACET("graphMEANS", input$listFACETS, Fflip = GRAPH$flipFACETS$SUMM) + FONT(GRAPH$FONTsize())
})

graphCOURSE	=	function(FILT, zoom = FALSE)	{
	ALPHA	=	0.05
	if	(length(unique(DATA$results$Location)) == 1)	ALPHA	=	1

	GEOM	=	list(geom_point(alpha = ALPHA), geom_smooth(method="gam", formula= y ~ s(x, bs = "cs")))
	if (zoom)	GEOM	=	geom_step()

	ggplot(data = DATA$results[FILT, ], aes(x = TimeInSeconds, y = get(input$datatypeG))) +
	ggtitle(DATA$game, subtitle = paste0(input$datatypeG, " - Course")) +
	geom_hline(yintercept = 1000/60, color = "red") +
	GEOM +
	scale_x_continuous(name="Time (s)", breaks=seq(from=0, to=max(DATA$results$TimeInSeconds), by=60), labels = labelBreak, expand=c(0.02, 0)) +
	scaleY("graphCOURSE", input$datatypeG) +
	coord_cartesian(ylim = c(0, GRAPH$FtimeLimitMS())) +
	theme(plot.title.position = "plot")
}

output$graphCOURSEfacet	=	renderPlot({
	req(DATA$results)
	graphCOURSE(GRAPH$FILT(), zoom = FALSE) + FACET("graphCOURSE", Fflip = GRAPH$flipFACETS$COURSE) + FONT(GRAPH$FONTsize())
})

graphFREQ	=	function(FILT, GEO = FALSE)	{
	GROUPS	<-	list(
		GPU			=	DATA$results[FILT, ]$GPU,
		API			=	DATA$results[FILT, ]$API,
		Quality		=	DATA$results[FILT, ]$Quality,
		Location	=	DATA$results[FILT, ]$Location
	)
	if (!("GPU"			%in% input$listFACETS))	GROUPS$GPU		<-	NULL
	if (!("API"			%in% input$listFACETS))	GROUPS$API		<-	NULL
	if (!("Quality"		%in% input$listFACETS))	GROUPS$Quality	<-	NULL
	if (!("Location"	%in% input$listFACETS))	GROUPS$Location	<-	NULL

	MEAN	=	list(geom_vline(data = aggregate(DATA$results[FILT, as.character(input$datatypeG)], GROUPS, mean), aes(xintercept = get(input$datatypeG)), color = "darkgreen"))
	if (GEO)	MEAN	=	list(geom_vline(data = aggregate(DATA$results[FILT, as.character(input$datatypeG)], GROUPS, meanGEO), aes(xintercept = get(input$datatypeG)), color = "darkgreen"))

	ggplot(DATA$results[FILT, ], aes(get(x = input$datatypeG))) +
	ggtitle(DATA$game, subtitle=paste0(input$datatypeG, " - Frequency Plot")) +
	geom_vline(xintercept = 1000/60, color = "red") +
	geom_freqpoly(binwidth=0.03, linewidth=0.25) +
	# FACET("graphFREQ") +
		MEAN +
		geom_vline(data = aggregate(DATA$results[FILT, as.character(input$datatypeG)], GROUPS, median), aes(xintercept = get(input$datatypeG)), color = "darkcyan", linetype="dotdash") +
	scaleX("graphFREQ", input$datatypeG) +
	coord_cartesian(xlim = c(0, GRAPH$FtimeLimitMS())) +
	scale_y_continuous(name="Count", expand=c(0.02, 0)) +
	theme(plot.title.position = "plot")
}

output$graphFREQfacet	=	renderPlot({
	req(DATA$results)
	graphFREQ(GRAPH$FILT(), isTruthy(input$graphGEO)) + FACET("graphFREQ", input$listFACETS, Fflip = GRAPH$flipFACETS$FREQ) + FONT(GRAPH$FONTsize())
})

graphQQ	=	function(FILT)	{
	PERCSdef	=	c(.001, .01, .5, .99, .999)
	PERCS		=	sort(unique(	signif(c(PERCSdef, GRAPH$QUAN()))	))

	ggplot(data = DATA$results[FILT, ], aes(sample = get(input$datatypeG))) +
	ggtitle(DATA$game, subtitle = paste0(input$datatypeG, " - Quantile Distribution")) +
	geom_hline(yintercept = 1000/60, color	=	"red") +
		lapply(PERCS, function(IN) geom_qq_rect(Q = IN,	alpha = 0.1, fill = ifelse(IN <=0.5, "blue", "red"), color = "grey")) +
	stat_qq_line(line.p = GRAPH$QUAN(), color = "green", size = 1.1, linetype = "dotted") +
	stat_qq() +
	stat_qq_line(line.p = GRAPH$QUAN(), color = "green", alpha = 0.5, size = 1.1, linetype = "dotted") +
	geom_qq_label(Q = GRAPH$QUAN(), parse = TRUE, hjust="right", vjust="bottom", fill = "darkgrey", color = "green") +
	scaleY("graphQQ", input$datatypeG) +
	coord_cartesian(ylim = c(0, GRAPH$FtimeLimitMS())) +
	scale_x_continuous(name = "Percentile", breaks = qnorm(PERCS), labels = labelBreak(paste0(PERCS * 100, "%")), minor_breaks = NULL, expand = c(0.02, 0)) +
	theme(plot.title.position = "plot")
}

output$graphQQfacet	=	renderPlot({
	req(DATA$results)
	graphQQ(GRAPH$FILT()) + FACET("graphQQ", input$listFACETS, Fflip = GRAPH$flipFACETS$QQ) + FONT(GRAPH$FONTsize())
})

graphDIFF	=	function(FILT, PERC	=	FALSE)	{
	limX	=	c(0, GRAPH$FtimeLimitMS())
	limY	=	c(-GRAPH$diffLim(), GRAPH$diffLim())

	scale_X	=	scale_x_continuous(
		name	=	"ms",
		breaks	=	ybreaks,	labels	=	labelRoundB,	limits	=	limX,
		expand	=	c(0.02, 0)
	)
	scale_Y	=	scale_y_continuous(
		name	=	"Consecutive Time Difference (ms)",
		breaks	=	c(ybreaks, limY),	labels	=	labelRound,		limits	=	limY,
		expand	=	c(0.02, 0)
	)

	if	(input$datatypeG == "MsBetweenPresents")	{
		scale_X	=	scale_x_continuous(
			name	=	"Frame Time (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	limits	=	limX,
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Frame Time Difference (ms)",
			breaks	=	c(ybreaks, limY),	labels	=	labelRound,		limits	=	limY,
			expand	=	c(0.02, 0)
		)
	}
	if	(input$datatypeG == "MsBetweenDisplayChange")	{
		scale_X	=	scale_x_continuous(
			name	=	"Refresh Cycles Later (1/60 s)",
			breaks	=	ybreaks,	labels	=	labelDisp,		limits	=	limX,
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Display Time Difference (ms)",
			breaks	=	c(ybreaks, limY),	labels	=	labelRound,		limits	=	limY,
			expand	=	c(0.02, 0)
		)
	}
	if	(input$datatypeG == "MsUntilRenderComplete")	{
		scale_X	=	scale_x_continuous(
			name	=	"Render Time (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	limits	=	limX,
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Render Time Difference (ms)",
			breaks	=	c(ybreaks, limY),	labels	=	labelRound,		limits	=	limY,
			expand	=	c(0.02, 0)
		)
	}
	if	(input$datatypeG == "MsEstimatedDriverLag")	{
		scale_X	=	scale_x_continuous(
			name	=	"Estimated Driver Lag (ms)",
			breaks	=	ybreaks,	labels	=	labelRoundB,	limits	=	limX,
			expand	=	c(0.02, 0)
		)
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Lag Difference (ms)",
			breaks	=	c(ybreaks, limY),	labels	=	labelRound,		limits	=	limY,
			expand	=	c(0.02, 0)
		)
	}

	if (PERC)	{
		limY	=	c(-GRAPH$diffPERCLim(), GRAPH$diffPERCLim())
		scale_Y	=	scale_y_continuous(
			name	=	"Consecutive Difference as Percentage",
			breaks	=	seq(-2, 2, by = 0.25),	labels	=	function(IN)	paste0(IN*100, "%"),	limits	=	limY,
			expand	=	c(0.02, 0)
		)
	}

	graphPARTS	=	list(
		ggtitle(DATA$game, subtitle=paste0(input$datatypeG, " Consecutive Differences")),
		geom_point(alpha = 0.1),
		# stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) + scale_fill_viridis_c(),
		# stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE),	scale_fill_viridis_c(),
		# FACET("graphDIFF"),
		scale_X,
		scale_Y,
		theme(plot.title.position = "plot")
	)

	if (!PERC)	{
		ggplot(data = DATA$results[FILT, ], aes(x = get(input$datatypeG), y = diff.CONS(get(input$datatypeG))) ) +
		graphPARTS
	}	else	{
		ggplot(data = DATA$results[FILT, ], aes(x = get(input$datatypeG), y = diff.CONS(get(input$datatypeG))/get(input$datatypeG)) ) +
		graphPARTS
	}

}

output$graphDIFFfacet	=	renderPlot({
	req(DATA$results)
	graphDIFF(GRAPH$FILT()) + FACET("graphDIFF", input$listFACETS, Fflip = GRAPH$flipFACETS$DIFF) + FONT(GRAPH$FONTsize()) +
	stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) + scale_fill_viridis_c()
})

output$graphDIFFpercfacet	=	renderPlot({
	req(DATA$results)
	graphDIFF(GRAPH$FILT(), TRUE) + FACET("graphDIFF", input$listFACETS, Fflip = GRAPH$flipFACETS$DIFF) + FONT(GRAPH$FONTsize()) +
	stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) + scale_fill_viridis_c()
})

if (VIEW$BRUSH)	source("app_graphs_zoom.r", local = TRUE)