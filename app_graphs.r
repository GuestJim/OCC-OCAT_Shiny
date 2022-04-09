require(ggplot2)

GRAPH$FILT	=	reactiveVal()
observeEvent(list(input$dataInput, DATA$LOAD),	{GRAPH$FILT	=	reactiveVal(1:nrow(DATA$results))},	ignoreInit = TRUE)
# GRAPH$STATS	=	reactiveVal()
# GRAPH$STATS	=	reactiveVal(sepCOL(aggregate(DATA$results[, "MsBetweenPresents"], DATA$GROUPS, statGRAPH, quan=c(1, 99)/100)))
observeEvent(input$filtSEL,	{
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

	GRAPH$FILT(Reduce(intersect, list(filtGPU, filtQUA, filtAPI, filtLOC)))
})

GRAPH$STATS	<-	reactive({
	req(DATA$results)
	GROUPS	<-	list(
		GPU			=	DATA$results[GRAPH$FILT(), ]$GPU,
		Quality		=	DATA$results[GRAPH$FILT(), ]$Quality,
		API			=	DATA$results[GRAPH$FILT(), ]$API,
		Location	=	DATA$results[GRAPH$FILT(), ]$Location
	)
	
	sepCOL(aggregate(DATA$results[GRAPH$FILT(), input$datatypeG], GROUPS[input$listFACETS], statGRAPH, quan=c(1, 99)/100))
})

# GRAPH$STATS	=	reactiveVal()
# if (exists("results", envir = DATA))	GRAPH$STATS(sepCOL(aggregate(DATA$results[, "MsBetweenPresents"], DATA$GROUPS, statGRAPH, quan=c(1, 99)/100)))

# observeEvent(list(input$dataInput, DATA$LOAD, input$datatypeG, input$QUANrefresh, input$listFACETS), {
	# req(DATA$results)
	# GRAPH$STATS(sepCOL(aggregate(DATA$results[, input$datatypeG], DATA$GROUPS[names(DATA$GROUPS) %in% input$listFACETS], statGRAPH, quan=GRAPH$QUAN())))
# },	ignoreInit	=	TRUE)

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
diffLimRefresh	=	eventReactive(input$diffLimRefresh,	{
	GRAPH$diffLim(as.numeric(input$diffLim))
},	ignoreNULL	=	FALSE,	label	=	"diffLimRefresh")


observeEvent(list(input$dataInput, DATA$LOAD), {
	FAC	=	c("GPU", "API", "Quality", "Location")
	if (exists("checkAPI", envir = DATA))	if (!DATA$checkAPI)	FAC	=	c("GPU", "Quality", "Location")

	updateCheckboxGroupInput(
		inputId	=	"listFACETS",
		choices		=	FAC,	selected	=	FAC
	)
})



facWRAP	=	labeller(	Location	=	label_wrap_gen(input$facWID),
						API			=	label_wrap_gen(input$facWID),
						Quality		=	label_wrap_gen(input$facWID),
						GPU			=	label_wrap_gen(input$facWID)	)

FACET	=	function(graphtype, IN = c("Location", "Quality", "API", "GPU"))	{
	FACS	=	c(
		GPU			=	"GPU"		%in%	IN,
		Location	=	"Location"	%in%	IN,
		API			=	"API" 		%in%	IN,
		Quality		=	"Quality"	%in%	IN
		)
	
	FACETselect	=	function(IN2)	paste0(names(FACS[IN2])[FACS[IN2]], collapse = ", ")
	#	this will return only the names that are present in FACS and are desired, as set below
	
	if	(graphtype == "graphMEANS")	{
		ROWS	=	FACETselect(c("Quality", "API"))
		COLS	=	FACETselect(c("Location"))
	}
	if	(graphtype == "graphCOURSE")	{
		ROWS	=	"Location, Quality, API"
		COLS	=	"GPU"
	}
	if	(graphtype	%in%	c("graphFREQ", "graphQQ", "graphDIFF"))	{
		ROWS	=	FACETselect(c("Location", "Quality", "API"))
		COLS	=	FACETselect(c("GPU"))
	}
	
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
		labelYS	=	ybreaks
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
		labelXS	=	ybreaks
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

	graphSUMM	=	function(FILT = TRUE)	{
		ggplot(data = DATA$results[FILT, ], aes(x = GPU, y = get(input$datatypeG))) +
		ggtitle(DATA$game, subtitle = paste0(input$datatypeG, " - Means, Medians, and Percentiles")) + #input$listGPU +
		geom_hline(yintercept = 1000/60, color = "red") +
		# geom_boxplot(outlier.alpha = 0) +
		stat_summary(fun.data = BoxPerc, geom = "boxplot", width = 0.6) +
		geom_bar(aes(fill = GPU), stat = "summary", fun = mean) + scale_fill_hue(drop = FALSE) +
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
		graphSUMM(GRAPH$FILT()) + FACET("graphMEANS", input$listFACETS)
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
		graphCOURSE(GRAPH$FILT(), zoom = FALSE) + FACET("graphCOURSE")
	})

	graphFREQ	=	function(FILT)	{
		LINES	=	list(
			geom_vline(data = GRAPH$STATS(), aes(xintercept = Mean), color = "darkgreen"),
			geom_vline(data = GRAPH$STATS(), aes(xintercept = Median), color = "darkcyan", linetype="dotdash")
		)
		if (!is.logical(FILT))	{
			hold	=	as.data.frame(t(
				statGRAPH(as.data.frame(DATA$results)[FILT, as.character(input$datatypeG)])
				))
			#	tibbles complicating things
			#	output is numeric, so must convert to data frame, and transpose it to stats in columns instead of rows
			LINES	=	list(
				geom_vline(data = hold, aes(xintercept = Mean), color = "darkgreen"),
				geom_vline(data = hold, aes(xintercept = Median), color = "darkcyan", linetype="dotdash")			
			)
		}
		
		ggplot(DATA$results[FILT, ], aes(get(x = input$datatypeG))) +
		ggtitle(DATA$game, subtitle=paste0(input$datatypeG, " - Frequency Plot")) +
		geom_vline(xintercept = 1000/60, color = "red") +
		geom_freqpoly(binwidth=0.03, size=0.25) +
		# FACET("graphFREQ") +
		LINES + 
		scaleX("graphFREQ", input$datatypeG) +
		coord_cartesian(xlim = c(0, GRAPH$FtimeLimitMS())) +
		scale_y_continuous(name="Count", expand=c(0.02, 0)) +
		theme(plot.title.position = "plot")
	}

	output$graphFREQfacet	=	renderPlot({
		req(DATA$results)
		graphFREQ(GRAPH$FILT()) + FACET("graphFREQ", input$listFACETS)
	})

	graphQQ	=	function(FILT, zoom = FALSE)	{
		PERCS	=	c(.001, .01, .5, .99, .999)
		PERCS	=	sort(unique(	c(PERCS, GRAPH$QUAN())	))
		
		# STATS	=	sepCOL(aggregate(DATA$results[, as.character(input$datatypeG)], DATA$GROUPS[names(DATA$GROUPS) %in% input$listFACETS], statGRAPH, quan=GRAPH$QUAN()))
		
		RECT	=	list(
			geom_rect(aes(ymax = get("0.1"),		xmax = qnorm(.001)), alpha=0.1, fill=c("blue"), color = "grey"),
			geom_rect(aes(ymax = get("1"),			xmax = qnorm(.010)), alpha=0.1, fill=c("blue"), color = "grey"),
			geom_rect(aes(ymax = get("Median"),		xmax = qnorm(.500)), alpha=0.1, fill=c("blue"), color = "grey"),
			geom_rect(aes(ymax = get("99"),			xmax = qnorm(.990)), alpha=0.1, fill=c("red"), color = "grey"),
			geom_rect(aes(ymax = get("99.9"),		xmax = qnorm(.999)), alpha=0.1, fill=c("red"), color = "grey")
		)
		if (zoom)	RECT	=	NULL

		ggplot(data = GRAPH$STATS(), aes(ymin = -Inf, xmin = -Inf)) +
		ggtitle(DATA$game, subtitle = paste0(input$datatypeG, " - QQ Distribution")) +
		geom_hline(yintercept = 1000/60, color	=	"red") +
			RECT + 
		stat_qq_line(data = DATA$results[FILT, ], aes(sample=get(input$datatypeG)), line.p = GRAPH$QUAN(), color = "green", size = 1.1, linetype = "dotted") +
		stat_qq(data = DATA$results[FILT, ], aes(sample=get(input$datatypeG))) +
		stat_qq_line(data = DATA$results[FILT, ], aes(sample=get(input$datatypeG)), line.p = GRAPH$QUAN(), color = "green", alpha = 0.5, size = 1.1, linetype = "dotted") +
		geom_label(data = GRAPH$STATS(), aes(x = Inf, y = -Inf, label = paste0("Slope: ", Slope)), parse = TRUE, hjust="right", vjust="bottom", fill = "darkgrey", color = "green") +
		scaleY("graphQQ", input$datatypeG) +
		coord_cartesian(ylim = c(0, GRAPH$FtimeLimitMS())) +
		scale_x_continuous(name = "Percentile", breaks = qnorm(unique(PERCS)), labels = labelBreakQQ, minor_breaks = NULL, expand = c(0.02, 0)) +
		theme(plot.title.position = "plot")
	}

	output$graphQQfacet	=	renderPlot({
		req(DATA$results)
		test	<<-	GRAPH$STATS()
		graphQQ(GRAPH$FILT()) + FACET("graphQQ", input$listFACETS)
	})

	graphDIFF	=	function(FILT)	{
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
				breaks	=	c(ybreaks, limY),
				limits	=	limY,
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

		ggplot(data = DATA$results[FILT, ], aes(x = get(input$datatypeG), y = diff.CONS(get(input$datatypeG))) ) +
		ggtitle(DATA$game, subtitle=paste0(input$datatypeG, " Consecutive Differences")) +
		geom_point(alpha = 0.1) +
		stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), show.legend = FALSE) + scale_fill_viridis_c() +
		# stat_density_2d(geom = "polygon", aes(fill = stat(nlevel), alpha = stat(nlevel)), show.legend = FALSE) + 	scale_fill_viridis_c() +
		# FACET("graphDIFF") +
		scale_X +
		scale_Y +
		theme(plot.title.position = "plot")
	}

	output$graphDIFFfacet	=	renderPlot({
		req(DATA$results)
		# graphDIFFfacet()
		diffLimRefresh()
		graphDIFF(GRAPH$FILT()) + FACET("graphDIFF", input$listFACETS)
	})

source("app_graphs_zoom.r", local = TRUE)