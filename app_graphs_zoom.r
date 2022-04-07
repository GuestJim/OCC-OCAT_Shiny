	observeEvent(list(input$dataInput, DATA$LOAD),	{
		req(DATA$results)
		updateSelectInput(inputId	=	"brushSUMMgpu",	choices	=	levels(DATA$results$GPU)		)
		updateSelectInput(inputId	=	"brushSUMMapi",	choices	=	levels(DATA$results$API)		)
		updateSelectInput(inputId	=	"brushSUMMqua",	choices	=	levels(DATA$results$Quality)	)
		updateSelectInput(inputId	=	"brushSUMMloc",	choices	=	levels(DATA$results$Location)	)
		
		updateSelectInput(inputId	=	"brushCOURSEgpu",	choices	=	levels(DATA$results$GPU)		)
		updateSelectInput(inputId	=	"brushCOURSEapi",	choices	=	levels(DATA$results$API)		)
		updateSelectInput(inputId	=	"brushCOURSEqua",	choices	=	levels(DATA$results$Quality)	)
		updateSelectInput(inputId	=	"brushCOURSEloc",	choices	=	levels(DATA$results$Location)	)
		
		updateSelectInput(inputId	=	"brushFREQgpu",		choices	=	levels(DATA$results$GPU)		)
		updateSelectInput(inputId	=	"brushFREQapi",		choices	=	levels(DATA$results$API)		)
		updateSelectInput(inputId	=	"brushFREQqua",		choices	=	levels(DATA$results$Quality)	)
		updateSelectInput(inputId	=	"brushFREQloc",		choices	=	levels(DATA$results$Location)	)

		updateSelectInput(inputId	=	"brushQQgpu",		choices	=	levels(DATA$results$GPU)		)
		updateSelectInput(inputId	=	"brushQQapi",		choices	=	levels(DATA$results$API)		)
		updateSelectInput(inputId	=	"brushQQqua",		choices	=	levels(DATA$results$Quality)	)
		updateSelectInput(inputId	=	"brushQQloc",		choices	=	levels(DATA$results$Location)	)
		
		updateSelectInput(inputId	=	"brushDIFFgpu",		choices	=	levels(DATA$results$GPU)		)
		updateSelectInput(inputId	=	"brushDIFFapi",		choices	=	levels(DATA$results$API)		)
		updateSelectInput(inputId	=	"brushDIFFqua",		choices	=	levels(DATA$results$Quality)	)
		updateSelectInput(inputId	=	"brushDIFFloc",		choices	=	levels(DATA$results$Location)	)
	})

#	Summary
	brushSUMMzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL)
	observeEvent(input$brushSUMMdbl,	{
		req(DATA$results)
		brush 		<- input$brushSUMMdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		# brushSUMMzoom$x				<-	NULL
		# brushSUMMzoom$GPU			<-	NULL
		brushSUMMzoom$API			<-	NULL
		brushSUMMzoom$Quality		<-	NULL
		brushSUMMzoom$Location		<-	NULL
		brushSUMMzoom$FILTER		<-	TRUE
		if (!is.null(brush)) {
			# brushSUMMzoom$x			<-	c(brush$xmin, brush$xmax)

			# brushSUMMzoom$GPU			<-	brushFILT$GPU
			brushSUMMzoom$Quality		<-	brushFILT$Quality
			brushSUMMzoom$Location		<-	brushFILT$Location

			brushSUMMzoom$FILTER		<-	which(
				# DATA$results$GPU		==	brushSUMMzoom$GPU		&
				DATA$results$Quality	==	brushSUMMzoom$Quality	&
				DATA$results$Location	==	brushSUMMzoom$Location
				)
			if (!is.null(brushFILT$API))	{
				brushSUMMzoom$API		<-	brushFILT$API
				brushSUMMzoom$FILTER	<-	intersect(brushSUMMzoom$FILTER, which(DATA$results$API	==	brushSUMMzoom$API))
			}	else	{
				if (exists("APIs", DATA))	brushSUMMzoom$API	<-	DATA$APIs
			}
		}
		
		# updateSelectInput(inputId	=	"brushSUMMgpu",	selected	=	brushSUMMzoom$GPU			)
		updateSelectInput(inputId	=	"brushSUMMapi",	selected	=	brushSUMMzoom$API			)
		updateSelectInput(inputId	=	"brushSUMMqua",	selected	=	brushSUMMzoom$Quality		)
		updateSelectInput(inputId	=	"brushSUMMloc",	selected	=	brushSUMMzoom$Location	)
	})
	observeEvent(list(input$brushSUMMupdate),	{
		req(DATA$results)
		# brushSUMMzoom$x			=	c(input$brushSUMMstart, input$brushSUMMstart + input$brushSUMMlength)
		# brushSUMMzoom$GPU			<-	input$brushSUMMgpu
		brushSUMMzoom$API			<-	input$brushSUMMapi
		brushSUMMzoom$Quality		<-	input$brushSUMMqua
		brushSUMMzoom$Location		<-	input$brushSUMMloc
		brushSUMMzoom$FILTER		=	which(
				# DATA$results$GPU		==	brushSUMMzoom$GPU		&
				# DATA$results$API		==	brushSUMMzoom$API		&
				DATA$results$Quality	==	brushSUMMzoom$Quality	&
				DATA$results$Location	==	brushSUMMzoom$Location
				)
		if (!is.null(brushSUMMzoom$API))	brushSUMMzoom$FILTER	<-	intersect(brushSUMMzoom$FILTER,
			which(DATA$results$API	==	brushSUMMzoom$API)	)
	})

	observeEvent(list(input$brushSUMMdbl, input$brushSUMMupdate),	{
		output$brushSUMMfacet	=	renderPlot({
			req(DATA$results)
			graphSUMM(brushSUMMzoom$FILTER)	+ labs(caption = paste0(
				paste(c(brushSUMMzoom$API, brushSUMMzoom$Quality, brushSUMMzoom$Location), collapse = ", ")
			)	)
		})
	},	ignoreInit	=	TRUE)

#	Course
	brushCOURSEzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL)
	observeEvent(input$brushCOURSE, {
		req(DATA$results)
		brush 		<- input$brushCOURSE
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		if (!is.null(brush)) {
			brushCOURSEzoom$x			<-	c(brush$xmin, brush$xmax)

			brushCOURSEzoom$GPU			<-	brushFILT$GPU
			brushCOURSEzoom$Quality		<-	brushFILT$Quality
			brushCOURSEzoom$Location	<-	brushFILT$Location

			brushCOURSEzoom$FILTER		<-	which(
				DATA$results$GPU		==	brushCOURSEzoom$GPU		&
				DATA$results$Quality	==	brushCOURSEzoom$Quality	&
				DATA$results$Location	==	brushCOURSEzoom$Location
				)
			if (!is.null(brushFILT$API))	{
				brushCOURSEzoom$API		<-	brushFILT$API
				brushCOURSEzoom$FILTER	<-	intersect(brushCOURSEzoom$FILTER, which(DATA$results$API	==	brushCOURSEzoom$API))
			}	else	{
				if (exists("APIs", DATA))	brushCOURSEzoom$API	<-	DATA$APIs
			}

		} else {
			brushCOURSEzoom$x			<-	NULL
			brushCOURSEzoom$GPU			<-	NULL
			brushCOURSEzoom$API			<-	NULL
			brushCOURSEzoom$Quality		<-	NULL
			brushCOURSEzoom$Location	<-	NULL
			brushCOURSEzoom$FILTER		<-	TRUE
		}

		updateNumericInput(inputId	=	"brushCOURSEstart",
			value	=	round(	brushCOURSEzoom$x[1], 			4)	)
		updateNumericInput(inputId	=	"brushCOURSElength",
			value	=	round(	abs(diff(brushCOURSEzoom$x)),	4)	)
		updateSelectInput(inputId	=	"brushCOURSEgpu",	selected	=	brushCOURSEzoom$GPU			)
		updateSelectInput(inputId	=	"brushCOURSEapi",	selected	=	brushCOURSEzoom$API			)
		updateSelectInput(inputId	=	"brushCOURSEqua",	selected	=	brushCOURSEzoom$Quality		)
		updateSelectInput(inputId	=	"brushCOURSEloc",	selected	=	brushCOURSEzoom$Location	)
	})
	observeEvent(list(input$brushCOURSEupdate),	{
		req(DATA$results)
		brushCOURSEzoom$x			=	c(input$brushCOURSEstart, input$brushCOURSEstart + input$brushCOURSElength)
		brushCOURSEzoom$GPU			<-	input$brushCOURSEgpu
		brushCOURSEzoom$API			<-	input$brushCOURSEapi
		brushCOURSEzoom$Quality		<-	input$brushCOURSEqua
		brushCOURSEzoom$Location	=	input$brushCOURSEloc
		brushCOURSEzoom$FILTER		=	which(
				DATA$results$GPU		==	brushCOURSEzoom$GPU		&
				# DATA$results$API		==	brushCOURSEzoom$API		&
				DATA$results$Quality	==	brushCOURSEzoom$Quality	&
				DATA$results$Location	==	brushCOURSEzoom$Location
				)
		if (!is.null(brushCOURSEzoom$API))	brushCOURSEzoom$FILTER	<-	intersect(brushCOURSEzoom$FILTER,
			which(DATA$results$API	==	brushCOURSEzoom$API)	)
	})

	observeEvent(input$brushCOURSEdbl,	{
		brush 		<-	input$brushCOURSEdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		brushCOURSEzoom$GPU			<-	brushFILT$GPU
		brushCOURSEzoom$Quality		<-	brushFILT$Quality
		brushCOURSEzoom$Location	<-	brushFILT$Location

		brushCOURSEzoom$FILTER		<-	which(
			DATA$results$GPU		==	brushCOURSEzoom$GPU		&
			DATA$results$Quality	==	brushCOURSEzoom$Quality	&
			DATA$results$Location	==	brushCOURSEzoom$Location
			)
		if (!is.null(brushFILT$API))	{
			brushCOURSEzoom$API		<-	brushFILT$API
			brushCOURSEzoom$FILTER	<-	intersect(brushCOURSEzoom$FILTER, which(DATA$results$API	==	brushCOURSEzoom$API))
		}

		brushCOURSEzoom$x			<-	c(0, max(DATA$results[brushCOURSEzoom$FILTER, "TimeInSeconds"]))
		
		updateNumericInput(inputId	=	"brushCOURSEstart",
			value	=	brushCOURSEzoom$x[1])
		updateNumericInput(inputId	=	"brushCOURSElength",
			value	=	brushCOURSEzoom$x[2] - brushCOURSEzoom$x[1])
		updateSelectInput(inputId	=	"brushCOURSEgpu",	selected	=	brushCOURSEzoom$GPU			)
		updateSelectInput(inputId	=	"brushCOURSEapi",	selected	=	brushCOURSEzoom$API			)
		updateSelectInput(inputId	=	"brushCOURSEqua",	selected	=	brushCOURSEzoom$Quality		)
		updateSelectInput(inputId	=	"brushCOURSEloc",	selected	=	brushCOURSEzoom$Location	)
	})
	
	output$brushCOURSEtext	=	renderText({	"Click and Drag to Zoom Below"	})
	
	BRUSH$courseFILT	=	NULL
	observeEvent(list(DATA$results, input$brushCOURSE, input$brushCOURSEdbl, input$brushCOURSEupdate, input$datatypeG),	{
		hold	=	as.data.frame(DATA$results[brushCOURSEzoom$FILTER, ])
		hold	<-	hold[
			hold$TimeInSeconds	>=	brushCOURSEzoom$x[1] &
			hold$TimeInSeconds	<=	brushCOURSEzoom$x[2]
			, as.character(input$datatypeG)]
		BRUSH$courseFILT	<- hold
		rm(hold)
	},	ignoreInit	=	TRUE)

	observeEvent(list(input$brushCOURSE, input$brushCOURSEupdate),	{
		output$brushCOURSEfacet	=	renderPlot({
			req(DATA$results)

			graphCOURSE(brushCOURSEzoom$FILTER) + labs(caption =
				paste0("X: ", paste(round(brushCOURSEzoom$x, 4), collapse = " to "), " (s) : ",
				paste(c(brushCOURSEzoom$GPU, brushCOURSEzoom$API, brushCOURSEzoom$Quality, brushCOURSEzoom$Location), collapse = ", ")
				)	) +
			coord_cartesian(xlim = brushCOURSEzoom$x,	ylim = c(0, GRAPH$FtimeLimitMS()),	expand = FALSE)
		})
	},	ignoreInit	=	TRUE)

	observeEvent(list(input$brushCOURSE, input$brushCOURSEdbl, input$brushCOURSEupdate, input$manuPERC, input$datatypeG, input$roundTerm),	{
		req(BRUSH$courseFILT)
		outMEAN	=	as.data.frame(t(meanMS(BRUSH$courseFILT)))
		outPERC	=	as.data.frame(t(percMS(BRUSH$courseFILT, to.NUM(c(input$manuPERC)))))
		
		out	=	cbind(Unit = "ms", outMEAN, outPERC)

		out$Unit	=	"ms"
		colDATA	=	sapply(out, is.numeric)
		
		out	=	rbind(c(Unit = "FPS", 1000/out[, colDATA]), out)
		
		output$brushCOURSEsumm	=	renderTable({
			filtCOL	=	names(out) %in% c("Unit", input$tabCOLs)
			filtROW	=	TRUE

			if (!("ms" %in% input$tabUNIT))		filtROW	=	out$Unit != "ms"
			if (!("FPS" %in% input$tabUNIT))	filtROW	=	out$Unit != "FPS"

			out[filtROW, filtCOL]
		},	digits	=	input$roundTerm)
	},	ignoreInit	=	TRUE)
	
	observeEvent(list(input$brushCOURSE, input$brushCOURSEdbl, input$brushCOURSEupdate, input$manuECDF, input$datatypeG, input$roundTerm),	{
		req(BRUSH$courseFILT)
		outECDF	=	as.data.frame(t(ecdfFPS(BRUSH$courseFILT, to.NUM(c(input$manuECDF)))))
		
		outECDF$Unit	=	"%"
		colDATA	=	sapply(outECDF, is.numeric)
		
		outECDF	=	outECDF[, c(which(!colDATA), which(colDATA))]
		
		output$brushCOURSEecdf	=	renderTable({
			filtCOL	=	names(outECDF) %in% c("Unit", input$tabCOLs)

			outECDF[, filtCOL]
		},	digits	=	input$roundTerm)
	},	ignoreInit	=	TRUE)


#	Frequency
	brushFREQzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL)
	observeEvent(input$brushFREQdbl,	{
		req(DATA$results)
		brush 		<- input$brushFREQdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		# brushFREQzoom$x				<-	NULL
		brushFREQzoom$GPU			<-	NULL
		brushFREQzoom$API			<-	NULL
		brushFREQzoom$Quality		<-	NULL
		brushFREQzoom$Location		<-	NULL
		brushFREQzoom$FILTER		<-	TRUE
		if (!is.null(brush)) {
			# brushFREQzoom$x			<-	c(brush$xmin, brush$xmax)

			brushFREQzoom$GPU			<-	brushFILT$GPU
			brushFREQzoom$Quality		<-	brushFILT$Quality
			brushFREQzoom$Location		<-	brushFILT$Location

			brushFREQzoom$FILTER		<-	which(
				DATA$results$GPU		==	brushFREQzoom$GPU		&
				DATA$results$Quality	==	brushFREQzoom$Quality	&
				DATA$results$Location	==	brushFREQzoom$Location
				)
			if (!is.null(brushFILT$API))	{
				brushFREQzoom$API		<-	brushFILT$API
				brushFREQzoom$FILTER	<-	intersect(brushFREQzoom$FILTER, which(DATA$results$API	==	brushFREQzoom$API))
			}	else	{
				if (exists("APIs", DATA))	brushFREQzoom$API	<-	DATA$APIs
			}
		}
		
		updateSelectInput(inputId	=	"brushFREQgpu",	selected	=	brushFREQzoom$GPU			)
		updateSelectInput(inputId	=	"brushFREQapi",	selected	=	brushFREQzoom$API			)
		updateSelectInput(inputId	=	"brushFREQqua",	selected	=	brushFREQzoom$Quality		)
		updateSelectInput(inputId	=	"brushFREQloc",	selected	=	brushFREQzoom$Location	)
	})
	observeEvent(list(input$brushFREQupdate),	{
		req(DATA$results)
		# brushFREQzoom$x			=	c(input$brushFREQstart, input$brushFREQstart + input$brushFREQlength)
		brushFREQzoom$GPU			<-	input$brushFREQgpu
		brushFREQzoom$API			<-	input$brushFREQapi
		brushFREQzoom$Quality		<-	input$brushFREQqua
		brushFREQzoom$Location		<-	input$brushFREQloc
		brushFREQzoom$FILTER		=	which(
				DATA$results$GPU		==	brushFREQzoom$GPU		&
				# DATA$results$API		==	brushFREQzoom$API		&
				DATA$results$Quality	==	brushFREQzoom$Quality	&
				DATA$results$Location	==	brushFREQzoom$Location
				)
		if (!is.null(brushFREQzoom$API))	brushFREQzoom$FILTER	<-	intersect(brushFREQzoom$FILTER,
			which(DATA$results$API	==	brushFREQzoom$API)	)
	})

	observeEvent(list(input$brushFREQdbl, input$brushFREQupdate),	{
		output$brushFREQfacet	=	renderPlot({
			req(DATA$results)
			graphFREQ(brushFREQzoom$FILTER)	+ labs(caption = paste0(
				paste(c(brushFREQzoom$GPU, brushFREQzoom$API, brushFREQzoom$Quality, brushFREQzoom$Location), collapse = ", ")
			)	)
		})
	},	ignoreInit	=	TRUE)

#	QQ
	brushQQzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL)
	observeEvent(input$brushQQ, {
		req(DATA$results)
		brush 		<<- input$brushQQ
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		brushQQzoom$x			<-	NULL
		brushQQzoom$y			<-	NULL
		brushQQzoom$GPU			<-	NULL
		brushQQzoom$API			<-	NULL
		brushQQzoom$Quality		<-	NULL
		brushQQzoom$Location	<-	NULL
		brushQQzoom$FILTER		<-	TRUE
		
		filtGPU	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)
		if (!is.null(brush)) {
			brushQQzoom$x			<-	c(brush$xmin, brush$xmax)
			brushQQzoom$y			<-	c(brush$ymin, brush$ymax)

			if (!is.null(brushQQzoom$GPU))		filtGPU		<-	which(DATA$results$GPU		==	brushQQzoom$GPU)
			if (!is.null(brushQQzoom$Quality))	filtQUA		<-	which(DATA$results$Quality	==	brushQQzoom$Quality)
			if (!is.null(brushQQzoom$Location))	filtLOC		<-	which(DATA$results$Location	==	brushQQzoom$Location)

			brushQQzoom$FILTER		<-	intersect(intersect(filtGPU, filtQUA), filtLOC)
		
			if (!is.null(brushFILT$API))	{
				brushQQzoom$API		<-	brushFILT$API
				brushQQzoom$FILTER	<-	intersect(brushQQzoom$FILTER, which(DATA$results$API	==	brushQQzoom$API))
			}	else	{
				if (exists("APIs", DATA))	brushQQzoom$API	<-	DATA$APIs
			}
		}

		updateNumericInput(inputId	=	"brushQQlowerX",
			value	=	round(	pnorm(brushQQzoom$x[1])*100, 	4)	)
		updateNumericInput(inputId	=	"brushQQupperX",
			value	=	round(	pnorm(brushQQzoom$x[2])*100,	4)	)

		updateNumericInput(inputId	=	"brushQQlowerY",
			value	=	round(	brushQQzoom$y[1],	4)	)
		updateNumericInput(inputId	=	"brushQQupperY",
			value	=	round(	brushQQzoom$y[2],	4)	)
		updateSelectInput(inputId	=	"brushQQgpu",	selected	=	brushQQzoom$GPU			)
		updateSelectInput(inputId	=	"brushQQapi",	selected	=	brushQQzoom$API			)
		updateSelectInput(inputId	=	"brushQQqua",	selected	=	brushQQzoom$Quality		)
		updateSelectInput(inputId	=	"brushQQloc",	selected	=	brushQQzoom$Location	)
	})
	observeEvent(input$listFACETS,	{
		if (!("GPU"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushQQgpu",	selected	=	""	)
		if (!("API"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushQQapi",	selected	=	""	)
		if (!("Quality"		%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushQQqua",	selected	=	""	)
		if (!("Location"	%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushQQloc",	selected	=	""	)
	})
	observeEvent(list(input$brushQQupdate),	{
		req(DATA$results)
		brushQQzoom$x			=	qnorm(c(input$brushQQlowerX, input$brushQQupperX)/100)
		if	(input$brushQQlowerX == 0)		brushQQzoom$x[1]	=	qnorm(0.000000001)
		if	(input$brushQQupperX == 100)	brushQQzoom$x[2]	=	qnorm(0.999999999)
		brushQQzoom$y			=	c(input$brushQQlowerY, input$brushQQupperY)
		brushQQzoom$GPU			<-	input$brushQQgpu
		brushQQzoom$API			<-	input$brushQQapi
		brushQQzoom$Quality		<-	input$brushQQqua
		brushQQzoom$Location	=	input$brushQQloc
		brushQQzoom$FILTER		=	which(
				DATA$results$GPU		==	brushQQzoom$GPU		&
				# DATA$results$API		==	brushQQzoom$API		&
				DATA$results$Quality	==	brushQQzoom$Quality	&
				DATA$results$Location	==	brushQQzoom$Location
		)
		if (!is.null(brushQQzoom$API))	brushQQzoom$FILTER	<-	intersect(brushQQzoom$FILTER,
			which(DATA$results$API	==	brushQQzoom$API)	)
	})
	
	observeEvent(input$brushQQdbl,	{
		brush 		<-	input$brushQQdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		brushQQzoom$x	=	c(qnorm(0.000000001), qnorm(0.999999999))

		brushQQzoom$GPU			<-	brushFILT$GPU
		brushQQzoom$Quality		<-	brushFILT$Quality
		brushQQzoom$Location	<-	brushFILT$Location

		brushQQzoom$FILTER		<-	which(
			DATA$results$GPU		==	brushQQzoom$GPU		&
			DATA$results$Quality	==	brushQQzoom$Quality	&
			DATA$results$Location	==	brushQQzoom$Location
			)
		if (!is.null(brushFILT$API))	{
			brushQQzoom$API		<-	brushFILT$API
			brushQQzoom$FILTER	<-	intersect(brushQQzoom$FILTER, which(DATA$results$API	==	brushQQzoom$API))
		}
		
		updateNumericInput(inputId	=	"brushQQlowerX",	value	=	0)
		updateNumericInput(inputId	=	"brushQQupperX",	value	=	100)

		updateNumericInput(inputId	=	"brushQQlowerY",	value	=	0)
		updateNumericInput(inputId	=	"brushQQupperY",	value	=	input$FtimeLimitMS	)
		
		updateSelectInput(inputId	=	"brushQQgpu",	selected	=	brushQQzoom$GPU			)
		updateSelectInput(inputId	=	"brushQQapi",	selected	=	brushQQzoom$API			)
		updateSelectInput(inputId	=	"brushQQqua",	selected	=	brushQQzoom$Quality		)
		updateSelectInput(inputId	=	"brushQQloc",	selected	=	brushQQzoom$Location	)
	})
	
	BRUSH$qqFILT	=	NULL
	observeEvent(list(DATA$results, input$brushQQ, input$brushQQdbl, input$brushQQupdate, input$datatypeG),	{
		hold	=	as.data.frame(DATA$results[brushQQzoom$FILTER, ])
		hold	<-	hold[
			hold$TimeInSeconds	>=	brushQQzoom$x[1] &
			hold$TimeInSeconds	<=	brushQQzoom$x[2]
			, as.character(input$datatypeG)]
		BRUSH$qqFILT	<- hold
		rm(hold)
	},	ignoreInit	=	TRUE)

	observeEvent(list(input$brushQQ, input$brushQQdbl, input$brushQQupdate),	{
		output$brushQQfacet	=	renderPlot({
			req(DATA$results, BRUSH$qqFILT)

			graphQQ(brushQQzoom$FILTER) + labs(caption = paste0(
				"Y: ", paste(round(brushQQzoom$y, 4), collapse = " to "), " (ms) : ",
				"X: ", paste(round(pnorm(brushQQzoom$x)*100, 4), collapse = " to "), " (%) : ",
				paste(c(brushQQzoom$GPU, brushQQzoom$API, brushQQzoom$Quality, brushQQzoom$Location), collapse = ", ")
			)	) +
			coord_cartesian(xlim = brushQQzoom$x,	ylim = brushQQzoom$y,	expand = FALSE)
		})
	},	ignoreInit	=	TRUE)

	qqTABLE	=	function(IN)	{
		dataCOL	=	!(names(IN) %in% nodataCOL)
		hold	=	as.data.frame(lapply(IN[, dataCOL], qqnorm, plot.it = FALSE))
		names(hold)	=	gsub(".x",	"QQ",	names(hold),	fixed = TRUE)
		names(hold)	=	gsub(".y",	"",		names(hold),	fixed = TRUE)
		out	=	merge(IN, hold, sort = FALSE)
		return(out)
	}

	qqMINMAX	=	function(IN, BRUSH = brushQQzoom, datatype = input$datatypeG, r	=	input$roundTerm)	{
		hold	=	qqTABLE(IN)
		dataCOL	=	grep(datatype, names(hold))
		if (is.na(BRUSH$x[1]))	BRUSH$x[1]	=	-Inf
		if (is.na(BRUSH$x[2]))	BRUSH$x[2]	=	Inf

		holdLIM	=	hold[
			hold[, dataCOL[2]] >= BRUSH$x[1]	&
			hold[, dataCOL[2]] <= BRUSH$x[2]	&
			hold[, dataCOL[1]] >= BRUSH$y[1]	&
			hold[, dataCOL[1]] <= BRUSH$y[2],
		dataCOL]

		out	=	rbind(
			"Min"	=	holdLIM[which.min(holdLIM[, 2]), ],
			"Max"	=	holdLIM[which.max(holdLIM[, 2]), ])
		out[, 2]	=	pnorm(out[, 2]) * 100
		out	=	rbind(out, "Difference" = sapply(out, diff))
		out[, 2]	=	paste0(round(out[, 2], r), "%")
		names(out)	=	c("Value (ms)", "Percentile")
		return(out)
	}

	observeEvent(list(input$brushQQ, input$brushQQdbl, input$brushQQupdate, input$roundTerm),	{
		req(brushQQzoom$FILTER, BRUSH$qqFILT)
		output$brushQQtable	=	renderTable({
			qqMINMAX(DATA$results[brushQQzoom$FILTER, ], brushQQzoom, input$datatypeG, input$roundTerm)
		},	digits	=	input$roundTerm,	rownames	=	TRUE,	align	=	"r")
	},	ignoreInit	=	TRUE)

#	Consecutive Difference
	brushDIFFzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL)
	observeEvent(input$brushDIFFdbl,	{
		req(DATA$results)
		brush 		<- input$brushDIFFdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		# brushDIFFzoom$x				<-	NULL
		brushDIFFzoom$GPU			<-	NULL
		brushDIFFzoom$API			<-	NULL
		brushDIFFzoom$Quality		<-	NULL
		brushDIFFzoom$Location		<-	NULL
		brushDIFFzoom$FILTER		<-	TRUE
		if (!is.null(brush)) {
			# brushDIFFzoom$x			<-	c(brush$xmin, brush$xmax)

			brushDIFFzoom$GPU			<-	brushFILT$GPU
			brushDIFFzoom$Quality		<-	brushFILT$Quality
			brushDIFFzoom$Location		<-	brushFILT$Location

			brushDIFFzoom$FILTER		<-	which(
				DATA$results$GPU		==	brushDIFFzoom$GPU		&
				DATA$results$Quality	==	brushDIFFzoom$Quality	&
				DATA$results$Location	==	brushDIFFzoom$Location
				)
			if (!is.null(brushFILT$API))	{
				brushDIFFzoom$API		<-	brushFILT$API
				brushDIFFzoom$FILTER	<-	intersect(brushDIFFzoom$FILTER, which(DATA$results$API	==	brushDIFFzoom$API))
			}	else	{
				if (exists("APIs", DATA))	brushDIFFzoom$API	<-	DATA$APIs
			}
		}
		
		updateSelectInput(inputId	=	"brushDIFFgpu",	selected	=	brushDIFFzoom$GPU			)
		updateSelectInput(inputId	=	"brushDIFFapi",	selected	=	brushDIFFzoom$API			)
		updateSelectInput(inputId	=	"brushDIFFqua",	selected	=	brushDIFFzoom$Quality		)
		updateSelectInput(inputId	=	"brushDIFFloc",	selected	=	brushDIFFzoom$Location	)
	})
	observeEvent(list(input$brushDIFFupdate),	{
		req(DATA$results)
		# brushDIFFzoom$x			=	c(input$brushDIFFstart, input$brushDIFFstart + input$brushDIFFlength)
		brushDIFFzoom$GPU			<-	input$brushDIFFgpu
		brushDIFFzoom$API			<-	input$brushDIFFapi
		brushDIFFzoom$Quality		<-	input$brushDIFFqua
		brushDIFFzoom$Location		<-	input$brushDIFFloc
		brushDIFFzoom$FILTER		=	which(
				DATA$results$GPU		==	brushDIFFzoom$GPU		&
				# DATA$results$API		==	brushDIFFzoom$API		&
				DATA$results$Quality	==	brushDIFFzoom$Quality	&
				DATA$results$Location	==	brushDIFFzoom$Location
				)
		if (!is.null(brushDIFFzoom$API))	brushDIFFzoom$FILTER	<-	intersect(brushDIFFzoom$FILTER,
			which(DATA$results$API	==	brushDIFFzoom$API)	)
	})

	observeEvent(list(input$brushDIFFdbl, input$brushDIFFupdate),	{
		output$brushDIFFfacet	=	renderPlot({
			req(DATA$results)
			graphDIFF(brushDIFFzoom$FILTER)	+ labs(caption = paste0(
				paste(c(brushDIFFzoom$GPU, brushDIFFzoom$API, brushDIFFzoom$Quality, brushDIFFzoom$Location), collapse = ", ")
			)	)
		})
	},	ignoreInit	=	TRUE)