	observeEvent(list(input$dataInput, DATA$LOAD, input$dataSelLOAD),	{
		req(DATA$results)
		updateSelectInput(inputId	=	"brushSUMMgpu",		choices	=	DATA$GPUs)
		updateSelectInput(inputId	=	"brushSUMMapi",		choices	=	DATA$APIs)
		updateSelectInput(inputId	=	"brushSUMMqua",		choices	=	DATA$QUAs)
		updateSelectInput(inputId	=	"brushSUMMloc",		choices	=	DATA$LOCs)

		updateSelectInput(inputId	=	"brushCOURSEgpu",	choices	=	DATA$GPUs)
		updateSelectInput(inputId	=	"brushCOURSEapi",	choices	=	DATA$APIs)
		updateSelectInput(inputId	=	"brushCOURSEqua",	choices	=	DATA$QUAs)
		updateSelectInput(inputId	=	"brushCOURSEloc",	choices	=	DATA$LOCs)

		updateSelectInput(inputId	=	"brushFREQgpu",		choices	=	DATA$GPUs)
		updateSelectInput(inputId	=	"brushFREQapi",		choices	=	DATA$APIs)
		updateSelectInput(inputId	=	"brushFREQqua",		choices	=	DATA$QUAs)
		updateSelectInput(inputId	=	"brushFREQloc",		choices	=	DATA$LOCs)

		updateSelectInput(inputId	=	"brushQQgpu",		choices	=	DATA$GPUs)
		updateSelectInput(inputId	=	"brushQQapi",		choices	=	DATA$APIs)
		updateSelectInput(inputId	=	"brushQQqua",		choices	=	DATA$QUAs)
		updateSelectInput(inputId	=	"brushQQloc",		choices	=	DATA$LOCs)

		updateSelectInput(inputId	=	"brushDIFFgpu",		choices	=	DATA$GPUs)
		updateSelectInput(inputId	=	"brushDIFFapi",		choices	=	DATA$APIs)
		updateSelectInput(inputId	=	"brushDIFFqua",		choices	=	DATA$QUAs)
		updateSelectInput(inputId	=	"brushDIFFloc",		choices	=	DATA$LOCs)

		updateSelectInput(inputId	=	"brushDIFFpercgpu",		choices	=	DATA$GPUs)
		updateSelectInput(inputId	=	"brushDIFFpercapi",		choices	=	DATA$APIs)
		updateSelectInput(inputId	=	"brushDIFFpercqua",		choices	=	DATA$QUAs)
		updateSelectInput(inputId	=	"brushDIFFpercloc",		choices	=	DATA$LOCs)
	})
	observeEvent(input$listFACETS,	{
		# if (!("GPU"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushSUMMgpu",	selected	=	""	)
		if (!("API"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushSUMMapi",	selected	=	""	)
		if (!("Quality"		%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushSUMMqua",	selected	=	""	)
		if (!("Location"	%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushSUMMloc",	selected	=	""	)

		# if (!("GPU"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushCOURSEgpu",	selected	=	""	)
		# if (!("API"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushCOURSEapi",	selected	=	""	)
		# if (!("Quality"		%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushCOURSEqua",	selected	=	""	)
		# if (!("Location"	%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushCOURSEloc",	selected	=	""	)
		#	Course graph will not use facet control because it is inappropriate to do so

		if (!("GPU"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushFREQgpu",	selected	=	""	)
		if (!("API"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushFREQapi",	selected	=	""	)
		if (!("Quality"		%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushFREQqua",	selected	=	""	)
		if (!("Location"	%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushFREQloc",	selected	=	""	)

		if (!("GPU"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushQQgpu",	selected	=	""	)
		if (!("API"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushQQapi",	selected	=	""	)
		if (!("Quality"		%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushQQqua",	selected	=	""	)
		if (!("Location"	%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushQQloc",	selected	=	""	)

		if (!("GPU"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushDIFFgpu",	selected	=	""	)
		if (!("API"			%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushDIFFapi",	selected	=	""	)
		if (!("Quality"		%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushDIFFqua",	selected	=	""	)
		if (!("Location"	%in%	input$listFACETS))	updateSelectInput(inputId	=	"brushDIFFloc",	selected	=	""	)
	})


#	Summary
	brushSUMMzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL,	CHANGE	=	FALSE)
	observeEvent(input$brushSUMMdbl,	{	req(DATA$results)
		brush 		<- input$brushSUMMdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		# brushSUMMzoom$GPU			<-	NULL
		brushSUMMzoom$API			<-	NULL
		brushSUMMzoom$Quality		<-	NULL
		brushSUMMzoom$Location		<-	NULL
		brushSUMMzoom$FILTER		<-	TRUE
		# filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		# if (exists("GPU",		brushFILT))	{
			# brushSUMMzoom$GPU		<-	brushFILT$GPU
			# filtGPU					<-	which(DATA$results$GPU		==	brushSUMMzoom$GPU)
		# }
		if (exists("API",		brushFILT))	{
			brushSUMMzoom$API		<-	brushFILT$API
			filtAPI					<-	which(DATA$results$API		==	brushSUMMzoom$API)
		}
		if (exists("Quality",	brushFILT))	{
			brushSUMMzoom$Quality	<-	brushFILT$Quality
			filtQUA					<-	which(DATA$results$Quality	==	brushSUMMzoom$Quality)
		}
		if (exists("Location",	brushFILT))	{
			brushSUMMzoom$Location	<-	brushFILT$Location
			filtLOC					<-	which(DATA$results$Location	==	brushSUMMzoom$Location)
		}

		brushSUMMzoom$FILTER	<-	Reduce(intersect, list(filtAPI, filtQUA, filtLOC))

		# updateSelectInput(inputId	=	"brushSUMMgpu",	selected	=	brushSUMMzoom$GPU			)
		updateSelectInput(inputId	=	"brushSUMMapi",	selected	=	brushSUMMzoom$API			)
		updateSelectInput(inputId	=	"brushSUMMqua",	selected	=	brushSUMMzoom$Quality		)
		updateSelectInput(inputId	=	"brushSUMMloc",	selected	=	brushSUMMzoom$Location	)
		brushSUMMzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(input$brushSUMMupdate,	{	req(DATA$results)
		# brushSUMMzoom$GPU			<-	input$brushSUMMgpu
		brushSUMMzoom$API			<-	input$brushSUMMapi
		brushSUMMzoom$Quality		<-	input$brushSUMMqua
		brushSUMMzoom$Location		<-	input$brushSUMMloc

		# filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		# if (!is.null(brushSUMMzoom$GPU))		filtGPU		<-	which(DATA$results$GPU		==	brushSUMMzoom$GPU)
		if (!is.null(brushSUMMzoom$API))		filtGPU		<-	which(DATA$results$API		==	brushSUMMzoom$API)
		if (!is.null(brushSUMMzoom$Quality))	filtQUA		<-	which(DATA$results$Quality	==	brushSUMMzoom$Quality)
		if (!is.null(brushSUMMzoom$Location))	filtLOC		<-	which(DATA$results$Location	==	brushSUMMzoom$Location)

		brushSUMMzoom$FILTER	<-	Reduce(intersect, list(filtAPI, filtQUA, filtLOC))


		brushSUMMzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(list(input$brushSUMMdbl, input$brushSUMMupdate),	{
		output$brushSUMMfacet	=	renderPlot({
			# req(DATA$results)
			req(DATA$results, brushSUMMzoom$CHANGE)
			# req(brushSUMMzoom$CHANGE)
			graphSUMM(brushSUMMzoom$FILTER)	+ labs(caption = paste0(
				paste(c(brushSUMMzoom$API, brushSUMMzoom$Quality, brushSUMMzoom$Location), collapse = ", ")
			)	)
		})
	},	ignoreInit	=	TRUE)


#	Course
	brushCOURSEzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL,	CHANGE	=	FALSE)
	observeEvent(input$brushCOURSE, {	req(DATA$results)
		brush 		<- input$brushCOURSE
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		brushCOURSEzoom$x			<-	NULL
		brushCOURSEzoom$GPU			<-	NULL
		brushCOURSEzoom$API			<-	NULL
		brushCOURSEzoom$Quality		<-	NULL
		brushCOURSEzoom$Location	<-	NULL
		brushCOURSEzoom$FILTER		<-	TRUE
		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		brushCOURSEzoom$x	<-	c(brush$xmin, brush$xmax)
		filtX	<-	which(DATA$results$TimeInSeconds >= brushCOURSEzoom$x[1] & DATA$results$TimeInSeconds < brushCOURSEzoom$x[2])
		if (exists("GPU",		brushFILT))	{
			brushCOURSEzoom$GPU			<-	brushFILT$GPU
			filtGPU						<-	which(DATA$results$GPU		==	brushCOURSEzoom$GPU)
		}
		if (exists("API",		brushFILT))	{
			brushCOURSEzoom$API			<-	brushFILT$API
			filtAPI						<-	which(DATA$results$API		==	brushCOURSEzoom$API)
		}
		if (exists("Quality",	brushFILT))	{
			brushCOURSEzoom$Quality		<-	brushFILT$Quality
			filtQUA						<-	which(DATA$results$Quality	==	brushCOURSEzoom$Quality)
		}
		if (exists("Location",	brushFILT))	{
			brushCOURSEzoom$Location	<-	brushFILT$Location
			filtLOC						<-	which(DATA$results$Location	==	brushCOURSEzoom$Location)
		}

		brushCOURSEzoom$FILTER	<-	Reduce(intersect, list(filtX, filtGPU, filtAPI, filtQUA, filtLOC))

		updateNumericInput(inputId	=	"brushCOURSEstart",
			value	=	round(	brushCOURSEzoom$x[1], 			4)	)
		updateNumericInput(inputId	=	"brushCOURSElength",
			value	=	round(	abs(diff(brushCOURSEzoom$x)),	4)	)
		updateSelectInput(inputId	=	"brushCOURSEgpu",	selected	=	brushCOURSEzoom$GPU			)
		updateSelectInput(inputId	=	"brushCOURSEapi",	selected	=	brushCOURSEzoom$API			)
		updateSelectInput(inputId	=	"brushCOURSEqua",	selected	=	brushCOURSEzoom$Quality		)
		updateSelectInput(inputId	=	"brushCOURSEloc",	selected	=	brushCOURSEzoom$Location	)

		brushCOURSEzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(input$brushCOURSEupdate,	{	req(DATA$results)
		brushCOURSEzoom$x			=	c(input$brushCOURSEstart, input$brushCOURSEstart + input$brushCOURSElength)
		brushCOURSEzoom$GPU			<-	input$brushCOURSEgpu
		brushCOURSEzoom$API			<-	input$brushCOURSEapi
		brushCOURSEzoom$Quality		<-	input$brushCOURSEqua
		brushCOURSEzoom$Location	<-	input$brushCOURSEloc

		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)
		filtX	<-	which(DATA$results$TimeInSeconds >= brushCOURSEzoom$x[1] & DATA$results$TimeInSeconds < brushCOURSEzoom$x[2])
		if (!is.null(brushCOURSEzoom$GPU))		filtGPU		<-	which(DATA$results$GPU		==	brushCOURSEzoom$GPU)
		if (!is.null(brushCOURSEzoom$API))		filtAPI		<-	which(DATA$results$API		==	brushCOURSEzoom$API)
		if (!is.null(brushCOURSEzoom$Quality))	filtQUA		<-	which(DATA$results$Quality	==	brushCOURSEzoom$Quality)
		if (!is.null(brushCOURSEzoom$Location))	filtLOC		<-	which(DATA$results$Location	==	brushCOURSEzoom$Location)

		brushCOURSEzoom$FILTER		<-	Reduce(intersect, list(filtX, filtGPU, filtAPI, filtQUA, filtLOC))

		brushCOURSEzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(input$brushCOURSEdbl,	{	req(DATA$results)
		brush 		<-	input$brushCOURSEdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		brushCOURSEzoom$GPU			<-	brushFILT$GPU
		brushCOURSEzoom$API			<-	brushFILT$API
		brushCOURSEzoom$Quality		<-	brushFILT$Quality
		brushCOURSEzoom$Location	<-	brushFILT$Location

		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)
		if (!is.null(brushCOURSEzoom$GPU))		filtGPU		<-	which(DATA$results$GPU		==	brushCOURSEzoom$GPU)
		if (!is.null(brushCOURSEzoom$API))		filtAPI		<-	which(DATA$results$API		==	brushCOURSEzoom$API)
		if (!is.null(brushCOURSEzoom$Quality))	filtQUA		<-	which(DATA$results$Quality	==	brushCOURSEzoom$Quality)
		if (!is.null(brushCOURSEzoom$Location))	filtLOC		<-	which(DATA$results$Location	==	brushCOURSEzoom$Location)

		# brushCOURSEzoom$FILTER		<-	intersect(intersect(intersect(filtGPU, filtAPI), filtQUA), filtLOC)
		brushCOURSEzoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		brushCOURSEzoom$x			<-	c(0, max(DATA$results[brushCOURSEzoom$FILTER, "TimeInSeconds"]))

		updateNumericInput(inputId	=	"brushCOURSEstart",
			value	=	brushCOURSEzoom$x[1])
		updateNumericInput(inputId	=	"brushCOURSElength",
			value	=	brushCOURSEzoom$x[2] - brushCOURSEzoom$x[1])
		updateSelectInput(inputId	=	"brushCOURSEgpu",	selected	=	brushCOURSEzoom$GPU			)
		updateSelectInput(inputId	=	"brushCOURSEapi",	selected	=	brushCOURSEzoom$API			)
		updateSelectInput(inputId	=	"brushCOURSEqua",	selected	=	brushCOURSEzoom$Quality		)
		updateSelectInput(inputId	=	"brushCOURSEloc",	selected	=	brushCOURSEzoom$Location	)

		brushCOURSEzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	output$brushCOURSEtext	=	renderText({	"Click and Drag to Zoom Below"	})

	BRUSH$courseFILT	<-	eventReactive(list(DATA$results, input$brushCOURSE, input$brushCOURSEdbl, input$brushCOURSEupdate, input$datatypeG),	{
		hold	=	as.data.frame(DATA$results[brushCOURSEzoom$FILTER, ])
		hold[
			hold$TimeInSeconds	>=	brushCOURSEzoom$x[1] &
			hold$TimeInSeconds	<=	brushCOURSEzoom$x[2]
			, as.character(input$datatypeG)]
	})

	observeEvent(list(input$brushCOURSE, input$brushCOURSEdbl, input$brushCOURSEupdate),	{
		req(DATA$results, brushCOURSEzoom$CHANGE)
		#	Graph
		output$brushCOURSEfacet	=	renderPlot({

			graphCOURSE(brushCOURSEzoom$FILTER, zoom = TRUE) + labs(caption =
				paste0("X: ", paste(round(brushCOURSEzoom$x, 4), collapse = " to "), " (s) : ",
				paste(c(brushCOURSEzoom$GPU, brushCOURSEzoom$API, brushCOURSEzoom$Quality, brushCOURSEzoom$Location), collapse = ", ")
				)	) +
			coord_cartesian(xlim = brushCOURSEzoom$x,	ylim = c(0, GRAPH$FtimeLimitMS()),	expand = FALSE)
		})
		
		#	Tables
		observeEvent(list(input$datatypeG, input$roundTerm),	{
			req(BRUSH$courseFILT())
			observeEvent(input$manuPERC,	{
				out	=	cbind(Unit = "ms", as.data.frame(t(summMS(BRUSH$courseFILT(), to.NUM(c(input$manuPERC))))))

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
			})
			observeEvent(input$manuECDF,	{
				outECDF	=	as.data.frame(t(ecdfFPS(BRUSH$courseFILT(), to.NUM(c(input$manuECDF)))))

				outECDF$Unit	=	"%"
				colDATA	=	sapply(outECDF, is.numeric)

				outECDF	=	outECDF[, c(which(!colDATA), which(colDATA))]

				output$brushCOURSEecdf	=	renderTable({
					filtCOL	=	names(outECDF) %in% c("Unit", input$tabCOLs)

					outECDF[, filtCOL]
				},	digits	=	input$roundTerm)
			})
		})
	},	ignoreInit	=	TRUE)


#	Frequency
	brushFREQzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL,	CHANGE	=	FALSE)
	observeEvent(input$brushFREQdbl,	{	req(DATA$results)
		brush 		<- input$brushFREQdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		brushFREQzoom$GPU			<-	NULL
		brushFREQzoom$API			<-	NULL
		brushFREQzoom$Quality		<-	NULL
		brushFREQzoom$Location		<-	NULL
		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		if (exists("GPU",		brushFILT))	{
			brushFREQzoom$GPU		<-	brushFILT$GPU
			filtGPU					<-	which(DATA$results$GPU		==	brushFREQzoom$GPU)
		}
		if (exists("API",		brushFILT))	{
			brushFREQzoom$API		<-	brushFILT$API
			filtAPI					<-	which(DATA$results$API		==	brushFREQzoom$API)
		}
		if (exists("Quality",	brushFILT))	{
			brushFREQzoom$Quality	<-	brushFILT$Quality
			filtQUA					<-	which(DATA$results$Quality	==	brushFREQzoom$Quality)
		}
		if (exists("Location",	brushFILT))	{
			brushFREQzoom$Location	<-	brushFILT$Location
			filtLOC					<-	which(DATA$results$Location	==	brushFREQzoom$Location)
		}

		brushFREQzoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		updateSelectInput(inputId	=	"brushFREQgpu",	selected	=	brushFREQzoom$GPU			)
		updateSelectInput(inputId	=	"brushFREQapi",	selected	=	brushFREQzoom$API			)
		updateSelectInput(inputId	=	"brushFREQqua",	selected	=	brushFREQzoom$Quality		)
		updateSelectInput(inputId	=	"brushFREQloc",	selected	=	brushFREQzoom$Location		)

		brushFREQzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)
	observeEvent(input$brushFREQupdate,	{	req(DATA$results)
		# brushFREQzoom$x			=	c(input$brushFREQstart, input$brushFREQstart + input$brushFREQlength)
		brushFREQzoom$GPU			<-	input$brushFREQgpu
		brushFREQzoom$API			<-	input$brushFREQapi
		brushFREQzoom$Quality		<-	input$brushFREQqua
		brushFREQzoom$Location		<-	input$brushFREQloc

		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		if (!is.null(brushFREQzoom$GPU))		filtGPU		<-	which(DATA$results$GPU		==	brushFREQzoom$GPU)
		if (!is.null(brushFREQzoom$API))		filtAPI		<-	which(DATA$results$API		==	brushFREQzoom$API)
		if (!is.null(brushFREQzoom$Quality))	filtQUA		<-	which(DATA$results$Quality	==	brushFREQzoom$Quality)
		if (!is.null(brushFREQzoom$Location))	filtLOC		<-	which(DATA$results$Location	==	brushFREQzoom$Location)

		brushFREQzoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		brushFREQzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(list(input$brushFREQdbl, input$brushFREQupdate),	{
		output$brushFREQfacet	=	renderPlot({
			req(DATA$results, brushFREQzoom$CHANGE)
			graphFREQ(brushFREQzoom$FILTER)	+ labs(caption = paste0(
				paste(c(brushFREQzoom$GPU, brushFREQzoom$API, brushFREQzoom$Quality, brushFREQzoom$Location), collapse = ", ")
			)	)
		})
	},	ignoreInit	=	TRUE)

	dataFILTfreq	<-	reactive(DATA$results[brushFREQzoom$FILTER, ][[input$datatypeG]])
	observeEvent(list(input$brushFREQfac, input$roundTerm),	{
		req(DATA$results)
		brush 		<- input$brushFREQfac
				
		FREQecdf	<-	(1 - ecdf(dataFILTfreq())(c(brush$xmax, brush$xmin))) * 100
		FREQecdfTAB	<-	cbind(
				ms			=	c(brush$xmax,		brush$xmin,			brush$xmax - brush$xmin),
				FPS			=	c(1000/brush$xmax,	1000/brush$xmin,	1000/brush$xmin - 1000/brush$xmax),
				"% Above"	=	c(FREQecdf[1],		FREQecdf[2],		FREQecdf[2] - FREQecdf[1])
			) %>% as.data.frame()

		rownames(FREQecdfTAB)	<-	c("Upper Limit", "Lower Limit", "Difference")
		
		output$brushFREQfacetTAB	<-	NULL
		if (!is.null(brush))	output$brushFREQfacetTAB	<-	renderTable(FREQecdfTAB, digits = input$roundTerm, rownames = TRUE)
	})

#	QQ
	brushQQzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL,	CHANGE	=	FALSE)
	observeEvent(input$brushQQ, {	req(DATA$results)
		brush 		<-	input$brushQQ
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		brushQQzoom$GPU			<-	NULL
		brushQQzoom$API			<-	NULL
		brushQQzoom$Quality		<-	NULL
		brushQQzoom$Location	<-	NULL
		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		brushQQzoom$x				<-	c(brush$xmin, brush$xmax)
		brushQQzoom$y				<-	c(brush$ymin, brush$ymax)
		if (exists("GPU",		brushFILT))	{
			brushQQzoom$GPU			<-	brushFILT$GPU
			filtGPU					<-	which(DATA$results$GPU		==	brushQQzoom$GPU)
		}
		if (exists("API",		brushFILT))	{
			brushQQzoom$API			<-	brushFILT$API
			filtAPI					<-	which(DATA$results$API		==	brushQQzoom$API)
		}
		if (exists("Quality",	brushFILT))	{
			brushQQzoom$Quality		<-	brushFILT$Quality
			filtQUA					<-	which(DATA$results$Quality	==	brushQQzoom$Quality)
		}
		if (exists("Location",	brushFILT))	{
			brushQQzoom$Location	<-	brushFILT$Location
			filtLOC					<-	which(DATA$results$Location	==	brushQQzoom$Location)
		}

		brushQQzoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

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

		brushQQzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	ends	=	0.00001
	observeEvent(list(input$brushQQupdate),	{	req(DATA$results)
		brushQQzoom$x			=	qnorm(c(input$brushQQlowerX, input$brushQQupperX)/100)
		if	(input$brushQQlowerX == 0)		brushQQzoom$x[1]	=	qnorm(ends)
		if	(input$brushQQupperX == 100)	brushQQzoom$x[2]	=	qnorm(1 - ends)
		brushQQzoom$y			=	c(input$brushQQlowerY, input$brushQQupperY)
		brushQQzoom$GPU			<-	input$brushQQgpu
		brushQQzoom$API			<-	input$brushQQapi
		brushQQzoom$Quality		<-	input$brushQQqua
		brushQQzoom$Location	<-	input$brushQQloc

		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)
		if (!is.null(brushQQzoom$GPU))		filtGPU		<-	which(DATA$results$GPU		==	brushQQzoom$GPU)
		if (!is.null(brushQQzoom$API))		filtAPI		<-	which(DATA$results$API		==	brushQQzoom$API)
		if (!is.null(brushQQzoom$Quality))	filtQUA		<-	which(DATA$results$Quality	==	brushQQzoom$Quality)
		if (!is.null(brushQQzoom$Location))	filtLOC		<-	which(DATA$results$Location	==	brushQQzoom$Location)

		brushQQzoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		brushQQzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(input$brushQQdbl,	{	req(DATA$results)
		brush 		<-	input$brushQQdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		brushQQzoom$x	=	c(qnorm(ends), qnorm(1 - ends))
		brushQQzoom$y	=	c(0, input$FtimeLimitMS)

		brushQQzoom$GPU			<-	NULL
		brushQQzoom$API			<-	NULL
		brushQQzoom$Quality		<-	NULL
		brushQQzoom$Location	<-	NULL
		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		if (exists("GPU",		brushFILT))	{
			brushQQzoom$GPU			<-	brushFILT$GPU
			filtGPU					<-	which(DATA$results$GPU		==	brushFILT$GPU)
		}
		if (exists("API",		brushFILT))	{
			brushQQzoom$API			<-	brushFILT$API
			filtAPI					<-	which(DATA$results$API		==	brushFILT$API)
		}
		if (exists("Quality",	brushFILT))	{
			brushQQzoom$Quality		<-	brushFILT$Quality
			filtQUA					<-	which(DATA$results$Quality	==	brushFILT$Quality)
		}
		if (exists("Location",	brushFILT))	{
			brushQQzoom$Location	<-	brushFILT$Location
			filtLOC					<-	which(DATA$results$Location	==	brushFILT$Location)
		}

		brushQQzoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		updateNumericInput(inputId	=	"brushQQlowerX",	value	=	0)
		updateNumericInput(inputId	=	"brushQQupperX",	value	=	100)

		updateNumericInput(inputId	=	"brushQQlowerY",	value	=	0)
		updateNumericInput(inputId	=	"brushQQupperY",	value	=	input$FtimeLimitMS	)

		updateSelectInput(inputId	=	"brushQQgpu",	selected	=	brushQQzoom$GPU			)
		updateSelectInput(inputId	=	"brushQQapi",	selected	=	brushQQzoom$API			)
		updateSelectInput(inputId	=	"brushQQqua",	selected	=	brushQQzoom$Quality		)
		updateSelectInput(inputId	=	"brushQQloc",	selected	=	brushQQzoom$Location	)

		brushQQzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE,	priority	=	5)

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

	observeEvent(list(input$brushQQ, input$brushQQdbl, input$brushQQupdate),	{
		req(brushQQzoom$FILTER, brushQQzoom$CHANGE)
		output$brushQQfacet	=	renderPlot({
			graphQQ(brushQQzoom$FILTER) +
			labs(caption = paste0(
				"Y: ", paste(round(brushQQzoom$y, 4), collapse = " to "), " (ms) : ",
				"X: ", paste(round(pnorm(brushQQzoom$x)*100, 4), collapse = " to "), " (%) : ",
				paste(c(brushQQzoom$GPU, brushQQzoom$API, brushQQzoom$Quality, brushQQzoom$Location), collapse = ", ")
			)	) +
			coord_cartesian(xlim = brushQQzoom$x,	ylim = brushQQzoom$y,	expand = FALSE)
		})

		observeEvent(input$roundTerm,	{
			output$brushQQtable	=	renderTable({
				qqMINMAX(DATA$results[brushQQzoom$FILTER, ], brushQQzoom, input$datatypeG, input$roundTerm)
			},	digits	=	input$roundTerm,	rownames	=	TRUE,	align	=	"r")
		})
	},	ignoreInit	=	TRUE)


#	Consecutive Difference
	brushDIFFzoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL,	CHANGE	=	FALSE)
	observeEvent(input$brushDIFFdbl,	{	req(DATA$results)
		brush 		<- input$brushDIFFdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		# brushDIFFzoom$x				<-	NULL
		brushDIFFzoom$GPU			<-	NULL
		brushDIFFzoom$API			<-	NULL
		brushDIFFzoom$Quality		<-	NULL
		brushDIFFzoom$Location		<-	NULL
		# brushDIFFzoom$FILTER		<-	TRUE
		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		if (exists("GPU",		brushFILT))	{
			brushDIFFzoom$GPU		<-	brushFILT$GPU
			filtGPU					<-	which(DATA$results$GPU		==	brushDIFFzoom$GPU)
		}
		if (exists("API",		brushFILT))	{
			brushDIFFzoom$API		<-	brushFILT$API
			filtAPI					<-	which(DATA$results$API		==	brushDIFFzoom$API)
		}
		if (exists("Quality",	brushFILT))	{
			brushDIFFzoom$Quality	<-	brushFILT$Quality
			filtQUA					<-	which(DATA$results$Quality	==	brushDIFFzoom$Quality)
		}
		if (exists("Location",	brushFILT))	{
			brushDIFFzoom$Location	<-	brushFILT$Location
			filtLOC					<-	which(DATA$results$Location	==	brushDIFFzoom$Location)
		}

		brushDIFFzoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		updateSelectInput(inputId	=	"brushDIFFgpu",	selected	=	brushDIFFzoom$GPU			)
		updateSelectInput(inputId	=	"brushDIFFapi",	selected	=	brushDIFFzoom$API			)
		updateSelectInput(inputId	=	"brushDIFFqua",	selected	=	brushDIFFzoom$Quality		)
		updateSelectInput(inputId	=	"brushDIFFloc",	selected	=	brushDIFFzoom$Location		)

		brushDIFFzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(input$brushDIFFupdate,	{	req(DATA$results)
		# brushDIFFzoom$x			=	c(input$brushDIFFstart, input$brushDIFFstart + input$brushDIFFlength)
		brushDIFFzoom$GPU			<-	input$brushDIFFgpu
		brushDIFFzoom$API			<-	input$brushDIFFapi
		brushDIFFzoom$Quality		<-	input$brushDIFFqua
		brushDIFFzoom$Location		<-	input$brushDIFFloc

		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		if (!is.null(brushDIFFzoom$GPU))		filtGPU		<-	which(DATA$results$GPU		==	brushDIFFzoom$GPU)
		if (!is.null(brushDIFFzoom$API))		filtAPI		<-	which(DATA$results$API		==	brushDIFFzoom$API)
		if (!is.null(brushDIFFzoom$Quality))	filtQUA		<-	which(DATA$results$Quality	==	brushDIFFzoom$Quality)
		if (!is.null(brushDIFFzoom$Location))	filtLOC		<-	which(DATA$results$Location	==	brushDIFFzoom$Location)

		brushDIFFzoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		brushDIFFzoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(list(input$brushDIFFdbl, input$brushDIFFupdate, input$diffLimHeat, input$brushDIFFalphup),	{
		HEATMAP	=	list(stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), alpha = input$brushDIFFalpha, show.legend = FALSE),  scale_fill_viridis_c())
		if (!input$diffLimHeat)	HEATMAP	=	NULL

		output$brushDIFFfacet	=	renderPlot({
			req(DATA$results, brushDIFFzoom$CHANGE)
			graphDIFF(brushDIFFzoom$FILTER)	+ labs(caption = paste0(
				paste(c(brushDIFFzoom$GPU, brushDIFFzoom$API, brushDIFFzoom$Quality, brushDIFFzoom$Location), collapse = ", ")
			)	) + HEATMAP
		})
	},	ignoreInit	=	TRUE)

	dataFILTdiff	<-	reactive(DATA$results[brushDIFFzoom$FILTER, ][[input$datatypeG]])
	observeEvent(list(input$brushDIFFfac, input$roundTerm),	{
		brush	<-	input$brushDIFFfac

		DIFF	<-	diff.CONS(unlist(dataFILTdiff()))
		rangeX	<-	which(dataFILTdiff()	>= brush$xmin	&	dataFILTdiff()	<= brush$xmax)
		rangeY	<-	which(DIFF 				>= brush$ymin	&	DIFF			<= brush$ymax)

		DIFFperc	<-	length(intersect(rangeX, rangeY))/length(dataFILTdiff())
		DIFFtabl	<-	as.data.frame(rbind(
			list("Range Time (ms)",			brush$xmin, "to", brush$xmax),
			list("Range Difference (ms)",	brush$ymin, "to", brush$ymax)
		)	)

		output$brushDIFFfacetSEL	=	renderText(
			paste0("Amount of data selected: ",	round(DIFFperc * 100, input$roundTerm), "%")
		)

		output$brushDIFFfacetRAN	<-	NULL
		if (!is.null(brush))	output$brushDIFFfacetRAN	=	renderTable(	DIFFtabl,
									colnames = FALSE, digits = input$roundTerm,	align = 'lrcr')
	},	ignoreInit = TRUE)


#	Consecutive Difference (Percentage)
	brushDIFFperczoom	=	reactiveValues(x = c(-Inf, Inf),	y = c(-Inf, Inf),	FILTER	=	TRUE,
		GPU	=	NULL,	API	=	NULL,	Quality	=	NULL,	Location = NULL,	CHANGE	=	FALSE)
	observeEvent(input$brushDIFFpercdbl,	{	req(DATA$results)
		brush 		<- input$brushDIFFpercdbl
		brushFILT	<-	setNames(brush[grep("panelvar", names(brush))], brush$mapping[grep("panelvar", names(brush$mapping))]	)

		# brushDIFFperczoom$x				<-	NULL
		brushDIFFperczoom$GPU			<-	NULL
		brushDIFFperczoom$API			<-	NULL
		brushDIFFperczoom$Quality		<-	NULL
		brushDIFFperczoom$Location		<-	NULL
		# brushDIFFperczoom$FILTER		<-	TRUE
		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		if (exists("GPU",		brushFILT))	{
			brushDIFFperczoom$GPU		<-	brushFILT$GPU
			filtGPU					<-	which(DATA$results$GPU		==	brushDIFFperczoom$GPU)
		}
		if (exists("API",		brushFILT))	{
			brushDIFFperczoom$API		<-	brushFILT$API
			filtAPI					<-	which(DATA$results$API		==	brushDIFFperczoom$API)
		}
		if (exists("Quality",	brushFILT))	{
			brushDIFFperczoom$Quality	<-	brushFILT$Quality
			filtQUA					<-	which(DATA$results$Quality	==	brushDIFFperczoom$Quality)
		}
		if (exists("Location",	brushFILT))	{
			brushDIFFperczoom$Location	<-	brushFILT$Location
			filtLOC					<-	which(DATA$results$Location	==	brushDIFFperczoom$Location)
		}

		brushDIFFperczoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		updateSelectInput(inputId	=	"brushDIFFpercgpu",	selected	=	brushDIFFperczoom$GPU			)
		updateSelectInput(inputId	=	"brushDIFFpercapi",	selected	=	brushDIFFperczoom$API			)
		updateSelectInput(inputId	=	"brushDIFFpercqua",	selected	=	brushDIFFperczoom$Quality		)
		updateSelectInput(inputId	=	"brushDIFFpercloc",	selected	=	brushDIFFperczoom$Location		)

		brushDIFFperczoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(input$brushDIFFpercupdate,	{	req(DATA$results)
		# brushDIFFperczoom$x			=	c(input$brushDIFFpercstart, input$brushDIFFpercstart + input$brushDIFFperclength)
		brushDIFFperczoom$GPU			<-	input$brushDIFFpercgpu
		brushDIFFperczoom$API			<-	input$brushDIFFpercapi
		brushDIFFperczoom$Quality		<-	input$brushDIFFpercqua
		brushDIFFperczoom$Location		<-	input$brushDIFFpercloc

		filtGPU	<-	1:nrow(DATA$results)
		filtAPI	<-	1:nrow(DATA$results)
		filtQUA	<-	1:nrow(DATA$results)
		filtLOC	<-	1:nrow(DATA$results)

		if (!is.null(brushDIFFperczoom$GPU))		filtGPU		<-	which(DATA$results$GPU		==	brushDIFFperczoom$GPU)
		if (!is.null(brushDIFFperczoom$API))		filtAPI		<-	which(DATA$results$API		==	brushDIFFperczoom$API)
		if (!is.null(brushDIFFperczoom$Quality))	filtQUA		<-	which(DATA$results$Quality	==	brushDIFFperczoom$Quality)
		if (!is.null(brushDIFFperczoom$Location))	filtLOC		<-	which(DATA$results$Location	==	brushDIFFperczoom$Location)

		brushDIFFperczoom$FILTER		<-	Reduce(intersect, list(filtGPU, filtAPI, filtQUA, filtLOC))

		brushDIFFperczoom$CHANGE	<-	TRUE
	},	ignoreInit	=	TRUE)

	observeEvent(list(input$brushDIFFpercdbl, input$brushDIFFpercupdate, input$diffPERCLimHeat, input$brushDIFFpercalphup),	{
		HEATMAP	=	list(stat_density_2d(geom = "polygon", aes(fill = after_stat(nlevel)), alpha = input$brushDIFFpercalpha, show.legend = FALSE),  scale_fill_viridis_c())
		if (!input$diffPERCLimHeat)	HEATMAP	=	NULL

		output$brushDIFFpercfacet	=	renderPlot({
			req(DATA$results, brushDIFFperczoom$CHANGE)
			graphDIFF(brushDIFFperczoom$FILTER, TRUE)	+ labs(caption = paste0(
				paste(c(brushDIFFperczoom$GPU, brushDIFFperczoom$API, brushDIFFperczoom$Quality, brushDIFFperczoom$Location), collapse = ", ")
			)	) + HEATMAP
		})
	},	ignoreInit	=	TRUE)

	dataFILTdiffPERC	<-	reactive(DATA$results[brushDIFFperczoom$FILTER, ][[input$datatypeG]])
	observeEvent(list(input$brushDIFFpercfac, input$roundTerm),	{
		brush	<-	input$brushDIFFpercfac

		DIFFperc	<-	diff.CONS(unlist(dataFILTdiffPERC()))/dataFILTdiffPERC()
		rangeX	<-	which(dataFILTdiffPERC()	>= brush$xmin	&	dataFILTdiffPERC()	<= brush$xmax)
		rangeY	<-	which(DIFFperc 				>= brush$ymin	&	DIFFperc			<= brush$ymax)

		DIFFpercperc	<-	length(intersect(rangeX, rangeY))/length(dataFILTdiffPERC())
		DIFFperctabl	<-	as.data.frame(rbind(
			list("Range Time (ms)",			brush$xmin, "to", brush$xmax),
			list("Range Difference (%)",	brush$ymin*100, "to", brush$ymax*100)
		)	)

		output$brushDIFFpercfacetSEL	=	renderText(
			paste0("Amount of data selected: ",	round(DIFFpercperc * 100, input$roundTerm), "%")
		)

		output$brushDIFFpercfacetRAN	<-	NULL
		if (!is.null(brush))	output$brushDIFFpercfacetRAN	=	renderTable(	DIFFperctabl,
									colnames = FALSE, digits = input$roundTerm,	align = 'lrcr')
	},	ignoreInit = TRUE)