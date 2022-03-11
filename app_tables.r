	observeEvent(input$roundTerm,	{
		DATA$r	=	input$roundTerm
	})

	tableCOLs	=	reactiveVal(c(DATA$nameMEAN, DATA$namePERC, DATA$nameECDF))
	
	observeEvent(list(input$fileInput, input$manuPERC, input$manuECDF), {
		PERC	=	namePERC(c(to.NUM(DATA$namePERC), to.NUM(input$manuPERC)))
		ECDF	=	nameECDF(c(to.NUM(DATA$nameECDF), to.NUM(input$manuECDF)))

		tableCOLs(c(DATA$nameMEAN, PERC, ECDF))

		updateCheckboxGroupInput(
			inputId	=	"tabCOLs",
			choices		=	tableCOLs(),	selected	=	c(nameDEFs, paste0(to.NUM(input$manuPERC), "%"), paste0(to.NUM(input$manuECDF), " FPS"), input$tabCOLs)
		)
	})

	DATA$tableSUMM	=	reactiveVal()
	observeEvent(list(input$dataInput, DATA$LOAD, input$manuPERC, input$datatype),	{
		req(DATA$results)
		
		outMEAN	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS, meanMS))
		outPERC	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS, percMS, to.NUM(c(input$manuPERC))))
		out	=	merge(outMEAN,	outPERC, sort = FALSE)

		DATA$namePERC	=	namePERC(to.NUM(input$manuPERC))

		out$Unit	=	"ms"
		colDATA	=	sapply(out, is.numeric)

		outFPS	=	out[out$Unit == "ms", ]
		outFPS$Unit	=	"FPS"
		outFPS[, colDATA]	=	1000/outFPS[, colDATA]

		# out	=	rbind(out, outFPS)
		out	=	rbind(outFPS, out)

		DATA$tableSUMM(out[, c(which(!colDATA), which(colDATA))])
	})
	
	DATA$tableECDF	=	reactiveVal()
	observeEvent(list(input$dataInput, DATA$LOAD, input$manuECDF, input$datatype),	{
		req(DATA$results)
		
		outECDF	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS, ecdfFPS, to.NUM(c(input$manuECDF))))
		out	=	outECDF

		DATA$nameECDF	=	nameECDF(to.NUM(input$manuECDF))

		out$Unit	=	"%"
		colDATA	=	sapply(out, is.numeric)
		# out	=	out[, c(which(!colDATA), which(colDATA))]
		
		DATA$tableECDF(out[, c(which(!colDATA), which(colDATA))])
	})

	tableFILT	=	function(TAB)	{
		req(input$listGPU, input$listQUA, input$listLOC)
		
		out	=	TAB[TAB$GPU %in% input$listGPU & TAB$Quality %in% input$listQUA & TAB$Location %in% input$listLOC, ]
		groups	=	names(out)[!sapply(out, is.numeric)]
		if (DATA$checkAPI)	out	=	out[out$API %in% input$listAPI, ]

		filtCOL	=	names(out) %in% c(groups, input$tabCOLs)
		#	for some reason, Shiny does not like searching by name, but this gets around that
		filtROW	=	TRUE

		if (!("ms" %in% input$tabUNIT))		filtROW	=	out$Unit != "ms"
		if (!("FPS" %in% input$tabUNIT))	filtROW	=	out$Unit != "FPS"

		return(out[filtROW, filtCOL])
	}
	
	observeEvent(list(input$dataInput, DATA$LOAD, input$manuRefresh, input$roundTerm),	{
		test	<<-	DATA$tableSUMM()
		output$tableSUMM	=	renderTable({
			tableFILT(DATA$tableSUMM())
		},	digits	=	input$roundTerm,	striped	=	TRUE)
		output$tableECDF	=	renderTable({
			tableFILT(DATA$tableECDF())
		},	digits	=	input$roundTerm,	striped	=	TRUE)
	}	)
	#	it is necessary to put renderTable into observeEvent like this so the digits shown can be dynamically controlled

	output$tableSUMMdown	=	downloadHandler(
		filename	=	function()	{paste0(DATA$game, " - Summary.csv")},
		content	=	function(file)	{write_csv(tableROUND(tableFILT(tableSUMM()), input$roundTerm), file)}
	)
	output$tableECDFdown	=	downloadHandler(
		filename	=	function()	{paste0(DATA$game, " - ECDF.csv")},
		content	=	function(file)	{write_csv(tableROUND(tableFILT(tableECDF()), input$roundTerm), file)}
	)
	output$tableSUMMhtml	=	downloadHandler(
		filename	=	function()	{paste0(DATA$game, " - Summary.HTML")},
		content	=	function(file)	{write_tableHTML(OCCHTML(tableROUND(tableFILT(tableSUMM()), input$roundTerm)), file)}
	)
	output$tableECDFhtml	=	downloadHandler(
		filename	=	function()	{paste0(DATA$game, " - ECDF.HTML")},
		content	=	function(file)	{write_tableHTML(OCCHTML(tableROUND(tableFILT(tableECDF()), input$roundTerm)), file)}
	)