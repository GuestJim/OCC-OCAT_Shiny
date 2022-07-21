observeEvent(input$roundTerm,	{	DATA$r	=	input$roundTerm	})

tableCOLs	=	reactiveVal(c(DATA$nameMEAN, DATA$namePERC, DATA$nameECDF))

tableFILT	=	function(TAB)	{
	req(input$listGPU, input$listQUA, input$listLOC)

	rowGPU	=	TRUE	;	rowQUA	=	TRUE	;	rowAPI	=	TRUE	;	rowLOC	=	TRUE
	if ("GPU"		%in% colnames(TAB))	rowGPU	=	TAB$GPU			%in% input$listGPU
	if ("Quality"	%in% colnames(TAB))	rowQUA	=	TAB$Quality		%in% input$listQUA
	if ("Location"	%in% colnames(TAB))	rowLOC	=	TAB$Location	%in% input$listLOC
	if ("API"		%in% colnames(TAB))	rowAPI	=	TAB$API			%in% input$listAPI

	out	=	TAB[rowGPU & rowQUA & rowLOC & rowAPI, ]
	#	the check above for API should be enough to handle if API is not present in the data
	groups	=	names(out)[!sapply(out, is.numeric)]
	# if (DATA$checkAPI)	out	=	out[rowAPI, ]
	
	filtCOL	=	names(out) %in% c(groups, input$tabCOLs)
	#	for some reason, Shiny does not like searching by name, but this gets around that
	filtROW	=	rep(TRUE, nrow(out))

	if (!("FPS" %in% input$tabUNIT))	filtROW	=	out$Unit != "FPS"
	if (!("ms" %in% input$tabUNIT))		filtROW	=	out$Unit != "ms"

	return(out[which(filtROW), which(filtCOL)])
}

deviaFILT	=	function(TAB)	{
	req(input$listGPU, input$listQUA, input$listLOC)

	rowGPU	=	TRUE	;	rowQUA	=	TRUE	;	rowAPI	=	TRUE	;	rowLOC	=	TRUE
	if ("GPU"		%in% colnames(TAB))	rowGPU	=	TAB$GPU			%in% input$listGPU
	if ("Quality"	%in% colnames(TAB))	rowQUA	=	TAB$Quality		%in% input$listQUA
	if ("Location"	%in% colnames(TAB))	rowLOC	=	TAB$Location	%in% input$listLOC
	if ("API"		%in% colnames(TAB))	rowAPI	=	TAB$API			%in% input$listAPI

	out	=	TAB[rowGPU & rowQUA & rowLOC & rowAPI, ]
	out$Unit	=	"ms"
	numCOL		=	sapply(out, is.numeric)
	out[, c(which(!numCOL), which(numCOL))]
}

observeEvent(list(input$fileInput, input$manuPERC, input$manuECDF, input$dataSelLOAD), {
	PERC	=	namePERC(c(to.NUM(DATA$namePERC), to.NUM(input$manuPERC)))
	ECDF	=	nameECDF(c(to.NUM(DATA$nameECDF), to.NUM(input$manuECDF)))

	tableCOLs(c(DATA$nameMEAN, PERC, ECDF))

	updateCheckboxGroupInput(
		inputId	=	"tabCOLs",
		choices		=	tableCOLs(),	selected	=	c(paste0(to.NUM(input$manuPERC), "%"), paste0(to.NUM(input$manuECDF), " FPS"), input$tabCOLs)
	)
})

DATA$tableSUMM	=	reactiveVal(NULL)	;	DATA$tableECDF	=	reactiveVal(NULL)	;	DATA$tableDEVI	=	reactiveVal(NULL)
observeEvent(list(input$dataInput, DATA$LOAD, input$dataSelLOAD),	{
	if (exists("tableSUMM", envir = DATA))	DATA$tableSUMM	=	reactiveVal(DATA$tableSUMM)
	if (exists("tableECDF", envir = DATA))	DATA$tableECDF	=	reactiveVal(DATA$tableECDF)
})

output$tableSUMM	=	renderTable({	tableFILT(DATA$tableSUMM())	},	digits	=	2,	striped	=	TRUE)
output$tableECDF	=	renderTable({	tableFILT(DATA$tableECDF())	},	digits	=	2,	striped	=	TRUE)
output$tableDEVI	=	renderTable({	deviaFILT(DATA$tableDEVI())	},	digits	=	2,	striped	=	TRUE)
#	these will load a pre-computed version of the table into the applet, rather than needing to wait for the work to be done
#	only relevant with the Static version, rather than Upload

observeEvent(list(input$dataInput, DATA$LOAD, input$dataSelLOAD, input$manuPERC, input$datatype, input$listGROUPS),	{
	req(DATA$results)

	# outMEAN	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS, meanMS))
	# outPERC	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS, percMS, to.NUM(c(input$manuPERC))))
	# out	=	merge(outMEAN,	outPERC, sort = FALSE)
	# out	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS, summMS, to.NUM(c(input$manuPERC))))
	out	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS[names(DATA$GROUPS) %in% input$listGROUPS], summMS, to.NUM(c(input$manuPERC))))

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

observeEvent(list(input$dataInput, DATA$LOAD, input$dataSelLOAD, input$datatype, input$listGROUPS),	{
	req(DATA$results)

	DATA$tableDEVI(merge(
		sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS[names(DATA$GROUPS) %in% input$listGROUPS], function(IN) c("Mean" = mean(IN, na.rm = TRUE), "Median" = median(IN, na.rm = TRUE))	)),
		sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS[names(DATA$GROUPS) %in% input$listGROUPS], deviMS))
		,	sort = FALSE)	)
}	)

observeEvent(list(input$dataInput, DATA$LOAD, input$dataSelLOAD, input$manuECDF, input$datatype, input$listGROUPS),	{
	req(DATA$results)

	# outECDF	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS, ecdfFPS, to.NUM(c(input$manuECDF))))
	outECDF	=	sepCOL(aggregate(DATA$results[, as.character(input$datatype)], DATA$GROUPS[names(DATA$GROUPS) %in% input$listGROUPS], ecdfFPS, to.NUM(c(input$manuECDF))))
	out	=	outECDF

	DATA$nameECDF	=	nameECDF(to.NUM(input$manuECDF))

	out$Unit	=	"%"
	colDATA	=	sapply(out, is.numeric)
	# out	=	out[, c(which(!colDATA), which(colDATA))]

	DATA$tableECDF(out[, c(which(!colDATA), which(colDATA))])
})


observeEvent(list(input$dataInput, DATA$LOAD, input$manuRefresh, input$roundTerm, input$datatype, input$listGROUPS),	{
	output$tableSUMM	=	renderTable({
		tableFILT(DATA$tableSUMM())
	},	digits	=	input$roundTerm,	striped	=	TRUE)
	output$tableECDF	=	renderTable({
		tableFILT(DATA$tableECDF())
	},	digits	=	input$roundTerm,	striped	=	TRUE)
	output$tableDEVI	=	renderTable({
		deviaFILT(DATA$tableDEVI())
	},	digits	=	input$roundTerm,	striped	=	TRUE)
},	ignoreInit	=	TRUE)
# })
#	it is necessary to put renderTable into observeEvent like this so the digits shown can be dynamically controlled

output$tableSUMMdown	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Summary.csv")},
	content	=	function(file)	{write_csv(tableROUND(tableFILT(DATA$tableSUMM()), input$roundTerm), file)}
)
output$tableECDFdown	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - ECDF.csv")},
	content	=	function(file)	{write_csv(tableROUND(tableFILT(DATA$tableECDF()), input$roundTerm), file)}
)
output$tableDEVIdown	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Deviations.csv")},
	content	=	function(file)	{write_csv(tableROUND(DATA$tableDEVI(), input$roundTerm), file)}
)
output$tableSUMMhtml	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Summary.HTML")},
	content	=	function(file)	{write_tableHTML(OCCHTML(tableROUND(tableFILT(DATA$tableSUMM()), input$roundTerm)), file)}
)
output$tableECDFhtml	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - ECDF.HTML")},
	content	=	function(file)	{write_tableHTML(OCCHTML(tableROUND(tableFILT(DATA$tableECDF()), input$roundTerm)), file)}
)
output$tableDEVIhtml	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Deviations.HTML")},
	content	=	function(file)	{write_tableHTML(OCCHTML(tableROUND(DATA$tableDEVI(), input$roundTerm)), file)}
)