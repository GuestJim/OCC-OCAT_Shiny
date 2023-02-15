tableCOLs	=	reactiveVal(c(DATA$nameMEAN, DATA$namePERC, DATA$nameECDF))

observeEvent(list(input$fileInput, input$manuPERC, input$manuECDF, input$dataSelLOAD), {
	PERC	=	c(DATA$namePERC, input$manuPERC)	|>	to.NUM()	|>	PERCdefa()	|>	names()
	ECDF	=	c(DATA$nameECDF, input$manuECDF)	|>	to.NUM()	|>	ECDFdefa()	|>	names()

	tableCOLs(c(DATA$nameMEAN, PERC, ECDF))

	updateCheckboxGroupInput(
		inputId		=	"tabCOLs",	choices		=	tableCOLs(),
		selected	=	c(paste0(to.NUM(input$manuPERC), "%"), paste0(to.NUM(input$manuECDF), " FPS"), input$tabCOLs)
	)
})

TABLES$GRPrem	<-	reactive({	defs$GROUPS[!defs$GROUPS	%in%	input$listGROUPS]	})
TABLES$GRPord	<-	reactive({	input$orderGROUPS[input$orderGROUPS	%in%	input$listGROUPS]	})
TABLES$filtDATA	<-	reactive({
	out	<-	DATA$results
	if (length(TABLES$GRPrem())	!=	0)	out	<-	DATA$results	|>	filter(
		GPU			%in%	input$listGPU	&
		Quality		%in%	input$listQUA	&
		Location	%in%	input$listLOC	&
		if (!anyNA(DATA$results$API))	API	%in%	input$listAPI &
		TRUE
	)
	out
})	|>	bindEvent(input$listGROUPS, input$listGPU, input$listQUA, input$listLOC, input$listAPI, input$dataInput)
#	for filtering data within groups, including disabled groups

TABLES$summ	<-	reactive({
	hold	<-	SUMMfunc(TABLES$filtDATA()	|>	ungroup(all_of(TABLES$GRPrem()))	)	|>
		arrange(!!!rlang::syms(TABLES$GRPord()))
		#	this !!!rlang::syms is necessary for the string to be converted into symbols that are then interpreted as symbols
	hold	|>	bind_rows(FPSconv(hold))
})
TABLES$perc	<-	reactive({
	hold	<-	PERCfunc(TABLES$filtDATA()	|>	ungroup(all_of(TABLES$GRPrem())),	PERCdefa(to.NUM(input$manuPERC))	)	|>
		arrange(!!!rlang::syms(TABLES$GRPord()))
		#	this !!!rlang::syms is necessary for the string to be converted into symbols that are then interpreted as symbols
	hold	|>	bind_rows(FPSconv(hold))
})
TABLES$ecdf	<-	reactive({
	ECDFfunc(TABLES$filtDATA()	|>	ungroup(all_of(TABLES$GRPrem())),	ECDFdefa(to.NUM(input$manuECDF))	)	|>
		arrange(!!!rlang::syms(TABLES$GRPord()))
		#	this !!!rlang::syms is necessary for the string to be converted into symbols that are then interpreted as symbols
	})
TABLES$devi	<-	reactive({
	DEVIfunc(TABLES$filtDATA()	|>	ungroup(all_of(TABLES$GRPrem()))	)	|>
		arrange(!!!rlang::syms(TABLES$GRPord()))
		#	this !!!rlang::syms is necessary for the string to be converted into symbols that are then interpreted as symbols
})

tableFILT	=	function(TAB, COLS = TRUE)	{
	req(input$listGPU, input$listQUA, input$listLOC)

	rowGPU	=	TRUE	;	rowQUA	=	TRUE	;	rowAPI	=	TRUE	;	rowLOC	=	TRUE
	if ("GPU"		%in% colnames(TAB))	rowGPU	<-	TAB$GPU			%in% input$listGPU
	if ("Quality"	%in% colnames(TAB))	rowQUA	<-	TAB$Quality		%in% input$listQUA
	if ("Location"	%in% colnames(TAB))	rowLOC	<-	TAB$Location	%in% input$listLOC
	if ("API"		%in% colnames(TAB) & !anyNA(TAB$API))	rowAPI	<-	TAB$API			%in% input$listAPI

	out	=	TAB[rowGPU & rowQUA & rowLOC & rowAPI, ]
	#	the check above for API should be enough to handle if API is not present in the data
	groups	=	names(out)[!sapply(out, is.numeric)]

	filtCOL	=	names(out) %in% c(groups, input$tabCOLs)
	#	for some reason, Shiny does not like searching by name, but this gets around that
	filtROW	=	rep(TRUE, nrow(out))

	if (exists("Unit", out))	if ("FPS" %in% out$Unit)	{
		if (!("FPS" %in% input$tabUNIT))	filtROW	=	out$Unit != "FPS"
		if (!("ms" %in% input$tabUNIT))		filtROW	=	out$Unit != "ms"
	}

	if (COLS)	return(out[which(filtROW), which(filtCOL)])
	return(out[which(filtROW), ])
}


output$tableSUMM	=	renderTable({	full_join(TABLES$summ(), TABLES$perc())	|>	tableFILT()	},
	digits	=	reactive(input$roundTerm),	striped	=	TRUE)
# output$tablePERC	=	renderTable({	TABLES$perc()							|>	tableFILT()	},
	# digits	=	reactive(input$roundTerm),	striped	=	TRUE)
output$tableECDF	=	renderTable({	TABLES$ecdf()							|>	tableFILT()	},
	digits	=	reactive(input$roundTerm),	striped	=	TRUE)
output$tableDEVI	=	renderTable({	TABLES$devi()							|>	tableFILT(FALSE)	},
	digits	=	reactive(input$roundTerm),	striped	=	TRUE)


output$tableSUMMdown	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Summary.csv")},
	content	=	function(file)	{write_csv(
		full_join(TABLES$summ(), TABLES$perc())	|>	tableFILT()	|>	tableROUND(input$roundTerm),
	file)}
)
output$tableECDFdown	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - ECDF.csv")},
	content	=	function(file)	{write_csv(
		TABLES$ecdf()	|>	tableFILT()	|>	tableROUND(input$roundTerm),
	file)}
)
output$tableDEVIdown	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Deviations.csv")},
	content	=	function(file)	{write_csv(
		TABLES$devi()	|>	tableFILT(FALSE)	|>	tableROUND(input$roundTerm),
	file)}
)
output$tableSUMMhtml	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Summary.HTML")},
	content	=	function(file)	{write_tableHTML(OCCHTML(
		full_join(TABLES$summ(), TABLES$perc())	|>	tableFILT()	|>	tableROUND(input$roundTerm)),
	file)}
)
output$tableECDFhtml	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - ECDF.HTML")},
	content	=	function(file)	{write_tableHTML(OCCHTML(
			TABLES$ecdf()	|>	tableFILT()	|>	tableROUND(input$roundTerm)),
	file)}
)
output$tableDEVIhtml	=	downloadHandler(
	filename	=	function()	{paste0(DATA$game, " - Deviations.HTML")},
	content	=	function(file)	{write_tableHTML(OCCHTML(
		TABLES$devi()	|>	tableFILT(FALSE)	|>	tableROUND(input$roundTerm)),
	file)}
)
