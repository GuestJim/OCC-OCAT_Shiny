if (!require(sortable))	install.packages("sortable")	;	library(sortable)

DataInputUI	<-	function(id, ..., label = "Data Loading")	{
	ns	<-	NS(id)

	if	(!exists("FILE", envir=DATA) & VIEW$UPLOAD)	{
		tagList(
			selectInput('dataSel',	label	=	"Data to Load",	selectize	=	FALSE,
				# choices	=	setNames(FILES, gsub(".env|.RData", "", names(FILES))), selected	=	FILES[1]
				choices	=	FILES, selected	=	FILES[1]
			),
			actionButton(inputId	=	"dataSelLOAD",	label	=	"Load Selected Data"),
			# bookmarkButton(label = "Bookmark Data"),
			fileInput(inputId	=	"dataInput",
				label	=	"CSV Data to Import",
				accept	=	c(".csv", ".csv.bz2", ".csv.gz", ".csv.xz", ".RData"),
				placeholder	=	"No File Selected"
			),
			textInput(inputId	=	"gameName",
				label	=	"Game Name",		),
			actionButton(inputId	=	"manuRefresh",
				label	=	"Update Game Name"	),
		)
	}
}

tableExportUI	<-	function(id, SEP = FALSE, DOWN = FALSE, ..., label = "Data Saving")	{
	ns	<-	NS(id)

	if (DOWN)	require(readr)
	tagSUMM	=	tagList(
		tableOutput("tableSUMM"),
		if (DOWN)	downloadButton(outputId	=	"tableSUMMdown",	label	=	"Download Summary Table (CSV)"),
		if (DOWN)	downloadButton(outputId	=	"tableSUMMhtml",	label	=	"Download Summary Table (HTML)"),
	)
	tagECDF	=	tagList(
		tableOutput("tableECDF"),
		if (DOWN)	downloadButton(outputId	=	"tableECDFdown",	label	=	"Download ECDF Table (CSV)"),
		if (DOWN)	downloadButton(outputId	=	"tableECDFhtml",	label	=	"Download ECDF Table (HTML)"),
	)
	tagDEVI	=	tagList(
		tableOutput("tableDEVI"),
		div(HTML("
			<style>
				th, td {
				  padding-right: 16px;
				}
			</style>
			<table>
				<tr><th>SD </th><td>Standard Deviation</td></tr>
				<tr><th>SE </th><td>Standard Error</td></tr>
				<tr><th>CoV </th><td>Coefficient of Variation (Ratio of SD to Mean as %)</td></tr>
				<tr><th>MAD </th><td>Median Absolute Deviation</td></tr>
			</table>
		")),
		if (DOWN)	downloadButton(outputId	=	"tableDEVIdown",	label	=	"Download Deviation Table (CSV)"),
		if (DOWN)	downloadButton(outputId	=	"tableDEVIhtml",	label	=	"Download Deviation Table (HTML)"),
	)

	if (SEP)	{
		tagList(
			tabsetPanel(
				tabPanel("Summary Table",		tagSUMM	),
				tabPanel("FPS Targets Table",	tagECDF	),
				if (VIEW$DEVI)	tabPanel("Deviations Table",	tagDEVI	),
				)	)
		}	else	{
			tagList(
				tagSUMM,
				hr(),
				tagECDF,
				if (VIEW$DEVI) tagList(hr(), tagDEVI)
			)
		}
}

graphControlUI	<-	function(id, showGRAPHS = TRUE, ..., label = "Graphs Control")	{
	ns	<-	NS(id)

	if (!showGRAPHS)	return(NULL)

	tabPanel("Graph Control",
		tagList(
			tabsetPanel(
				tabPanel("Control",
					if (VIEW$GEO)	tagList(
						checkboxInput(inputId = "graphGEO",	label = "Use Geometric Mean"),
						helpText("(Relevant for Summary and Frequency graphs)")
						),
					numericInput(inputId	=	"FtimeLimitFPS",
						label	=	"Frame Time Scale Upper Limit (FPS)",
						value	=	15,	min	=	0,	max	=	1000,	step	=	1
					),
					numericInput(inputId	=	"FtimeLimitMS",
						label	=	"Frame Time Scale Upper Limit (ms)",
						value	=	round(1000/15, 2),	min = 0,	max = 1000, step = 0.1
					),
					actionButton(inputId	=	"FtimeLimitRefresh",
						label	=	"Apply Scale"
					),
					checkboxGroupInput(inputId	=	"listFACETS",	label	=	"Facets to use:",
						choices		=	c("GPU", "API", "Quality", "Location"),
						selected	=	c("GPU", "API", "Quality", "Location")
					),
					helpText("Course graph immune to dropping facets"),
					if (VIEW$FACflip)	checkboxGroupInput(inputId	=	"flipFACETS",	label	=	"Graphs to Flip Facets:",
						choiceNames		=	c("Summary", "Course", "Frequency", "QQ", "Consecutive Difference"),
						choiceValues	=	c("SUMM", "COURSE", "FREQ", "QQ", "DIFF"),
						selected	=	NULL
					),
					varSelectInput(inputId	=	"datatypeG",	label	=	"Data Type (Graph)",	multiple	=	FALSE,
						data	=	data.frame(list(MsBetweenPresents = 0))	),	#	the dummy frame is so "datatype" has an initial value
					# numericInput(inputId	=	"facWID",
						# label	=	"Facet Label Width",
						# value	=	25
					# ),
				),
			tabPanel("Selection",
				checkboxGroupInput(inputId	=	"filtGPU",	label	=	"GPUs to include:",
					choices		=	NULL	),
				checkboxGroupInput(inputId	=	"filtAPI",	label	=	"APIs to include:",
					choices		=	NULL	),
				checkboxGroupInput(inputId	=	"filtQUA",	label	=	"Qualities to include:",
					choices		=	NULL	),
				checkboxGroupInput(inputId	=	"filtLOC",	label	=	"Locations to include:",
					choices		=	NULL	),
				actionButton(inputId	=	"filtSEL",	label	=	"Apply Selection"	),
				),
			),
		)
	)
}

GraphTabUI	<-	function(id, showGRAPHS = TRUE, BRUSH = TRUE, ..., label = "Graphs UI")	{
	ns	<-	NS(id)

	ifBRUSH	=	function(IN)	{
		if (BRUSH)	return(IN)
		return(NULL)
	}
	if (!showGRAPHS)	return(NULL)

	tabPanel("Graphs",
		tagList(
			tabsetPanel(
				tabPanel("Summary",
					
					plotOutput('graphMEANfacet',	height=720,
						dblclick	=	ifBRUSH("brushSUMMdbl")),
					if (BRUSH)	tagList(
						fixedRow(
							column(3,	selectInput(inputId	=	"brushSUMMloc",
								label	=	"Location",	choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	NULL),
							column(3,	selectInput(inputId	=	"brushSUMMapi",
								label	=	"API",		choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	selectInput(inputId	=	"brushSUMMqua",
								label	=	"Quality",	choices	=	NULL,	selectize	=	FALSE)
								),
						),
						actionButton(inputId	=	"brushSUMMupdate", label = "Update Single Plot"),
						plotOutput('brushSUMMfacet',	height=720,)
					)
				),
				tabPanel("Course",
					plotOutput('graphCOURSEfacet',	height=720,
						dblclick	=	ifBRUSH("brushCOURSEdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushCOURSE", resetOnNew	=	TRUE, direction	=	"x"))),
					if (BRUSH)	tagList(
						# p("Click and Drag to Zoom Below"),
						fixedRow(
							column(3,	numericInput(inputId	=	"brushCOURSEstart",
								label	=	"Zoom Start (s)",	value	=	0,	step	=	1,
								min	=	0,	max	=	300)
								),
							column(3,	numericInput(inputId	=	"brushCOURSElength",
								label	=	"Zoom Length (s)",	value	=	300,	step	=	0.1,
								min	=	0,	max	=	300)
								),
							column(3,	selectInput(inputId	=	"brushCOURSEloc",
								label	=	"Location",	choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	actionButton(inputId	=	"brushCOURSEupdate", label = "Update Zoom"))
						),
						fixedRow(
							column(3,	selectInput(inputId	=	"brushCOURSEgpu",
								label	=	"GPU",		choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	selectInput(inputId	=	"brushCOURSEapi",
								label	=	"API",		choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	selectInput(inputId	=	"brushCOURSEqua",
								label	=	"Quality",	choices	=	NULL,	selectize	=	FALSE)
								),
						),
						tableOutput('brushCOURSEsumm'),
						tableOutput('brushCOURSEecdf'),
						plotOutput('brushCOURSEfacet',	height=720),
					)
				),
				tabPanel("Frequency",
					plotOutput('graphFREQfacet',	height=720,
						dblclick	=	ifBRUSH("brushFREQdbl")),
					if (BRUSH)	tagList(
						fixedRow(
							column(3,	selectInput(inputId	=	"brushFREQloc",
								label	=	"Location",	choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	selectInput(inputId	=	"brushFREQgpu",
								label	=	"GPU",		choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	selectInput(inputId	=	"brushFREQapi",
								label	=	"API",		choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	selectInput(inputId	=	"brushFREQqua",
								label	=	"Quality",	choices	=	NULL,	selectize	=	FALSE)
								),
						),
						actionButton(inputId	=	"brushFREQupdate", label = "Update Single Plot"),
						plotOutput('brushFREQfacet',	height=720,
							brush	=	ifBRUSH(brushOpts(id	=	"brushFREQfac", resetOnNew	=	TRUE, direction	=	"x"))),
						if (BRUSH)	tableOutput('brushFREQfacetTAB')
					)
				),
				tabPanel("QQ",
					fixedRow(
						column(3,
							numericInput(inputId	=	"QUANlow",
								label	=	"QQ Percentile - Low",
								value	=	1,	min	=	0,	max	=	100
								),
							),
						column(3,
							numericInput(inputId	=	"QUANhigh",
								label	=	"QQ Percentile - High",
								value	=	99,	min	=	0,	max	=	100
								),
							),
						column(2,
							actionButton(inputId	=	"QUANrefresh",
								label	=	"Refresh QQ Points"
								),
							),
						),
					plotOutput('graphQQfacet',		height=720,
						dblclick	=	ifBRUSH("brushQQdbl"),
						brush	=	ifBRUSH(brushOpts(id	=	"brushQQ", resetOnNew	=	TRUE, direction	=	"xy"))),
					if (BRUSH)	tagList(
						fixedRow(
							column(3,	numericInput(inputId	=	"brushQQlowerX",
								label	=	"Zoom Start (%)",	value	=	0,	step	=	0.1,
								min	=	0,	max	=	100)
								),
							column(3,	numericInput(inputId	=	"brushQQupperX",
								label	=	"Zoom End (%)",	value	=	100,	step	=	0.1,
								min	=	0,	max	=	100)
								),
							column(3,	selectInput(inputId	=	"brushQQloc",
								label	=	"Location",	choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	actionButton(inputId	=	"brushQQupdate", label = "Update Zoom"))
							),
						fixedRow(
							column(3,	numericInput(inputId	=	"brushQQlowerY",
								label	=	"Zoom Lower ms",	value	=	0,	step	=	1,
								min	=	0,	max	=	Inf)
								),
							column(3,	numericInput(inputId	=	"brushQQupperY",
								label	=	"Zoom Upper ms",	value	=	round(1000/15, 1),	step	=	1,
								min	=	0,	max	=	1000)
								),
							),
						fixedRow(
							column(3,	selectInput(inputId	=	"brushQQgpu",
								label	=	"GPU",		choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	selectInput(inputId	=	"brushQQapi",
								label	=	"API",		choices	=	NULL,	selectize	=	FALSE)
								),
							column(3,	selectInput(inputId	=	"brushQQqua",
								label	=	"Quality",	choices	=	NULL,	selectize	=	FALSE)
								),
							),
						tableOutput('brushQQtable'),
						plotOutput('brushQQfacet',	height=720,)
					)
				),
				tabPanel("Consecutive Difference",
					fixedRow(
						column(6,
							textInput(inputId	=	"diffLim",
								label	=	"Consecutive Difference Limits (ms)",
								value	=	paste0(20)
							),
						),
						column(2,
							actionButton(inputId	=	"diffLimRefresh",
								label	=	"Refresh Difference Scale"
							),
						),
					),
					plotOutput('graphDIFFfacet',	height=720,
						dblclick	=	ifBRUSH("brushDIFFdbl")),
					if (BRUSH)	tagList(	fixedRow(
						column(3,	selectInput(inputId	=	"brushDIFFloc",
							label	=	"Location",	choices	=	NULL,	selectize	=	FALSE)
							),
						column(3,	selectInput(inputId	=	"brushDIFFgpu",
							label	=	"GPU",		choices	=	NULL,	selectize	=	FALSE)
							),
						column(3,	selectInput(inputId	=	"brushDIFFapi",
							label	=	"API",		choices	=	NULL,	selectize	=	FALSE)
							),
						column(3,	selectInput(inputId	=	"brushDIFFqua",
							label	=	"Quality",	choices	=	NULL,	selectize	=	FALSE)
							),
					),
					actionButton(inputId	=	"brushDIFFupdate", label = "Update Single Plot"),
					fixedRow(
						column(3, checkboxInput(inputId	=	"diffLimHeat",	label	=	"Enable Heat Map",	value	=	TRUE)	),
						column(5, sliderInput(inputId	=	"brushDIFFalpha", label = "Heatmap Opacity",
							min = 0,	max = 1,	value = 1,	step = 0.1)	),
						column(3, actionButton(inputId	=	"brushDIFFalphup", label = "Apply Opacity")	),
					),
					plotOutput('brushDIFFfacet',	height=720,
						brush	=	ifBRUSH(brushOpts(id	=	"brushDIFFfac", resetOnNew	=	TRUE, direction	=	"xy")))	),
					if (BRUSH)	tagList(
						fillRow(
							textOutput('brushDIFFfacetSEL'),
							tableOutput('brushDIFFfacetRAN'),
							flex = c(2, 3)
						)
					)
				),
				tabPanel("Consecutive Difference (Percentage)",
					fixedRow(
						column(6,
							textInput(inputId	=	"diffPERCLim",
								label	=	"Consecutive Difference Limits (%)",
								value	=	paste0(110)
							),
						),
						column(2,
							actionButton(inputId	=	"diffPERCLimRefresh",
								label	=	"Refresh Difference Scale"
							),
						),
					),
					plotOutput('graphDIFFpercfacet',	height=720,
						dblclick	=	ifBRUSH("brushDIFFpercdbl")),
					if (BRUSH)	tagList(	fixedRow(
						column(3,	selectInput(inputId	=	"brushDIFFpercloc",
							label	=	"Location",	choices	=	NULL,	selectize	=	FALSE)
							),
						column(3,	selectInput(inputId	=	"brushDIFFpercgpu",
							label	=	"GPU",		choices	=	NULL,	selectize	=	FALSE)
							),
						column(3,	selectInput(inputId	=	"brushDIFFpercapi",
							label	=	"API",		choices	=	NULL,	selectize	=	FALSE)
							),
						column(3,	selectInput(inputId	=	"brushDIFFpercqua",
							label	=	"Quality",	choices	=	NULL,	selectize	=	FALSE)
							),
					),
					actionButton(inputId	=	"brushDIFFpercupdate", label = "Update Single Plot"),
					fixedRow(
						column(3, checkboxInput(inputId	=	"diffPERCLimHeat",	label	=	"Enable Heat Map",	value	=	TRUE)	),
						column(5, sliderInput(inputId	=	"brushDIFFpercalpha", label = "Heatmap Opacity",
							min = 0,	max = 1,	value = 1,	step = 0.1)	),
						column(3, actionButton(inputId	=	"brushDIFFpercalphup", label = "Apply Opacity")	),
					),
					plotOutput('brushDIFFpercfacet',	height=720,
						brush	=	ifBRUSH(brushOpts(id	=	"brushDIFFpercfac", resetOnNew	=	TRUE, direction	=	"xy")))	),
					if (BRUSH)	tagList(
						fillRow(
							textOutput('brushDIFFpercfacetSEL'),
							tableOutput('brushDIFFpercfacetRAN'),
							flex = c(2, 3)
						)
					)
				),
				id	=	"graphsFACET",
				type	=	"tabs"
			)
		)
	)
}

tableFIND	<-	function(NAME)	{
	hold	<-	grepl(NAME, list.files())
	if (any(hold))	return(list.files()[which(hold)])
	return(NULL)
}
tableNAME	<-	function(NAME, TITLE = TRUE)	{
	if (!is.null(tableFIND(NAME)))	return(tagList(
		if (TITLE)	h4(gsub(".html", "", NAME)),
		includeHTML(tableFIND(NAME))
	))
}

graphicsHTML	<-	function(id, ..., label = "Configuration Tables")	{
	ns	<-	NS(id)
	
	if (any(c("configPRES", "config60FP") %in% ls(DATA)))	return(
		tabPanel("Graphics Configuration",
			tagList(
				HTML(DATA$configPRES),
				HTML(DATA$config60FP)
			)
		)
	)
	
	if (any(sapply(c("Presets.html", "60 FPS Target.html"), grepl, list.files())))	return(
		tabPanel("Graphics Configuration",
			tagList(
				tableNAME("Presets.html"),
				tableNAME("60 FPS Target.html")
			)
		)
	)
	return(NULL)
}

specsHTML	<-	function(id, ..., label = "Specification Tables")	{
	ns	<-	NS(id)
	if (any(c("specsDESK", "specsTEST") %in% ls(DATA)))	return(
		tabPanel("System Specs",
			tagList(
				tableNAME("CSS.html", FALSE),
				if (VIEW$tabDESK)	tagList(
					HTML(DATA$specsDESK),
					HTML("<br>"),
				),
				if (VIEW$tabTEST)	HTML(DATA$specsTEST)
			)
		)
	)
	
	if (any(sapply(c("Specs_Desktop.html", "Specs_Test.html"), grepl, list.files())))	return(
		tabPanel("System Specs",
			tagList(
				tableNAME("CSS.html", FALSE),
				tableNAME("Specs_Desktop.html", FALSE),
				HTML("<br>"),
				tableNAME("Specs_Test.html", FALSE)
			)
		)
	)
	return(NULL)
}

ui <- function(request)	{fluidPage(
	# titlePanel(paste0("OCAT Frametime Performance Statistics and Graphs")),
	uiOutput('Title'),
	sidebarLayout(
		sidebarPanel(
			# bookmarkButton(label = "Bookmark Data"),
			DataInputUI("dataFile",
				reactive(input$dataInput), reactive(input$gameName), reactive(input$manuRefresh)	),
			#	combined with the ... to the module, this will have it work with these inputs, rather than its own versions
			if (is.character(VIEW$YTlink))	actionButton('tutorial', "Show YouTube Tutorial"),
			tabsetPanel(
				tabPanel("Table Control",
					tabsetPanel(
						tabPanel("Stats",
							checkboxGroupInput(inputId	=	"tabUNIT",	label	=	"FPS or ms",	inline	=	TRUE,
								choices	=	c("FPS", "ms"),	selected	=	"FPS"),
							numericInput(inputId	=	"roundTerm",
								label	=	"Decimals in Tables",	value	=	2,
								min	=	0,	step	=	1
							),
							textInput(inputId	=	"manuPERC",
								label	=	"Additional Percentiles"
							),
							textInput(inputId	=	"manuECDF",
								label	=	"Additional FPS Targets"
							),
							checkboxGroupInput(inputId	=	"tabCOLs",	label	=	"Table Columns",
								choices	=	c(DATA$nameMEAN, DATA$namePERC, DATA$nameECDF),	selected	=	nameDEFs),
							varSelectInput(inputId	=	"datatype",	label	=	"Data Type",	multiple	=	FALSE,
								data	=	data.frame(list(MsBetweenPresents = 0))	)	#	the dummy frame is so "datatype" has an initial value
						),
						tabPanel("Groups",
							checkboxGroupInput(inputId	=	"listGROUPS",	label	=	"Groups to use:",
								choices		=	defs$GROUPS,	selected	=	defs$GROUPS
							),
							rank_list("Order of Groups:", defs$GROUPS, "orderGROUPS"),
							checkboxGroupInput(inputId	=	"listGPU",	label	=	"GPUs to show:"),
							checkboxGroupInput(inputId	=	"listAPI",	label	=	"APIs to show:"),
							checkboxGroupInput(inputId	=	"listQUA",	label	=	"Qualities to show:"),
							checkboxGroupInput(inputId	=	"listLOC",	label	=	"Locations to show:"),
						),
					),
				),
				graphControlUI("graphControl", showGRAPHS = VIEW$GRAPHS,
					reactive(input$FtimeLimitFPS), reactive(input$FtimeLimitMS), reactive(input$FtimeLimitRefresh), reactive(input$listFACETS), reactive(input$datatypeG), reactive(input$facWID)),
				id	=	"tables",
				type	=	"pills"
			),
		width	=	3
		),
		mainPanel(
			tabsetPanel(
				tabPanel("Tables",
					tableExportUI("tableExportUI", VIEW$SEP, VIEW$DOWN)
				),
				GraphTabUI("graphTab", VIEW$GRAPHS, VIEW$BRUSH),
				# if (any(c(VIEW$tabDESK, VIEW$tabTEST)))	specsHTML("specsTABLES", DATA),
				# if (VIEW$gTABLES)						graphicsHTML("graphicsTABLES", DATA),
				id	=	"outputs",
				type	=	"pills"
			),
			width	=	9
		)
	)
)	}