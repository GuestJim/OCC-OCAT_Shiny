DATA	=	new.env()	;	TABLES	=	new.env()
GRAPH	=	new.env()	;	BRUSH	=	new.env()
VIEW	=	new.env()	#	for tracking what UI elements should be used
#	rather than using super-assignment and pushing variables to Global, I'm putting them into this environment
#	this keeps DATA within the Shiny environment too, so when Shiny ends, the data is apparently removed, which I'm good with
DATA$LOAD	=	FALSE	#	used for tracking if data has been loaded automatically
defs	<-	list(
	GROUPS	=	c("GPU", "API", "Quality", "Location"),
	PERCs	=	c(0.1, 1, 99, 99.9) / 100,
	ECDFs	=	c(30, 60)
	)
#	default values used in several places, now in one location

# DATA$FILE	=	"The Witcher 3 Next-gen 4.01 - PA.RData"
# DATA$FILE	=	"Dying Light 2 - Review.RData"
#	by giving this a file, we can avoid needing to upload a file
VIEW$YTlink	=	"aXd1ll_lNag"	#	ID string for YouTube tutorial video
								#	if not a string, it will be disabled
VIEW$SEP	=	TRUE	#	control if the tables should be separated or not
VIEW$DEVI	=	TRUE	#	control if deviation based statistics are shown
VIEW$GEO	=	FALSE	#	control if geometric mean is shown
VIEW$DOWN	=	FALSE	#	control if it should be possible to download tables
						#	usually it would be !exists("FILE", envir=DATA)
VIEW$UPLOAD	=	TRUE	#	override for if DataInputUI should be shown
VIEW$GRAPHS	=	TRUE	#	control if Graphs should be included or not
VIEW$BRUSH	=	TRUE	#	control if Zoomed/Single Graphs should be included or not
VIEW$FACflip	=	TRUE	#	control if Facet Flipping should be allowed
VIEW$gTABLES	=	TRUE	#	control if HTML tables for Presets and 60 FPS Target configurations are shown
							#	does check if files exists
VIEW$tabDESK	=	TRUE	#	control if Desktop Specifications are shown	
VIEW$tabTEST	=	TRUE	#	control if Test System Specifications are shown	

mergeENV	=	function(env1, env2)	for (obj in ls(env2, all.names = TRUE))	assign(obj, get(obj, env2), envir = env1)

FILES		=	list.files(pattern = "*.env$|*.RData$",	recursive = TRUE)
FILES.names	=	unlist(sapply(FILES, strsplit, "/"), use.names = FALSE)

FILES	=	as.data.frame(cbind(
	TITLE	=	sapply(FILES.names, function(IN) strsplit(IN, " - ")[[1]][1]),
	TYPE	=	gsub(".RData", "", sapply(FILES.names, function(IN) strsplit(IN, " - ")[[1]][2])),
	File	=	FILES.names
))
FILES	=	FILES[order(FILES$TITLE), ]
rownames(FILES) <- NULL
FILES	=	as.list(by(FILES[, c("File")], FILES$TYPE, identity))
FILES	=	list("Review" = FILES$Review, "Performance Analysis" = FILES$PA, "Article" = FILES$Article)

dataLOAD	=	function(name, datapath	=	NULL)	{
	if (is.null(datapath))	datapath	=	name
	if (!file.exists(name))	datapath	=	paste0("Data/", name)

	rm("specsDESK", "specsTEST", "configPRES", "config60FP",
		envir = DATA)
	
	DATA$game	=	gsub("Data/", "", unlist(strsplit(name, " - "))[1])
	if (endsWith(datapath, ".RData"))	{
		mergeENV(DATA, readRDS(datapath))
	}	else	{
		if (!require(readr))	install.packages("readr")
		library(readr)
		DATA$results	=	read_csv(datapath, guess_max = 10, lazy = TRUE, col_select=!all_of(noCOL),	show_col_types = FALSE)
		DATA$results$GPU		=	ordered(DATA$results$GPU,		unique(DATA$results$GPU))
		DATA$results$Quality	=	ordered(DATA$results$Quality,	unique(DATA$results$Quality))
		DATA$results$Location	=	ordered(DATA$results$Location,	unique(DATA$results$Location))
		DATA$results$API		=	ordered(DATA$results$API,		unique(DATA$results$API))

		DATA$GROUPS	=	list(GPU = DATA$results$GPU,	Quality = DATA$results$Quality, API = DATA$results$API,	Location = DATA$results$Location)
		DATA$GPUs	=	LEVs(DATA$results$GPU)
		DATA$QUAs	=	LEVs(DATA$results$Quality)
		DATA$LOCs	=	LEVs(DATA$results$Location)
		DATA$APIs	=	LEVs(DATA$results$API)

		DATA$checkAPI	=	TRUE
		# if (is.na(unique(DATA$GROUPS$API)))		DATA$checkAPI	=	FALSE
		if (anyNA(unique(DATA$GROUPS$API)))		DATA$checkAPI	=	FALSE
		if (!DATA$checkAPI)	DATA$GROUPS$API		=	NULL
	}
	if (!require(tibble))	install.packages("tibble")	;	library(tibble)
	
	DATA$results	<-	tibble(DATA$results)	|>	group_by(GPU, Quality, Location)
	if (exists("API", DATA$results))	DATA$results	<-	DATA$results |> group_by(API, .add = TRUE)
	DATA$LOAD	<-	TRUE
}

noCOL	=	c("ProcessID", "SwapChainAddress", "SyncInterval", "PresentFlags", "AllowsTearing", "PresentMode", "DwmNotified")
nodataCOL	=	c("Application", "Runtime", "WasBatched", "Dropped", "TimeInSeconds", "Width", "Height", "GPU", "Quality", "Location", "API")

source("app_functions.r", local	=	TRUE)

source("app_UI.r", local	=	TRUE)

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
	setBookmarkExclude(c("QUANlow", "QUANhigh", "datatype", "datatypeG", "filtAPI", "filtGPU", "filtLOC", "filtQUA", "filtSEL", "manuECDF", "manuPERC", "flipFACETS", "roundTerm", "FtimeLimitMS", "graphsFACET", "listAPI", "listFACETS", "listGPU", "listGROUPS", "listLOC", "listQUA", "manuRefresh", "FtimeLimitFPS", "FtimeLimitRefresh", "gameName", "outputs", "QUANrefresh", "dataInput", "diffLim", "diffLimHeat", "diffLimRefresh", "diffPERCLim", "diffPERCLimHeat", "diffPERCLimRefresh", "tabCOLs", "tables", "tabUNIT", "brushCOURSEapi", "brushCOURSEgpu", "brushCOURSElength", "brushCOURSEloc", "brushCOURSEqua", "brushCOURSEstart", "brushCOURSEupdate", "brushDIFFalpha", "brushDIFFalphup", "brushDIFFapi", "brushDIFFgpu", "brushDIFFloc", "brushDIFFpercalpha", "brushDIFFpercalphup", "brushDIFFpercapi", "brushDIFFpercgpu", "brushDIFFpercloc", "brushDIFFpercqua", "brushDIFFpercupdate", "brushDIFFqua", "brushDIFFupdate", "brushFREQapi", "brushFREQgpu", "brushFREQloc", "brushFREQqua", "brushFREQupdate", "brushQQapi", "brushQQgpu", "brushQQloc", "brushQQlowerX", "brushQQlowerY", "brushQQqua", "brushQQupdate", "brushQQupperX", "brushQQupperY", "brushSUMMapi", "brushSUMMloc", "brushSUMMqua", "brushSUMMupdate", "dataSelLOAD"))
	#	"dataSelLOAD" being non-zero will load the data immediately

	if (exists("FILE", envir	=	DATA))	{
		dataLOAD(DATA$FILE)
		DATA$LOAD	<-	TRUE
	}
	observeEvent(input$dataInput,	{
		FILE	=	input$dataInput
		dataLOAD(FILE$name, FILE$datapath)
	},	priority	=	10)

	observeEvent(input$dataSelLOAD,	{
		removeTab("outputs",	"System Specs"	)
		removeTab("outputs",	"Graphics Configuration"	)
		FILE	=	input$dataSel
		
		dataLOAD(FILE)
	},	priority	=	10)
		
	observeEvent(input$tutorial,	{
		showModal(	modalDialog(
			HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', VIEW$YTlink,'" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe><br><a href="https://www.youtube.com/watch?v=', VIEW$YTlink, '" target="_blank">YouTube Link</a> (Some features may appear different)')),
			title = "YouTube Tutorial Video",
			easyClose = TRUE,	footer = modalButton("Close")	)	)
	})
	
	refreshMan	=	eventReactive(input$manuRefresh, {
		if (input$gameName	!=	"")	DATA$game	=	input$gameName
	},	ignoreNULL	=	FALSE,	label	=	"refreshMan")


	output$Title	=	renderUI({	titlePanel("Frame Time Statistics and Graphs")	})

	observeEvent(list(input$dataInput, DATA$LOAD, input$manuRefresh, input$dataSelLOAD),	{
		req(DATA$game)
		TITLE	=	paste0(DATA$game, " - Frame Time Statistics")
		if (VIEW$GRAPHS)	TITLE	=	paste0(TITLE, " and Graphs")
		output$Title	=	renderUI({	titlePanel(TITLE)	})
	},	label	=	"updateTitle")

	observeEvent(list(input$dataInput, DATA$LOAD, input$dataSelLOAD), {
		updateTextInput(session,
			inputId	=	"gameName",
			value	=	DATA$game
		)
	})

	# hideTab(inputId	=	"outputs",	target	=	"Graphs")
	hideTab(inputId	=	"graphsFACET",	target	=	"Summary")
	hideTab(inputId	=	"graphsFACET",	target	=	"Course")
	hideTab(inputId	=	"graphsFACET",	target	=	"Frequency")
	hideTab(inputId	=	"graphsFACET",	target	=	"QQ")
	hideTab(inputId	=	"graphsFACET",	target	=	"Consecutive Difference")
	hideTab(inputId	=	"graphsFACET",	target	=	"Consecutive Difference (Percentage)")

	observeEvent(list(input$dataInput, DATA$LOAD, input$dataSelLOAD), {
		#Table Controls
		cullGPUs	=	intersect(DATA$GPUs,	unique(DATA$results$GPU))
		cullQUAs	=	intersect(DATA$QUAs,	unique(DATA$results$Quality))
		cullLOCs	=	intersect(DATA$LOCs,	unique(DATA$results$Location))
		cullAPIs	=	intersect(DATA$APIs,	unique(DATA$results$API))
		updateCheckboxGroupInput(	inputId	=	"listGPU",
			choices		=	cullGPUs,	selected	=	cullGPUs
		)
		updateCheckboxGroupInput(	inputId	=	"listQUA",
			choices		=	cullQUAs,	selected	=	cullQUAs
		)
		updateCheckboxGroupInput(	inputId	=	"listLOC",
			choices		=	cullLOCs,	selected	=	DATA$LOCs
		)
		updateCheckboxGroupInput(	inputId	=	"listAPI",
			label	=	ifelse(is.null(DATA$APIs), "No API Information", "APIs to show:"),
			choices		=	cullAPIs,	selected	=	cullAPIs
		)
		updateVarSelectInput(		inputId	=	"datatype",
			data	=	DATA$results[, !(names(DATA$results) %in% nodataCOL)],
			selected	=	"MsBetweenPresents"
		)
		#Graph Controls
		updateCheckboxGroupInput(	inputId	=	"filtGPU",
			choices		=	cullGPUs,	selected	=	cullGPUs
		)
		updateCheckboxGroupInput(	inputId	=	"filtQUA",
			choices		=	cullQUAs,	selected	=	cullQUAs
		)
		updateCheckboxGroupInput(	inputId	=	"filtLOC",
			choices		=	cullLOCs,	selected	=	cullLOCs
		)
		updateCheckboxGroupInput(	inputId	=	"filtAPI",
			label	=	ifelse(is.null(DATA$APIs), "No API Information", "APIs to show:"),
			choices		=	cullAPIs,	selected	=	cullAPIs
		)
		if (is.null(DATA$APIs))	{
			updateCheckboxGroupInput(	inputId	=	"listFACETS",
				choices		=	c("GPU", "Quality", "Location"),
				selected	=	c("GPU", "Quality", "Location")
			)
			updateCheckboxGroupInput(	inputId	=	"listAPI",
				choices		=	NA,	selected	=	NULL
			)
			updateCheckboxGroupInput(	inputId	=	"filtAPI",
				choices		=	NA,	selected	=	NULL
			)
		}

		updateVarSelectInput(
			inputId	=	"datatypeG",
			data	=	DATA$results[, !(names(DATA$results) %in% nodataCOL)],
			selected	=	"MsBetweenPresents"
		)
		
		{	req(DATA$results)
		showTab(inputId	=	"outputs",	target	=	"Graphs")
		showTab(inputId	=	"graphsFACET",	target	=	"Summary",	select	=	TRUE)
		showTab(inputId	=	"graphsFACET",	target	=	"Course")
		showTab(inputId	=	"graphsFACET",	target	=	"Frequency")
		showTab(inputId	=	"graphsFACET",	target	=	"QQ")
		showTab(inputId	=	"graphsFACET",	target	=	"Consecutive Difference")
		showTab(inputId	=	"graphsFACET",	target	=	"Consecutive Difference (Percentage)")
		}
		
		if (any(VIEW$tabDESK, VIEW$tabTEST)	&	any(
			!is.null(DATA$specsDESK), !is.null(DATA$specsTEST) |
			# c("specsDESK", "specsTEST") %in% ls(DATA) | 
			sapply(c("Specs_Desktop.html", "Specs_Test.html"), grepl, list.files())
			)	)	appendTab("outputs",	specsHTML("specsTABLES", DATA)	)
			
		if (VIEW$gTABLES					&	any(
			!is.null(DATA$configPRES), !is.null(DATA$config60FP) |
			# c("configPRES", "config60FP") %in% ls(DATA) | 
			sapply(c("Presets.html", "60 FPS Target.html"), grepl, list.files())
			)	)	appendTab("outputs",	graphicsHTML("graphicsTABLES", DATA)	)
	})

	output$textTest	<-	renderText("Test")

source("app_tables.r", local	=	TRUE)
if (VIEW$GRAPHS)	source("app_graphs.r", local	=	TRUE)

}

# Create Shiny app ----
shinyApp(ui = ui, server = server, enableBookmarking = "url")