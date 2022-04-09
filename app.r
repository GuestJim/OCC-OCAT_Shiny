library(shiny)
# setwd("C:/Users/Jim/Documents/OCC/@Reviews/@OCAT R Scripts/OCC-OCAT_Shiny")
# setwd("E:/Users/Jim/My Documents/OCC/@Reviews/@OCAT R Scripts/OCC-OCAT_Shiny")
# runApp()
# options(shiny.error = browser)
# options(shiny.reactlog=TRUE) 
options(shiny.maxRequestSize = 100*1024^2)
#	Shiny has a normal limit of 5 MB, so this raises it to 100 MB

DATA	=	new.env()
DATA$LOAD	=	FALSE	#used for tracking if data has been loaded automatically
GRAPH	=	new.env()
TABLES	=	new.env()
BRUSH	=	new.env()
#	rather than using super-assignment and pushing variables to Global, I'm putting them into this environment
#	this keeps DATA within the Shiny environment too, so when Shiny ends, the data is apparently removed, which I'm good with

# DATA$FILE	=	"Chorus - Epic.csv.bz2"
# DATA$FILE	=	"Crysis Remastered - Small.csv.bz2"
# DATA$FILE	=	"Dying Light 2 - All.csv.bz2"
# DATA$FILE	=	"Dying Light 2 - All.RData"
#	by giving this a file, we can avoid needing to upload a file

mergeENV	=	function(env1, env2)	for (obj in ls(env2, all.names = TRUE))	assign(obj, get(obj, env2), envir = env1)

dataLOAD	=	function(name, datapath	=	NULL)	{
	if (is.null(datapath))	datapath	=	name

	DATA$game	=	unlist(strsplit(name, " - "))[1]
	if (endsWith(datapath, ".RData"))	{
		mergeENV(DATA, readRDS(datapath))
	}	else	{
		require(readr)
		DATA$results	=	read_csv(datapath, guess_max = 10, lazy = TRUE, col_select=!all_of(noCOL),	show_col_types = FALSE)
		DATA$results$GPU			=	ordered(DATA$results$GPU,		unique(DATA$results$GPU))
		DATA$results$Quality		=	ordered(DATA$results$Quality,	unique(DATA$results$Quality))
		DATA$results$Location		=	ordered(DATA$results$Location,	unique(DATA$results$Location))
		DATA$results$API			=	ordered(DATA$results$API,		unique(DATA$results$API))
		
		DATA$GROUPS	=	list(GPU = DATA$results$GPU,	Quality = DATA$results$Quality, API = DATA$results$API,	Location = DATA$results$Location)
		DATA$GPUs	=	LEVs(DATA$results$GPU)
		DATA$QUAs	=	LEVs(DATA$results$Quality)
		DATA$LOCs	=	LEVs(DATA$results$Location)
		DATA$APIs	=	LEVs(DATA$results$API)
		
		DATA$checkAPI	=	TRUE
		if (is.na(unique(DATA$GROUPS$API)))		DATA$checkAPI	=	FALSE
		if (!DATA$checkAPI)	DATA$GROUPS$API		=	NULL
	}
}

noCOL	=	c("ProcessID", "SwapChainAddress", "SyncInterval", "PresentFlags", "AllowsTearing", "PresentMode", "DwmNotified")
nodataCOL	=	c("Application", "Runtime", "WasBatched", "Dropped", "TimeInSeconds", "Width", "Height", "GPU", "Quality", "Location", "API")

source("app_functions.r", local	=	TRUE)

if	(exists("FILE", envir=DATA))	{
	source("app_UI_static.r", local	=	TRUE)
}	else	{
	source("app_UI_upload.r", local	=	TRUE)
}

# Define server logic to summarize and view selected dataset ----
server <- function(input, output, session) {
	if (exists("FILE", envir	=	DATA))	{
		dataLOAD(DATA$FILE)
		DATA$LOAD	=	TRUE
	}
	observeEvent(input$dataInput,	{
		FILE	=	input$dataInput
		dataLOAD(FILE$name, FILE$datapath)
	},	priority	=	10)

	refreshMan	=	eventReactive(input$manuRefresh, {
		if (input$gameName	!=	"")	DATA$game	=	input$gameName
	},	ignoreNULL	=	FALSE,	label	=	"refreshMan")


	output$Title	=	renderUI({	titlePanel("Frame Time Statistics and Graphs")	})

	observeEvent(list(input$dataInput, DATA$LOAD, input$manuRefresh),	{
		req(DATA$game)
		output$Title	=	renderUI({	titlePanel(paste0(DATA$game, " - Frame Time Statistics and Graphs"))	})
	},	label	=	"updateTitle")

	observeEvent(list(input$dataInput, DATA$LOAD), {
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

	observeEvent(list(input$dataInput, DATA$LOAD), {
		#Table Controls
		updateCheckboxGroupInput(	inputId	=	"listGPU",
			choices		=	DATA$GPUs,	selected	=	DATA$GPUs
		)
		updateCheckboxGroupInput(	inputId	=	"listQUA",
			choices		=	DATA$QUAs,	selected	=	DATA$QUAs
		)
		updateCheckboxGroupInput(	inputId	=	"listLOC",
			choices		=	DATA$LOCs,	selected	=	DATA$LOCs
		)
		updateCheckboxGroupInput(	inputId	=	"listAPI",
			label	=	ifelse(is.null(DATA$APIs), "No API Information", "APIs to show:"),
			choices		=	DATA$APIs,	selected	=	DATA$APIs
		)
		updateVarSelectInput(		inputId	=	"datatype",
			data	=	DATA$results[, !(names(DATA$results) %in% nodataCOL)],
			selected	=	"MsBetweenPresents"
		)
		#Graph Controls
		updateCheckboxGroupInput(	inputId	=	"filtGPU",
			choices		=	DATA$GPUs,	selected	=	DATA$GPUs
		)
		updateCheckboxGroupInput(	inputId	=	"filtQUA",
			choices		=	DATA$QUAs,	selected	=	DATA$QUAs
		)
		updateCheckboxGroupInput(	inputId	=	"filtLOC",
			choices		=	DATA$LOCs,	selected	=	DATA$LOCs
		)
		updateCheckboxGroupInput(	inputId	=	"filtAPI",
			label	=	ifelse(is.null(DATA$APIs), "No API Information", "APIs to show:"),
			choices		=	DATA$APIs,	selected	=	DATA$APIs
		)
		GRAPH$FILT	<-	reactiveVal(1:nrow(DATA$results))

		updateVarSelectInput(
			inputId	=	"datatypeG",
			data	=	DATA$results[, !(names(DATA$results) %in% nodataCOL)],
			selected	=	"MsBetweenPresents"
		)
		showTab(inputId	=	"outputs",	target	=	"Graphs")
		showTab(inputId	=	"graphsFACET",	target	=	"Summary",	select	=	TRUE)
		showTab(inputId	=	"graphsFACET",	target	=	"Course")
		showTab(inputId	=	"graphsFACET",	target	=	"Frequency")
		showTab(inputId	=	"graphsFACET",	target	=	"QQ")
		showTab(inputId	=	"graphsFACET",	target	=	"Consecutive Difference")
	})

source("app_tables.r", local	=	TRUE)
source("app_graphs.r", local	=	TRUE)

}

# Create Shiny app ----
shinyApp(ui = ui, server = server)