library("shinydashboard")
library("plotly")

dashboardPage(
	dashboardHeader(
		title = "Power Quality - SE"
	),
	dashboardSidebar(
		disable = TRUE
	),
	dashboardBody(
		fluidRow(
			box(
			  title = "Controls", 
			  status = "primary", 
			  solidHeader = TRUE,
			  collapsible =  TRUE,
			  width = 4,
			  uiOutput("locationListCtrl"),
			  sliderInput(
			  	inputId = "freq", 
			  	label = "Frequency (Hz)",
			  	value = c(50, 550), 
			  	min = 0,
			  	max = 3000,
			  	step = 10
			  ),
			  sliderInput(
			  	inputId = "volt", 
			  	label = "Voltage (V)", 
			  	value = c(60, 90),
			  	min = 0,
			  	max = 100,
			  	step = 1
			  ),
			  sliderInput(
			  	inputId = "durn", 
			  	label = "Duration (sec)", 
			  	value = c(0, 100000),
			  	min = 0,
			  	max = 1000000,
			  	step = 1
			  ),
			  uiOutput("dateRangeCtrl"),
			  selectInput(
			  	inputId = "plotType",
			  	label = "Plot Type",
			  	choices = c("Simple Waveform" = "waveformplot",
			  				"Fast Fourier Transform" = "fftplot",
			  				"Variations in Power Quality" = "violinplot", 
			  				"Discrete Wavelet Transformation" = "dwtplot"
			  				)
			  )
			),
			box(
				title = "Events",
				status = "primary",
				solidHeader = TRUE,
				collapsible = TRUE,
				width = 8,
				DT::dataTableOutput("eventsList")
			)
		), fluidRow(
			box(
				title = "Event Details", 
				status = "primary", 
				solidHeader = TRUE,
				collapsible =  TRUE,
				width = 4,
				verbatimTextOutput("eventDetails")
			),
			box(
				title = "Event Plots", 
				status = "primary", 
				solidHeader = TRUE,
				collapsible =  TRUE,
				width = 8,
				plotlyOutput("eventPlot")
			)
		)
	)
)