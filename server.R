# installs a package if not available on the system
usePackage <- function(p) {
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}

# please ensure that these are installed properly.
# especially sqldf - it has the habit of exitting with status 0.
usePackage("shinydashboard")
usePackage("shiny")
usePackage("ggplot2")
#usePackage("plotly")
usePackage("DT")
#usePackage("sqldf")
usePackage("stats")
usePackage("wavelets")
usePackage("stringr")


# get data for the event selected (from ui)
getDataForSpecificEvent <- function(folderPath, eventDate, eventTime, eventType){
	# creating filepath to read data from file
	# useful in retrieving file -- replacing : and . with -
	eventTime <- gsub(":|\\.", "-", eventTime)
	eventTime <- substr(eventTime, 1, 12)

	filePath <- paste0(folderPath, eventDate, " (T ", eventTime, ")", eventType, " Waveform.csv")
	if(!file.exists(filePath)){
		filePath <- paste0(folderPath, eventDate, " (T ", eventTime, ")", eventType, " Waveform_End.csv")
	}
	print(filePath)
	eventData <- read.csv(filePath, header = TRUE, stringsAsFactors = FALSE, skip = 21)
	if(ncol(eventData) == 1){
		eventData <- read.csv(filePath, header = TRUE, stringsAsFactors = FALSE, skip = 20, sep = ";", dec = ",")
	}
	return(eventData)
}

# filter data depending on the vaues of the control widgets (from ui) 
filterEvents <- function(eventsData, location, freqStart, freqEnd, voltStart, voltEnd, durnStart, durnEnd, dateRangeStart, dateRangeEnd){
	# getting only one location data points
	eventsData <- subset(eventsData, Location == location)
	# removing data points with NA magnitude or NA duration
	eventsData <- na.omit(eventsData)
	# extracting numeric value of magnitude -- initial value contains '%'' and 'Hz' strings
	eventsData$Magnitude_num <- as.numeric(stringr::str_extract(eventsData$Magnitude, "[[:digit:]]+\\.[[:digit:]]+"))
	
	# frequency and voltage filters are independent of each other
	# warning --  this is not universal, needs to be modified later
	eventsData_freq <- subset(eventsData, Event == "Under-frequency")
	eventsData_volt <- subset(eventsData, Event == "Voltage Sag")

	eventsData_freq <- subset(eventsData_freq, Magnitude_num >= freqStart & Magnitude_num <= freqEnd)
	eventsData_volt <- subset(eventsData_volt, Magnitude_num >= voltStart & Magnitude_num <= voltEnd)

	# merging the independently filtered datasets
	eventsData <- rbind(eventsData_freq, eventsData_volt)

	# filter by duration
	eventsData <- subset(eventsData, Duration >= durnStart & Duration <= durnEnd)
	# filter by date ranges
	eventsData <- subset(eventsData, Date >= dateRangeStart & Date <= dateRangeEnd)
	# removing artificial column created
	eventsData$Magnitude_num <- NULL
	return(eventsData)
}

setColNames <- function(plotDF){
	if(ncol(plotDF) == 8){
		colnames(plotDF) <- c("milliseconds", "voltage_ne", "voltage_l1n", "current_l1", "current_l2", "current_l3", "current_n", "current_e")
	}else if(ncol(plotDF) == 7){
		colnames(plotDF) <- c("milliseconds", "voltage_ne", "voltage_l1n", "current_l1", "current_l2", "current_l3", "dig1")
	}else if(ncol(plotDF) == 9){
		colnames(plotDF) <- c("milliseconds", "voltage_ne", "voltage_l1n", "current_l1", "current_l2", "current_l3", "current_n", "current_e", "none")
	}else if(ncol(plotDF) == 6){
		colnames(plotDF) <- c("milliseconds", "voltage_ne", "current_l1", "current_l2", "current_e", "none")
	}else{
		colnames(plotDF)[1] <- "milliseconds"
		colnames(plotDF)[2] <- "voltage_ne"
	}
	return(plotDF)
}

# plots a simple waveform from original dataset
generateWaveform <- function(plotDF){
	plot <- ggplot(data = plotDF, aes(x = milliseconds, y = voltage_ne)) +
			geom_line(colour = "dodgerblue") +
			ggtitle("Simple Waveform") +
			xlab("Time (ms)") + 
			ylab("Voltage") +
			theme_bw()
	return(plot)
}

# plots the fast fourier transform voltage vs frequency in hertz
generateFFTPlot <- function(plotDF){
	plotDF$freq <- 1000/plotDF$milliseconds
	plotDF$fft_val <- Re(fft(plotDF$voltage_ne))

	plot <- ggplot(data = plotDF, aes(x = freq, y = fft_val)) +
			geom_line(colour = "gray50") + 
			ggtitle("Fast Fourier Transform") +
			xlab("Frequency (Hz)") + 
			ylab("FFT") +
			theme_bw()
	#plot <- ggplotly(plot)
	return(plot)
}

# plot violinplot (a version of boxplot) to indicate the variation of power quality
generateViolinPlot <- function(plotDF){
	site <- plotDF$Location[1]
	plotDF$Magnitude <- as.numeric(stringr::str_extract(plotDF$Magnitude, "[[:digit:]]+\\.[[:digit:]]+"))
	plot <- ggplot(data = plotDF, aes(factor(Event), Magnitude), labels = FALSE) + 
			geom_violin(aes(fill = factor(Event), outlier.colour = "red")) +
			labs(fill = "") + 
			ggtitle(paste0("Variations in Power Quality for site: ", site)) +
			xlab("Event Type") + 
			ylab("Magnitude") +
			theme_bw()
	return(plot)
}

generateDWTPlot <- function(plotDF){
	dwtObject <- dwt(plotDF$voltage_ne, n.levels = 1)
	plotDF <- cbind.data.frame(plotDF$milliseconds, dwtObject@W$W1)
	names(plotDF) <- c("Time", "Coefficients")
	plot <- ggplot(data = plotDF, aes(y = Coefficients, x = Time)) +
			geom_line(colour = "gray50") +
			ggtitle(paste0("Discrete Wavelet Transformations")) + 
			theme_bw()
	return(plot)
}

# generic function to create plot
generatePlotForSpecificEvent <- function(plotDF, listOfEvents, plotType){
	if(plotType == "fftplot"){
		plot <- generateFFTPlot(plotDF)
	}else if(plotType == "waveformplot"){
		plot <- generateWaveform(plotDF)
	}else if(plotType == "violinplot"){
		plot <- generateViolinPlot(listOfEvents)
	}else if(plotType == "dwtplot"){
		plot <- generateDWTPlot(plotDF)
	}
	return(plot)
}



server <- function(input, output, session){
	dataFolderPath <<- "./data/selectedsites/"
	# events -- more must be added
	events <- c("Voltage Sag", "Sag became Major Sag", "Under-frequency", "Waveshape Change", "Digital 1 High", "High Frequency Impulse", "Voltage Swell", "Phase Current Trigger", "Over-frequency")
	# load head data
	listOfEvents <- read.csv(paste0(dataFolderPath, "EventsList.csv"), header = TRUE, stringsAsFactors = FALSE)
	# get only problematic data
	listOfEvents <- subset(listOfEvents, Event %in% events)
	# modifying data ever so slightly 
	listOfEvents$Date <- as.Date(as.character(listOfEvents$Date))
	listOfEvents$Time <- substr(listOfEvents$Time, 3, 18)
	# getting location list
	locationsList <- unique(listOfEvents$Location)
	output$locationListCtrl <- renderUI({
	    selectInput(
	    	inputId = "location",
	    	label = "Location",
	    	choices = locationsList
	    	)
	})

	# dynamic date range control
	output$dateRangeCtrl <- renderUI({
		listOfEvents <- subset(listOfEvents, Location == input$location)
		dateStart <- min(as.Date(as.character(listOfEvents$Date)), na.rm = TRUE)
		dateEnd <- max(as.Date(as.character(listOfEvents$Date)), na.rm = TRUE)

		dateRangeInput(
					inputId = "dateRange",
					label = "Date Range",
					start = dateStart,
					end = dateEnd
				)
	})
	
	# render events list
	output$eventsList <- DT::renderDataTable(
						filterEvents(listOfEvents, input$location, input$freq[1], input$freq[2], input$volt[1], input$volt[2], input$durn[1], input$durn[2], input$dateRange[1], input$dateRange[2])[c("Event", "Date", "Time", "Magnitude", "Duration")],
						options = list(
									lengthMenu = list(c(10, 20, -1), c('10', '20', 'All')),
									pageLength = 10,
									searching = FALSE
								),
						selection = "single"
					)

	# render event plot
	output$eventPlot <-  renderPlot({
		if(length(input$eventsList_rows_selected)){
			selectedEvent <- input$eventsList_rows_selected[length(input$eventsList_rows_selected)]
			selectedDF  <- listOfEvents[selectedEvent, ]
			plotDF <- getDataForSpecificEvent(folderPath = dataFolderPath, eventDate = selectedDF$Date, eventTime = selectedDF$Time, eventType = selectedDF$Event)
			plotDF <- setColNames(plotDF)
			plot <- generatePlotForSpecificEvent(plotDF, listOfEvents, input$plotType)
			return(plot)
		}
	})

	# render individual event info
	output$eventDetails <- renderPrint({
		if(length(input$eventsList_rows_selected)){
			selectedEvent <- input$eventsList_rows_selected[length(input$eventsList_rows_selected)]
			selectedDF <- listOfEvents[selectedEvent, ]
			cat(paste("Location:", selectedDF$Location, "\n"))
			cat(paste("ID:", selectedDF$ID, "\n"))
			cat(paste("Event:", selectedDF$Event, "\n"))
			cat(paste("Date:", selectedDF$Date, "\n"))
			cat(paste("Time:", selectedDF$Time, "\n"))
			cat(paste("Magnitude:", selectedDF$Magnitude, "\n"))
			cat(paste("Duration:", selectedDF$Duration, "(sec)\n"))
		}
	})
}

