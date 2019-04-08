#####################################
#####################################
#        APPA FIA VISUALS           #
#####################################
# Authors:       Hunter Stanke      #
# Last Updated:  4 February 2019    #
#####################################

# Displays results of intial summarization
# of status/trends in forest condition
# along AT. Leaflet plots embedded in 
# shiny app 

## TO RUN, simply point working directory to hucData folder within Shiny folder. The data in this folder were produced 
##         by the app2.0_preprocessing file, and are set up to run with the app. Once directory is set, either source or 
##         hit Run APP at top of screen.
#
#
#
# 


#setwd("/home/hunter/NPS/app3.0")



####################################         IMPORTS          #################################################
library(shiny)
library(rgdal)
library(leaflet)
library(sp)
library(htmltools)
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(stringr)
library(kableExtra)
library(shinyjs)



####################################       FUNCTIONS         #################################################

#### Standard Error Function
se <- function(x, na.rm=FALSE, proportion = FALSE, sampleSize = NULL) {
  
  # If proportion is false, return standard error of the mean
  if (na.rm) x <- na.omit(x)
  # If true, return standard error of a proportion (x must be single value, the sample proportion) (sampleSize required, total sample size )
  if(proportion == TRUE){
    sem <- sqrt(x * (1-x) / sampleSize)
  } else{
    sem <- sd(x) / sqrt(length(x))
  }
  return(sem)
}


####################################     GLOBAL VARIABLES     #################################################


### Read in region level data
regionDataFull <- read.csv('./ecoregionData/summaryAPPA_app2.0.csv')
# Trim out whitespaces, not sure why they are added, but makes for joining issues
regionDataFull$SUBSECTION <- trimws(regionDataFull$ECOSUBCD)
# Remove year 9999 & make time numeric
regionDataFull <- regionDataFull[regionDataFull$INVYR != 9999,]
#regionDataFull$hucID <- paste(0,trimws(as.character(regionDataFull$hucID)), sep = "")
#regionDataFull$date <- ymd(regionDataFull$date) 

# Make all percentage columns percents
#regionData[,c(10,11,12,13,16,17,18,19)] <- regionData[,c(10,11,12,13,16,17,18,19)] * 100




### Read in species level Data
speciesDataFull = read.csv('./ecoregionData/speciesSummaryAPPA_app2.0.csv')
# Trim out whitespaces, not sure why they are added, but makes for joining issues
speciesDataFull$SUBSECTION <- trimws(speciesDataFull$ECOSUBCD)
# Remove year 9999 & make time numeric
speciesDataFull <- speciesDataFull[speciesDataFull$INVYR != 9999,]
#speciesDataFull$hucID <- paste(0,trimws(as.character(speciesDataFull$hucID)),sep = "")

# Format species names
speciesDataFull$COMMON_NAME <- str_to_title(as.character(speciesDataFull$COMMON_NAME))





## Read in and transform HUC 10 shell (ecoregions)
shellOrig <- readOGR(dsn = "./GIS/shell",
                     layer = "APPA_Ecoregions_USFS_HUC10_Shell_AEA",
                     verbose = FALSE)
proj4string(shellOrig) <- '+proj=aea +lat_1=20 +lat_2=60 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80
+towgs84=0,0,0'
shellOrig <- spTransform(shellOrig, CRSobj = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
shellOrig$SUBSECTION <- as.character(shellOrig$SUBSECTION) # Warnings in join with factors, clunky but it works

# AT Centerline
at <- readOGR(dsn = './GIS/atCenter',
              layer = 'at_centerline', 
              verbose = FALSE)
at <- spTransform(at, CRSobj = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')

# Plot locations
plt <- readOGR(dsn = './GIS/fuzzedPoints', 
               layer = 'fuzzedPoints',
               verbose = FALSE)


# Set up a bunch of variable names
vars <- c(
  'Total TPA of Canopy Stems' = "meanCanopyTPAtotal",
  'Total TPA of Canopy Stems (SE)' = "seCanopyTPAtotal",
  'TPA of Canopy Stems by Species' = 'meanCanopyTPA',
  'TPA of Canopy Stems by Species (SE)' = 'seCanopyTPA',
  'Species Richness' = 'meanRichness',
  'Species Richness (SE)' = 'seRichness',
  'Species Evenness' = 'meanEvenness',
  'Species Evenness (SE)' = 'seEvenness',
  'Annual Recruitment' = "meanTotalRecruit",
  'Annual Recruitment (SE)' = "seTotalRecruit",
  'Annual Mortality' = 'meanTotalMortality',
  'Annual Mortality (SE)' = 'seTotalMortality',
  'Annual Recruitment by Species' = 'meanAnnualRecruit',
  'Annual Recruitment by Species(SE)' = 'seAnnualRecruit',
  'Annual Mortality by Species' = 'meanAnnualMortality',
  'Annual Mortality by Species (SE)' = 'seAnnualMortality',
  'Annual DBH Growth by Species' = 'meanAnnualDIAGrowth',
  'Annual DBH Growth by Species (SE)' = 'seAnnualDIAGrowth',
  'Percent Annual DBH Growth by Species' = 'meanAnnualDBHChange',
  'Percent Annual DBH Growth by Species (SE)' = 'seAnnualDBHChange',
  'Percent Annual BA Growth by Species' = "meanAnnualBAChange",
  'Percent Annual BA Growth by Species (SE)' = "seAnnualBAChange",
  'Annual BA Growth by Species' = 'meanAnnualBAGrowth',
  'Annual BA Growth by Species (SE)' = 'seAnnualBAGrowth',
  'Sapling TPA' = 'meanRegenTotal',
  'Sapling TPA (SE)' = 'seRegenTotal',
  'Sapling TPA by Species' = 'meanRegenTPA',
  'Sapling TPA by Species (SE)' = 'seRegenTPA',
  'Total BA of live stems' = 'meanLiveBA',
  'Total BA of live stems (SE)' = 'seLiveBA',
  'BA of live stems by Species' = 'meanLiveBAspec',
  'BA of live stems by Species (SE)' = 'seLiveBAspec',
  'Total Estimated Carbon Mass (kg/acre)' = 'meanCarbonMass',
  'Total Estiamted Carbon Mass (kg/acre) (SE)' = 'seCarbonMass',
  'Estimated Carbon Mass by Species' = 'meanCarbonMassspec',
  'Estimated Carbon Mass by Species (SE)' = 'seCarbonMassspec',
  "Snags Per Acre" = "meanSnagTPA",
  "Snags Per Acre (SE)" = "seSnagTPA",
  "Snag BA Per Acre" = "meanSnagBA",
  "Snag BA Per Acre (SE)" = "seSnagBA",
  "Snag Volume Per Acre (cu. m)" = "meanSnagVol",
  "Snag Volume Per Acre (cu. m) (SE)" = "seSnagVol",
  "Percent Stems that are Snags" = 'meanPropDead',
  "Percent Stems that are Snags (SE)" = 'sePropDead',
  "Percent Large Stems that are Snags" = 'meanPropDeadLarge',
  "Percent Large Stems that are Snags (SE)" = 'sePropDeadLarge',
  'CWD Volume Per Acre (cu. m)' = 'meanCWDVol',
  'CWD Volume Per Acre (cu. m) (SE)' = 'secWDVol',
  'Percent Area in Pole Class' = 'propPole',
  'Percent Area in Pole Class (SE)' = 'sepropPole',
  'Percent Area in Mature Class' = 'propMature',
  'Percent Area in Mature Class (SE)' = 'sepropMature',
  'Percent Area in Late Seral Class' = 'propLate',
  'Percent Area in Late Seral Class (SE)' = 'sepropLate',
  'Percent Area in Mosaic Class' = 'propMosaic',
  'Percent Area in Mosaic Class (SE)' = 'sepropMosaic',
  'Percent Invasive Cover' = 'meanInvCover',
  'Percent Invasive Cover (SE)' = 'seInvCover'
  
)




####################################            UI          ###################################################

# Define the UI
# Use a fluid Bootstrap layout
ui <-   fluidPage(    
  
  theme="https://www.nps.gov/lib/bootstrap/3.3.2/css/nps-bootstrap.min.css", 
  style="padding: 0px",
  title="NCRN Water Quality",
  
  column(12, id="NPSBanner", style="margin: 0px",
         useShinyjs(),
         tags$head(includeScript ("https://www.nps.gov/common/commonspot/templates/js/federated-analytics.js")),
         tags$head(tags$script(
           'type = "text/javascript"',' var ss = document.createElement("link"); ss.type="text/css"; ss.rel="stylesheet"; 
                      ss.href = window.self === window.top ? "NCRN.css" : "NCRNframe.css"; document.getElementsByTagName("head")[0].appendChild(ss);'
         )),
         tags$head(HTML( '<link rel="icon", href="AH_small_flat_4C_12x16.png", type="image/png" />')),
         
         div(
           h1(style="background-color: black; color: white; height: 125px; padding: 10px; margin: 0px",
              
              HTML('<img src="ah_large_black.gif", style="float:right; padding-right:25px"/>',
                   'Status & Trends in Forest Condition <br> Along the Appalachian Trail'
              ))
         )
  ),
  
  
  
  # Give the page a title

  
  mainPanel(
    leafletOutput("mymap", height = "850")),
  
  sidebarPanel(wellPanel(style='overflow: hidden',

                
                # Select for mapped variable
                selectInput('variable', label = 'Mapped Variable', vars),
                
                
                # Only show this panel if variable is summarised at species level, does not include invasives
                conditionalPanel(condition = "input.variable == 'meanCanopyTPA' || 
                                 input.variable == 'seCanopyTPA' ||
                                 input.variable == 'meanAnnualRecruit' || 
                                 input.variable == 'seAnnualRecruit' || 
                                 input.variable == 'meanAnnualMortality' ||
                                 input.variable == 'seAnnualMoratlity' ||
                                 input.variable == 'meanAnnualDIAGrowth' ||
                                 input.variable == 'seAnnualDIAGrowth' ||
                                 input.variable == 'meanAnnualBAGrowth' ||
                                 input.variable == 'seAnnualBAGrowth' ||
                                 input.variable == 'meanAnnualDBHChange' ||
                                 input.variable == 'seAnnualDBHChange' ||
                                 input.variable == 'meanAnnualBAChange' ||
                                 input.variable == 'seAnnialBAChange' ||
                                 input.variable == 'meanRegenTPA' ||
                                 input.variable == 'seRegenTPA' ||
                                 input.variable == 'meanLiveBA' ||
                                 input.variable == 'seLiveBA' ||
                                 input.variable == 'meanLiveBAspec' ||
                                 input.variable == 'seLiveBAspec' ||
                                 input.variable == 'meanCarbonMassspec' ||
                                 input.variable == 'seCarbonMassspec'",
                                 
                                 selectInput(inputId = 'species',
                                             label = "Species",
                                             str_sort(unique(speciesDataFull$COMMON_NAME[is.na(speciesDataFull$cover)]), locale = 'en'),
                                             selected = "Northern Red Oak")),
                
                # Display invasive species dropdown
                conditionalPanel(condition = "input.variable == 'meanInvCover' || input.variable == 'seInvCover'",
                                 selectInput(inputId = 'invasive',
                                             label = "Invasive Species",
                                             str_sort(unique(speciesDataFull$COMMON_NAME[!is.na(speciesDataFull$cover)]), locale = 'en'),
                                             selected = "Japanese Barberry")),
                br(),
                
                # Slider to select year range of interest
                sliderInput("INVYR", 
                            label = 'Inventory Year Range  (Most Recent Subset within Range)', 
                            min = min(regionDataFull$INVYR), 
                            max = max(regionDataFull$INVYR),
                            value = c(min(regionDataFull$INVYR), max(regionDataFull$INVYR)),
                            step = 1, sep = "",
                            animate = TRUE),
                # h1("
                #    "),
                
                br(),
                
                # Select for color scheme
                selectInput("color", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div"))),
                            selected = "YlOrBr"),
                
                # Link to R Markdown
                actionLink('link', em(strong('See Metadata')), onclick = "window.open('analysisMarkdown.html')"),

                h3("
                   "),
                
                # Button to download data
                downloadLink('downloadDataSP', em('Download Spatial Data')),
                
                h3("
                   "),
                
                downloadLink('downloadData', em('Download All Data'))),
                
               br(),
               
               
                wellPanel(
                  h4(em(strong("Top 5 Ecoregions"))),
                  tableOutput('shellTable')
                )
                
                
                ))





#####################################         SERVER          #################################################


# Define the server code
server <- function(input, output, session) {
  
  # Initial maps, no data, just basemaps. This version is never actually seen, because there is a default variable selection when the app is launched
  #   and reactive data below will kick in and plot
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addPolygons(data = shellOrig,
                  weight = 1.5,
                  opacity = .9,
                  color = "grey",
                  dashArray = "",
                  fillOpacity = 0.7,
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#7777",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) %>%
    
      # Base groups
      addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Imagery (default)") %>%
      addProviderTiles(providers$Esri.WorldTopoMap, group = "Topo") %>%
      # Layers control
      addLayersControl(
        overlayGroups = c("Ecoregion Subsections", 'AT Centerline', 'Fuzzed FIA Plots'),
        baseGroups = c("Imagery (default)", "OSM", "Topo"),
        options = layersControlOptions(collapsed = FALSE),
        position = 'bottomleft') %>%
      setView(lng = -77.737907, lat =  40, zoom = 6) %>%
      hideGroup('Fuzzed FIA Plots') %>%
      hideGroup('AT Centerline')
    
    })
  

  

  ### All below is wrapped in observer so that when user input changes, the current extent does not change and the map does not redraw entirely
  observe({
    # Setting selected variable name to a variable for use in subestting
    variable <- input$variable
    fancyName <- names(vars)[vars == variable]
    year1 <- input$INVYR[1]
    year2 <- input$INVYR[2]
    color <- input$color
    species <- input$species
    invasive <- input$invasive
    
    # Cut down each of the datasets to reflect user input on year ranges and species
    # ONLY DISPLAYS MOST RECENT DATA FROM EACH PLOT
    regionData <- regionDataFull %>%
      filter(as.numeric(INVYR) >= year1 & as.numeric(INVYR) <= year2) %>%
      mutate(date = ymd(date)) %>%
      group_by(plotID) %>%
      filter(date == max(date))
    
    speciesData <- speciesDataFull %>%
      filter(as.numeric(INVYR) >= year1 & as.numeric(INVYR) <= year2) %>%
      mutate(date = ymd(date)) %>%
      group_by(plotID) %>%
      filter(date == max(date))
    
    
    #############  Calculate each variable (mean & standard error) based on user selection  ################################
    ## (ONLY CALCULATES SELECTED VARIABLE)
    
    ### TPA OF CANOPY SPECIES
    if (variable == 'meanCanopyTPA' | variable == 'seCanopyTPA'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanCanopyTPA = mean(tpaCanopy, na.rm = TRUE),
                  seCanopyTPA = se(tpaCanopy, na.rm = TRUE),
                  count = n())
    }
    
    ### TOTAL TPA OF CANOPY
    if (variable == 'meanCanopyTPAtotal' | variable == 'seCanopyTPAtotal'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanCanopyTPAtotal = mean(tpaCanopy, na.rm = TRUE),
                  seCanopyTPAtotal = se(tpaCanopy, na.rm = TRUE),
                  count = n())
    }
    
    ### SPECIES RICHNESS
    if (variable == 'meanRichness' | variable == 'seRichness'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanRichness = mean(richness, na.rm = TRUE),
                  seRichness = se(richness, na.rm = TRUE),
                  count = n())
    }
    
    ### SPECIES EVENNESS
    if (variable == 'meanEvenness' | variable == 'seEvenness'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanEvenness = mean(evenness, na.rm = TRUE),
                  seEvenness = se(evenness, na.rm = TRUE),
                  count = n())
    }
    
    ### ANNUAL RECRUITEMENT by SPECIES
    if (variable == 'meanAnnualRecruit' | variable == 'seAnnualRecruit'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanAnnualRecruit = mean(annualRecruit, na.rm = TRUE) * 100,
                  seAnnualRecruit = se(annualRecruit, na.rm = TRUE) * 100,
                  count = n())
    }
    
    ### TOTAL ANNUAL RECRUITEMENT
    if (variable == 'meanTotalRecruit' | variable == 'seTotalRecruit'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanTotalRecruit = mean(annualRecruit, na.rm = TRUE) * 100,
                  seTotalRecruit = se(annualRecruit, na.rm = TRUE) * 100,
                  count = n())
    }
    
    ### ANNUAL MORTALITY by SPECIES
    if (variable == 'meanAnnualMortality' | variable == 'seAnnualMortality'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanAnnualMortality = mean(annualMortality, na.rm = TRUE) * 100,
                  seAnnualMortality = se(annualMortality, na.rm = TRUE) * 100,
                  count = n())
    }
    
    ### TOTAL ANNUAL MORTALITY
    if (variable == 'meanTotalMortality' | variable == 'seTotalMortality'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanTotalMortality = mean(annualMortality, na.rm = TRUE) * 100,
                  seTotalMortality = se(annualMortality, na.rm = TRUE) * 100,
                  count = n())
    }
    
    ### ANNUAL DBH GROWTH
    if (variable == 'meanAnnualDIAGrowth' | variable == 'seAnnualDIAGrowth'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanAnnualDIAGrowth = mean(annualDBHGrowth, na.rm = TRUE),
                  seAnnualDIAGrowth = se(annualDBHGrowth, na.rm = TRUE),
                  count = n())
    }
    
    ### ANNUAL DBH CHANGE
    if (variable == 'meanAnnualDBHChange' | variable == 'seAnnualDBHChange'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanAnnualDBHChange = mean(annualDBHChange, na.rm = TRUE)*100,
                  seAnnualDBHChange = se(annualDBHChange, na.rm = TRUE)*100,
                  count = n())
    }
    
    ### ANNUAL BA GROWTH
    if (variable == 'meanAnnualBAGrowth' | variable == 'seAnnualBAGrowth'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanAnnualBAGrowth = mean(annualBAGrowth, na.rm = TRUE),
                  seAnnualBAGrowth = se(annualBAGrowth, na.rm = TRUE),
                  count = n())
    }
    
    ### ANNUAL BA CHANGE
    if (variable == 'meanAnnualBAChange' | variable == 'seAnnualBAChange'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanAnnualBAChange = mean(annualBAChange, na.rm = TRUE)*100,
                  seAnnualBAChange = se(annualBAChange, na.rm = TRUE)*100,
                  count = n())
    }
    
    ### SAPLING TPA by SPECIES
    if (variable == 'meanRegenTPA' | variable == 'seRegenTPA'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanRegenTPA = mean(tpaRegen, na.rm = TRUE),
                  seRegenTPA = se(tpaRegen, na.rm = TRUE),
                  count = n())
    }
    
    ### TOTAL SAPLING TPA
    if (variable == 'meanRegenTotal' | variable == 'seRegenTotal'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanRegenTotal = mean(tpaRegen, na.rm = TRUE),
                  seRegenTotal = se(tpaRegen, na.rm = TRUE),
                  count = n())
    }
    
    ### TOTAL LIVE BASAL AREA
    if (variable == 'meanLiveBA' | variable == 'seLiveBA'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanLiveBA = mean(baLive, na.rm = TRUE),
                  seLiveBA = se(baLive, na.rm = TRUE),
                  count = n())
    }
    
    ### TOTAL LIVE BASAL AREA BY SPECIES
    if (variable == 'meanLiveBAspec' | variable == 'seLiveBAspec'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanLiveBAspec = mean(baLive, na.rm = TRUE),
                  seLiveBAspec = se(baLive, na.rm = TRUE),
                  count = n())
    }
    
    ### TOTAL LIVE CARBON MASS
    if (variable == 'meanCarbonMass' | variable == 'seCarbonMass'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanCarbonMass = mean(carbonLive, na.rm = TRUE),
                  seCarbonMass = se(carbonLive, na.rm = TRUE),
                  count = n())
    }
    
    ### TOTAL LIVE CARBON MASS BY SPECIES
    if (variable == 'meanCarbonMassspec' | variable == 'seCarbonMassspec'){
      data <- speciesData %>%
        filter(COMMON_NAME == species) %>%
        group_by(SUBSECTION) %>%
        summarise(meanCarbonMassspec = mean(carbonLive, na.rm = TRUE),
                  seCarbonMassspec = se(carbonLive, na.rm = TRUE),
                  count = n())
    }
    
    ### SNAG ABUNDANCE
    if (variable == 'meanSnagTPA' | variable == 'seSnagTPA'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanSnagTPA = mean(tpaDead, na.rm = TRUE),
                  seSnagTPA = se(tpaDead, na.rm = TRUE),
                  count = n())
    }
    
    ### SNAG BA
    if (variable == 'meanSnagBA' | variable == 'seSnagBA'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanSnagBA = mean(baDead, na.rm = TRUE),
                  seSnagBA = se(baDead, na.rm = TRUE),
                  count = n())
    }
    
    ### SNAG VOLUME
    if (variable == 'meanSnagVol' | variable == 'seSnagVol'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanSnagVol = mean(volumeDead, na.rm = TRUE),
                  seSnagVol = se(volumeDead, na.rm = TRUE),
                  count = n())
    }
    
    ### Proportion of stems that are dead
    if (variable == 'meanPropDead' | variable == 'sePropDead'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanPropDead = mean(propDead, na.rm = TRUE),
                  sePropDead = se(propDead, na.rm = TRUE),
                  count = n())
    }
    
    ### Proportion of stems that are dead and LARGE
    if (variable == 'meanPropDeadLarge' | variable == 'sePropDeadLarge'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanPropDeadLarge = mean(propDeadLarge, na.rm = TRUE),
                  sePropDeadLarge = se(propDeadLarge, na.rm = TRUE),
                  count = n())
    }
    
    ### COARSE WOODY DEBRIS VOLUME
    if (variable == 'meanCWDVol' | variable == 'secWDVol'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(meanCWDVol = mean(volCWD, na.rm = TRUE),
                  secWDVol = se(volCWD, na.rm = TRUE),
                  count = n())
    }
    
    ### PROPORTION IN POLE
    if (variable == 'propPole' | variable == 'sepropPole'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(propPole = length(stage[stage=='pole']) / length(stage) * 100,
                  sePropPole = se(propPole/100, sampleSize = n(), proportion = TRUE) * 100,
                  count = n()) 
    }
    
    ### PROPORTION IN MATURE
    if (variable == 'propMature' | variable == 'sepropMature'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(propMature = length(stage[stage=='mature']) / length(stage) * 100,
                  sepropMature = se(propMature/100, proportion = TRUE, sampleSize = n()) * 100,
                  count = n())
    }
    
    ### PROPORTION IN LATE SERAL
    if (variable == 'propLate' | variable == 'sepropLate'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(propLate = length(stage[stage=='late']) / length(stage) *100,
                  sepropLate = se(propLate/100, sampleSize = n(), proportion = TRUE) *100,
                  count = n())
    }
    
    ### PROPORTION IN MOSAIC
    if (variable == 'propMosaic' | variable == 'sepropMosaic'){
      data <- regionData %>%
        group_by(SUBSECTION) %>%
        summarise(propMosaic = length(stage[stage=='mosaic']) / length(stage) *100,
                  sepropMosaic = se(propMosaic/100, sampleSize = n(), proportion = TRUE) *100,
                  count = n())
    }
    
    ### INVASIVE COVER
    if (variable == 'meanInvCover' | variable == 'seInvCover'){
      data <- speciesData %>%
        filter(COMMON_NAME == invasive) %>%
        group_by(SUBSECTION) %>%
        summarise(meanInvCover = mean(cover, na.rm = TRUE) ,
                  seInvCover = se(cover, na.rm = TRUE),
                  count = n())
    }
    
    
    
    # Avoid overwriting data when variable names are changed back and forth
    shell <- shellOrig
    
    # Join data of selected variable to spatial polygons dataframe
    shell@data <- left_join(shellOrig@data,
                            data,
                            by = 'SUBSECTION')
    
    # Set up color bins (use data rather than huc so that breaks are constant across time)
    if (variable %in% names(speciesData)) {
      pal <- colorBin(input$color, domain = data[[variable]], bins = 4, pretty = TRUE)
      
    }else{
      pal <- colorBin(input$color, domain = data[[variable]], bins = 4, pretty = TRUE)
    }
    

    
    ###### FOR ECOREGION
    # Set up labels
    # Displays differently depending on whether the variable chosen is a standard error or mean
    if (str_sub(fancyName, -4, -1) != "(SE)") {
      labels <- sprintf(
        paste("<strong>", fancyName ,": %g</strong></br>SE: %g</strong></br># Plots: %g</strong><br/>Subsection: %s"),
        round(as.numeric(shell[[variable]]),2), round(as.numeric(shell@data[,17]),2), shell@data[,18], shell$SUBSECTI_1) %>%
        lapply(htmltools::HTML)
      
    } else{
      labels <- sprintf(
        paste("<strong>", fancyName ,": %g</strong></br>Mean: %g</strong></br># Plots: %g</strong><br/>Subsection ID: %s"),
        round(as.numeric(shell[[variable]]),2), round(as.numeric(shell@data[,16]),2), shell@data[,18], shell$SUBSECTI_1) %>%
        lapply(htmltools::HTML)
    }
    
    
    
    # Update leaflet map
    leafletProxy('mymap', data = shell) %>%
      clearShapes() %>%
      clearControls() %>%
      
      # Set up map panes
      addMapPane("polys", zIndex = 410) %>%
      addMapPane("at", zIndex = 420) %>%


    # Add Ecoregions
      addPolygons(fillColor = ~pal(shell[[variable]]),
                  weight = 1.5,
                  opacity = .9,
                  color = "grey",
                  dashArray = "",
                  fillOpacity = 0.7,
                  options = pathOptions(pane = "polys"),
                  group = "Ecoregion Subsections",
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#7777",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE),
                  label = labels,
                  labelOptions = labelOptions(
                    style = list("font-weight" = "normal", padding = "3px 8px"),
                    textsize = "15px",
                    direction = "auto")) %>%
      # Add AT Centerline
      addPolylines(data = at,
                   weight = 2, 
                   group = 'AT Centerline',
                   color = 'darkblue',
                   options = pathOptions(pane = 'at')) %>%
      # Add FIA Plots
      addCircles(data = plt,
                 group = 'Fuzzed FIA Plots',
                 radius = .1,
                 weight = .1,
                 fillOpacity = 1,
                 color = 'black',
                 options = pathOptions(pane = 'at')) %>%
      
      # Add legend
      addLegend(pal = pal,
                values = ~shell[[variable]],
                opacity = 0.7,
                title = fancyName,
                position = "bottomright",
                na.label = 'No Data')
    
    
   ###### Make table output ######

    output$shellTable <- function() {
      req(shell)
      
      shellTable <- shell@data[!is.na(shell@data[[variable]]),]
      shellTable <- shellTable[sort(shell@data[[variable]], decreasing = TRUE, index.return = TRUE)$ix[1:5],]
      shellTable <- shellTable[,c(12, 16, 17)]
      names(shellTable) <- c('Subsection', 'Mean', 'SE')
      shellTable$Mean <- round(shellTable$Mean, 1)
      shellTable$SE <- round(shellTable$SE, 1)
        

      shellTable %>%
        knitr::kable("html", row.names = FALSE) %>%
        kable_styling("striped", full_width = TRUE) #%>%
        #scroll_box(height = '500px', width = '500px')
    }

    
    ###### ALLOW USERS TO DOWNLOAD DATA
    output$downloadData <- downloadHandler(
      filename = 'test.zip',
      content = function(fname) {
        
        # Write out dataframes
        write.csv(speciesDataFull, file = "speciesDataFull.csv")
        write.csv(regionDataFull, file = "regionDataFull.csv")
        write.csv(as.data.frame(shell), file = "dataDisplayed.csv")
        
        
        # Zip together all exported dataframes
        zip(zipfile=fname, files=c("speciesDataFull.csv","regionDataFull.csv", "dataDisplayed.csv"))
        
      },
      contentType = "application/zip")
    
    # Spatial Data Download
    output$downloadDataSP <- downloadHandler(
      filename = "shapefile.zip",
      content = function(file) {
        # create a temp folder for shp files
        temp_shp <- tempdir()
        # write shp files
        writeOGR(shell, temp_shp, "dataDisplayedSP", "ESRI Shapefile", 
                 overwrite_layer = TRUE)
        # zip all the shp files
        zip_file <- file.path(temp_shp, "dataDisplayedSP_shp.zip")
        shp_files <- list.files(temp_shp,
                                "dataDisplayedSP",
                                full.names = TRUE)
        
        # the following zip method works for me in linux but substitute with whatever method working in your OS
        zip_command <- paste("zip -j",
                             zip_file,
                             paste(shp_files, collapse = " "))
        system(zip_command)
        # copy the zip file to the file argument
        file.copy(zip_file, file)
        # remove all the files created
        file.remove(zip_file, shp_files)
      }
    )
    
    

    })


  
}




#################################################   RUN APP   #############################################3

# Return a Shiny app object
shinyApp(ui = ui, server = server)
