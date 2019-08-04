chooserInput <- function(inputId, leftLabel, rightLabel, leftChoices, rightChoices,
                         size = 5, multiple = FALSE) {
  
  leftChoices <- lapply(leftChoices, tags$option)
  rightChoices <- lapply(rightChoices, tags$option)
  
  if (multiple)
    multiple <- "multiple"
  else
    multiple <- NULL
  
  tagList(
    singleton(tags$head(
      tags$script(src="chooser-binding.js"),
      tags$style(type="text/css",
                 HTML(".chooser-container { display: inline-block; }")
      )
    )),
    div(id=inputId, class="chooser",
        div(class="chooser-container chooser-left-container",
            tags$select(class="left", size=size, multiple=multiple, leftChoices)
        ),
        div(class="chooser-container chooser-center-container",
            icon("arrow-circle-o-right", "right-arrow fa-3x"),
            tags$br(),
            icon("arrow-circle-o-left", "left-arrow fa-3x")
        ),
        div(class="chooser-container chooser-right-container",
            tags$select(class="right", size=size, multiple=multiple, rightChoices)
        )
    )
  )
}

registerInputHandler("shinyjsexamples.chooser", function(data, ...) {
  if (is.null(data))
    NULL
  else
    list(left=as.character(data$left), right=as.character(data$right))
}, force = TRUE)
##globle datas
#nameToID <- read.table("data/sheets/nameToID.txt", header = TRUE,sep = "\t", fileEncoding="UCS-2LE")
# states <- c("Alabama" ,"Alaska" ,"Arizona" ,"Arkansas", "California" ,"Colorado" ,"Connecticut",
#             "Delaware", "Florida", "Georgia" ,"Hawaii", "Idaho" ,"Illinois" ,"Indiana" ,"Iowa", "Kansas",
#             "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts" ,"Michigan", "Minnesota", "Mississippi",
#             "Missouri" ,"Montana", "Nebraska" ,"Nevada", "New Hampshire" ,"New Jersey" ,"New Mexico",
#             "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma" ,"Oregon", "Pennsylvania",
#             "Rhode Island", "South Carolina" ,"South Dakota", "Tennessee", "Texas", "Utah" ,"Vermont", "Virginia",
#             "Washington", "West Virginia", "Wisconsin" ,"Wyoming")

# shiny ui
ui = fluidPage(
  shinyjs::useShinyjs(),
  h4("Raking Weight Application"),
  br(),
  br(),
  
  fluidRow(
    column(6, fileInput("surveyDt", "Individual Survey Data Upload", width = "70%")),
    column(6,textInput("split",label = "Data Split" ,value = "", width = "50%"),
           div(p("Please enter the # of last column for each feature, separate by comma"), style="font-size:70%"))
  ),
  
  fluidRow(
    column(12, textInput("names",label = NULL ,value = "", width = "70%", placeholder = "Please input the names of the features(seperated by comma)"))
  ),
  
  
  hr(),##############
  
  fluidRow(column(9, uiOutput("feature")),column(3, uiOutput("usr_type"))),
  
  fluidRow(column(12,uiOutput("usr_reqss"),
          #div(p("Please specify how you want to process this feature"), style="font-size:70%"), 
          div(em("For factor: c('A', 'B')='A+B';c('D', 'E') = 'D+E', meaning aggregate factor A, B together as A+B, the same for D,E"), style="font-size:70%"),
          div(em("For numeric: 0,20, 30, 40, meaning cut the numerical value into [0,20), [20,30), [30,40)"), style="font-size:70%"),
          div(em("For logic: chinese, japanese; french,germany; Asian, European, meaning put column chinese, japanese together as asian, column french, germany together as european"), 
              style="font-size:70%"))),
  
  br(),
  fluidRow(column(6, DT::dataTableOutput('originalFeat')),column(6, DT::dataTableOutput('processFeat'))),
  
  fluidRow(column(6, offset = 6, actionButton("nexts", "NEXT", width = "20%"), actionButton("pass", "PASS", width = "20%"),
                  actionButton("go", "Go", width = "20%"), actionButton("done", "DONE", width = "20%"))),
  
  fluidRow(column(5, offset = 7, actionButton("intera", "INTERACTION", width = "30%"))),
  fluidRow(column(5, offset = 7, actionButton("internext", "INTER NEXT", width = "30%"), actionButton("same", "SAME", width = "30%"))),
  fluidRow(column(8, uiOutput('fthv'))),
  fluidRow(column(6, textInput("inters",label = NULL ,value = "", width = "100%", 
                               placeholder = "Please input the interaction feature, separate by comma"))),
  fluidRow(column(8, uiOutput('finalft'))),
  
  fluidRow(column(3,offset = 9, downloadButton('dlSurvey', 'Download Survey Data'))),
  
  hr(),###############
  fluidRow(column(8, uiOutput("feature2")), column(4, passwordInput("password", NULL, value = "", placeholder = "Your Census Data Key"))),
  #fluidRow(column(9, passwordInput("password", "Please input your census data key", value = ""))),
  fluidRow(column(9, textInput("sf1",label = NULL ,value = "", width = "100%", placeholder = "Please input the sf1 fields IDs to pull, separate by comma")),
           column(3,
                  div(helpText(a("Click here for sf1 field IDs",href="https://api.census.gov/data/2010/sf1/variables.html", target="_blank"), style = "font-size: 70%")
                  ))),

  fluidRow(column(9, textInput("acs",label = NULL ,value = "", width = "100%", placeholder = "Please input the acs fields IDs to pull, separate by comma")),
           column(3,
                  div(helpText(a("Click Here for acs field IDs",href="https://api.census.gov/data/2015/acs1/variables.html", target="_blank"), style = "font-size: 70%")
                  ))),
  br(),
  fluidRow(column(4, selectizeInput('level',NULL, choices = c('state',"county","tract","block group","block","cdp","congressional_district"),
                                    options = list(placeholder = 'Level',onInitialize = I('function() { this.setValue(""); }')))),
           column(4,selectizeInput('state',NULL, choices = state.name,
                  options = list(placeholder = 'States',onInitialize = I('function() { this.setValue(""); }')))),
           column(4,textInput("county",label = NULL ,value = "", width = "80%", placeholder = "County Code"))),
  
  
  br(),
  fluidRow(column(6, DT::dataTableOutput('processedFeat')), column(6, DT::dataTableOutput("census"))),
  br(),
  fluidRow(column(6, div(em("Please specify the order of the pid to match the survey data")), div(uiOutput("mychooser")))),
  br(),
  fluidRow(column(12, uiOutput("usr_combine"), 
           div(em("For example: chinese, japanese; french,germany; Asian, European, meaning put column chinese, japanese together as asian, column french, 
                  germany together as european"), style="font-size:70%"))),
  
  #fluidRow(column(6, DT::dataTableOutput("descript"))),
  fluidRow(column(6, DT::dataTableOutput("proCensus"))),
  
  fluidRow(column(8,offset = 4, actionButton("orderonly", "orderOnly",width = "20%"), actionButton("combineonly", "cmbnOnly",width = "20%"), 
                  actionButton("both", "BOTH",width = "20%"), actionButton("either", "EITHER", width = "20%"))),
  fluidRow(column(5, offset = 7, actionButton("nextss", "NEXT", width = "20%"), actionButton("done2", "DONE", width = "20%"))),
  fluidRow(column(3,offset = 9, downloadButton('dlCensus', 'Download Census Data'))),
  hr(),
  fluidRow(DT::dataTableOutput("weights")),
  fluidRow(column(3,offset = 9, downloadButton('dlWeights', 'Download Weights'))),
  hr(),##
  fluidRow(column(3, verbatimTextOutput("value1")),
           column(3, verbatimTextOutput("value2")),
           column(3, verbatimTextOutput("value3"))
           )
)


# server function
server = function(input, output, session){
  ##use shinyjs to disable the next, pass, done button if certain condition doesn't met 
  observe({
    shinyjs::toggleState("pass", !is.null(input$type) && input$type != "")
  })
  
  observe({
    shinyjs::toggleState("nexts", !is.null(input$type) && input$type != "")
  })
  
  observe({
    shinyjs::toggleState("done", input$pass[1]+input$nexts[1] == length(unlist(strsplit(input$names,','))))
  })
  
  observe({
    shinyjs::toggleState("county", !(input$level %in% c('state',"cdp","congressional_district")))
  })
  
  ##initialize the reactiveValues
  values <- reactiveValues(wholePDT = vector("list"), wholePDTfnl = vector('list'),types = vector("character"), wholeCDT = vector("list"), counts = 0, counts2 = 1, counts3=0, counts4=1)
  ####get the individual survey data, split the data into single features, then name each feature by user input
  sData <- reactive({
    req(input$surveyDt)
    req(input$split)
    req(input$names)
    ##get the data
    dat <- read.csv(input$surveyDt$datapath, header = TRUE)
    ##get rid of NAs
    dat <- na.omit(dat)
    
    ##separate dat, one category/or one interaction combination one dataframe
    cats <- as.numeric(unlist(strsplit(input$split,",")))
    for (i in 1: length(cats)-1){
      cats <- c(cats, cats[i]+1)
    }
    
    cats <- sort(c(1, cats))
    datas <- vector("list", length(cats)/2)
    for (i in 1:(length(cats)/2)){
      datas[[i]] <- dat[,cats[2*i-1]:cats[2*i]]
    }
    namess <- unlist(strsplit(input$names, ','))
    names(datas) <- namess
    return(datas)
    })
  
  
  ##################################START DATA PRE-PROCESSING########################################################
  ##show which feature we are processing right now
  output$feature <- renderUI({
    namess <- unlist(strsplit(input$names,','))
    em(paste0("Pre-processing: ", namess[input$nexts[1]+input$pass[1]+1-input$intera[1]+input$same[1]]))
  })
  
  ##when usr hit NEXT, clear all the type and reqss
  output$usr_type <- renderUI({
    if(input$intera|input$go){selectizeInput(
      'type',NULL, choices = c("factor", "numeric", "logics"),
      options = list(
        placeholder = 'Feature Type',
        onInitialize = I('function() { this.setValue(""); }')))}else{
          selectizeInput(
            'type',NULL, choices = c("factor", "numeric", "logics"),
            options = list(
              placeholder = 'Feature Type',
              onInitialize = I('function() { this.setValue(""); }')))
        }
    })
    
  output$usr_reqss <- renderUI({
    if(input$intera|input$go){textInput("reqss",label = NULL ,value = "", width = "70%")}else{
      textInput("reqss",label = NULL ,value = "", width = "70%", placeholder = "Please specify how you want to process this feature")
    }})  
  
  ##show the original feature data being processed
  output$originalFeat <- DT::renderDataTable({
    namess <- unlist(strsplit(input$names, ','))
    #if (input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]==0){
    #  data <- as.data.frame(sData()[[namess[1]]])
    # if(dim(data)[2] == 1){colnames(data) <- namess[1]}
    #}else{
    #  data <- as.data.frame(sData()[[namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]+input$go[1]]]])
    #  if(dim(data)[2] == 1){colnames(data) <- namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]+input$go[1]]}
    #}
    data <- as.data.frame(sData()[[namess[1+input$go[1]-input$intera]]])
    if(dim(data)[2] == 1){colnames(data) <- namess[1+input$go[1]-input$intera]}
    
    
    
    datatable(head(data), class = 'cell-border stripe compact',
              caption = 'Table 1: Original Feature Data.', options = list(dom = 't', scrollX = TRUE))
  })
  
  ##process the feature 
  observeEvent(input$nexts,{
    req(input$type)
    req(input$reqss)
    
    ##get the individual data
    namess <- unlist(strsplit(input$names,','))
    iDT <- as.data.frame(sData()[[namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]]])
    
    ##pre-process the data
    if(input$type == "factor"){rqs = input$reqss
    }else if(input$type == "numeric"){
      rqs = as.numeric(unlist(strsplit(input$reqss,",")))
    }else if(input$type == "logics"){
      rq <- strsplit(unlist(strsplit(input$reqss,";")), ",")
      rqs <- lapply(rq, str_trim)
      #rqs = strsplit(unlist(strsplit(input$reqss,";")), ",")         
    }
    
    pDt <- byType(dat= iDT, types = input$type, reqs=rqs)
    if(dim(pDt)[2] == 1){colnames(pDt) <- namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]}
    #put the processed data in the list wholePDT
    values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- pDt
  
    print(input$nexts[1])
    print(input$intera[1])
    print(input$pass[1])
    print(input$same[1])
    print(input$internext[1])
    print(namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]])
    
    names(values$wholePDT)[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]
    View(values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]])
  })
  
  ##give the names to the extra interaction
  observeEvent(input$internext,{
    req(input$type)
    req(input$reqss)
    
    ##get the individual data
    namess <- unlist(strsplit(input$names,','))
    iDT <- as.data.frame(sData()[[namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]]])
    
    ##pre-process the data
    if(input$type == "factor"){rqs = input$reqss
    }else if(input$type == "numeric"){
      rqs = as.numeric(unlist(strsplit(input$reqss,",")))
    }else if(input$type == "logics"){
      rq <- strsplit(unlist(strsplit(input$reqss,";")), ",")
      rqs <- lapply(rq, str_trim)
      #rqs = strsplit(unlist(strsplit(input$reqss,";")), ",")         
    }
    
    pDt <- byType(dat= iDT, types = input$type, reqs=rqs)
    if(input$internext[1]+values$counts2 == input$nexts[1]+input$pass[1]+input$internext[1]){values$counts <- values$counts +1}else{
      values$counts <- 1
      values$counts2 <- input$nexts[1]+input$pass[1]
    }
    
    if(dim(pDt)[2] == 1){colnames(pDt) <- namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]}
    #put the processed data in the list wholePDT
    values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- pDt
    
    print(values$counts)
    print(values$counts2)
    print(input$nexts[1])
    print(input$intera[1])
    print(input$pass[1])
    print(input$same[1])
    print(input$internext[1])
    print(paste0(namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]], values$counts))
    
    names(values$wholePDT)[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- 
      paste0(namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]], values$counts)
    View(values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]])
  })
  
  ##show the processed feature data
  output$processFeat <- DT::renderDataTable({
    if(input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1] != 0){
      datatable(head(as.data.frame(values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]])), class = 'cell-border stripe compact',
                caption = 'Table 2: Processed Feature Data.', options = list(dom = 't', scrollX = TRUE)) 
    }
  })
  
  ##put all the processed data together in a list, so can go find the census data
  observeEvent(input$pass,{
    namess <- unlist(strsplit(input$names,','))
    print(values$counts)
    print(values$counts2)
    print(input$nexts[1])
    print(input$intera[1])
    print(input$pass[1])
    print(input$same[1])
    print(input$internext[1])
    print(namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]])
    
    sdf <- as.data.frame(sData()[[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]])
    if(dim(sdf)[2] == 1){colnames(sdf) <- 
      namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]}
    
    ##values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- sData()[[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]]
    values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- sdf
    names(values$wholePDT)[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]]
    View(values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]])
  })
  
  observeEvent(input$same,{
    if(input$same[1]+values$counts4 == input$nexts[1]+input$pass[1]+input$same[1]){values$counts3 <- values$counts3 +1}else{
      values$counts3 <- 1
      values$counts4 <- input$nexts[1]+input$pass[1]
    }    
    
    namess <- unlist(strsplit(input$names,','))
    
    print(values$counts3)
    print(values$counts4)
    print(input$nexts[1])
    print(input$intera[1])
    print(input$pass[1])
    print(input$same[1])
    print(input$internext[1])
    print(paste0(namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]], values$counts3))
    
    values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- values$wholePDT[[input$nexts[1]+input$pass[1]+input$internext[1]+input$same[1]-1]]
    names(values$wholePDT)[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]] <- 
      paste0(namess[input$nexts[1]+input$pass[1]-input$intera[1]+input$same[1]+input$internext[1]], values$counts3)
    View(values$wholePDT[[input$nexts[1]+input$pass[1]+input$same[1]+input$internext[1]]])
  })
  
  #observeEvent(input$nexts,{
  #  print(input$pass[1])
  #  print(input$nexts[1])
  #  values$wholePDT[[input$nexts[1]+input$pass[1]]] <- piDt()
  #  View(values$wholePDT[[input$nexts[1]+input$pass[1]]])
  #})
  
  ##output what features we have after process the features need to get interaction
  output$fthv <- renderUI({
    em(paste0("All the features we have now:  ", names(values$wholePDT)))
  })
  
  ##get the interaction for the features
  observe({
    req(input$inters)
    iterfts <- strsplit(unlist(strsplit(input$inters,";")), ",")
    interlist <- getInteraction(values$wholePDT,iterfts)
    ##append all the interaction back to values$wholePDT
    values$wholePDT <- c(values$wholePDT, interlist)
  })
  
  ##a radio button list of all the fetures to select the final features needed
  output$finalft <- renderUI({
    checkboxGroupInput("fnlfts", h6("Check the features to include"), names(values$wholePDT))
  })
  
  ##get the final features to use
  observe({
    req(input$fnlfts)
    for (names in input$fnlfts){
      values$wholePDTfnl[[names]]<-values$wholePDT[[names]]
      }
  })
  
  ################################################################??????????############################
  observe({
    #names(values$wholePDT) <- input$names
    values$types[input$nexts[1]+input$pass[1]+1] <- input$type
  })
  
  
  ################################################START GETTING THE CENSUS DATA####################################
  ##show which feature we are getting the census data right now
  output$feature2 <- renderUI({
    namess <- unlist(strsplit(input$names,','))
    em(paste0("Getting Census Data for ", namess[input$nextss[1]+1]))
  })
  
  ##?????????????????change all datas into logical form
  LSData <- eventReactive(input$done,{
    namess <- unlist(strsplit(input$names,','))
    wholeLDT <- vector("list", length(namess))
    names(wholeLDT) <- namess
    for (i in 1: length(namess)){
      if(values$types[i] != "logics"){
        wholeLDT[[i]] <- ade4::acm.disjonctif(as.data.frame(values$wholePDTfnl[[i]]))
      }else{
        wholeLDT[[i]] <- values$wholePDTfnl[[i]]
      }
    }
    return(wholeLDT)
  })

  
  ##show the processed feature data which is gonna get the census data
  output$processedFeat <- DT::renderDataTable({
    namess <- unlist(strsplit(input$names,','))
    data <- as.data.frame(LSData()[[namess[input$nextss[1]+1]]])
    colnames(data) <- gsub(pattern = ".*\\.", replacement = "", x = colnames(data))
    datatable(head(data), class = 'cell-border stripe compact',
              caption = 'Table 3: Processed Feature Data.', options = list(dom = 't', scrollX = TRUE))
  })
  
  ##############################################Getting the Census Data############################################
  #observe({
  #  shinyjs::toggleState("nextss", !is.null(input$type) && input$type != "")
  #})
  
  ##getting the census data
  censusDt <- reactive({
    req(input$password)
    req(input$level)
    req(input$state)
    ##set the parameters
    keys <- input$password
    statefip <- substr(UScensus2010::nameTofips('',input$state)[1],1,2)
    levels <- ifelse(input$level == 'state', 'county', input$level)
    
    ##getting the census data(depends on if it's sf1 or ACS)
    if(input$sf1 != ""){
      print(input$sf1)
      sfdt <- CensusAPI2010s(trimws(unlist(strsplit(input$sf1,','))), statefip, levels, keys, summaryfile ="sf1")
    }
    
    if(input$acs != ""){
      acsdt <- CensusAPI2010s(trimws(unlist(strsplit(input$acs,','))), statefip, levels, keys, summaryfile ="ACS")
      if(levels == 'congressional_district'){
        acsdt <- acsdt[,-c(1:2)]
      }  ##else{acsdt <- acsdt[,-1]}
    }
    
    ##put those two census data together
    if(input$sf1 != ""){
      if(input$acs != ""){
        cdt <- cbind(sfdt, acsdt)
      }else{cdt <- sfdt}
    }else{cdt <- acsdt}
    
    ##combine or filter census data according to the levels
    if (input$level == 'state'){
      cdt <- colSums(cdt[,-1])
    }else if(input$level == 'cdp'){
      cdt <- cdt[,-1]
    }else if(input$level == 'congressional_district'){
      cdt <- cdt[,-c(1:2)]
    }else{
      if(!is.null(input$county)){
        counties <- trimws(unlist(strsplit(input$county,',')))
        ##pcounties <- sprintf("%03d",as.numeric(counties))
        ##sc <- paste0(statefip, pcounties)
        View(cdt)
        print(colnames(cdt))
        print(cdt$fips[1])
        cdt <- cdt %>% filter(cdt$fips %in% counties)
      }
      cdt <- cdt[,-1]
    }
    
    return(cdt)
  })
  
  ##specify the chooser for ordering the census data fields
  output$mychooser <- renderUI({
    #varss <- ifelse(input$names[input$nextss[1]+1] == "age", colnames(censusDt()), varss())
    if(input$nextss){
      chooserInput("orders", "Original Ordering", "Re-Ordering",
                   colnames(censusDt()), c(), size = 10, multiple = TRUE)
    }else{chooserInput("orders", "Original Ordering", "Re-Ordering",
                       colnames(censusDt()), c(), size = 10, multiple = TRUE)}
  })
  
  ##specify how to combine the columns in census data
  output$usr_combine <- renderUI({
    if(input$nextss){textInput("combine",label = NULL ,value = "", width = "70%")}else{
      textInput("combine",label = NULL ,value = "", width = "70%", placeholder = "Please specify which columns you want to combine")
    }})  
  
  ##??????????????get the names of the fields 
  #fieldsDt <- reactive({
  #  req(input$names)
  #  req(input$done)
  #  namess <- unlist(strsplit(input$names,','))
  #  if(namess[input$nextss[1]+1] != "age"){
  #    nameToID %>% select(division, pid) %>% filter(pid %in% colnames(censusDt()))
  #  }
  #})
  
  ##process the census data, to order and combine 
  observeEvent(input$either,{
    print("1")
    values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]] <- censusDt()
    View(values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]])
  })
  
  observeEvent(input$orderonly,{
    print("2")
    cCensus <- censusDt()[,(input$orders$right)]
    values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]] <- cCensus
    View(values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]])
  })
  
  observeEvent(input$combineonly,{
    print("3")
    #combine 
    #noWhite <- str_replace_all(input$combine, fixed(" "), "")
    rqs <- strsplit(unlist(strsplit(input$combine,";")), ",")
    rqss <- lapply(rqs, str_trim)
    
    dtL <- oCensus
    len <- length(rqss)
    
    dtTemp <- vector("list", length(rqss)) 
    for(j in 1:(length(rqss)-1)){
      #select the columns needs to be add together
      dtTem <- dtL %>% select(one_of(rqss[[j]])) %>% rowSums()
      dtTem <- as.data.frame(dtTem)
      colnames(dtTem) <- rqss[[len]][j]
      dtTemp[[j]] <- dtTem
      
      dtL <- dtL %>% select(-one_of(rqss[[j]]))
    }
    
    dtTemp[[len]] <- dtL
    cCensus <- do.call("cbind", dtTemp)
    values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]] <- cCensus
    View(values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]])
  })
  
  observeEvent(input$both,{
    print("4")
    #change the order and select the one needed 
    oCensus <- censusDt()[,(input$orders$right)]
    #combine 
    #noWhite <- str_replace_all(input$combine, fixed(" "), "")
    rqs <- strsplit(unlist(strsplit(input$combine,";")), ",")
    rqss <- lapply(rqs, str_trim)
    
    dtL <- oCensus
    len <- length(rqss)
    
    dtTemp <- vector("list", length(rqss)) 
    for(j in 1:(length(rqss)-1)){
      #select the columns needs to be add together
      dtTem <- dtL %>% select(one_of(rqss[[j]])) %>% rowSums()
      dtTem <- as.data.frame(dtTem)
      colnames(dtTem) <- rqss[[len]][j]
      dtTemp[[j]] <- dtTem
      
      dtL <- dtL %>% select(-one_of(rqss[[j]]))
    }
    
    dtTemp[[len]] <- dtL
    cCensus <- do.call("cbind", dtTemp)
    values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]] <- cCensus
    View(values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]])
  })
  
  
  
  
  #pCensusDt <- reactive({
    #req(input$orders)
    #req(input$combine)
  #  print("1")
  #  if(is.null(input$combine)){
  #    if(is.null(input$orders)){
  #     cCensus <- censusDt()
  #    }else{
  #      cCensus <- censusDt()[,(input$orders$right)]
  #    }
  #  }else{
  #    #change the order and select the one needed 
  #    oCensus <- censusDt()[,(input$orders$right)]
  #    #combine 
  #    #noWhite <- str_replace_all(input$combine, fixed(" "), "")
  #    rqs <- strsplit(unlist(strsplit(input$combine,";")), ",")
  #    rqss <- lapply(rqs, str_trim)
  #    
  #    dtL <- oCensus
  #    len <- length(rqss)
  #    
  #    dtTemp <- vector("list", length(rqss)) 
  ##    for(j in 1:(length(rqss)-1)){
  #      #select the columns needs to be add together
  #      dtTem <- dtL %>% select(one_of(rqss[[j]])) %>% rowSums()
  #      dtTem <- as.data.frame(dtTem)
  #      colnames(dtTem) <- rqss[[len]][j]
  #      dtTemp[[j]] <- dtTem
  #      
  #      dtL <- dtL %>% select(-one_of(rqss[[j]]))
  #    }
  #    
  #    dtTemp[[len]] <- dtL
  #    cCensus <- do.call("cbind", dtTemp)
  #  }
    
    #put all cCensus together
  #  print(input$nextss[1])
  #  View(cCensus)
  #  values$wholeCDT[[input$nextss[1]+1]] <- cCensus
  #  View(values$wholeCDT[[input$nextss[1]+1]])
  #  return(cCensus)
  #})
  
  ##output the original census data
  output$census <- DT::renderDataTable({
    data <- censusDt() 
    datatable(head(data), class = 'cell-border stripe compact',
              caption = 'Table 4: Original Census Data', options = list(dom = 't', scrollX = TRUE))
  })
  
  
  #observe({
  #  namess <- unlist(strsplit(input$names,','))
  #  shinyjs::toggleState("descript",namess[input$nextss[1]+1] != "age")
  #})
  
  
  ##output the description of the census data column name 
  #output$descript <- DT::renderDataTable({
  #  data <- fieldsDt() 
  #  colnames(data) <- c("Description", "Pid")
  #  datatable(data, class = 'cell-border stripe compact',
  #            caption = 'Table 5: Description for Census Data', options = list(dom = 't', scrollY = TRUE))
  #})
  
  ##output the processed census data
  output$proCensus <- DT::renderDataTable({
    if(input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1] >= 1){
      data <- values$wholeCDT[[input$either[1]+input$both[1]+input$orderonly[1]+input$combineonly[1]]]
      datatable(head(data), class = 'cell-border stripe compact',
                caption = 'Table 6: Processed Census Data', options = list(dom = 't', scrollX = TRUE))
    }
  })
  
  
  #########################################Get the final weights###################################################
  ##change the survey data and census data from a list to a dataset
  srvyFinal <- reactive({
    req(input$done2)
    sDT <- do.call(cbind,LSData())
    sDT <- as.data.frame(sDT)
    return(sDT)})
  
  output$dlSurvey <- downloadHandler(
    filename = function() { paste0("surveyData_",today(), '.txt') },
    content = function(file) {
      write.table(srvyFinal(), file, row.names = FALSE, sep = ",")
    }
  )
  
  censusFinal <- reactive({
    req(input$done2)
    cDT <- do.call(cbind,values$wholeCDT)
    cDT <- as.data.frame(cDT)
    return(cDT)})
  
  output$dlCensus <- downloadHandler(
    filename = function() { paste0("censusData_",today(), '.txt') },
    content = function(file) {
      write.table(censusFinal(), file, row.names = FALSE, sep = ",")
    }
  )
  
  ##get the final weights
  finalWeights <- reactive({
    req(input$done2)
    getWeights(srvyFinal(), censusFinal(), sapply(LSData(), ncol), tol=1.15e-04)})
  
  output$dlWeights <- downloadHandler(
    filename = function() { paste0("Weights_",today(), '.txt') },
    content = function(file) {
      write.table(finalWeights(), file, row.names = FALSE, sep = ",")
    }
  )
  
  ##output the weights
  output$weights <- DT::renderDataTable({
    data <- finalWeights() 
    datatable(head(data), class = 'cell-border stripe compact',
              caption = 'Table 7: the weights', options = list(dom = 't', scrollX = TRUE))
  })
  
  
  
  
  output$value1 <- renderPrint({ input$names })
  output$value2 <- renderPrint({as.numeric(unlist(strsplit(input$split,",")))})
  output$value3 <- renderPrint({input$nexts[1]+input$pass[1]+1})
}

shinyApp(ui=ui,server=server)




