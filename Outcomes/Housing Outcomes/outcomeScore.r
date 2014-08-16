  # APP: HOUSING OUTCOMES
  # File: outcomeScore.r
  #------------------------------------
  # Global dependencies:
  #   input$reportLevel
  #   programTypes_Reduced_2()
  # Local dependencies:
  #   "MLE_[prog type]_SFY2014.csv"
  #   Exits()
  #   Exits3()
  #   Exits4()
  #   "Prog data_[prog type]_SFY2014.csv"
  # Objects created:
  #   MLE()
  #   Outcome_predicted()
  #   output$significance
  #   DoD_reference()
  #   Outcome_actual()
  #------------------------------------
  #------------------------------------

  #################################
  # OUTCOME SCORING
  #################################
  #-------------------
  MLE <- reactive({
    input$update
    isolate(if(input$reportLevel!="Program") return())
    if(length(grep("Housing Outcomes/lib",getwd()))==0) {
      setwd(libPath3)
    }
    isolate(prog_type <- switch(programTypes_Reduced_2(),
      "ES" = "ES",
      "TH" = "TH",
      "PSH" = "PSH",
      "RapidReHousing" = "RRH",
      "Prevention" = "Prev",
      "None"
    ))
    if(prog_type=="None") return()
    MLE <- read.csv(paste("MLE",prog_type,"SFY2014.csv",sep="_"))
    setwd("../")
    return(MLE)
  })

  Outcome_predicted <- reactive({
    likelihood <- Exits()$perm_90day_likelihood
    if(is.null(likelihood)) return()
    if(length(likelihood[!is.na(likelihood)])/length(likelihood) < .9) return(c(999,round(length(likelihood[!is.na(likelihood)])/length(likelihood)*100,1)))
    Outcome_predicted <- mean(likelihood[which(!is.na(likelihood))])
    return(Outcome_predicted)
  })
  
  output$significance <- renderText({
    if(length(Exits()[,1])<10) return()
    if(Outcome_predicted()[1]==999) return()
    input$update
    isolate(if(input$reportLevel!="Program") return())
    x <- sum(Exits3()[which(!is.na(rowSums(Exits3()[,as.character(MLE()[2:length(MLE()[,1]),"Parameter"])]))),"Dest_Perm_90"])
    n <- length(Exits4()[,1])
    p <- Outcome_predicted()
    p_value <- binom.test(x=x,n=n,p=p)$p.value
    return(paste(
      "Given their degree of difficulty, clients performed",
      if(p_value<.05) {
        if(x/n<p) {
          "WORSE"
        } else {
          "BETTER"
        }
      } else {
        "NEITHER WORSE NOR BETTER"
      },
      "than expected in this program."
    ))
  })
  
  DoD_reference <- reactive({
    input$update
    isolate(if(Outcome_predicted()==999 | input$reportLevel!="Program") return())
    if(length(grep("Destinations/lib",getwd()))==0) {
      setwd(libPath3)
    }
    isolate(prog_type <- switch(programTypes_Reduced_2(),
      "ES" = "ES",
      "TH" = "TH",
      "PSH" = "PSH",
      "RapidReHousing" = "RRH",
      "Prevention" = "Prev"
    ))
    DoD_reference <- read.csv(paste("Prog data",prog_type,"SFY2014.csv",sep="_"), stringsAsFactors=FALSE)
    setwd("../")
    return(DoD_reference)
  })
  
  Outcome_actual <- reactive({
    sum(Exits4()$Dest_Perm_90)/length(Exits4()[,1])
  })
  
  

