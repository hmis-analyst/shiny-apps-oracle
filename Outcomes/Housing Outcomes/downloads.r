  # APP: HOUSING OUTCOMES
  # File: downloads.r
  #------------------------------------
  # Global dependencies:
  #   groupKeys()
  # Local dependencies:
  #   Exits_access()
  #   Exits()
  #   Exits3()
  #   Exits_summary()
  # Objects created:
  #   output$downloadEnrolls
  #   output$downloadReturns
  #   output$downloadProgs
  #------------------------------------
  #------------------------------------

  #################################
  # DOWNLOADS
  #################################
  
  output$downloadEnrolls <- downloadHandler(
    filename = function() {
      paste('progs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(
        if(is.null(groupKeys())) {
          "Access to client data denied. Please enter a valid passkey, click ANALYZE, and re-download."
        } else {
          if(length(Exits_access()[,1])==0) {
            "Access to client data denied. Please enter a valid passkey, click ANALYZE, and re-download."
          } else {
            merge(Exits_access(),Exits(),by="PROGRAM_ENROLLMENT_KEY")
          }
        }, 
        file,na="",
        row.names=FALSE
      )
    }
  )
  output$downloadReturns <- downloadHandler(
    filename = function() {
      paste('progs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(
        if(is.null(groupKeys())) {
          "Access to client data denied. Please enter a valid passkey, click ANALYZE, and re-download."
        } else {
          if(length(Exits_access()[,1])==0) {
            "Access to client data denied. Please enter a valid passkey, click ANALYZE, and re-download."
          } else {
            merge(Exits_access(),Exits3(),by="PROGRAM_ENROLLMENT_KEY")
          }
        }, 
        file,na="",
        row.names=FALSE
      )
    }
  )
  output$downloadProgs <- downloadHandler(
    filename = function() {
      paste('progs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Exits_progs(), file,na="",row.names=FALSE)
    }
  )
