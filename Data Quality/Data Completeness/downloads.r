  # APP: DATA COMPLETENESS
  #------------------------------------
  #------------------------------------
  
  #################################
  # DOWNLOADS
  #################################
  
  output$downloadSummary <- downloadHandler(
    filename = function() {
      paste('summary-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dqReport_short(), file,na="",row.names=FALSE)
    }
  )
  
  output$downloadDQ <- downloadHandler(
    filename = function() {
      paste('elements-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(dqReport(), file,na="",row.names=FALSE)
    }
  )
  
  output$downloadClients <- downloadHandler(
    filename = function() {
      paste('clients-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(Violations(), file,na="",row.names=FALSE)
    }
  )
  
  output$downloadProgs <- downloadHandler(
    filename = function() {
      paste('progs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(progsTable(), file,na="",row.names=FALSE)
    }
  )
  
