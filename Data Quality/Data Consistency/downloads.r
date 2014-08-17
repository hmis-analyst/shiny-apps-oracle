  # APP: DATA CONSISTENCY
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
      write.csv(summaryData(), file,na="",row.names=FALSE)
    }
  )
  
  
  output$downloadClients <- downloadHandler(
    filename = function() {
      paste('clients-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(clientsData(), file,na="",row.names=FALSE)
    }
  )
  
  output$downloadProgs <- downloadHandler(
    filename = function() {
      paste('progs-', Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(progsData(), file,na="",row.names=FALSE)
    }
  )
  
