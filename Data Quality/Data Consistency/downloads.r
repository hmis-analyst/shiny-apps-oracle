  # APP: DATA CONSISTENCY
  # File: tables.r
  #------------------------------------
  # Global dependencies:
  #   [None]
  # Local dependencies:
  #   summaryData_2()
  #   clientsData()
  #   progsData_2()
  # Objects created:
  #   output$downloadSummary
  #   output$downloadClients
  #   output$downloadProgs
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
      write.csv(summaryData_2(), file,na="",row.names=FALSE)
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
      write.csv(progsData_2(), file,na="",row.names=FALSE)
    }
  )
  
