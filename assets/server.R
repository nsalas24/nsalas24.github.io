library(shiny)

shinyServer(function(input, output, session){
  
  values <- reactiveValues()
  
  #Creating initial table
  values$DT <- data.frame(Name = 0,
                          Interest = 0,
                          Principal = 0,
                          stringsAsFactors = FALSE)
  
  #newEntry objects edit the table in real time
  newEntry <- observeEvent(input$addrow, {
    newLine <- c(input$nameIn,input$intIn, input$prinIn)
    values$DT <- rbind(values$DT, newLine)
  })
  
  newEntry <- observeEvent(input$revrow, {
    deleteLine <- values$DT[-nrow(values$DT), ]
    values$DT <- deleteLine
  })
  
  newEntry <- observeEvent(input$removeall, {
    values$DT <- data.frame(Name = 0,
                            Interest = 0,
                            Principal = 0,
                            stringsAsFactors = FALSE)
  })
  
  #Performs the calculations on action of the input$calc button
  #Returns total_n and paid_off
  calculation <- eventReactive(input$calc, {
    nper <- function(rate,pmt,pv,fv=0,typ=0){
      result = log10(pmt/(pmt -pv*(rate)))/log10(1+rate)
      return(result)
    }
    
    accr <- function(rate, n, pv){
      a = (pv*(1+(rate))^(n))-pv
      return(a)
    }
    
    eff_int <- function(rate,period){
      (1+rate/period)^(period/12)-1
    }
    
    temp <- values$DT[-1,-1]
    paid_off <- 0
    total_n <- 0
    
    if (input$comp=="Daily") {
      temp[,1] <- eff_int(as.numeric(temp[,1]),365)
    } else {
      temp[,1] <- as.numeric(temp[,1])/12
    }
    
    if(as.numeric(input$user_pmt) < sum(accr(as.numeric(temp[,1]),1,as.numeric(temp[,2])))){
      x <- "ERROR: The monthly payment you entered is less than the interest you accrue each month :("
      y <- "ERROR: The monthly payment you entered is less than the interest you accrue each month :("
      results <- c(x,y,input$interest_bal)
      return(results)
    } else {
      interest_bal <- as.numeric(input$interest_bal)
      
      while(interest_bal>0){
        interest_bal = interest_bal - (as.numeric(input$user_pmt) - sum(accr(as.numeric(temp[,1]),1,as.numeric(temp[,2]))))
        total_n = total_n + 1
        paid_off = paid_off + as.numeric(input$user_pmt)
      }
      
      for(i in 1:nrow(temp)){
        idx = which.max(accr(as.numeric(temp[,1]),1,as.numeric(temp[,2])))                    # which loan will accrue the most money
        n = nper(as.numeric(temp[idx,1]),as.numeric(input$user_pmt),
                 as.numeric(temp[idx,2])+interest_bal)                                        # # of months it will take to pay off that loan
        total_n = total_n + n                                                                 # # of months counter
        temp[,2] = accr(as.numeric(temp[,1]),n,as.numeric(temp[,2])) + as.numeric(temp[,2])   # adding in accrual of other loan
        paid_off = paid_off + n*as.numeric(input$user_pmt)                                    # amount of paid off loans counter
        temp[idx,2] = 0
        interest_bal = 0
      }
      results <- c(paid_off,total_n,as.numeric(input$interest_bal))
      return(results)
    }
    }) #end of calculation function
  
  #Shows table in real time, ignoring first row of 0's
  output$table <- renderTable({
    values$DT[-1,]
  })
  #Shows sum of principals as text
  output$initial_amt <- renderText({
    val = round(sum(as.numeric(values$DT[,3]))+as.numeric(calculation()[3]),2)
    paste("$",val,sep=" ")
  })
  #Shows total amount paid off as text
  output$paid_off <- renderText({
    if (class(calculation()[1])=="character") {
      paste("$",calculation()[1],sep=" ")
    } else {
      paste("$",round(unname(calculation()[1]),2),sep=" ")
    }
  })
  #Shows interest accrued as text
  output$total_accr <- renderText({
    if (class(calculation()[1])=="character") {
      paste("$",calculation()[1],sep=" ")
    } else {
      val = round(unname(calculation()[1]) - (sum(as.numeric(values$DT[,3]))+calculation()[3]),2)
      paste("$",val,sep=" ")
    }
  })
  #Shows number of months to pay off debt as text
  output$total_n <- renderText({
    if (class(calculation()[2])=="character") {
      calculation()[2]
    } else {
      unname(ceiling(calculation()[2]))
    }
  })
})