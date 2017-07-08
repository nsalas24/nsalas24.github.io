---
layout: post
title: "Student Loan Calculator"
author: "Noah Salas"
date: "July 5, 2017"
categories: Shiny
---

As I was finishing up school, I wanted a quick and dirty way to
calculate how long it might take me to pay off my student loans and at
what cost. So I did what I always do and googled it. But as I did so, I
realized I couldn't really find what I was looking for. Most calculators
could only accept one loan at a time. Even the tools offered from the
company administering my loans ([Great
Lakes](https://www.mygreatlakes.org/)) couldn't help me answer simple
question like *'In which order should I pay off my loans?'* and *'How
will changing my monthly payment affect my time to payoff and overall
interest accrual?'* So to answer these questions I built my own
calculator!

To use [this
app](https://nsalas24.shinyapps.io/student_loan_calculator/), all you
have to do is enter an interest balance (or leave at zero) and enter
your loans into the table. From there select what monthly payment you
would like, and the app outputs two important things: the total interest
accrued and the number of months that it will take you to pay off these
loans.

These are calculated in the following way, and this can be found in the
server file [here](link%20to%20code%20files):

1.  Pay off any existing interest balance. Great Lakes told me all of
    your intial payments will go towards this amount until it is paid
    off.
2.  Calculate which loan will accrue the most interest in the next
    month. The majority of the time this will be the loan with the
    highest interest rate. Proceed to pay off this loan, calculating the
    number of months it took and total accrued interest.
3.  Calculate the amount of accrued interest to your other loans during
    the time it took to pay off the loan in Step 2. Add these amounts to
    their repsective balances.
4.  Repeat Step 2 and Step 3 until all loans are paid off. Sum up all
    months and accrued interest amounts. Report out.

Most of these steps live in the *calculation* event:

{% highlight r %}
    #Performs the calculations on action of the input$calc button
    #Returns total_n and paid_off
      calculation <- eventReactive(input$calc, {

            # get the number of periods it took to payoff a loan
            nper <- function(rate,pmt,pv,fv=0,typ=0){
              result = log10(pmt/(pmt -pv*(rate)))/log10(1+rate)
              return(result)
            }
            
            # accrual amount
            accr <- function(rate, n, pv){
              a = (pv*(1+(rate))^(n))-pv
              return(a)
            }
            
            # effective interest rate
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
{% endhighlight %}

Notice I used some functions to help with my calculations: get the
number of periods it took to payoff a loan, the accural amount, and an
effective interest rate.

{% highlight r %}
            # get the number of periods it took to payoff a loan
            nper <- function(rate,pmt,pv,fv=0,typ=0){
              result = log10(pmt/(pmt -pv*(rate)))/log10(1+rate)
              return(result)
            }
            
            # accrual amount
            accr <- function(rate, n, pv){
              a = (pv*(1+(rate))^(n))-pv
              return(a)
            }
            
            # effective interest rate
            eff_int <- function(rate,period){
              (1+rate/period)^(period/12)-1
            }
{% endhighlight %}

This is similar to an
[avalanche](http://www.investopedia.com/terms/d/debt-avalanche.asp)
style repayment strategy and you should be able to actually implement
this in real life. ([Great Lakes](https://www.mygreatlakes.org/)) lets
you specify which loan you want your payment to be applied to (as
opposed to spreading it out across all your loans). If you wanted to get
really fancy, you could recalculate which loan you should pay off based
on the next month's interest accural *every month*.

Good luck and enjoy!
