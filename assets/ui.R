library(shiny)

shinyUI(fluidPage(
  # Application title
  titlePanel("Student Loan Calculator"),
  p("Welcome to my app that can help you scenario plan for multiple loans!"),
  p("If you haven't figured out by now, the bar to your left is where you can set all of your input parameters. To add loans, simply enter the name, interest, and principal amount of the loan then click" 
    ,strong("'Add Row'"), "and your loan table will updated automatically. You can also delete loans (rows) anytime. After setting what monthly payment you'd like, click the " ,strong("'Calculate!'"), "button and check out your results. " ,em("p.s. I don't store
    any results, so if you're worried about that, don't be :)") ),
  p("I wrote a blog post detailing the methods I use to generate these results, found" ,a("here", href="http://nsalas24.github.io/shiny/2017/07/05/Student-Loan-Calculator/"),
    "The source code for this shiny app lives" ,a("here", href="boopadoop"), ", good luck!"),
  
  sidebarLayout(
    sidebarPanel(
      h4("What's your total interest balance?"),
      textInput("interest_bal","","0"),
      h4("What loan(s) do you have?"),
      textInput("nameIn","Loan's Name", "Year 1"),
      textInput("intIn", "Interest Rate (between 0 and 1)", ".05"),
      textInput("prinIn", "Principal Amount", "7000"),
      actionButton("addrow", "Add Row"),
      actionButton("revrow", "Remove Row"),
      actionButton("removeall","Remove All Rows"),
      h4("How often do your loans compound?"),
      selectInput("comp", 
                  label = "",
                  choices = c("Daily", "Monthly"),
                  selected = "Daily"),
      h4("What monthly payment would you like?"),
      textInput("user_pmt",""),
      actionButton("calc","Calculate!")
    ),
    mainPanel(
      tableOutput("table"),
      br(),
      hr(),
      br(),
      "Total amount owed:",
      strong(textOutput("initial_amt")),
      "Total amount paid off:",
      strong(textOutput("paid_off")),
      "Total interest accrued:",
      strong(textOutput("total_accr")),
      "Number of months to pay off:",
      strong(textOutput("total_n")),
      width=8
    )
  )
)
)