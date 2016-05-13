library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Quartic Equation Solver"),
  
  withMathJax(),
  h4("Frank La (May 2016)"),
  p("As part of the Developing Data Products module, Coursera"),
  br(),
  # Sidebar with controls. Note the use of the
  # br() element to introduce extra vertical spacing
  sidebarLayout(
    sidebarPanel(
      
      numericInput("A0",label = "Coefficient \\(A_0\\)",value = 0,min = -10,max = 10,step = 1),
      numericInput("A1",label = "Coefficient \\(A_1\\)",value = 0,min = -10,max = 10,step = 1),
      numericInput("A2",label = "Coefficient \\(A_2\\)",value = 0,min = -10,max = 10,step = 1),
      numericInput("A3",label = "Coefficient \\(A_3\\)",value = 0,min = -10,max = 10,step = 1),
      numericInput("A4",label = "Coefficient \\(A_4\\)",value = 1,min = -10,max = 10,step = 1),
      br(),
      submitButton("Submit")
    ),
    
    # Show a tabset that includes a summary and plot view
    mainPanel(
      tabsetPanel(type = "tabs", 
                  tabPanel("About",
                           h4('Overview'),
                           p('Welcome to the Quartic Equation Solver! Quartic equation is an equation of the form 
                             $$f(x) = A_0 + A_1 x + A_2 x^2 + A_3 x^3 + A_4 x^4 = 0.$$'),
                           p('Here, you get to input coefficients \\(A_0\\), \\(A_1\\), ..., \\(A_4\\). 
                              The app will solve the equation, tell you the results, and even make some plots!'),
                           br(),
                           h4("Input Panel"),
                           p('Simply key in the values of the coefficients (real numbers allowed) or use the up/down
                             arrows to select values (from -10 to 10). When you are ready, hit the Submit button.'),
                           p('You can even enter \\(A_4=0\\); the app is smart enough to realize this is a cubic or 
                              lower order equation. But do not submit with any field empty or else an error will show. 
                             Enter 0 for all coefficients is not valid and may cause some errors.'),
                           br(),
                           h4('Summary Tab'),
                           p('Details of the solution are displayed here. These include a (smart)', 
                             em('math formula'), 'of the equation, all', em('real'), 'and', em('complex roots'), 
                             ', all', em('minima, maxima'), 'as well as', em('inflection points'), 
                             'of the function \\(f(x)\\) that you input. 
                             All of these are computed reactively by Shiny server.'),
                           p('If a root is counted more than one, it is a', em('multiple root.'),
                             'Complex roots should appear in conjugate pairs. Total number of roots are 4 for 
                             quartic equations. These are fundamental results from algebra.'),
                           br(),
                           h4('Graph Tab'),
                           p('This tab visualizes the graph of function \\(f(x)\\), with all the aforementioned points
                             shown and labeled.'),
                           br(),
                           h4('Complex Root Plot Tab'),
                           p('This tab shows you where the roots lie in the complex plane.'),
                           br(),
                           h4('Examples'),
                           p('Enter \\(A_0=-1\\), \\(A_1=1\\), \\(A_2=-2\\), \\(A_3=-1\\) and \\(A_4=1\\), among many others,
                             for an example where all the points exist.'),
                           br(),
                           h4('See also:'),
                           p(a('Quartic function', 
                               href = 'https://en.wikipedia.org/wiki/Quartic_function'), target="_blank"),
                           p(a('Maxima and minima', 
                               href = 'https://en.wikipedia.org/wiki/Maxima_and_minima'), target="_blank"),
                           p(a('Inflection point', 
                               href = 'https://en.wikipedia.org/wiki/Inflection_point'), target="_blank")
                           ),
                  tabPanel("Summary",
                           h4("You asked to solve:"),
                           uiOutput("eqn"),
                           #br(),
                           h4("Real roots of equation:"),
                           verbatimTextOutput("realRoot"),
                           #br(),
                           h4("Complex roots of equation:"),
                           verbatimTextOutput("cplxRoot"),
                           #br(),
                           h4("Minima of \\(y=f(x)\\):"),
                           tableOutput("minimum"),
                           #br(),
                           h4("Maxima of \\(y=f(x)\\):"),
                           tableOutput("maxima"),
                           #br(),
                           h4("Inflection points of \\(y=f(x)\\):"),
                           tableOutput("inflection") 
                           ), 
                  tabPanel("Graph",
                           plotOutput("graph")),
                  tabPanel("Complex root plot",
                           plotOutput("complexPlot"))
      )
    )
  )
))