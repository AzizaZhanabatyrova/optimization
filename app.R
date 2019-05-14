library(shiny)
#library(pracma)
library(numDeriv)

# Graphical user interface
ui <- fluidPage(
  titlePanel("NLP assignment"),
  sidebarLayout(
    sidebarPanel(
      
      selectInput(
        "type", label = h4("Type of a problem"),
        c(Unconstrained = "uncon",
          Constrained = "con")),
      
      radioButtons("radio", label = h4("Min/Max"),
                   choices = list("Min" = 1, 
                                  "Max" = 2), selected = 1),
      textInput("text1", label = h4("Cost function (format of variables: x1, x2, x3,...)"), "(x2 - x1^2)^2 + (x1 - 1)^2"),
    
    
      # Only show this panel if the plot type is a constrained
      conditionalPanel(
        condition = "input.type == 'con'",
        textInput("text2", label = h4("Constraints (separated by commas)"), "x1^2 >= 2"),
        numericInput("penalty_tol", label = h4("Penalty tolerance (norm of difference between optimal solutions)"), value = 1e-06),
        numericInput("kj", label = h4("Penalty initial Kj"), value = 1),
        numericInput("kj_increase", label = h4("Penalty Kj increase rate"), value = 10)),
        numericInput("iter2", label = h4("Penalty max number of iterations"), value = 5),
      
      
      textInput("x0", label = h4("Gradient initial solution x0 \n(comma delimited)"), "1,2"),
      numericInput("tol", label = h4("Gradient tolerance (norm of the gradient)"), value = 1e-06),
      numericInput("alpha", label = h4("Gradient positive step size"), value = 0.01),
      numericInput("iter", label = h4("Gradient max number of iterations"), value = 5000),
      #submitButton("Submit!")
      actionButton(
        inputId = "submit_loc",
        label = "Submit"
      ),
      p(""),
      p("\nImplemented by Aziza Zhanabatyrova, s4396350, for the course of Optimization techniques, 09.05.2017")
    ),
    
    mainPanel(
      verbatimTextOutput("txt1"),
      verbatimTextOutput("txt2"),
      verbatimTextOutput("txt3"),
      verbatimTextOutput("txt4"),
      verbatimTextOutput("txt5"),
      verbatimTextOutput("txt6"),
      verbatimTextOutput("txt7"),
      verbatimTextOutput("txt8"),
      verbatimTextOutput("txt9")
    )
  )
)

# The program
server <- function(session, input, output) {

  observeEvent(
    eventExpr = input[["submit_loc"]],
    handlerExpr = {
      
      begin_time <- Sys.time()
      
      output$txt1 <- renderText({
        paste("")
      })
      output$txt2 <- renderText({
        paste("")
      })
      output$txt3 <- renderText({
        paste("")
      }) 
      output$txt4 <- renderText({
        paste("")
      })
      output$txt5 <- renderText({
        paste("")
      })
      output$txt6 <- renderText({
        paste("")
      })
      output$txt7 <- renderText({
        paste("")
      })
      output$txt8 <- renderText({
        paste("")
      })
      output$txt9 <- renderText({
        paste("")
      })
    
      # cost function
      if (input$radio == 1)
        str1 <- input$text1
      else
        str1 <- paste("-1*(",input$text1,")")
      
      cost_f <- parse_function(str1)
      
      # if unconstrained 
      if (input$type == "uncon") {
      
        # call gradient alg
        gradient(cost_f, 
                 as.numeric(unlist(strsplit(input$x0,","))), 
                 input$alpha, 
                 input$tol) 
      }
      
      #else constrained
      else {
        # call penalty function, pass cost_f
        penalty_function(str1, cost_f)
        }
  
      finish_time <- Sys.time()
      elapsed_time <- finish_time - begin_time
      output$txt7 <- renderText({
        paste("Elapsed time in seconds = ", elapsed_time)
      })
      
      
  }) #end of observe event
  
  
  # Gradient function takes cost function, initial approximation x0, desired step-size (constant), tolerance e
  gradient <- function(func, xk, alpha, e) {
    
    # check feasibility of the problem (min existance/ unbounded problem, step size, strict tolerance)
    iteration = 1
    #verify stopping citeria

    gradient <- grad(func, xk)  
    norm_gradient <- sqrt(sum(gradient^2))
    str3 <- "Less than first 100 iterations for gradient\n" 
    str2 <- ""

    while (norm_gradient > e) 
    {
    xk <- xk - (alpha*gradient)
    if (sqrt(sum(xk^2)) > 1e+20) {
      output$txt4 <- renderText({
        paste("The solution is diverging to infinity")
      })
      break
    }
    
    if (iteration <= 100)
      str2 <- paste(str2, "\nIteration # ", iteration, "; Solution = ", toString(xk), "; Cost = ", func(xk))
    
    iteration = iteration + 1
    if (iteration >= input$iter)
      break
    
    gradient = grad(func, xk)  
    norm_gradient <- sqrt(sum(gradient^2))
 
    
    }
    str1 <- paste(str3, str2)
      
    #final solution xk
    output$txt1 <- renderText({
      paste("The cost is: ", func(xk))
    })
    output$txt2 <- renderText({
      paste("The solution = ", toString(xk))
    })
    output$txt3 <- renderText({
      paste("Found at iteration = ", iteration)
    })
    output$txt8 <- renderText({
    paste(str1)
    })
    invisible(xk)
  }
  
  

# Convert string into a function
parse_function <- function(func_string){
  func_string = paste(" ", func_string, " ")
  y <- unlist(regmatches(func_string, gregexpr("(\\W(x[0-9]))", func_string)))
  new_y = ""
  for (i in 1:length(y)){
    new_y = paste(new_y, y[i])
  }
  y2 <- unique(unlist(regmatches(new_y, gregexpr("x[0-9]*",new_y))))
  y3 <- sort(y2)
  y4 <- gsub("x", "", y3)
  head ="f <- function(x) { }"
  new_func_string = func_string
  y5 = y
  for (i in 1:length(y3)){
    y5 = gsub(y3[i],paste("x[",y4[i],"]", sep = ""),y5, fixed = TRUE)
  }
  for (i in 1:length(y)){
    new_func_string = gsub(trimws(y[i]), y5[i], new_func_string, fixed = TRUE)
  }
  eval(parse(text = head))
  body(f) = (parse(text = new_func_string))
  
  invisible(f)
}



n_variables <- function(func_string){
  func_string = paste(" ", func_string, " ")
  y <- unlist(regmatches(func_string, gregexpr("(\\W(x[0-9])\\W)", func_string)))
  invisible(length(y))
}



penalty_function <- function(cost_f, cost_f2){
  
  ProblemNo = 1
  x0 <- as.numeric(unlist(strsplit(input$x0,",")))
  solution_norm <- sqrt(sum((x0)^2))
  Kj <- input$kj
  str3 <- "Less than first 100 iterations for penalty\n" 
  str2 <- ""
  
    
  while (solution_norm > input$penalty_tol) 
  {
    #overall penalty p_f
    cost_fTilda <- paste(cost_f, "+", Kj, "*", parse_penalty(input$text2))
    cost_fTilda <- parse_function(cost_fTilda)
    
     x_star <- gradient(cost_fTilda, x0, input$alpha, input$tol)
     
     if (sqrt(sum(x_star^2)) > 1e+20) {
       output$txt4 <- renderText({
         paste("The solution is diverging to infinity")
       })
       break
     }
     
     if (ProblemNo <= 100)
       str2 <- paste(str2, "\nIteration # ", ProblemNo, "; Solution = ", toString(x_star), "; Cost = ", cost_f2(x_star))
     
     solution_norm <- sqrt(sum((x_star- x0)^2))
     x0 <- x_star
     Kj = Kj * input$kj_increase
    ProblemNo = ProblemNo + 1
    
    if(ProblemNo >=input$iter2)
      break
    
  }
  str1 <- paste(str3, str2)
  
  # print ProblemNo, Kj
  output$txt5 <- renderText({
    paste("Iteration in penalty = ", ProblemNo)
  })
  output$txt6 <- renderText({
    paste("Constant Kj = ", Kj)
  })
  output$txt9 <- renderText({
    paste(str1)
  })
  output$txt8 <- renderText({
    paste("")
  })
}



parse_penalty <- function(cons){
  constraints <- unlist(strsplit(cons, ","))
  penalty <- "("
  for (i in 1:length(constraints)){
    if (grepl("<=", constraints[[i]])){
      constraints[[i]] = gsub("<=", "-(", paste(constraints[[i]],")"))
      constraints[[i]] = paste("max(0,(", constraints[[i]], "))^2")
    }
    else if (grepl(">=", constraints[[i]])){
      constraints[[i]] = gsub(">=", "-", paste(constraints[[i]],")"))
      constraints[[i]] = paste("max(0,-(", constraints[[i]], "))^2")
    }
    else if (grepl("=", constraints[[i]])){
      constraints[[i]] = gsub("=", "-", constraints[[i]])
      constraints[[i]] = paste("(", constraints[[i]], ")^2")
    }
    else {
      stop(paste("Invalid constraint ", constraints[[i]]))
    }
    if (i != 1){
      penalty = paste(penalty, " + ", constraints[[i]])
    }
    else {
      penalty = paste(penalty, constraints[[i]])
    }
    paste(penalty,")")
  }
  return(penalty)
}



} #end of server



shinyApp(ui = ui, server = server)
  






