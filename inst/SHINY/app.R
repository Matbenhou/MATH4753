# app.R

library(shiny)
library(ggplot2)
library(MASS)  # for fitdistr
library(dplyr)

ui <- fluidPage(
  titlePanel("Maximum Likelihood Estimation for Univariate Distributions"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Select Distribution:",
                  choices = c("Normal", "Exponential", "Poisson", "Binomial", "Uniform")),
      
      numericInput("n", "Sample size (n):", 100, min = 10, max = 1000),
      
      # Parameters depending on distribution
      conditionalPanel(
        condition = "input.dist == 'Normal'",
        numericInput("mean", "True mean:", 0),
        numericInput("sd", "True SD:", 1, min = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Exponential'",
        numericInput("rate", "True rate (λ):", 1, min = 0.01)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Poisson'",
        numericInput("lambda", "True λ:", 3, min = 0.1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Binomial'",
        numericInput("size", "Number of trials (n):", 10, min = 1),
        numericInput("prob", "True probability (p):", 0.5, min = 0, max = 1)
      ),
      
      conditionalPanel(
        condition = "input.dist == 'Uniform'",
        numericInput("min", "True min:", 0),
        numericInput("max", "True max:", 10)
      ),
      
      actionButton("simulate", "Simulate Data & Estimate MLE")
    ),
    
    mainPanel(
      plotOutput("histPlot"),
      verbatimTextOutput("mleOutput")
    )
  )
)

server <- function(input, output) {
  
  data <- eventReactive(input$simulate, {
    switch(input$dist,
           "Normal" = rnorm(input$n, mean = input$mean, sd = input$sd),
           "Exponential" = rexp(input$n, rate = input$rate),
           "Poisson" = rpois(input$n, lambda = input$lambda),
           "Binomial" = rbinom(input$n, size = input$size, prob = input$prob),
           "Uniform" = runif(input$n, min = input$min, max = input$max)
    )
  })
  
  mle_estimates <- reactive({
    x <- data()
    switch(input$dist,
           "Normal" = fitdistr(x, "normal")$estimate,
           "Exponential" = fitdistr(x, "exponential")$estimate,
           "Poisson" = fitdistr(x, "poisson")$estimate,
           "Binomial" = fitdistr(x, "binomial", start = list(size = input$size, prob = 0.5))$estimate,
           "Uniform" = c(min = min(x), max = max(x))
    )
  })
  
  output$mleOutput <- renderPrint({
    est <- mle_estimates()
    cat("Estimated Parameters (MLE):\n")
    print(est)
  })
  
  output$histPlot <- renderPlot({
    x <- data()
    est <- mle_estimates()
    df <- data.frame(x = x)
    
    ggplot(df, aes(x)) +
      geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "white") +
      theme_minimal() +
      labs(title = paste("MLE Fit for", input$dist, "Distribution"),
           x = "x", y = "Density") +
      {
        if (input$dist == "Normal") {
          stat_function(fun = dnorm, args = list(mean = est["mean"], sd = est["sd"]),
                        color = "red", size = 1.2)
        } else if (input$dist == "Exponential") {
          stat_function(fun = dexp, args = list(rate = est["rate"]),
                        color = "red", size = 1.2)
        } else if (input$dist == "Poisson") {
          NULL # discrete distribution, no overlay
        } else if (input$dist == "Binomial") {
          NULL # discrete distribution
        } else if (input$dist == "Uniform") {
          stat_function(fun = dunif, args = list(min = est["min"], max = est["max"]),
                        color = "red", size = 1.2)
        }
      }
  })
}

shinyApp(ui = ui, server = server)
