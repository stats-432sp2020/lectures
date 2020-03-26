#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(cowplot)


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Bivariate logistic regression model"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("n", "How much data (n):",
                        min = 10, max = 250, value = 100),
            sliderInput("b0", "What is the intercept?",
                        min=-2, max=2, value=0, step=.1),
            sliderInput("b1", "What is the coefficient on X1?",
                        min=-2, max=2, value=1, step=.1),
            sliderInput("b2", "What is the coefficient on X2?",
                        min=-2, max=2, value=-1, step=.1)
        ),

        mainPanel(
           plotOutput("logrplot")
        )
    )
)

server <- function(input, output) {
    logit <- function(z) log(z)-log(1-z)
    ilogit <- function(z) exp(z)/(1+exp(z))

    output$logrplot <- renderPlot({
        generate_data <- eventReactive(c(input$n, input$b0, input$b1, input$b2), {
            X = matrix(runif(input$n * 2,-1,1), ncol=2)
            lin = X %*% c(input$b1, input$b2) + input$b0
            y = rbinom(input$n, 1, prob = ilogit(lin))
            df = data.frame(y = factor(y), X)
            return(df)
        })
        df = generate_data()
        rast_df = expand.grid(seq(-1,1,length.out = 250),seq(-1,1,length.out=250))
        rast_df = tibble(X1 = rast_df$Var1, X2 = rast_df$Var2,
                         Z = ilogit(input$b0 + input$b1*X1 + input$b2*X2))
        ggplot(rast_df, aes(X1, X2)) + geom_raster(aes(fill=Z)) +
            scale_fill_gradient2(midpoint=.5, name="P(Y=1)", limits=c(0,1)) +
            geom_point(data=df, aes(X1, X2, shape=y)) +
            theme_cowplot(14) + coord_cartesian(expand = FALSE)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
