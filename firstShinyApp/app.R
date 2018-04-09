ipak <- function(pkg){
  new_pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new_pkg) > 0) {
    install.packages(new_pkg, dependencies = TRUE)
  } 
  sapply(pkg, require, character.only = TRUE)
}

# Load packages
packages <- c("shiny", "ggplot2")
ipak(packages)

# Data: bimodal normal distribution
mu1 <- -2
mu2 <-1
sd1 <-1
sd2 <-.5
N <-10000
alpha <- .5

U <- runif(N)

x <- rep(NA, N)

for(i in 1:N) {
  if(U[i] < alpha) {
    x[i] <- rnorm(1, mean=mu1, sd=sd1)
  }
  else {
    x[i] <-rnorm(1, mean=mu2, sd=sd2)
  }
}
df <- data.frame(x=x, var="Estimated\n density")
x2 <- seq(min(x), max(x), length=100)
y <- alpha*dnorm(x2, mean=mu1, sd=sd1) +
  (1-alpha)*dnorm(x2, mean=mu2, sd=sd2)
df2 <- data.frame(x=x2, y=y, var="True\n density")

# Define UI
ui <- fluidPage(
  
  # App title
  titlePanel("My first Shiny App"),
  
  # Sidebar layout
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      # Bandwidth choice
      radioButtons("bwChoice", "Bandwidth Choice:",
                    choices = c("Manual" = 1,
                                "Automatic" = 2)),
      
      # Conditional slider input
      conditionalPanel(
        condition = "input.bwChoice == 1",
        # Input
        sliderInput(inputId = "bandwidth",
                    label = "Bandwidth",
                    min = 0.05,
                    max = 5,
                    value = 2)        
      ),
      
      conditionalPanel(
        condition = "input.bwChoice == 2",
        # Input
        radioButtons("bwDefault", "Automatic Bandwidth Choices:",
                     choices = c("Silverman" = "nrd0",
                                 "Normal reference" = "nrd",
                                 "Sheather and Jones" = "SJ"))
      )
    ),
    
    # Main panel
    mainPanel(
      
      # Output: Density plot
      plotOutput(outputId = "densPlot"),
      br(),
      h4("This is my first Shiny App!")
  
    )
  )
)

server <- function(input, output) {

  # Output
  output$densPlot <- renderPlot({
    
    if (input$bwChoice == 1) {
      bandwidth <- input$bandwidth
    } else {
      bandwidth <- input$bwDefault
    }
    
    # Plot
    ggplot() +
      geom_density(data=df, aes(x=x, fill=var), alpha=0.5, bw=bandwidth) +
      geom_line(data=df2, aes(x=x, y=y, color=var), size=1.5) +
      labs(x="Variable",
           y="Density",
           title="Estimated density of a bimodal random variable") +
      theme(plot.title=element_text(hjust=0.5),
            legend.title=element_blank()) +
      scale_color_manual(values=c("#E69F00"))
#    ggplotly(p)
#    plot(density(df$x, bw=bandwidth))
#    lines(df2$x, df2$y)    
  })
}

shinyApp(ui=ui, server=server)

