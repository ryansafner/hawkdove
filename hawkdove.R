library(shiny)
library(tidyverse)
library(ggtext)
library(glue)
library(scales)

# Define the UI
ui <- fluidPage(
  titlePanel("Hawk Dove Game"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("resource_value", "Resource Value", min = 1, max = 10, value = 4, step = 1),
      sliderInput("cost_of_fighting", "Cost of Fighting", min = 0, max = 10, value = 8, step = 1),
    ),
    mainPanel(
      verbatimTextOutput("payoff_matrix"),
      plotOutput("plot"),
      p("This model is coded with", a("R and Shiny",href="https://shiny.rstudio.com/"), "by", a("Ryan Safner",href="http://ryansafner.com"))
    )
  )
)

# Define the server
server <- function(input, output) {
  # Collect inputs for payoffs
  
  payoff_matrix <- reactive({
    hh_payoff <- 0.5 * (input$resource_value - input$cost_of_fighting)
    dd_payoff <- 0.5 * input$resource_value
    hd_payoff <- input$resource_value
    dh_payoff <- 0
    
    # Define the payoff matrix
    payoff_matrix <- matrix(c(hh_payoff, dh_payoff, hd_payoff, dd_payoff), nrow = 2, ncol = 2, byrow = TRUE,
                            dimnames = list(c("Hawk", "Dove"), c("Hawk", "Dove")))
    
  })
  
  output$payoff_matrix <- renderPrint({
    payoff_matrix()
  })
  
  
  output$plot <- renderPlot({
    
    hh_payoff <- 0.5 * (input$resource_value - input$cost_of_fighting)
    dd_payoff <- 0.5 * input$resource_value
    hd_payoff <- input$resource_value
    dh_payoff <- 0
    
    h_payoff = function(p){hh_payoff*p+hd_payoff*(1-p)}
    d_payoff = function(p){dh_payoff*p+dd_payoff*(1-p)}
    
    p_eq = input$resource_value / input$cost_of_fighting
    pay_eq = h_payoff(p_eq)
    
    hawks_triangle <- tribble(
      ~x, ~y,
      0, d_payoff(0),
      0, h_payoff(0),
      p_eq, pay_eq
    )
    
    doves_triangle <- tribble(
      ~x, ~y,
      p_eq, pay_eq,
      (-1 * hd_payoff)/(hh_payoff - hd_payoff), 0,
      (-1 * dd_payoff)/(dh_payoff - dd_payoff), 0
    )
    
    subtitle <- if (p_eq >= 1){
      glue("**Monomorphic population**: all members of the population will be <span style = 'color:#FFA500'>Hawks</span>")
    } else {
      glue("<span style = 'font-weight:bold'>Polymorphic population</span>: ", percent({p_eq}), " will be <span style = 'color:#FFA500'>Hawks</span> and ",
           {percent((1-p_eq))}, " will be <span style = 'color:#047806'>Doves</span>")
    }
    
    ggplot(data = tibble(x = c(0,1)))+
      aes(x = x)+
      geom_polygon(data = hawks_triangle, aes(x = x, y = y), fill ="#FFA500", alpha = 0.5)+
      geom_polygon(data = doves_triangle, aes(x = x, y = y), fill ="#047806", alpha = 0.5)+
      stat_function(fun = h_payoff, color = "#FFA500", size = 2)+
      stat_function(fun = d_payoff, color = "#047806", size = 2)+
      annotate(geom = "label", x = 0.1, y = h_payoff(0.1),
               label = "Payoff to Hawk", color = "#FFA500")+
      annotate(geom = "label", x = 0.1, y = d_payoff(0.1),
               label = "Payoff to Dove", color = "#047806")+
      geom_segment(x = p_eq, xend = p_eq, y = -Inf, yend = pay_eq, size = 1, linetype = "dotted")+
      geom_segment(x = -Inf, xend = p_eq, y = pay_eq, yend = pay_eq, size = 1, linetype = "dotted")+
      scale_x_continuous(breaks = seq(0,1,0.1),
                         limits = c(0,1.05),
                         expand = c(0,0))+
      scale_y_continuous(breaks = seq(0,h_payoff(0),0.1 * h_payoff(0)),
                         limits = c(0,1.05 * h_payoff(0)),
                         expand = c(0,0))+
      labs(x = "Proportion of <span style = 'color:#FFA500'>Hawks</span> in population, p",
           y = "Expected Payoff (Fitness)",
           title = "Hawk Dove Game Population",
           subtitle = subtitle,
           color = NULL)+
      theme_light(base_family = "Fira Sans Condensed", base_size = 14)+
      theme(axis.title.x = element_markdown(),
            plot.subtitle = element_markdown(),
            plot.title = element_markdown(face = "bold"))
  })
}

shinyApp(ui = ui, server = server)