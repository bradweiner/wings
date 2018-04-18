
library(shiny)
library(readr)
library(tidyverse)
library(magrittr)
library(dummies)
library(shinydashboard)
library(ggthemes)

# read in the wings data
d <- read_csv("data/wings_data_public.csv", na = c("", "-", "N/A"))

# clean column names
names(d) <- gsub("[[:punct:]]| ", "", names(d))

# replace delimeters with single comma
d %<>% mutate(Style = tolower(Style),
              Style = gsub("\\?|\\?,", "", Style),
              Style = gsub("\\ &|\\/", ",", Style),
              Style = gsub(", ", ",", Style),
              Style = gsub(" ", "_", Style))

# removed duplicated restaurants
d <- d[!duplicated(d$Restaurant),]

# generate dummies from style coding
style_dummies <- d %>% select(Style, Restaurant) %>% 
        tibble::rownames_to_column(var = "id") %>% 
        mutate(Style = strsplit(as.character(Style), ",")) %>% 
        unnest() %>%
        mutate(present = 1) %>%
        spread(Style, present, fill = 0) %>%
        select(-id, -V1)

# drop original style column
d %<>% select(-Style)

# get column names of raw coded variables
raw_measures <- names(d)

# join dummies onto numeric cols
d %<>% left_join(style_dummies, by = "Restaurant")



# Define UI for application that draws a histogram
ui <- dashboardPage(
        dashboardHeader(title = "Twin Cities Chicken Wings Data Set",
                        titleWidth = 400),
        dashboardSidebar( 
                         width = 333,
                         imageOutput("hovlandImage"),
                         h5("JD Hovland"),
                         br(),
                         imageOutput("chickenWingsImage")
        ),
        dashboardBody(
                fluidRow(h3("Scatterplot for Numeric Measures of Chicken Wings"), align = "center"),
                fluidRow(
                br(),
                box(
                        title = "Scatter Plot Variables", 
                        width = 4,
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        uiOutput("x_varUI"),
                        uiOutput("y_varUI"),
                        selectInput("line_method",
                                    "Line Method",
                                    choices = c("loess",
                                                "lm",
                                                "gam"))
                ),
                
                box(
                        title = "Scatter Plot with Restaurant Tool Tip Hover ", 
                        width = 8,
                        solidHeader = TRUE,
                        # ui output for scatter plot
                        plotOutput("scatter_plot", hover = "scatter_plot_hover", hoverDelay = 0),
                        # dyanmic hover for scatter plot
                        uiOutput("scatter_plot_dynamic")
                )
                )
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
        
        output$hovlandImage <- renderImage({
        
                # When input$n is 3, filename is ./images/image3.jpeg
                filename <- file.path('www', 'ctyp-LucyHawthorne-WingMan-1.jpg')
                
                # Return a list containing the filename and alt text
                list(src = filename,
                     alt = 'JD Hovland')
                
        }, deleteFile = FALSE)
        
        output$chickenWingsImage <- renderImage({
                
                # When input$n is 3, filename is ./images/image3.jpeg
                filename <- file.path('www', 'fried-chicken-chicken-fried-crunchy-60616.jpeg')
                
                # Return a list containing the filename and alt text
                list(src = filename,
                     alt = 'JD Hovland')
                
        }, deleteFile = FALSE)
        
        # get numeric columns that were raw coded - exclude
        # dummy variables from style
        numeric_cols <- names(d)[sapply(d, is.numeric)]
        numeric_cols <- numeric_cols[numeric_cols %in% raw_measures]
        
        # get x col for scatter plot
        output$x_varUI <- renderUI({
                selectInput("x_varInput",
                            "Select X-axis Variable",
                            choices = numeric_cols,
                            selected = numeric_cols[3])
        })
        
        # get y var for scatter plot
        output$y_varUI <- renderUI({
                selectInput("y_varInput",
                            "Select Y-axis Variable",
                            choices = numeric_cols,
                            selected = numeric_cols[12])
        })
        
        # render the scatterplot
        output$scatter_plot <- renderPlot({
                req(input$x_varInput,
                    input$y_varInput,
                    input$line_method)
                
                ggplot(d, aes_string(x = input$x_varInput, y = input$y_varInput)) +
                        geom_point(size = 4, alpha = 0.5) +
                        geom_smooth(method = input$line_method) +
                        theme_tufte()
                
        })
        
        # render the restaurant name dynamically as text
        output$scatter_plot_dynamic <- renderUI({
                req(input$scatter_plot_hover) 
                verbatimTextOutput("scatter_plot_vals")
        })
        
        # add the hover functionality for the restaurant name
        output$scatter_plot_vals <- renderPrint({
                hover <- input$scatter_plot_hover 
                y <- nearPoints(d, input$scatter_plot_hover)["Restaurant"]
                req(nrow(y) != 0)
                writeLines(as.character(y))
        })
}

# Run the application 
shinyApp(ui = ui, server = server)

