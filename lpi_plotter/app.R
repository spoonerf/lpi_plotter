library(shiny)
library(vroom)
library(janitor)
library(dplyr)
library(tidyr)
library(ggplot2)

# df <- clean_names(vroom("LPI_pops_20160523_edited.csv")) %>% 
#     select(id, binomial, common_name, specific_location, latitude, longitude, starts_with("x")) %>% 
#     pivot_longer(starts_with("x")) %>% 
#     mutate(year = as.numeric(gsub("x", "", name))) %>% 
#     mutate(value = ifelse(value == "NULL", NA, value))
# 
# saveRDS(df, "lpi_long_data.RDS")
# saveRDS(df, "lpi_plotter/lpi_long_data.RDS")

df <- readRDS("lpi_long_data.RDS")

df$value <- as.numeric(df$value)
df$id <- as.factor(df$id)

df <- df %>% 
    group_by(id) %>% 
    mutate(data_years = sum(!is.na(value)))

common_names <- df %>%
    select(common_name) %>% 
    distinct() %>% 
    arrange(common_name) %>% 
    pull()
    
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("LPI plotter"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("species",
                        "Species Name:",
                        choices = common_names,
                        selected = common_names[10]),
            sliderInput("min_years", 
                        "Minimum number of years with data:",
                        min = 1, 
                        max = 20,
                        value = 5
                )
            ),
        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("lpiPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$lpiPlot <- renderPlot({
        
        df_fil <-  df %>% 
            filter(common_name == input$species) %>% 
            filter(data_years >= input$min_years)
       
        if(nrow(df_fil) > 0){
            min_yr <- df_fil$year[which.min(df_fil$value)]
            max_yr <- df_fil$year[which.max(df_fil$value)] 
            ggplot(data = df_fil, aes(x = year, y = value, group = id, colour = id))+
                geom_point()+
                geom_smooth()+
                scale_y_log10()+
                theme_bw()+
                labs(y = "Abundance (log10)", x = "Year")+
                xlim(min_yr, max_yr) 
        } else {
            ggplot()+
                ggtitle("No data available - please lower the minimum number of years data required")
        }


    })
}

# Run the application 
shinyApp(ui = ui, server = server)
