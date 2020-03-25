library(shiny)
library(DT)
library(htmltools)
library(tidyverse)
library(ggrepel)

options(knitr.kable.NA = 'unknown')

# population from https://www.populationpyramid.net/united-states-of-america/2020/

df <- tribble(
        ~age, ~risk_death, ~social_dist, ~value_life, ~cost_longterm, ~population,
        "< 10",           0,          0,         8e6,          0.5e6,    39721484,
        "10-19",      0.0018,         0,         9e6,          0.5e6,    42332395,
        "20-49",      0.0032,       0.5,         7e6,          0.5e6,   131110742,
        "50-59",       0.013,       0.5,         6e6,          0.5e6,    42120077,
        "60-69",       0.036,       0.8,         5e6,          0.5e5,    38488170,
        "70-79",        0.08,       0.8,         4e6,          0.5e4,    24082597,
        ">= 80",       0.148,       0.8,         3e6,              0,    13147182
    )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Covid-19 social costs in United States"),

    fluidRow(
        column(width = 6, 
            h4("Assumptions"),
            h6("Double-click to edit."),
            DT::dataTableOutput('assumptionsTable')
        ),

        # Show a plot of the generated distribution
        column(width = 6,
           #h4("Results"),
           
           tabsetPanel(type = "pills",
                       tabPanel("Deaths", 
                                plotOutput("deathsPlot"),
                                checkboxInput("log_deaths", label = "Logarithmic scale", value = FALSE),
                                h5("The deaths for a given age group are calculated with the formula:"),
                                h5("deaths = percent_infected * (1 - social_distancing) * population * risk_death")
                                ),
                       tabPanel("Costs", 
                                plotOutput("costsPlot"),
                                checkboxInput("log_costs", label = "Logarithmic scale", value = TRUE),
                                h5("The costs for a given age group are calculated with the formula:"),
                                h5("costs = percent_infected * (1 - social_distancing) * population * (risk_death * value_life + cost_longterm)")
                                )
           ),
           h1(),
           h5("Source code:", a("https://github.com/vladtarko/covid/"))
           
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # EDIT ASSUMPTIONS --------------------------------------------------------
    df1 <- reactive(df)
    output$assumptionsTable = DT::renderDataTable(
        DT::datatable(df, 
           selection = 'none', 
           editable = TRUE,
           colnames = c("Age group", "Risk of death*", "Social distancing**", 
                        "Value of statistical life", "Long term costs of infection", "Population***"),
           options = list(class = "compact", dom = 't', scrollY = '280px'),
           caption = htmltools::tags$caption(
               style = 'caption-side: bottom;',
               
               p('* Default death rates are from China:', 
                 a('https://www.cebm.net/global-covid-19-case-fatality-rates/')),
               
               p('**Social distancing is a number between 0 (life as usual) and 1 (complete separation).
                 For example, if older people separate, the index should be higher, such as 0.8.'),
               
               p('*** Population pyramid from:', 
                 a('https://www.populationpyramid.net/united-states-of-america/2020/'))
           )
        ) %>% 
         DT::formatCurrency(4:5, digits = 0) %>% 
         DT::formatPercentage(2, digits = 2) %>% 
         DT::formatPercentage(3, digits = 0) %>% 
         DT::formatRound(6, digits = 0)
    )
    
    proxy = dataTableProxy('assumptionsTable')
    
    observeEvent(input$assumptionsTable_cell_edit, {
        info = input$assumptionsTable_cell_edit
        str(info)
        i = info$row
        j = info$col
        v = info$value
        df[i, j] <<- DT::coerceValue(as.numeric(v), df[i, j])
        replaceData(proxy, df, resetPaging = FALSE)  # important
    })
    
    # DEATHS -------------------------------------------------------
    output$deathsPlot <- renderPlot({
        
        input$assumptionsTable_cell_edit
        
        # calculate social costs of infection for each age group
        df1 <- tibble(infected = seq(0, 0.6, 0.01)) %>% 
            merge(df) %>% 
            mutate(deaths = infected * (1 - social_dist) * population * risk_death,
                   cost   = infected * (1 - social_dist) * population * (risk_death * value_life + cost_longterm), 
                   labels = if_else(infected == max(infected), age, NA_character_)
            ) 
        # calculate the total social costs of infection 
        df_total <- df1 %>% group_by(infected) %>% summarise(deaths = sum(deaths))
        
        # plot
        g <- df1 %>% 
            ggplot(aes(x = infected, y = deaths, color = age)) +
            geom_line() +
            geom_line(data = df_total, aes(x = infected, y = deaths, color = "Total"), color = "black") +
            geom_label_repel(aes(label = labels),
                             nudge_x = 1,
                             na.rm = TRUE) +
            annotate(geom = "label", x = 0.65, y = max(df_total$deaths), label = "Total") +
            scale_color_discrete(guide = FALSE) +
            scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.65)) +
            labs(x = "Percentage of total population infected",
                 y = "Deaths") +
            theme_light(base_size = 18)
        
        if (input$log_deaths) {
            g <- g + scale_y_log10(labels = scales::comma) 
        } else {
            g <- g + scale_y_continuous(labels = scales::comma)
        }
        
        g
        
    })
    
    # COSTS -------------------------------------------
    output$costsPlot <- renderPlot({
        
        input$assumptionsTable_cell_edit

        # calculate social costs of infection for each age group
        df1 <- tibble(infected = seq(0, 0.6, 0.01)) %>% 
            merge(df) %>% 
            mutate(deaths = infected * (1 - social_dist) * population * risk_death,
                   cost   = infected * (1 - social_dist) * population * (risk_death * value_life + cost_longterm), 
                   labels = if_else(infected == max(infected), age, NA_character_)
                   ) 
        # calculate the total social costs of infection 
        df_total <- df1 %>% group_by(infected) %>% summarise(cost = sum(cost))
        
        # plot
        g <- df1 %>% 
            ggplot(aes(x = infected, y = cost / 1e12, color = age)) +
                geom_line() +
                geom_line(data = df_total, aes(x = infected, y = cost / 1e12, color = "Total"), color = "black") +
                geom_label_repel(aes(label = labels),
                                 nudge_x = 1,
                                 na.rm = TRUE) +
                geom_hline(yintercept = 22, linetype = "dashed") +
                annotate(geom = "label", x = 0.2, y = 22, label = "US GDP") +
                annotate(geom = "label", x = 0.65, y = max(df_total$cost)/1e12, label = "Total") +
                scale_color_discrete(guide = FALSE) +
                scale_x_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.65)) +
                labs(x = "Percentage of total population infected",
                     y = "Cost (trillions of dollars)") +
                theme_light(base_size = 18)
        
        if (input$log_costs) {g <- g + scale_y_log10() }
        
        g
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
