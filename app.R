
library(shinydashboard)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(lme4)
library(lmerTest)
library(plotly)
library(shiny)
library(broom)
library(broom.mixed)
library(kableExtra)
library(optimx)
library(sjPlot)
library(emmeans)
library(maps)
library(sf)
library(ggthemes)

dat_long <- read.csv("dat_long.csv", header = TRUE)
dat_wide <- read.csv("dat_wide.csv", header = TRUE)


ui <- dashboardPage(
  skin = "yellow",
  dashboardHeader(title = "BEE AWARE!"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
                menuItem("Abstract", tabName = "abstract", icon = icon("question-circle", lib = "font-awesome")),
                menuItem("US Map", tabName = "map",icon = icon("globe", lib = "font-awesome")),
                menuItem("Number of Colonies", tabName = "number_colonies", icon =icon("line-chart", lib = "font-awesome")),
                menuItem("Colony Loss Precentage", tabName = "colony_loss_precentage", icon =icon("line-chart", lib = "font-awesome")),
                menuItem("Seasonal Analysis", tabName = "seasons", icon =icon("line-chart", lib = "font-awesome"))
    )),
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "abstract",
        h2(tags$u("BEE AWARE!")),
        h3(tags$div("The production of 39 of the world's 57 most important monoculture crops benefits from an ecosystem relying on animal pollination(1). Western honeybees (Apis mellifera) are most valuable animal pollinators to agriculture(2). In 2009, after two winters with unexplainable large scale bee colony losses, the syndrome of a rapid loss of adult worker bees was name the Colony Collapse Disorder (CCD)(3)."),
           tags$br("Because of the importance of honeybees to the ecosystem, and specifically crops, a large-scale loss of bee colonies should be a major concern to people globally. To better understand the trend in honeybee's colony losses, was managed to put our hands on a top-secret document tracking the number of colonies and their stressors around the US (tidytuesday, 11/1/2022). In the following pages, using state of the art statistical modeling and visualization technics, we will try to understand the trends of US honeybee colonies and the causes to these trends.")),
        h4(tags$ul(
          tags$div(
            "1.	Klein, A. M., Vaissière, B. E., Cane, J. H., Steffan-Dewenter, I., Cunningham, S. A., Kremen, C., & Tscharntke, T. (2007). Importance of pollinators in changing landscapes for world crops. Proceedings of the royal society B: biological sciences, 274(1608), 303-313."
          ),
          tags$br("2.	Williams, G. R., Tarpy, D. R., Chauzat, M. P., Cox-Foster, D. L., Delaplane, K. S., Neumann, P., ... & Shutler, D. (2010). Colony collapse disorder in context. Bioessays, 32(10), 845."),
          tags$br("3.	VanEngelsdorp, D., Evans, J. D., Saegerman, C., Mullin, C., Haubruge, E., Nguyen, B. K., ... & Pettis, J. S. (2009). Colony collapse disorder: a descriptive study. PloS one, 4(8), e6481.")))
      ),
      tabItem(
        tabName = "map",
        fluidRow(
          column(width = 4, offset = 1,
                 sliderInput(inputId = "year",
                             label = "Choose Year or press PLAY",
                             min = 2016, max = 2021, value = 2016,
                             animate = animationOptions(interval = 2000, loop = TRUE)))),
        fillRow(
          box(width = 12,
              align = "Center", status = "warning", solidHeader = T, collapsible = F,
              title = "US Map of Conoly Change Precentage from the Previous Year",
              plotOutput(outputId = "plot_map", width = "100%")
          )
        )
      ),
      tabItem(
        tabName = "colony_loss_precentage",
        fluidRow(
          column(width = 4, offset = 1,
                 selectInput(inputId = "state",
                             label = "Choose State",
                             list(
                               "United States", "Alabama", "Arizona", "Arkansas", "California", "Colorado", "Connecticut", "Florida", "Georgia", "Hawaii", "Idaho", "Illinois", "Indiana", "Iowa", "Kansas", "Kentucky", "Louisiana", "Maine", "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi", "Missouri", "Montana", "Nebraska", "New Jersey", "New Mexico", "New York", "North Carolina", "North Dakota", "Ohio", "Oklahoma", "Oregon", "Pennsylvania", "South Carolina", "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", "Virginia", "Washington", "West Virginia", "Wisconsin", "Wyoming"
                             )))),
        fluidRow(
          box(
            title = "Colony Loss Precentage through the Years",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE, 
            plotlyOutput(outputId = "plot_colony"),
            tableOutput("lost_summary")
          ),
          box(
            title = "Precentage of Colonies Affected by different Stressors",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE, 
            plotlyOutput(outputId = "plot_stress"),
            tableOutput("lost_cor")
          )
        )
      ),
      tabItem(
        tabName = "number_colonies",
        fluidRow(
          tabBox(side = "right",
                 title = "Number of Colonies Throue the Years", width = 12,
                 tabPanel(
                   "The Big Picture",
                   plotlyOutput(outputId = "plot_ncolony"),
                   tableOutput("summary_ncolony")
                 ),
                 tabPanel(
                   "Geografical Effects",
                   selectInput(inputId = "Geography",
                               label = "Choose Geographical Level",
                               list("Region", "Division")
                   ),
                   plotlyOutput(outputId = "plot_geography"),
                   tableOutput("geography.table")
                 )
          )
        )
      ),
      tabItem(
        tabName = "seasons",
        "In the previous tabs we did not find a significant trend or correlation. We did notice a reapting patern within each year",
        tags$br(""),
        fluidRow(
          box(width = 12, align = "Center",
              title = "Colony Growth in all States by Seasons",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE, 
              plotlyOutput(outputId = "season_plot")
          )
        ),
        fluidRow(
          box(
            title = "Season Growth Model",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE, 
            tableOutput("growth.model")
          ),
          box(
            title = "Model Means",
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE, 
            tableOutput("growth.means")
          )
        )
      )
    )
  )
)



server <- function(input, output, session) {
  
  ### map
  us <- map_data("state")
  dat_map <- dat_long %>%
    filter(state != c("United State", "Other")) %>%
    group_by(state, year) %>%
    summarize(N = mean(colony_n, na.rm = T), year = year) %>%
    distinct()  %>%
    mutate(region = tolower(state))
  
  dat_map$change <- (dat_map$N-lag(dat_map$N))/lag(dat_map$N)*100
  dat_map <- dat_map %>% filter(year != 2015)
  
  mapData <- reactive({
    req(input$year)
    df <- dat_map %>% 
      filter(year == input$year) %>%
      group_by(region) %>%
      summarize(ch = mean(change), region = region, year = year, state = state) %>% 
      mutate(ch = case_when(ch >= 100 ~ 100, TRUE ~ ch))
  })
  
  output$plot_map <- renderPlot({
    ggplot() + 
      geom_map(data = us, map = us,
               aes(x = long, y = lat, map_id = region),
               fill = "#ffffff", color = "#ffffff", size = 0.15) +
      geom_map(data = mapData(), map = us,
               aes(fill = ch, map_id = region),
               color = "#ffffff", size = 0.15) + 
      scale_fill_continuous("Percentage of change", low = "yellow",  high = "black",
                            breaks=c(-100,0,100),labels=c("-100% or less","No change","100% or more"),
                            limits=c(-100,100)) +
      theme_minimal() +
      theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(),
            axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(),
            panel.grid.major.x = element_blank(), panel.grid.major.y = element_blank(),
            panel.grid.minor.x = element_blank(), panel.grid.minor.y = element_blank(),
            legend.key.size = unit(1.5, 'cm'), legend.title = element_text(size = 18),
            legend.text = element_text(size = 14)) + 
      coord_fixed(1.3)+
      labs(caption = "White colored countries are countries with missing data")
    
  })
  
  ### number of colonies
  #big picture
  
  data3 <- dat_wide %>%
    filter(period < 25) %>% 
    filter(state == "United States")
  
  output$plot_ncolony <- renderPlotly({
    ggplotly(
      ggplot(data3,
             aes(x = period, y = colony_n, group = 1,
                 text = paste0("Year: ", year,
                               "\nMonths: ", months,
                               "\nN Colonies: ", colony_n))) +
        geom_line(size = 1.5) +
        geom_smooth(method = "lm", se = FALSE, color = "brown", size = 1) +
        theme_minimal() +
        scale_x_continuous(name = "Time",
                           breaks = c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5),
                           labels = c("2015", "2016", "2017", "2018", "2019", "2020")) +
        geom_vline(xintercept = c(0.5, 4.5, 8.5, 12.5, 16.5, 20.5, 24.5),
                   linetype = "dashed", color = "yellow") +
        ylab("Number of Colonies") +
        theme(panel.grid.major.x = element_blank(),
              legend.position = "top"),
      tooltip = c("text")
    ) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(yaxis = list(fixedrange = TRUE),
             xaxis = list(fixedrange = TRUE))
  })
  output$summary_ncolony <- function()({
    data3 %>% lm(colony_n ~ period, data = .) %>% 
      summary(.) %>% 
      broom::tidy(conf.int=TRUE, p.value = TRUE) %>% 
      mutate(
        term = c("Intercept", "Time"),
        p.value = scales::pvalue(p.value)
      ) %>%
      select(Term = term,
             Estimate = estimate,
             `Lower Bound` = conf.low,
             `Upper Bound` = conf.high,
             'P-Value' = p.value) %>% 
      knitr::kable("html",
                   digits = c(0, 0, 0, 0, 3),
                   caption = "Effect of Time on the Number of Colonies") %>% 
      kable_styling("striped", full_width = T)
  })
  
  #geographical differences
  
  geography_plot <- reactive({
    req(input$Geography)
    if(input$Geography == "Region"){
      dat_wide %>% 
        filter(!is.na(Region)) %>% 
        filter(period < 26) %>% 
        group_by(period, Region) %>% 
        summarize(N = sum(colony_n), year = year, months = months) %>% 
        ggplot(data = .,
               aes(x = period, y = N, group = Region,
                   text = paste0("Year: ", year,
                                 "\nMonths: ", months,
                                 "\nRegion: ", Region,
                                 "\nN Colonies: ", N))) +
        geom_line(aes(colour = Region), size = 1.5) + 
        theme_minimal() +
        scale_color_manual(values=c("#D5CFB7","#EBE5C9","#BFB281","#E2CA79","#EEC438","black")) +
        scale_x_continuous(name = "Time",
                           breaks = c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5),
                           labels = c("2015", "2016", "2017", "2018", "2019", "2020")) +
        geom_vline(xintercept = c(0.5, 4.5, 8.5, 12.5, 16.5, 20.5, 24.5),
                   linetype = "dashed", color = "yellow") +
        ylab("Number Of Colonies") +
        theme(panel.grid.major.x = element_blank())
    }
    else if(input$Geography == "Division"){
      dat_wide %>% 
        filter(!is.na(Division)) %>% 
        filter(period < 26) %>% 
        group_by(period, Division) %>% 
        summarize(N = sum(colony_n), year = year, months = months) %>% 
        ggplot(data = .,
               aes(x = period, y = N, group = Division,
                   text = paste0("Year: ", year,
                                 "\nMonths: ", months,
                                 "\nDivision: ", Division,
                                 "\nN Colonies: ", N))) +
        geom_line(aes(colour = Division), size = 1.5) + 
        theme_minimal() +
        scale_color_manual(values=c("#FFD500", "#D4B000", "#E6FF00", "#ADC000","#738000", "#6E6B6B", "#000000", "#9D9D9D", "#2C2C2C")) +
        scale_x_continuous(name = "Time",
                           breaks = c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5),
                           labels = c("2015", "2016", "2017", "2018", "2019", "2020")) +
        geom_vline(xintercept = c(0.5, 4.5, 8.5, 12.5, 16.5, 20.5, 24.5),
                   linetype = "dashed", color = "yellow") +
        ylab("Number Of Colonies") +
        theme(panel.grid.major.x = element_blank())
    }
  })
  
  
  output$plot_geography <- renderPlotly({
    ggplotly(geography_plot(), tooltip = c("text")) %>% 
      layout(annotations = 
               list(x = 1, y = -0.1,
                    text = "Data from the months of April-June 2019 is missing",
                    showarrow = FALSE,
                    xref = 'paper',
                    yref = 'paper',
                    font = list(size = 10)))
  })
  
  
  geography_model <- reactive({
    req(input$Geography)
    if(input$Geography == "Region"){
      dat_wide %>% 
        filter(!is.na(Region)) %>% 
        filter(period < 26) %>% 
        group_by(period, Region) %>% 
        summarize(N = sum(colony_n), year = year, months = months) %>%
        lmer(data = .,
             N ~ Region*period + (1|Region),
             control = lmerControl(optimizer = "Nelder_Mead"))
    } else if(input$Geography == "Division"){
      dat_wide %>% 
        filter(!is.na(Division)) %>% 
        filter(period < 26) %>% 
        group_by(period, Division) %>% 
        summarize(N = sum(colony_n), year = year, months = months) %>%
        lmer(data = .,
             N ~ Division*period + (1|Division),
             control = lmerControl(optimizer = "Nelder_Mead"))
    }
  })
  
  geography_terms <- reactive({
    req(input$Geography)
    if(input$Geography == "Region"){
      c("Region", "Time", "Region:Time")
    } else if(input$Geography == "Division"){
      c("Division", "Time", "Division:Time")
    }
  })
  
  geography_title <- reactive({
    req(input$Geography)
    if(input$Geography == "Region"){
      "Effect of Time and Region on the Number of Colonies"
    } else if(input$Geography == "Division"){
      "Effect of Time and Division on the Number of Colonies"
    }
  })
  output$geography.table <- 
    function()({
      anova(geography_model()) %>%
        broom::tidy(effects = "fixed", p.value = TRUE) %>% 
        mutate(term = geography_terms(),
               p.value = scales::pvalue(p.value)) %>% 
        select(Term = term,
               'F-Statistic' = statistic,
               'P-Value' = p.value) %>% 
        knitr::kable("html",
                     digits = c(0, 3, 3),
                     caption = geography_title()) %>% 
        kable_styling("striped", full_width = T) %>% 
        footnote(alphabet = c(
          "Data was cut such that it begins and ends on the same season (months) to provide an accurate estimation of change",
          "The results displayed are from a mixed linear model allowing random intercepts for regions"))
    })
  
  
  ### loss precentage
  
  data1 <- reactive({
    req(input$state)
    df_colony <- dat_wide %>%
      filter(period < 25) %>% 
      filter(state %in% input$state)
  })
  data2 <- reactive({
    req(input$state)
    df_stress <- dat_long %>% 
      filter(period < 25) %>% 
      filter(state %in% input$state)
  })
  
  output$plot_colony <- renderPlotly({
    ggplotly(
      ggplot(data1(),
             aes(x = period, y = colony_lost_pct, group = 1,
                 text = paste0("Year: ", year,
                               "\nMonths: ", months,
                               "\nPrecentage: ", colony_lost_pct))) +
        geom_line(size = 1.5) +
        geom_smooth(method = "lm", se = FALSE, color = "brown", size = 1) +
        theme_minimal() +
        scale_x_continuous(name = "Time",
                           breaks = c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5),
                           labels = c("2015", "2016", "2017", "2018", "2019", "2020")) +
        geom_vline(xintercept = c(0.5, 4.5, 8.5, 12.5, 16.5, 20.5, 24.5),
                   linetype = "dashed", color = "yellow") +
        ylab("Precentage of Colonies Lost") +
        theme(panel.grid.major.x = element_blank(),
              legend.position = "top"),
      tooltip = c("text")
    ) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(yaxis = list(fixedrange = TRUE),
             xaxis = list(fixedrange = TRUE),
             annotations = list(x = 0, y = 1,
                                text = "Data from the months of April-June 2019 is missing",
                                showarrow = FALSE,
                                xref = 'paper',
                                yref = 'paper',
                                font = list(size = 10)))
  })
  output$lost_summary <- function()({
    data1() %>% lm(colony_lost_pct ~ period, data = .) %>% 
      summary(.) %>% 
      broom::tidy(conf.int=TRUE, p.value = TRUE) %>% 
      mutate(
        term = c("Intercept", "Time"),
        p.value = scales::pvalue(p.value)
      ) %>%
      select(Variable = term,
             Estimate = estimate,
             `Lower Bound` = conf.low,
             `Upper Bound` = conf.high,
             'P-Value' = p.value) %>% 
      knitr::kable("html",
                   digits = c(0, 2, 2, 2, 3),
                   caption = "Effect of Time on Colony Loss Precentage") %>% 
      kable_styling("striped", full_width = T)
  })
  
  output$plot_stress <- renderPlotly({
    ggplotly(
      ggplot(data2(),
             aes(x = period, y = stress_pct, colour = stressor, group = 1,
                 text = paste0("Stressor: ", stressor,
                               "\nYear: ", year,
                               "\nMonths: ", months,
                               "\nColonies Affected: ", stress_pct, "%"))) +
        geom_line() +
        theme_minimal() +
        scale_color_manual(values=c("#D5CFB7","#EBE5C9","#BFB281","#E2CA79","#EEC438","black")) +
        scale_x_continuous(name = "Time",
                           breaks = c(2.5, 6.5, 10.5, 14.5, 18.5, 22.5),
                           labels = c("2015", "2016", "2017", "2018", "2019", "2020")) +
        geom_vline(xintercept = c(0.5, 4.5, 8.5, 12.5, 16.5, 20.5, 24.5),
                   linetype = "dashed", color = "yellow") +
        ylab("Precent of Colonies Affected by Stressor") +
        theme(panel.grid.major.x = element_blank()),
      tooltip = c("text")
    ) %>% 
      config(displayModeBar = FALSE) %>% 
      layout(yaxis = list(fixedrange = TRUE),
             xaxis = list(fixedrange = TRUE),
             annotations = list(x = 0, y = 1,
                                text = "Data from the months of April-June 2019 is missing",
                                showarrow = FALSE,
                                xref = 'paper',
                                yref = 'paper',
                                font = list(size = 10)))
  })
  
  output$lost_cor <- function()({
    data1() %>%
      select(colony_lost_pct,Varroa.mites,Other.pests.parasites,Disesases,Pesticides,Other,Unknown) %>%
      cor(., use="pairwise.complete.obs") %>% .[2:7, 1] %>% 
      knitr::kable("html",
                   digits = c(2),
                   caption = "Correlation of Stressors with Colony Loss Precentage",
                   col.names = c("Correlation")) %>% 
      kable_styling("striped", full_width = T)
  })
  
  
  ###periodic growth
  
  season.growth.graph <-
    dat_wide %>%
    filter(state != "United States") %>% 
    filter(!is.na(colony_lost) | !is.na(colony_added)) %>%
    filter(!is.na(Region)) %>% 
    mutate(
      months = factor(months, levels = c("January-March", "April-June", "July-September", "October-December"))) %>% 
    group_by(state, months) %>% 
    summarize(pct_growth = mean(100*((colony_added - colony_lost)/colony_n)), state = state) %>%
    distinct() %>% 
    ggplot(data = .,
           aes(x = months, y = pct_growth, group = 1,
               text = paste0("\nState: ", state,
                             "\nMean Growth: ", round(pct_growth,2)))) + 
    geom_jitter(colour = "orange") +
    scale_x_discrete(name = "Months (Season)",
                     labels = c("January-March \n(Winter-Spring)", "April-June \n(Spring-Summer)", "July-September \n(Summer-Fall)", "October-December \n(Fall-Winter")) +
    theme_minimal() +
    ylab("Precentage of Growth") +
    theme(panel.grid.major.x = element_blank())+
    ylim(-100, 100)+
    geom_vline(xintercept = c(1.5, 2.5, 3.5),
               linetype = "dashed", color = "yellow")
  
  output$season_plot <- renderPlotly({
    ggplotly(season.growth.graph, tooltip = "text")%>% 
      config(displayModeBar = FALSE) %>% 
      layout(yaxis = list(fixedrange = TRUE),
             xaxis = list(fixedrange = TRUE),
             annotations = list(x = 0, y = 1,
                                text = "Data for every season is averaged over the years 2015-2021",
                                showarrow = FALSE,
                                xref = 'paper',
                                yref = 'paper',
                                font = list(size = 10)))
  })
  
  pct_growth.model <- 
    dat_wide %>%
    filter(state != "United States") %>% 
    filter(!is.na(colony_lost) | !is.na(colony_added)) %>%
    filter(!is.na(Region)) %>% 
    mutate(
      months = factor(months, levels = c("January-March", "April-June", "July-September", "October-December"))) %>% 
    group_by(state, months) %>% 
    summarize(pct_growth = mean(100*((colony_added - colony_lost)/colony_n)), state = state) %>%
    distinct() %>% 
    lmer(data = .,
         pct_growth ~ months + (1|state))
  
  output$growth.model <- function()({
    anova(pct_growth.model) %>% 
      broom::tidy(effects = "fixed", p.value = TRUE) %>% 
      mutate(term = c("Months (season)"),
             p.value = scales::pvalue(p.value)) %>% 
      select(Term = term,
             'F-Statistic' = statistic,
             'P-Value' = p.value) %>% 
      knitr::kable("html",
                   digits = c(0, 3, 6),
                   caption = "Effect of the Month on Colonies Growth Precentage") %>% 
      kable_styling("striped", full_width = T) %>% 
      footnote(general = "The results displayed are from a mixed linear model allowing random intercepts for states")
  })
  
  output$growth.means <- function()({
    as.data.frame(emmeans(pct_growth.model, ~months)) %>% 
      select(
        Months = months,
        Mean = emmean,
        SE = SE,
        df = df,
        LowerCI = lower.CL,
        UpperCI = upper.CL) %>% 
      knitr::kable("html",
                   digits = c(0, 2, 2, 2, 3, 3)) %>% 
      kable_styling("striped", full_width = T)
  })
  
  
}



shinyApp(ui, server)
