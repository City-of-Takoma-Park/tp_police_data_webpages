library(shiny)
library(DT)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(plotly)
library(rlang)
library(tpfuncts)

dfs <- readRDS("./data/stops_dfs_list.rds")

choice_vector <- names(dfs)

choice_vector_base <- grep("(Traffic violations)|(Traffic stops)|(Computer-Aided Dispatch Base)", choice_vector, invert = F, value = T)
choice_vector_overall <- grep("(race)|(CAD)|(Traffic)|(Computer)", choice_vector, invert = T, value = T)
choice_vector_race <- grep("race", choice_vector, value = T, ignore.case = T)
choice_vector_cad <- grep("CAD", choice_vector, value = T, ignore.case = T)

unique_cad_incidents <- dfs$`Computer-Aided Dispatch Base Data`$`Incident type` %>% unique %>% as.character %>% sort
unique_cad_years <- dfs$`Computer-Aided Dispatch Base Data`$Year %>% unique %>% sort

unique_ts_outcomes <- dfs$`Traffic violations`$Outcome %>% unique %>% as.character %>% sort

unique_ts_descript <- dfs$`Traffic violations`$Summary %>% unique %>% as.character %>% sort

range_ts_dates <- dfs$`Traffic violations`$Date %>% range

range_cad_dates <- dfs$`Computer-Aided Dispatch Base Data`$Date %>% range


range_times <- c(0, seq(1:23))

unique_ts_race <- dfs$`Traffic violations`$`Race/ethnicity` %>% unique %>% as.character %>% sort

unique_ts_gender <- dfs$`Traffic violations`$Gender %>% unique %>% as.character

unique_ts_gender <- unique_ts_gender[!is.na(unique_ts_gender)]

unique_ts_state <- dfs$`Traffic violations`$State %>% unique%>% as.character

unique_ts_county <- dfs$`Traffic violations`$County %>% unique%>% as.character

axis_x_choices_bar_ts <- c("Outcome", 
                           "Summary", 
                           "Time",
                           "Race/ethnicity",
                           "Gender",
                           "State",
                           "County",
                           "Year",
                           "Age range") %>% 
  sort

axis_x_choices_bar_cad <- c("Incident type",
                            "Year")

axis_y_choices_bar <- c("Total",
                        "Percent")

axis_group_choices_ts <- axis_x_choices_bar_ts[axis_x_choices_bar_ts != "Year"]

axis_group_choices_cad <- axis_x_choices_bar_cad[axis_x_choices_bar_cad != "Year"]

axis_group_choices_ts_scatter <- axis_group_choices_ts[axis_group_choices_ts != "Time"]

col_rename <- function(x, tnum = T){
  val <- tolower(gsub(" ", "_", gsub("(/)|(\\?)", "", x)))
  
  if (tnum){
    val <- ifelse(grepl("time", val), "time_num", val)
  }
  return(val)
}

quo_global <- function(x) {
  enquo(x) %>%
    quo_set_env(global_env())
}


### Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  
  # Application title
  titlePanel("Traffic Stops Summary Data Explorer"),
  
  p("A dashboard allowing users to explore summary datasets used to produce the Takoma Park traffic stops data webpage (the ", strong("'Summary Datasets'"), " tab), and filter/generate plots on their own based on their own interests (the ", strong("'Plot Data'"), " tab)"),
  
  # navigation pane
  navbarPage("",
             tabPanel("Summary datasets",
                      # Show a plot of the generated distribution
                      mainPanel(
                        selectInput(inputId = "database", 
                                    label = "Select which summary dataset to view:",
                                    choices = list("Base datasets" = choice_vector_base,
                                                   "Overall" = choice_vector_overall,
                                                   "Race" = choice_vector_race,
                                                   "CAD" = choice_vector_cad)),
                        h2(textOutput("table_name")),
                        p("To filter the data below, click on the blank space below each column. To sort, click on the column name itself. To download the table, click on the download button below the table."),
                        
                        dataTableOutput("select_table"),
                        
                        downloadButton("down", "Download data")
                      )
             ),
             
             tabPanel("Plot data",
                      fluidRow(
                        # filter section
                        column(3,
                               column(6, strong("Select a base dataset, choose which filters to apply, and either view the filtered dataset (the 'Table' sub-tab) or generate plots of the  data (the 'Plot' sub-tab). Hit the reset button to reset filters"),
                                      actionButton("reset_input", "Reset filters"),
                                      uiOutput("filters_one")),
                               column(6, offset = 0, 
                                      uiOutput("filters_two")),
                        ),
                        
                        # start column section for plot and datatable
                        column(9,
                               
                               # tab panel for plot/datatable
                               tabsetPanel(
                                 
                                 # tab for plot
                                 tabPanel("Plot",
                                          fluidRow(p()),
                                          fluidRow(
                                            p(strong("Graphics are interactive."), "Click on legend items to show or hide those groups (double-click to hide all but one). Hover over the graph to see percent/total values. In the top-right of the graphic, click 'Compare data on hover' to compare values instead; click 'Download plot' to download the graphic you've constructed.")
                                            ),
                                          fluidRow(p()),
                                          
                                          # plot itself
                                          fluidRow(plotlyOutput("plot_gen")),
                                          
                                          # graph instructions below plot
                                          fluidRow(p("Select type of graph, axes, and groups for filtered data:")),
                                          ####  graph type selection
                                          fluidRow(
                                            
                                            # graph type
                                            column(3, 
                                                   selectInput("graph_type", 
                                                               label = "Graph type", 
                                                               choices = c("Bar", 
                                                                           "Scatter (trend)",
                                                                           "Pie"))),
                                            # graph parameters overall
                                            column(9, 
                                                   
                                                   # graph parameters based on selections
                                                   conditionalPanel(
                                                     condition = "input.graph_type == 'Bar' | input.graph_type == 'Scatter (trend)'",
                                                     
                                                     column(4, selectInput("y_axis_bar",
                                                                           "Y-axis (left side)",
                                                                           c(axis_y_choices_bar))),
                                                     
                                                     # graph parameters unique to bar/scatter
                                                     column(8,
                                                            
                                                            # graph parameters unique to bar
                                                            conditionalPanel(
                                                              condition = "input.graph_type == 'Bar'",
                                                              
                                                              # bar/traffic
                                                              conditionalPanel(condition = "input.plot_data == 'Traffic stops' | input.plot_data == 'Traffic violations'",
                                                                               column(6, selectInput("x_axis_ts",
                                                                                                     "X-axis (bottom)",
                                                                                                     choices = axis_x_choices_bar_ts)),
                                                                               
                                                                               column(6, selectInput("bar_group",
                                                                                                     "Group",
                                                                                                     choices = axis_group_choices_ts))
                                                              ),
                                                              
                                                              # bar/cad
                                                              conditionalPanel(condition = "input.plot_data == 'Computer-Aided Dispatch Base Data'",
                                                                               column(6, selectInput("x_axis_cad",
                                                                                                     "X-axis (bottom)",
                                                                                                     choices = axis_x_choices_bar_cad)),
                                                                               
                                                                               column(6, selectInput("bar_group_cad",
                                                                                                     "Group",
                                                                                                     choices = axis_group_choices_cad))
                                                              )
                                                            ),
                                                            
                                                            # graph parameters unique to scatter plot
                                                            conditionalPanel(
                                                              condition = "input.graph_type == 'Scatter (trend)'",
                                                              
                                                              # scatter/traffic
                                                              conditionalPanel(
                                                                condition = "input.plot_data == 'Traffic stops' | input.plot_data == 'Traffic violations'",
                                                                selectInput("scatter_group_ts",
                                                                            "Group",
                                                                            choices = axis_group_choices_ts_scatter)),
                                                              
                                                              # scatter/cad
                                                              conditionalPanel(
                                                                condition = "input.plot_data == 'Computer-Aided Dispatch Base Data'",
                                                                selectInput("scatter_group_cad",
                                                                            "Group",
                                                                            choices = axis_group_choices_cad))
                                                            )
                                                     )
                                                   ),
                                                   
                                                   # graph parameters if pie selected
                                                   conditionalPanel(condition = "input.graph_type == 'Pie'",
                                                                    
                                                                    # graph parameters pie/traffic stops/violations
                                                                    conditionalPanel(condition = "input.plot_data == 'Traffic stops' | input.plot_data == 'Traffic violations'",
                                                                                     selectInput("pie_group_ts",
                                                                                                 "Group",
                                                                                                 axis_x_choices_bar_ts)),
                                                                    
                                                                    
                                                                    # graph parameters pie/cad
                                                                    conditionalPanel(condition = "input.plot_data == 'Computer-Aided Dispatch Base Data'",
                                                                                     selectInput("pie_group_cad",
                                                                                                 "Group",
                                                                                                 axis_x_choices_bar_cad)
                                                                    )
                                                   )
                                                   
                                            ),
                                            
                                            p("Filtered and grouped data table:"),
                                            
                                            # filtered data table
                                            dataTableOutput("filter_df_show")
                                            
                                            
                                          ),
                                 ),
                                 # filtered data table on other tabset
                                 tabPanel("Table",
                                          downloadButton("down_filter", "Download filtered data"),
                                          p(""),
                                          dataTableOutput("plot_table")
                                          
                                          
                                 )
                               )
                               
                        )
                      )
             )
             
  )
  
)

# server arguments
server <- function(input, output) {
  
  # table
  output$table_name <- renderText({
    input$database
  })
  
  
  output$select_table <- renderDataTable({
    datatable(dfs[[input$database]], 
              filter = 'top', 
              options = list(autoWidth = TRUE))
  })
  
  # download button
  output$down <- downloadHandler(
    filename = function() paste0(input$database, ".xlsx"), 
    
    content = function(file) openxlsx::write.xlsx(dfs[[input$database]], 
                                                  file,
                                                  asTable = T)
  )
  
  output$table_name <- renderText({
    input$database
  })
  
  # define filters p1
  output$filters_one <- renderUI({
    times <- input$reset_input
    # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
    div(id=letters[(times %% length(letters)) + 1],
        # resettable ui
        selectInput(inputId = "plot_data",
                    label = "Base dataset:",
                    choices = choice_vector_base, 
                    selected = "Traffic stops"),
        
        conditionalPanel(
          condition = "input.plot_data == 'Traffic stops' | input.plot_data == 'Traffic violations'",
          
          # base filters
          checkboxGroupInput("ts_outcome",
                             label = "Outcomes:",
                             choices = unique_ts_outcomes,
                             selected = unique_ts_outcomes),
          
          dateRangeInput("ts_daterange",
                         label = "Dates:", 
                         # value = range_ts_dates[1],
                         start = range_ts_dates[1],
                         end = range_ts_dates[2]),
          
          sliderInput("ts_times",
                      label = "Times of day:",
                      value = c(0, 23),
                      min = 0,
                      max = 23),
          
          sliderInput("ts_age",
                      label = "Ages:",
                      value = c(15, 100),
                      min = 15,
                      max = 100),
          
          checkboxGroupInput("ts_race",
                             label = "Races:",
                             choices = unique_ts_race,
                             selected = unique_ts_race),
          
          checkboxGroupInput("ts_gender",
                             label = "Gender:",
                             choices = unique_ts_gender,
                             selected = unique_ts_gender)
        ),
        
        conditionalPanel(
          condition = "input.plot_data == 'Computer-Aided Dispatch Base Data'",
          
          # base filters
          checkboxGroupInput("cad_incidents",
                             label = "Incident:",
                             choices = unique_cad_incidents,
                             selected = unique_cad_incidents),
          
          dateRangeInput("cad_daterange",
                         label = "Dates:", 
                         # value = range_ts_dates[1],
                         start = range_cad_dates[1],
                         end = range_cad_dates[2])
        )
        
    )
  })
  
  # define filters p2
  output$filters_two <- renderUI({
    times <- input$reset_input
    # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
    div(id=letters[(times %% length(letters)) + 1],
        conditionalPanel(condition = "input.plot_data == 'Traffic stops' | input.plot_data == 'Traffic violations'",
                         selectInput("select_filter",
                                     "Select additional filters to apply (county/state, stop-reason). To deselect all choices and then select individual options, click on an empty space in the choice-box, hit ctrl-a, and hit backspace.",
                                     choices = c("Choose a filter",
                                                 "County",
                                                 "State",
                                                 "Reason"),
                                     selected = "Choose a filter"),
                         
                         conditionalPanel(
                           condition = "input.select_filter == 'State'",
                           selectInput(multiple = T,"ts_state",
                                              label = "State:",
                                              choices = sort(unique_ts_state),
                                              selected = unique_ts_state),
                         ),
                         
                         conditionalPanel(
                           condition = "input.select_filter == 'County'",
                           selectInput(multiple = T,"ts_county",
                                              label = "County:",
                                              choices = sort(unique_ts_county),
                                              selected = unique_ts_county),
                         ),
                         
                         conditionalPanel(
                           condition = "input.select_filter == 'Reason'",
                           selectInput(inputId = "ts_reason",
                                              label = "Stop reasons:",
                                       multiple = T,
                                              choices = unique_ts_descript,
                                              selected = unique_ts_descript)
                         )),
        
        # conditionalPanel(condition = "input.plot_data == 'Computer-Aided Dispatch Base Data'",
        #                  selectInput(multiple = T,"cad_officer",
        #                                     label = "Officers:",
        #                                     choices = unique_cad_officer,
        #                                     selected = unique_cad_officer))
        
    )
    
  })
  
  # create filtered dataset
  plot_filtered <- reactive({
    # browser()
    if (input$plot_data == 'Traffic stops' | input$plot_data == 'Traffic violations'){
      
      filter_df <- dfs[[input$plot_data]] %>%
        rename_with(.fn = ~ col_rename(.x, F)) %>%
        mutate(time_num = as.numeric(str_extract(time, "([0-9][0-9]){1}"))) %>%
        filter(outcome %in% input$ts_outcome) %>%
        filter(date >= input$ts_daterange[1] & date <= input$ts_daterange[2]) %>%
        filter(time_num >= input$ts_times[1] & time_num <= input$ts_times[2]) %>%
        filter(age >= input$ts_age[1] & age <= input$ts_age[2]) %>%
        filter(gender %in% input$ts_gender) %>%
        filter(raceethnicity %in% input$ts_race) %>%
        filter(county %in% input$ts_county) %>%
        filter(state %in% input$ts_state) %>%
        filter(summary %in% input$ts_reason)%>%
        mutate(age_range = factor(age_range, sort(.$age_range), sort(.$age_range)),
               time_num = factor(time_num, sort(.$time_num), sort(.$time_num)))
    }
    
    else if (input$plot_data == "Computer-Aided Dispatch Base Data"){
      filter_df <- dfs[[input$plot_data]] %>%
        rename_with(.fn = ~ col_rename(.x, F)) %>%
        filter(incident_type %in% input$cad_incidents) %>%
        filter(date >= input$cad_daterange[1] & date <= input$cad_daterange[2])
    }
    
    return(filter_df)
  })
  
  output$down_filter <- downloadHandler(
    filename = function() paste0("filter_", input$plot_data %>%
                                   tolower() %>% 
                                   gsub(" ", "_", .), ".xlsx"), 
    
    content = function(file) openxlsx::write.xlsx(plot_filtered(), 
                                                  file,
                                                  asTable = T)
  )
  
  # render data table for table tab
  output$plot_table <- renderDataTable({
    
    return <- plot_filtered()
    
    # drop time-num from traffic dfs
    if ("time_num" %in% colnames(return)){
      return <- return %>%
        select(-time_num)
    }
    
    datatable(return %>%
                `colnames<-`(colnames(dfs[[input$plot_data]])))
    
  })
  
  
  select_x_formal <- reactive({case_when(input$graph_type == "Bar" & grepl("Traffic", input$plot_data) ~ input$x_axis_ts,
                               input$graph_type == "Bar" & grepl("Computer", input$plot_data) ~ input$x_axis_cad,
                               input$graph_type == "Scatter (trend)" ~ "year",
                               input$graph_type == "Pie" & grepl("Traffic", input$plot_data) ~ input$pie_group_ts,
                               input$graph_type == "Pie" & grepl("Computer", input$plot_data) ~ input$pie_group_cad)})
  
  select_x <- reactive({col_rename(select_x_formal())})
  
  select_group_formal <- reactive({case_when(input$graph_type == "Bar"  & grepl("Traffic", input$plot_data)  ~ input$bar_group,
                                   input$graph_type == "Bar" & grepl("Computer", input$plot_data) ~ input$bar_group_cad,
                                   input$graph_type == "Scatter (trend)" & grepl("Traffic", input$plot_data) ~ input$scatter_group_ts,
                                   input$graph_type == "Scatter (trend)" & grepl("Computer", input$plot_data) ~ input$scatter_group_cad,
                                   input$graph_type == "Pie" & grepl("Traffic", input$plot_data) ~ input$pie_group_ts,
                                   input$graph_type == "Pie" & grepl("Computer", input$plot_data) ~ input$pie_group_cad)})
  
  select_group <- reactive({col_rename(select_group_formal())})
  
  
  type_val <- reactive({case_when(input$graph_type == "Bar" ~ "bar",
                        input$graph_type == "Scatter (trend)" ~ "scatter")})
  
  select_y <- reactive({case_when(input$y_axis_bar == "Total" ~ "grp_total",
                        input$y_axis_bar == "Percent" ~ "pct_grp")})
  
  select_y_formal <- reactive({input$y_axis_bar})
  
  
  # generate plotly plot based on bar type and dataset for plot tab
  grp_df <- reactive({
    
    req(input$plot_data)
    
    
    grp_df <- plot_filtered() %>%
      group_by(across(select_x())) %>%
      mutate("Total" = n()) %>%
      group_by(across(c(select_x(), select_group())), Total) %>%
      summarize(grp_total = n(),
                pct_grp = pct_round(grp_total, Total)) %>%
      rename(x_ax = !!sym(select_x()),
             grp = !!sym(select_group())) %>%
      distinct
    
    return(grp_df)
  })
  
  output$filter_df_show <- renderDataTable({

    datatable(grp_df())
  })
  
  output$plot_gen <- renderPlotly({

    grp_df <- grp_df()
    # select_group <- quo_global(select_group)
    # 
    # select_x <- quo_global(select_x)
    # 
    # select_y <- quo_global(select_y)
    
    # browser()
    
    if (input$graph_type == "Bar" | input$graph_type == "Scatter (trend)"){
      
      if (select_y() == "grp_total"){
        
        # if same group as x axis
        if (select_group() == select_x()){
          plot_obj <- plot_ly(
            data = grp_df,
            x = ~ grp,
            y = ~ grp_total,
            color = ~ grp,
            text = ~ paste0("Percent: ", pct_grp, "%"),
            name = ~ grp,
            type = type_val(),
            mode = "line+marker"
          ) %>%
            layout(xaxis = list(title = select_x_formal()),
                   yaxis = list(title = select_y_formal()),
                   title = paste0(input$plot_data, ": ", str_to_title(select_x_formal()), " by ", str_to_title(select_group_formal()), " total"))
        }
        
        else {
          # if x-axis and group are different
          plot_obj <- plot_ly(
            data = grp_df,
            x = ~ x_ax,
            y = ~ grp_total,
            text = ~ paste0("Percent: ", pct_grp, "%"),
            color = ~ grp,
            name = ~ grp,
            type = type_val(),
            mode = "line+marker"
          ) %>%
            layout(xaxis = list(title = select_x_formal()),
                   yaxis = list(title = select_y_formal()),
                   title = paste0(input$plot_data, ": ", str_to_title(select_x_formal()), " by ", str_to_title(select_group_formal()), " total"))
        }
        
      }
      
      else if (select_y() == "pct_grp"){
        
        if (select_x() == select_group()){
          plot_obj <- plot_ly(
            data = grp_df,
            x = ~ grp,
            y = ~ pct_grp,
            text = ~ paste0("Total: ", grp_total),
            color = ~ grp,
            name = ~ grp,
            type = type_val(),
            mode = "line+marker"
          ) %>%
            layout(xaxis = list(title = select_x_formal()),
                   yaxis = list(title = select_y_formal()),
                   title = paste0(input$plot_data, ": ", str_to_title(select_x_formal()), " by ", str_to_title(select_group_formal()), " percent"))
        }
        
        else {
          plot_obj <- plot_ly(
            data = grp_df,
            x = ~ x_ax,
            y = ~ pct_grp,
            text = ~ paste0("Total: ", grp_total),
            color = ~ grp,
            name = ~ grp,
            type = type_val(),
            mode = "line+marker"
          ) %>%
            layout(xaxis = list(title = select_x_formal()),
                   yaxis = list(title = select_y_formal()),
                   title = paste0(input$plot_data, ": ", str_to_title(select_x_formal()), " by ", str_to_title(select_group_formal()), " percent"))
        }
        
      }
      
    }
    
    else if (input$graph_type == "Pie"){

      plot_obj <- plot_ly(data = grp_df,
                          values = ~ grp_total,
                          labels = ~ grp,
                          type = "pie",
                          textinfo = 'label+percent',
                          texposition = "inside",
                          hole = 0.4) %>%
        layout(title = paste0(input$plot_data, ": ", str_to_title(select_x_formal())))
      
    }
    
    return(plot_obj)
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)