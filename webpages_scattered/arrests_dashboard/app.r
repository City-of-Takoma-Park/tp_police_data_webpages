library(shiny)
library(DT)
library(shinythemes)
library(dplyr)
library(tidyverse)
library(plotly)
library(rlang)
library(tpfuncts)

# read in dfs
dfs <- readRDS("./data/list_arrests.rds")

base_arrests <- read_rds("./data/all_arrests_final.rds") %>%
  select(-c(month))

base_arrests_listready <- base_arrests %>%
  rename("id" = unique_id,
         "report" = report.,
         "date" = date,
         "year" = year,
         "specific_type" = type_desc,
         "incident_ward" = i.ward,
         "arrestee_ward" = a.ward,
         "city_of_residence" = a.city,
         "county_of_residence" = a.county,
         "state_of_residence" = a.state,
         "race" = a.race,
         "gender" = a.gender,
         "age" = a.age,
         "initiation" = initiated)

dfs <- c(list("Arrests" = base_arrests_listready), dfs)

choice_vector <- names(dfs)

choice_vector_base <- "Arrests"
choice_vector_tp <- grep("TP resident", choice_vector, value = T, ignore.case = T)
choice_vector_race <- grep("Race", choice_vector, value = T, ignore.case = T) %>%
  grep("TP resident", ., value = T, ignore.case = T, invert = T)
choice_vector_overall <- choice_vector[!choice_vector %in% c(choice_vector_tp, choice_vector_race)]

choice_vector_order <- c("Arrest type",
                         "Year",
                         "Gender",
                         "Gender by type",
                         "Age by type",
                         "Arrestee county",
                         "Arrestee city",
                         "Arrests by arrestee city and type",
                         "Initiation",
                         "Initiated by year",
                         "Specific-type",
                         "Specific-type by year",
                         "Offense")

# choice_vector_base <- grep("(Traffic violations)|(Arrests)|(Computer-Aided Dispatch Base)", choice_vector, invert = F, value = T)

range_ar_dates <- base_arrests$date %>% range


uniquify <- function(col){
  base_arrests %>%
    pull({{col}}) %>%
    unique %>%
    as.character() %>%
    sort
}

unique_type <- uniquify(overall_type)

unique_cat <- base_arrests %>%
  pull(category) %>%
  unique %>%
  as.character() %>%
  sort

unique_typedesc <- base_arrests %>%
  pull(type_desc) %>%
  unique %>%
  as.character() %>%
  sort

unique_offense <- base_arrests %>%
  pull(offense) %>%
  unique %>%
  as.character() %>%
  sort

unique_ward <- base_arrests %>%
  pull(a.ward) %>%
  unique %>%
  as.character() %>%
  sort

unique_city <- base_arrests %>%
  pull(a.city) %>%
  unique %>%
  as.character() %>%
  sort

unique_county <- uniquify(a.county)

unique_state <- uniquify(a.state)

unique_race <- uniquify(a.race)

unique_gender <- uniquify(a.gender)

range_age <- range(base_arrests$a.age)

unique_init <- uniquify(initiated)

axis_x_choices_bar <- c("Year",
                        "Overall type",
                        "Category",
                        "Specific type",
                        "Arrestee ward",
                        "City of residence",
                        "County of residence",
                        "State of residence",
                        "Race",
                        "Gender",
                        "Age range",
                        "Initiation") %>%
  sort


axis_y_choices_bar <- c("Total",
                        "Percent")

axis_group_choices <- axis_x_choices_bar[axis_x_choices_bar != "Year"]

axis_group_choices_scatter <- axis_group_choices

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


ui <- fluidPage(

  theme = shinytheme("flatly"),

  # Application title
  titlePanel("Arrests Summary Data Explorer"),

  p("A dashboard allowing users to explore summary datasets used to produce the Takoma Park arrests data webpage (the ", strong("'Summary Datasets'"), " tab), and filter/generate plots on their own based on their own interests (the ", strong("'Plot Data'"), " tab)"),

  # navigation pane
  navbarPage("",
             tabPanel("Summary datasets",
                      # Show a plot of the generated distribution
                      mainPanel(
                        selectInput(inputId = "database",
                                    label = "Select which summary dataset to view:",
                                    choices = list("Base dataset" = choice_vector_base,
                                                   "Overall" = choice_vector_order,
                                                   "Takoma Park residents" = choice_vector_tp,
                                                   "Race" = choice_vector_race)),
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
                               column(6, strong("Choose which filters to apply to the arrests dataset, and either view the filtered dataset (the 'Table' sub-tab) or generate plots of the data (the 'Plot' sub-tab). Hit the reset button to reset filters"),
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

                                          # intro text
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

                                                              column(6, selectInput("x_axis_ts",
                                                                                    "X-axis (bottom)",
                                                                                    choices = axis_x_choices_bar)),

                                                              column(6, selectInput("bar_group",
                                                                                    "Group",
                                                                                    choices = axis_group_choices))

                                                            ),

                                                            # graph parameters unique to scatter plot
                                                            conditionalPanel(
                                                              condition = "input.graph_type == 'Scatter (trend)'",

                                                              # scatter/traffic
                                                              selectInput("scatter_group_ts",
                                                                          "Group",
                                                                          choices = axis_group_choices_scatter)

                                                              # scatter/cad
                                                            )
                                                     )
                                                   ),

                                                   # graph parameters if pie selected
                                                   conditionalPanel(condition = "input.graph_type == 'Pie'",

                                                                    # graph parameters pie/Arrests/violations

                                                                    selectInput("pie_group_ts",
                                                                                "Group",
                                                                                axis_x_choices_bar)                                                   )

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

# conditionalPanel(
#   condition = "input.plot_data == 'Computer-Aided Dispatch Base Data'",
#   checkboxGroupInput(inputId = "cad_incident",
#                      label = "Incident types:",
#                      choices = unique_cad_incidents,
#                      selected = unique_cad_incidents),
#
#   checkboxGroupInput(inputId = "cad_year",
#                      label = "Years:",
#                      choices = unique_cad_years,
#                      selected = unique_cad_years)
#
#
# )


# Define server logic required to draw a histogram
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

        # base filters
        checkboxGroupInput("ar_type",
                           label = "Arrest type:",
                           choices = unique_type,
                           selected = unique_type),

        dateRangeInput("ar_daterange",
                       label = "Dates:",
                       # value = range_ar_dates[1],
                       start = range_ar_dates[1],
                       end = range_ar_dates[2]),

        sliderInput("ar_age",
                    label = "Ages:",
                    value = range_age,
                    min = range_age[1],
                    max = range_age[2]),

        checkboxGroupInput("ar_race",
                           label = "Races:",
                           choices = unique_race,
                           selected = unique_race),

        checkboxGroupInput("ar_gender",
                           label = "Gender:",
                           choices = unique_gender,
                           selected = unique_gender),

        checkboxGroupInput("ar_initiation",
                           label = "Initiation:",
                           choices = unique_init,
                           selected = unique_init)
    )
  })

  # define filters p2
  output$filters_two <- renderUI({
    times <- input$reset_input
    # https://stackoverflow.com/questions/24265980/reset-inputs-button-in-shiny-app
    div(id=letters[(times %% length(letters)) + 1],
        selectInput("select_filter",
                    "Select additional filters to apply (county/state, specific-type, offense). To deselect all choices and then select individual options, click on an empty space in the choice-box, hit ctrl-a, and hit backspace.",
                    choices = c("Choose a filter",
                                "City",
                                "County",
                                "State",
                                "Category",
                                "Offense",
                                "Specific-type",
                                "Ward"),
                    selected = "Choose a filter"),

        conditionalPanel(
          condition = "input.select_filter == 'City'",
          selectInput(multiple = T,"ar_city",
                      label = "City:",
                      choices = sort(unique_city),
                      selected = unique_city),
        ),

        conditionalPanel(
          condition = "input.select_filter == 'State'",
          selectInput(multiple = T,"ar_state",
                      label = "State:",
                      choices = sort(unique_state),
                      selected = unique_state),
        ),

        conditionalPanel(
          condition = "input.select_filter == 'County'",
          selectInput(multiple = T,"ar_county",
                      label = "County:",
                      choices = sort(unique_county),
                      selected = unique_county),
        ),

        conditionalPanel(
          condition = "input.select_filter == 'Category'",
          selectInput(inputId = "ar_cat",
                      label = "Offense category:",
                      multiple = T,
                      choices = unique_cat,
                      selected = unique_cat)
        ),

        conditionalPanel(
          condition = "input.select_filter == 'Offense'",
          selectInput(multiple = T,"ar_offense",
                      label = "Offense:",
                      choices = unique_offense,
                      selected = unique_offense)
        ),
        conditionalPanel(
          condition = "input.select_filter == 'Specific-type'",
          selectInput(multiple = T,"ar_spectype",
                      label = "Specific-type:",
                      choices = unique_typedesc,
                      selected = unique_typedesc)
        ),
        conditionalPanel(
          condition = "input.select_filter == 'Ward'",
          selectInput(multiple = T,"ar_ward",
                      label = "Ward:",
                      choices = unique_ward,
                      selected = unique_ward)
        )
    )

  })

  # create filtered dataset
  plot_filtered <- reactive({

    # browser()
    filter_df <- base_arrests_listready %>%
      rename_with(.fn = ~ col_rename(.x, F)) %>%
      filter(overall_type %in% input$ar_type) %>%
      filter(date >= input$ar_daterange[1] & date <= input$ar_daterange[2]) %>%
      filter(age >= input$ar_age[1] & age <= input$ar_age[2]) %>%
      filter(gender %in% input$ar_gender) %>%
      filter(race %in% input$ar_race) %>%
      filter(county_of_residence %in% input$ar_county) %>%
      filter(state_of_residence %in% input$ar_state) %>%
      filter(initiation %in% input$ar_initiation) %>%
      filter(city_of_residence %in% input$ar_city) %>%
      filter(category %in% input$ar_cat) %>%
      filter(offense %in% input$ar_offense) %>%
      filter(specific_type %in% input$ar_spectype) %>%
      filter(arrestee_ward %in% input$ar_ward) %>%
      mutate(age_range = factor(age_range, sort(.$age_range), sort(.$age_range)))

    return(filter_df)
  })

  output$down_filter <- downloadHandler(
    filename = function() paste0("filter_arrests.xlsx"),

    content = function(file) openxlsx::write.xlsx(plot_filtered(),
                                                  file,
                                                  asTable = T)
  )

  # render data table for table tab
  output$plot_table <- renderDataTable({
    datatable(plot_filtered() %>%
                `colnames<-`(colnames(base_arrests_listready)))
  })


  select_x_formal <- reactive({case_when(input$graph_type == "Bar" ~ input$x_axis_ts,
                                         input$graph_type == "Scatter (trend)" ~ "year",
                                         input$graph_type == "Pie" ~ input$pie_group_ts)})

  select_x <- reactive({col_rename(select_x_formal())})

  select_group_formal <- reactive({case_when(input$graph_type == "Bar" ~ input$bar_group,
                                             input$graph_type == "Scatter (trend)" ~ input$scatter_group_ts,
                                             input$graph_type == "Pie" ~ input$pie_group_ts)})

  select_group <- reactive({col_rename(select_group_formal())})


  type_val <- reactive({case_when(input$graph_type == "Bar" ~ "bar",
                                  input$graph_type == "Scatter (trend)" ~ "scatter")})

  select_y <- reactive({case_when(input$y_axis_bar == "Total" ~ "grp_total",
                                  input$y_axis_bar == "Percent" ~ "pct_grp")})

  select_y_formal <- reactive({input$y_axis_bar})


  # generate plotly plot based on bar type and dataset for plot tab
  grp_df <- reactive({

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
                   title = paste0("Arrests: ", str_to_title(select_x_formal()), " by ", str_to_title(select_group_formal()), " total"))
        }

        else {
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
                   title = paste0("Arrests: ", str_to_title(select_x_formal()), " by ", str_to_title(select_group_formal()), " total"))
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
                   title = paste0("Arrests: ", str_to_title(select_x_formal()), " by ", str_to_title(select_group_formal()), " percent"))
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
                   title = paste0("Arrests: ", str_to_title(select_x_formal()), " by ", str_to_title(select_group_formal()), " percent"))
        }

      }

    }

    else if (input$graph_type == "Pie"){

      # grp_df <- plot_filtered() %>%
      #   group_by(across(pie_vals)) %>%
      #   summarize(select_vals = n()) %>%
      #   distinct %>%
      #   rename(pie_name = !!sym(pie_vals))

      # pie_vals <- quo_global(pie_vals)

      # select_vals <- "select_vals"
      #
      # select_vals <- quo_global(select_vals)

      plot_obj <- plot_ly(data = grp_df,
                          values = ~ grp_total,
                          labels = ~ grp,
                          type = "pie",
                          textinfo = 'label+percent',
                          texposition = "inside",
                          hole = 0.4) %>%
        layout(title = paste0("Arrests: ", str_to_title(select_x_formal())))

    }

    return(plot_obj)
  })


}

# Run the application
shinyApp(ui = ui, server = server)
