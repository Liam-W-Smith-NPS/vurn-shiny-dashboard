# Load packages and functions
library(tidyverse)
library(here)
library(sf)
library(odbc)
library(DBI)
library(tmap)
library(tmap.glyphs) # using the development version, install with: remotes::install_github("r-tmap/tmap.glyphs")
library(htmltools)
library(readxl)
library(shiny)
library(bslib)
library(plotly)
library(shinyWidgets)
library(classInt)
source("functions.R")
data <- load_data()
tmap_mode("view")

# User interface
ui <- page_sidebar(
  # CSS to remove horizontal scroll bars from animations
  tags$style(HTML("
    .shiny-image-output img {
    max-width: 100%;
    height: auto;
    }")),
  title = "Visitor Use Dashboard",
  # Sidebar with filters that apply to visualizations in all subpages
  sidebar = sidebar(
    selectInput(
      "park",
      label = "Park",
      choices = data$park_codes,
      selected = "KATM"
    ),
    sliderInput(
      "year",
      "Year Range",
      min = 2007,
      max = 2024,
      value = c(2007, 2024),
      sep = "" # this removes the comma in the year
    )
  ),
  page_navbar(
    # First page with park-level graphs
    nav_panel(title = "Park Summary",
              layout_columns(
                col_widths = breakpoints(
                  xl = c(6, 6), # larger screens have two columns of graphs
                  lg = c(12, 12) # on smaller screens each graph takes up the whole page width and you have to scroll
                ),
                fill = FALSE,
                layout_columns(
                  col_widths = c(12,12),
                  card(
                    card_header("Total Visitation over the Years"),
                    plotlyOutput("park_annual_visitation_graph"),
                    full_screen = TRUE
                  ),
                  card(
                    card_header("Monthly Visitation over the Years"),
                    plotlyOutput("park_monthly_visitation_graph"),
                    full_screen = TRUE
                  )),
                layout_columns(
                  col_widths = c(12,12),
                  card(
                    card_header("Daily Visitation Compared Across Years"),
                    plotlyOutput("park_daily_visitation_graph"),
                    full_screen = TRUE
                  ),
                  card(
                    card_header("Annual Visitor Use Animation"),
                    imageOutput("park_annual_visitation_animation"),
                    full_screen = TRUE
                  )
                ))
              ),
    # Second page with VUA-level graphs
    nav_panel(title = "Inspect by Area",
                  pickerInput(
                    "visitor_use_area",
                    label = "Visitor Use Area",
                    choices = character(0),
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  ),
              layout_columns(
                col_widths = breakpoints(
                  xl = c(6, 6), # larger screens have two columns of graphs
                  lg = c(12, 12) # on smaller screens each graph takes up the whole page width and you have to scroll
                ),
                fill = FALSE,
                layout_columns(
                  col_widths = c(12, 12),
                  card(
                    card_header("Monthly Visitation as a Proportion of All User Days"),
                    plotlyOutput("vua_monthly_proportion_graph"),
                    full_screen = TRUE
                    ),
                  card(
                    card_header("First Date of Use Through the Years"),
                    plotlyOutput("vua_first_use_graph"),
                    full_screen = TRUE
                    )),
                layout_columns(
                  card(
                    card_header("Daily Count"),
                    plotlyOutput("vua_daily_visitation_graph"),
                    full_screen = TRUE
                    ))
                )),
    # Third page with graphs and a map for selected commercial activities 
    nav_panel(title = "Inspect by Activity",
              pickerInput(
                "commercial_activity",
                label = "Commercial Activity",
                choices = character(0),
                multiple = TRUE,
                options = list(`actions-box` = TRUE)
                ),
              layout_columns(
                col_widths = breakpoints(
                  xl = c(6, 6), # larger screens have two columns of graphs
                  lg = c(12, 12) # on smaller screens each graph takes up the whole page width and you have to scroll
                ),
                fill = FALSE,
                layout_columns(
                  col_widths = c(12,12),
                  card(
                    card_header("Commercial Activities Daily Count"),
                    plotlyOutput("activity_daily_visitation_graph"),
                    full_screen = TRUE
                  ),
                  card(
                    card_header("Commercial Activities Annual Count through the Years"),
                    plotlyOutput("activity_annual_visitation_graph"),
                    full_screen = TRUE
                  )
                ),
                layout_columns(
                  card(
                    card_header("Map of Selected Activities"),
                    tmapOutput("activity_map"),
                    full_screen = TRUE
                  ))
              )),
    # Fourth page with top commercial activity map and donut map
    nav_panel(title = "Commercial Activity Maps",
              layout_columns(
                col_widths = breakpoints(
                  xl = c(6, 6), # larger screens have two columns of graphs
                  lg = c(12, 12) # on smaller screens each graph takes up the whole page width and you have to scroll
                ),
                fill = TRUE,
                card(
                  card_header("All Commercial Activities in Each Visitor Use Area"),
                  tmapOutput("map_donut"),
                  full_screen = TRUE),
                card(
                  card_header("Top Commercial Activity in Each Visitor Use Area"),
                  tmapOutput("map_top_activity"),
                  full_screen = TRUE
              ))),
    # Fifth page with animations
    nav_panel(title = "Animations",
              layout_columns(
                col_widths = breakpoints(
                  xl = c(6, 6), # larger screens have two columns of graphs
                  lg = c(12, 12) # on smaller screens each graph takes up the whole page width and you have to scroll
                ),
                fill = TRUE,
                card(
                  card_header("Monthly Visitor Use Animation"),
                  imageOutput("animation_by_month"),
                  full_screen = TRUE),
                card(
                  card_header("Annual Visitor Use Animation & Burned Areas"),
                  imageOutput("animation_fire"),
                  full_screen = TRUE)
                ))
  ))

# Server logic
server <- function(input, output, session) {
  # ----------------------------------------------------
  # Park and Year Sidebar
  # ----------------------------------------------------
  # Making year options change depending on the selected park
  min_year <- reactive({data$visitor_use_export |> 
      filter(UnitCode == input$park) |>
      pull(AccountYear) |>
      min()})
  
  max_year <- reactive({data$visitor_use_export |> 
      filter(UnitCode == input$park) |>
      pull(AccountYear) |>
      max()}) 

  observeEvent(input$park, {
    updateSliderInput(session = session,
                      inputId = "year",
                      min = min_year(),
                      max = max_year(),
                      value = c(min_year(), max_year()))
  })
  # ----------------------------------------------------
  # Park Summary page
  # ----------------------------------------------------
  # Total annual visitation over the years: data
  # Runs once
  park_annual_visitation_data <- data$visitor_use_export |>
    group_by(UnitCode,
             AccountYear) |>
    summarise(UserDays = sum(UserDays), .groups = "drop")
  
  # Total annual visitation over the years: graph
  # Runs when filters modified
  output$park_annual_visitation_graph <- renderPlotly({
    ggplotly(p = park_annual_visitation_data |>
               filter(UnitCode == input$park,
                      AccountYear <= input$year[2] & AccountYear >= input$year[1]) |>
               ggplot(aes(x = AccountYear,
                          y = UserDays)) +
               geom_line() +
               geom_point() +
               theme_classic() +
               xlab("Year") +
               ylab("Reported User Days")
    ) |>
      highlight(on = "plotly_click",
                off = "plotly_relayout")
  })
  
  # Daily visitation compared across years: data
  # Runs once
  park_daily_visitation_data <- data$daily_visitor_use |>
    group_by(UnitCode, AccountYear, Date) |> 
    summarise(UserDays = sum(UserDays), .groups = "drop") |>
    filter(UserDays != 0) |>
    mutate(DisplayDate = Date,
           Date =  update(Date, year = 3000))

  # Daily visitation compared across years: graph
  # Runs when filters modified
  output$park_daily_visitation_graph <- renderPlotly({
    ggplotly(p = park_daily_visitation_data |>
               filter(UnitCode == input$park,
                      AccountYear <= input$year[2] & AccountYear >= input$year[1]) |>
               mutate(AccountYear = as.factor(AccountYear)) |>
               highlight_key(~AccountYear) |>
               ggplot(aes(x = Date,
                          y = UserDays,
                          color = AccountYear,
                          label = DisplayDate)) +
               geom_point() +
               theme_classic() +
               xlab("Date") +
               ylab("Reported User Days") +
               labs(color = "Year"),
             tooltip = c("DisplayDate", "UserDays")
             ) |>
      highlight(on = "plotly_click",
                off = "plotly_relayout")
  })
  
  # Monthly visitation over the years: data
  # Runs once
  park_monthly_visitation_data <- data$daily_visitor_use |>
    group_by(UnitCode, AccountYear, MonthName, MonthNum) |>
    summarise(UserDays = sum(UserDays), .groups = "drop") |>
    mutate(MonthName = factor(MonthName, 
                              levels = c("January", "February", "March", "April", 
                                         "May", "June", "July", "August", "September", 
                                         "October", "November", "December")))
  
  # Monthly visitation over the years: graph
  # Runs when filters modified
  output$park_monthly_visitation_graph <- renderPlotly({
    ggplotly(p = park_monthly_visitation_data |>
               filter(UnitCode == input$park,
                      AccountYear <= input$year[2] & AccountYear >= input$year[1]) |>
               highlight_key(~MonthName) |>
               ggplot(aes(x = AccountYear,
                          y = UserDays,
                          color = MonthName)) +
               geom_line() +
               geom_point() +
               theme_classic() +
               xlab("Year") +
               ylab("Reported User Days") +
               labs(color = "Month")
    ) |>
      highlight(on = "plotly_click",
                off = "plotly_relayout")
  })
  
  # Annual Visitor Use Animation: data & animation
  # Runs when filters modified
  output$park_annual_visitation_animation <- renderImage({
    # Include progress bar
    withProgress(message = "Generating animation...", {
      # Switch to plot mode for the creation of the animation
      tmap_mode("plot")
      
      # Set the file path for the animation
      out <- tempfile(fileext = ".gif")
      
      # Create animation using function in functions.R
      animation <- create_animated_choropleth(data$visitor_use_export |>
                                                filter(UnitCode == input$park,
                                                       AccountYear <= input$year[2] & AccountYear >= input$year[1]), 
                                              data$vua_animation_data |>
                                                filter(UNITCODE == input$park), # selects just min_year:max_year based on visitor_use_export within function
                                              input$park)
      # Write animation to file
      tmap_animation(animation,
                     filename = out)
      
      # Switch back to view mode
      tmap_mode("view")
    })
    expr = list(src = out)}, # file path to animation
    deleteFile = TRUE
  )
  
  # ----------------------------------------------------
  # Inspect by Area page
  # ----------------------------------------------------
  # Making visitor use area options change depending on the selected park
  vua <- reactive({data$visitor_use_export |> 
      filter(UnitCode == input$park) |>
      distinct(Location) |>
      pull(Location) |>
      sort()}) 
  
  observeEvent(input$park, {
    updatePickerInput(session = session,
                      inputId = "visitor_use_area",
                      choices = vua(),
                      selected = character(0)) # by default nothing selected
  })
  
  # Monthly visitation as a proportion of all user days: data
  # Runs when filters modified -- needed because aggregating multiple years (which the user can filter) into monthly summary
  vua_monthly_proportion_data <- reactive({
    data$daily_visitor_use |>
      filter(UnitCode == input$park,
             Location %in% input$visitor_use_area,
             AccountYear <= input$year[2] & AccountYear >= input$year[1]) |>
      group_by(UnitCode, Location, MonthNum) |>
      summarise(UserDays = sum(UserDays), .groups = "drop_last") |>
      filter(UserDays != 0) |>
      mutate(Total = sum(UserDays)) |>
      ungroup() |>
      mutate(Percent = UserDays/Total * 100)
    })
  
  # Monthly visitation as a proportion of all user days: graph
  # Runs when filters modified
  output$vua_monthly_proportion_graph <- renderPlotly({
    # Display helpful validation message rather than scary red error
    validate(
      need(!is.na(input$visitor_use_area), "Please select visitor use area(s)")
    )
    # Create graph
    withProgress(message = "Generating figure...", {
      ggplotly(p = vua_monthly_proportion_data() |>
                 highlight_key(~Location) |>
                 ggplot(aes(x = MonthNum,
                            y = Percent,
                            color = Location)) +
                 geom_line() +
                 geom_point() +
                 theme_classic() +
                 xlab("Month") +
                 ylab("Percent of Total User Days")) |>
        highlight(on = "plotly_click",
                  off = "plotly_relayout")
    })
  })
  
  # Daily count: data
  # Runs when filters modified -- needed because aggregating multiple years (which the user can filter) into daily summary
  vua_daily_visitation_data <- reactive({data$daily_visitor_use |>
    filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
           UnitCode == input$park,
           Location %in% input$visitor_use_area) |>
    mutate(Date =  update(Date, year = 3000)) |>
    group_by(UnitCode, Location, Date) |>
    summarise(UserDays = sum(UserDays), .groups = "drop") |>
    filter(UserDays != 0) |>
    mutate(`Month-Date` = str_extract(Date, "(?<=-).*"))})
  
  # Daily count: graph
  # Runs when filters modified
  output$vua_daily_visitation_graph <- renderPlotly({
    # Display helpful validation message rather than scary red error
    validate(
      need(!is.na(input$visitor_use_area), "Please select visitor use area(s)")
    )
    # Create graph
    withProgress(message = "Generating figure...", {
      ggplotly(p = vua_daily_visitation_data() |>
                 highlight_key(~Location) |>
                 ggplot(aes(x = Date,
                            y = UserDays,
                            color = Location,
                            label = `Month-Date`)) +
                 geom_line() +
                 geom_point() +
                 theme_classic() +
                 xlab("Date") +
                 ylab("Reported User Days"),
               tooltip = c("Location", "Month-Date", "UserDays")
               ) |>
        highlight(on = "plotly_click",
                  off = "plotly_relayout")
    })
  })
  
  # First day of use through the years: data
  # Runs once
  vua_first_use_data <- data$daily_visitor_use |>
    group_by(UnitCode, Location, AccountYear) |>
    summarise(first_reported_use = min(Date), .groups = 'drop') |>
    mutate(first_reported_use = update(first_reported_use, year = 3000),
           `Month-Date` = str_extract(first_reported_use, "(?<=-).*"))
  
  # First day of use through the years: graph
  # Runs when filters modified
  output$vua_first_use_graph <- renderPlotly({
    # Display helpful validation message rather than scary red error
    validate(
      need(!is.na(input$visitor_use_area), "Please select visitor use area(s)")
    )
    # Create graph
    withProgress(message = "Generating figure...", {
      ggplotly(p = vua_first_use_data |>
                 filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
                        UnitCode == input$park,
                        Location %in% input$visitor_use_area) |>
                 highlight_key(~Location) |>
                 ggplot(aes(x = AccountYear, 
                            y = first_reported_use,
                            color = Location,
                            label = `Month-Date`)) +
                 geom_line() +
                 geom_point() +
                 theme_classic() +
                 xlab("Year") +
                 ylab("Date of First Reported Use") +
                 scale_x_continuous(breaks = seq(from = input$year[1], 
                                                 to = input$year[2], 
                                                 by = if(input$year[2] - input$year[1] > 7) 2 else 1)),
                 # Parameters that were helpful for creating the Chinitna Bay graph but don't generalize well
                 # scale_y_date(date_breaks = "5 days",
                 #              date_labels = "%B %d")
               tooltip = c("Location", "Month-Date", "AccountYear")
      ) |>
        highlight(on = "plotly_click",
                  off = "plotly_relayout")
    })
  })
  
  # ----------------------------------------------------
  # Inspect by Activity page
  # ----------------------------------------------------
  # Making commercial activity options change depending on the selected park
  activity <- reactive({data$visitor_use_export |> 
      filter(UnitCode == input$park) |>
      distinct(CommercialActivity) |>
      pull(CommercialActivity) |>
      sort()}) 
  
  observeEvent(input$park, {
    updatePickerInput(session = session,
                      inputId = "commercial_activity",
                      choices = activity(),
                      selected = character(0)) # by default nothing selected
  })
  
  # Map of selected activities: data and map
  # Runs when filters modified
  output$activity_map <- renderTmap({
    create_interactive_choropleth(vua_map_data = data$vua_geom |>
                                    left_join(data$visitor_use_export |>
                                                filter(CommercialActivity %in% input$commercial_activity,
                                                       AccountYear <= input$year[2] & AccountYear >= input$year[1]) |>
                                                group_by(UnitCode, Location) |>
                                                summarise(UserDays = sum(UserDays), .groups = "drop"), # sum UserDays together for each location
                                              by = join_by(UNITCODE == UnitCode, # redundant but safe 
                                                           Name == Location)),
                                  basemap = basemaps$park_tiles_standard,
                                  park = input$park)
  })
  
  # Commercial activities daily count: data
  # Runs when filters modified -- needed because aggregating multiple years (which the user can filter) into daily summary
  activity_daily_visitation_data <- reactive({data$daily_visitor_use |>
      filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
             UnitCode == input$park,
             CommercialActivity %in% input$commercial_activity) |>
      mutate(Date =  update(Date, year = 3000)) |>
      group_by(UnitCode, CommercialActivity, Date) |>
      summarise(UserDays = sum(UserDays), .groups = "drop") |>
      filter(UserDays != 0) |>
      mutate(`Month-Date` = str_extract(Date, "(?<=-).*"))})
  
  # Commercial activities daily count: graph
  # Runs when filters modified
  output$activity_daily_visitation_graph <- renderPlotly({
    # Display helpful validation message rather than scary red error
    validate(
      need(!is.na(input$commercial_activity), "Please select commercial activity(s)")
    )
    # Create graph
    ggplotly(p = activity_daily_visitation_data() |>
               highlight_key(~CommercialActivity) |>
               ggplot(aes(x = Date,
                          y = UserDays,
                          color = CommercialActivity,
                          label = `Month-Date`)) +
               geom_line() +
               geom_point() +
               theme_classic() +
               xlab("Date") +
               ylab("Reported User Days") +
               labs(color = "Commercial Activity"),
             tooltip = c("CommercialActivity", "Month-Date", "UserDays")
    ) |>
      highlight(on = "plotly_click",
                off = "plotly_relayout")
  })
  
  # Commercial activities annual count through the years: data
  # Runs once
  activity_annual_visitation_data <- data$visitor_use_export |>
    group_by(UnitCode,
             CommercialActivity,
             AccountYear) |>
    summarise(UserDays = sum(UserDays), .groups = "drop")
  
  # Commercial activities annual count through the years: graph
  # Runs when filters modified
  output$activity_annual_visitation_graph <- renderPlotly({
    # Display helpful validation message rather than scary red error
    validate(
      need(!is.na(input$commercial_activity), "Please select commercial activity(s)")
    )
    # Create graph
    ggplotly(p = activity_annual_visitation_data |>
               filter(UnitCode == input$park,
                      AccountYear <= input$year[2] & AccountYear >= input$year[1],
                      CommercialActivity %in% input$commercial_activity) |>
               highlight_key(~CommercialActivity) |>
               ggplot(aes(x = AccountYear,
                          y = UserDays,
                          color = CommercialActivity)) +
               geom_line() +
               geom_point() +
               theme_classic() +
               xlab("Year") +
               ylab("Reported User Days") +
               labs(color = "Commercial Activity")
    ) |>
      highlight(on = "plotly_click",
                off = "plotly_relayout")
  })
  
  # ----------------------------------------------------
  # Commercial Activity Maps page
  # ----------------------------------------------------
  # Donut map: data
  # Runs when filters modified
  donut_all_activities_data <- reactive({
    data$vua_geom |>
      left_join(data$visitor_use_export |>
                  filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
                         UnitCode == input$park) |>
                  group_by(UnitCode, Location, CommercialActivity) |>
                  summarise(UserDays = sum(UserDays), .groups = "drop") |> # sum UserDays for each activity in each location
                  pivot_wider(names_from = CommercialActivity, # pivot to create column of userday count for each activity
                              values_from = UserDays,
                              values_fill = 0) |> # impute 0 for non-reported activities (in locations where other activities were reported)
                  mutate(`Total Reported User Days` = rowSums(across(3:last_col()), na.rm = TRUE)), # calculate total user days
                by = join_by(UNITCODE == UnitCode, 
                             Name == Location))
      })
  
  # Donut map: map
  # Runs when filters modified
  output$map_donut <- renderTmap({
    create_donut_map(data$visitor_use_export |>
                       filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
                              UnitCode == input$park), 
                     donut_all_activities_data(), 
                     basemaps$park_tiles_standard, 
                     input$park)
  })
  
  # Top commercial activity: data and map
  # Runs when filters modified
  output$map_top_activity <- renderTmap({
    tm_shape(data$vua_geom |>
               filter(UNITCODE == input$park) |>
               left_join(data$visitor_use_export |>
                           filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
                                  UnitCode == input$park) |>
                           group_by(UnitCode, Location, CommercialActivity) |>
                           summarise(UserDays = sum(UserDays), .groups = "drop_last") |> # sum UserDays together for each combo of location and activity
                           slice_max(UserDays, with_ties = FALSE) |> # within each group (park & location), slice out the row with the most UserDays
                           ungroup(),
                         by = join_by(UNITCODE == UnitCode, # redundant but safe
                                      Name == Location)) |>
               rename(ActivityUserDays = UserDays),
             name = "Top Commercial Activities") +
      tm_polygons(fill = "CommercialActivity",
                  fill.scale = tm_scale_categorical(values = "Set3",
                                                    label.na = "No Commercial Use"),
                  fill.legend = tm_legend("Top Commercial Activity",
                                          bg.alpha = 0.9,
                                          col = 'black'
                  ),
                  fill_alpha = 0.7,
                  id = "Name",
                  col = "white",
                  col_alpha = 0.7,
                  lwd = 0.25) +
      tm_shape(data$vua_geom |>
                 filter(UNITCODE == input$park) |>
                 left_join(data$visitor_use_export |>
                             filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
                                    UnitCode == input$park) |>
                             group_by(UnitCode, Location) |>
                             summarise(UserDays = sum(UserDays), .groups = "drop"), # sum UserDays together for each location
                           by = join_by(UNITCODE == UnitCode, # redundant but safe
                                        Name == Location)) |>
                 st_point_on_surface(),
               name = "Total Reported User Days") +
      tm_bubbles(size = "UserDays",
                 size.scale = tm_scale_continuous(values.scale = 5),
                 size.legend = tm_legend("Total Reported User Days",
                                         fill_alpha = 1,
                                         bg.alpha = 0.9,
                                         col = 'white',
                                         lwd = 0.5
                 ),
                 fill = "white",
                 fill_alpha = 0.5,
                 col_alpha = 0.9,
                 lwd = 0.25,
                 col = "white",
                 id = "Name") +
      tm_basemap(basemaps$park_tiles_slate)
  })
  # ----------------------------------------------------
  # Animations page
  # ----------------------------------------------------
  # Monthly Visitor Use Animation: data and animation
  # Runs when filters modified
  output$animation_by_month <- renderImage({
    # Include progress bar
    withProgress(message = "Generating animation...", {
      # Switch to plot mode for the creation of the animation
      tmap_mode("plot")

      # Set the file path for the animation
      out <- tempfile(fileext = ".gif")

      # Create animation using function in functions.R
      animation <- create_monthly_choropleth_animation(data$vua_geom |>
                                                         left_join(data$daily_visitor_use |>
                                                                     filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
                                                                            UnitCode == input$park,
                                                                            UserDays != 0) |>
                                                                     group_by(UnitCode, Location, MonthNum) |>
                                                                     summarise(UserDays = sum(UserDays), .groups = "drop") |> # sum userdays for each year in each location
                                                                     pivot_wider(names_from = MonthNum, # pivot wider to create a column of userday totals for each year
                                                                                 values_from = UserDays),
                                                                   by = join_by(UNITCODE == UnitCode, # redundant but safe
                                                                                Name == Location)),
                                                       input$park)

      # Write animation to file
      tmap_animation(animation,
                     filename = out)

      # Switch back to view mode
      tmap_mode("view")
    })
    expr = list(src = out)}, # file path to animation
    deleteFile = TRUE 
  )
  
  # Annual Visitor Use Animation & Burned Areas: data and animation
  # Runs when filters modified
  output$animation_fire <- renderImage({
    # Include progress bar
    withProgress(message = "Generating animation...", {
      # Switch to plot mode for the creation of the animation
      tmap_mode("plot")
      
      # Set the file path for the animation
      out <- tempfile(fileext = ".gif")
      
      # Create animation using function in functions.R
      animation <- create_animated_fire_bubble(data$visitor_use_export |>
                                                 filter(AccountYear <= input$year[2] & AccountYear >= input$year[1],
                                                        UnitCode == input$park), 
                                               data$vua_geom, 
                                               data$fire_polygons, 
                                               data$ocean_polygon, 
                                               data$vua_animation_data_bubble |>
                                                 filter(UNITCODE == input$park), # selects just min_year:max_year based on visitor_use_export within function
                                               input$park)
      
      # Write animation to file
      tmap_animation(animation,
                     filename = out)
      
      # Switch back to view mode
      tmap_mode("view")
    })
    expr = list(src = out)}, # file path to animation
    deleteFile = TRUE 
  )
}

# Run the app
shinyApp(ui, server)