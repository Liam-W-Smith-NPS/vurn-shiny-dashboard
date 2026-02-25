# Function to load data from geodatabases, VURn database, etc and store it in a list
load_data <- function() {
  # Create list to return
  data <- list()
  
  # Load geometry data
  data$vua_geom <- st_read(dsn = here("data/AKR_VUA_Data_2025_name_final8.gdb"), # load data
                           layer = "ALL_VUA_2026_PY_20260112",
                           quiet = TRUE) |>
    st_cast("MULTIPOLYGON") |> # convert curved geometries to multipolygon to prevent issues with sf and tmap
    st_transform(6393) |> # convert to Alaska specific CRS
    select(UNITCODE, UNITNAME, Name, MAPLABEL, GROUPCODE, GROUPNAME) # select just the columns we need
  
  # Read in tabular data
  data$daily_visitor_use <- read_csv(here("data/daily_visitor_use.csv"))
  
  # Wrangle tabular data to match export from IRMA website 
  # (adjusted so trips splitting multiple months fall into each month proportinoally)
  data$visitor_use_export <- data$daily_visitor_use |>
    filter(AccountYear < 2025) |>
    group_by(UnitCode, AccountYear, Location, Park, Preserve, BC, FC, Wilderness, 
             Coast, CommercialActivity, MonthName, MonthNum) |>
    summarise(UserDays = sum(UserDays),
              .groups = "drop") |>
    filter(UserDays != 0) # NOTE: if we ever make visualizations about NumberTrips, make a copy of this without removing rows were UserDays is 0
  
  # Read fire data
  data$fire_polygons <- st_read(dsn = here("data/AlaskaFireHistory_Polygons.gdb"),
                                layer = "AK_fire_location_polygons_AKAlbersNAD83",
                                quiet = TRUE) |>
    filter(FIREYEAR %in% 1994:2024) |> # filter for polygons from last 30 years
    st_cast("MULTIPOLYGON") |> # convert curved geometries to multipolygon to prevent issues with sf and tmap
    st_transform(6393) # convert to match CRS for VUA
  
  # Create vector of park names
  data$park_codes <- data$visitor_use_export |> 
    distinct(UnitCode) |>
    arrange(UnitCode) |>
    pull(UnitCode)
  
  # Join visitation data (summarized across all years and activities) to vua geometries
  # We'll use this in our total visitation maps, among other things
  data$vua_map_data <- data$vua_geom |>
    left_join(data$visitor_use_export |>
                group_by(UnitCode, Location) |>
                summarise(UserDays = sum(UserDays), .groups = "drop"), # sum UserDays together for each location
              by = join_by(UNITCODE == UnitCode, # redundant but safe 
                           Name == Location))
  
  # Find the most popular activity in each VUA (all parks) and join this to our map data
  # We'll use this in our Static Choropleth and Bubble maps
  data$park_vua_activity_data <- data$vua_geom |>
    left_join(data$visitor_use_export |>
                group_by(UnitCode, Location, CommercialActivity) |>
                summarise(UserDays = sum(UserDays), .groups = "drop_last") |> # sum UserDays together for each combo of location and activity
                slice_max(UserDays, with_ties = FALSE) |> # within each group (park & location), slice out the row with the most UserDays
                ungroup(),
              by = join_by(UNITCODE == UnitCode, # redundant but safe 
                           Name == Location))
  
  # Find out how frequently each activity occurs in each VUA (all parks) and join to map data
  # We'll use this in our donut maps
  data$vua_all_activities <- data$vua_geom |>
    left_join(data$visitor_use_export |>
                group_by(UnitCode, Location, CommercialActivity) |>
                summarise(UserDays = sum(UserDays), .groups = "drop") |> # sum UserDays for each activity in each location
                pivot_wider(names_from = CommercialActivity, # pivot to create column of userday count for each activity
                            values_from = UserDays,
                            values_fill = 0) |> # impute 0 for non-reported activities (in locations where other activities were reported)
                mutate(`Total Reported User Days` = rowSums(across(`Air Taxi`: `Visitor Experience (Guided)`), na.rm = TRUE)), # calculate total user days
              by = join_by(UNITCODE == UnitCode, 
                           Name == Location))
  
  # Generate yearly summaries of both the total `UserDays` and the most popular commercial activity in each visitation area 
  # Join that information to our spatial data
  # We'll use this in our choropleth animation
  data$vua_animation_data <- data$vua_geom |>
    left_join(data$visitor_use_export |>
                group_by(UnitCode, Location, AccountYear) |>
                summarise(UserDays = sum(UserDays), .groups = "drop") |> # sum userdays for each year in each location
                pivot_wider(names_from = AccountYear, # pivot wider to create a column of userday totals for each year
                            values_from = UserDays),
              by = join_by(UNITCODE == UnitCode, # redundant but safe
                           Name == Location)) 
  
  # Impute 0 for NA for the previous dataset
  # We'll use this in our bubble map animation
  # (If there is a year with all NAs, tmap throws an error)
  # Bubbles are visualized the same for NAs and 0s -- with the absence of a bubble
  data$vua_animation_data_bubble <- data$vua_animation_data |>
    mutate(across(starts_with("20"), 
                  function(x) {
                    if_else(is.na(x), 0, x)}))
  
  # Define polygon somewhere in the ocean
  # This is kind of a hacky solution we use to make the fire polygon animations work correctly
  # (It errors out if there is not a polygon for each year, so we just make one outside the map canvas)
  data$ocean_polygon <- st_sfc(st_polygon(list(rbind(c(-10, -2),  
                                                c(-10,  2),
                                                c(-6,   2),
                                                c(-6,  -2),
                                                c(-10, -2)))),
                          crs = 6393)
  
  # Create data to use for monthly animations
  # To the geometries, joins visitor use export summarized for each month of each year
  data$vua_animation_monthly_data <- data$vua_geom |>
    left_join(data$daily_visitor_use |>
                filter(AccountYear < 2025,
                       UserDays != 0) |>
                group_by(UnitCode, Location, MonthName) |>
                summarise(UserDays = sum(UserDays), .groups = "drop") |> # sum userdays for each year in each location
                pivot_wider(names_from = MonthName, # pivot wider to create a column of userday totals for each year
                            values_from = UserDays),
              by = join_by(UNITCODE == UnitCode, # redundant but safe
                           Name == Location)) 
  return(data)
}

# Set urls for basemaps
basemaps <- list()
basemaps$park_tiles_standard <- "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck58pyquo009v01p99xebegr9/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"
basemaps$park_tiles_slate <- "https://atlas-stg.geoplatform.gov/styles/v1/atlas-user/ck5cpvc2e0avf01p9zaw4co8o/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1IjoiYXRsYXMtdXNlciIsImEiOiJjazFmdGx2bjQwMDAwMG5wZmYwbmJwbmE2In0.lWXK2UexpXuyVitesLdwUg"

# Function to generate interactive choropleth
create_interactive_choropleth <- function(vua_map_data, basemap, park) {
  # Pull the variable of interest in order to create class breaks
  variable <- vua_map_data |> filter(UNITCODE == park) |> pull(UserDays)
  
  # Remove NAs from the vector
  var_no_na <- variable[!is.na(variable)]
  
  # If there are at least 5 non-NA entries, classify with fisher-jenks
  # Otherwise, set ticks to distinct values
  if (length(var_no_na) <= 5) {
    ticks <- sort(var_no_na)
  } else {
    raw_breaks <- classIntervals(var = var_no_na,
                                 n = 5,
                                 style = "fisher")
    
    # Round the fisher-jenks breaks to the nearest 10 or 100
    # Depending on the magnitude of the UserDays in a given park
    round_to <- if(max(raw_breaks$brks) > 2000) -2 else -1 
    first <- floor(raw_breaks$brks[1]/10^-round_to)*10^-round_to
      
    middle <- raw_breaks$brks[2:5] |>
      round(round_to)
    
    last <- ceiling(raw_breaks$brks[6]/10^-round_to)*10^-round_to
    
    combined <- c(first, middle, last)
    
    # If there are redundant breaks just use the non-rounded values
    breaks <- if(length(unique(combined)) != 6) raw_breaks$brks else combined
  }
  
  # Create map
  tm_shape(vua_map_data |>
             filter(UNITCODE == park),
           name = "Visitor Use Areas") +
    tm_polygons(fill = "UserDays",
                fill_alpha = 0.7,
                col_alpha = 0.5,
                fill.legend = tm_legend("Total Reported User Days",
                                        bg.color = 'gray96'),
                id = "Name",
                fill.scale = { if (length(var_no_na) <= 5) {
                  tm_scale_discrete(values = "brewer.reds",
                                    label.na = "No Commercial Use",
                                    ticks = ticks)
                } else {
                  tm_scale_intervals(values = "brewer.reds",
                                     label.na = "No Commercial Use",
                                     breaks = breaks)
                }},
                lwd = .25) +
    tm_basemap(basemap)
}

# Function to generate static choropleth and bubble map
create_static_choropleth_and_bubble <- function(park_vua_activity_data, vua_map_data, park) {
  # Map of commercial activities in Alaska
  tm_shape(park_vua_activity_data |>
             filter(UNITCODE == park),
           name = "Top Commercial Activities") +
    tm_polygons(fill = "CommercialActivity",
                fill.scale = tm_scale_categorical(values = "brewer.set3",
                                                  label.na = "No Commercial Use"),
                fill.legend = tm_legend("Top Commercial Activity",
                                        bg.color = "black",
                                        text.color = "gray91",
                                        title.color = "gray91",
                                        position = tm_pos_out("right", "center")),
                fill_alpha = 0.7,
                id = "Name",
                col = "white",
                col_alpha = 0.3,
                lwd = 1) +
    tm_shape(vua_map_data |>
               filter(UNITCODE == park) |>
               st_point_on_surface(),
             name = "Total Reported User Days") +
    tm_bubbles(size = "UserDays",
               size.scale = tm_scale_continuous(values.scale = 3),
               size.legend = tm_legend("Total Reported User Days",
                                       text.color = "gray91",
                                       title.color = "gray91",
                                       fill_alpha = 0.8,
                                       position = tm_pos_out("right", "center")),
               fill = "white",
               fill_alpha = 0.5,
               col_alpha = 0.9,
               lwd = 1,
               col = "white",
               id = "Name") +
    tm_layout(bg.color = "black")+
    tm_basemap("CartoDB.DarkMatter")
}

# Function to generate static choropleth and bubble map
create_donut_map <- function(visitor_use_export, vua_all_activities, basemap, park) {
  # Park activities
  park_activities <- visitor_use_export |> 
    filter(UnitCode == park) |>
    group_by(CommercialActivity) |>
    summarise(total = sum(UserDays)) |>
    filter(total > 0) |>
    pull(CommercialActivity)
  
  # Select up to 9 activities in given park with most total user days
  top9_activities <- visitor_use_export |> 
    filter(UnitCode == park) |>
    group_by(CommercialActivity) |>
    summarise(total = sum(UserDays)) |>
    arrange(desc(total)) |>
    filter(row_number() < 10) |>
    pull(CommercialActivity)
  
  # Create donut map
  tm_shape(vua_all_activities |> # create column to show if commercial activity reported in a given polygon
             filter(UNITCODE == park) |>
             mutate(`Commercial Activity` = if_else(`Total Reported User Days` > 0, 
                                                    "Occurred", 
                                                    "None Reported", 
                                                    missing = "None Reported")) |>
             select(Name, all_of(park_activities), `Total Reported User Days`, `Commercial Activity`),
           name = "Commercial Use Areas") +
    tm_polygons(fill = "Commercial Activity",
                fill.scale = tm_scale_categorical(values = c("Occurred" = "gray91", 
                                                             "None Reported" = "gray")),
                fill.legend = tm_legend("Commercial Activity"),
                fill_alpha = 0.5,
                id = "Name",
                col = "gray25",
                col_alpha = 0.5,
                lwd = 0.5) +
    tm_shape(vua_all_activities |> # wrangle data for donut charts
               filter(UNITCODE == park) |>
               mutate(Other = `Total Reported User Days` - rowSums(across(all_of(top9_activities)))) |>
               (\(df){
                 if (all(df$Other == 0, na.rm = TRUE)) {
                   other <<- FALSE # create var in global environment to help with selection in tmap's tm_vars() function
                   select(df, -Other)
                 } else{
                   other <<- TRUE # create var in global environment to help with selection in tmap's tm_vars() function
                   df}
               })() |>
               select(Name, any_of(c(top9_activities, "Other")), `Total Reported User Days`) |>
               mutate(across(any_of(c(top9_activities, "Other")),
                             function(x) { # by trial and error I found the donut charts work if normalized to all sum to 1... maybe summing to 100 or something else would work too
                               if_else(!is.na(x), x/`Total Reported User Days`, x)})),
             name = "Donut Charts"
    ) +
    tm_donuts(parts = tm_vars(c(top9_activities, if(other) "Other"), multivariate = TRUE),
              fill.scale = tm_scale_categorical(values = "brewer.set3"),
              size = "Total Reported User Days",
              size.scale = tm_scale_continuous_sqrt(values.scale = 2,
                                                    n = 4),
              size.legend = tm_legend_hide(),
              id = "Name",
              col = "gray25",
              lwd = 0.5,
              options = opt_tm_donuts(fill_hole = FALSE,
                                      on_surface = TRUE)) +
    tm_basemap(basemap)
}

create_animated_choropleth <- function(visitor_use_export, vua_animation_data, park) {
  # Find earliest year of data
  min_year <- visitor_use_export |>
    filter(UnitCode == park) |>
    pull(AccountYear) |>
    min()
  
  # Find latest year of data
  max_year <- visitor_use_export |>
    filter(UnitCode == park) |>
    pull(AccountYear) |>
    max()
  
  # Create animation
  { if (park == "KLGO") { # kind of ridiculous workaround to correct fig size of KLGO
    tm_shape(vua_animation_data |> # create layer without KLGO minimum bouding circle
               filter(UNITCODE == park) |>
               st_minimum_bounding_circle()) } } +
    tm_shape(vua_animation_data |>
               filter(UNITCODE == park)) +
    tm_polygons(fill = paste0(min_year:max_year), # this is where we set the min and max year to display for each park
                fill.legend = tm_legend("Total Reported User Days",
                                        position = tm_pos_out("right", "center")),
                fill.scale = tm_scale_continuous(values = "brewer.reds",
                                                 label.na = "No Commercial Use"),
                fill.free = FALSE,
                fill_alpha = .7,
                col_alpha = .5,
                lwd = 1,
                col = "#CDC9C9") +
    tm_animate(fps = 1) +
    tm_basemap("Esri.WorldTopoMap")
}

create_animated_fire_bubble <- function(visitor_use_export, vua_geom, fire_polygons, 
                                        ocean_polygon, vua_animation_data_bubble, park) {
  # Find earliest year of data
  min_year <- visitor_use_export |>
    filter(UnitCode == park) |>
    pull(AccountYear) |>
    min()
  
  # Find latest year of data
  max_year <- visitor_use_export |>
    filter(UnitCode == park) |>
    pull(AccountYear) |>
    max()
  
  # Create a larger geometry to help us capture fires in the vicinity of park 
  buffered_park <- vua_geom |>
    filter(UNITCODE == park) |>
    st_union() |> # union the geometries together
    st_buffer(50000) # this is in meters since CRS is EPSG:6393
  
  # Count the number of fires that occurred in/near the given park
  number_fires <- fire_polygons |>
    st_intersects(buffered_park, sparse = FALSE) |>
    sum()
  
  # The following code only needs to be executed for parks that have had fires
  # and might fail to run for parks that have not had fires
  if (number_fires > 0) {
    # Filter for polygons in & around just one park
    fire_polygons_park <- fire_polygons |>
      filter(st_intersects(Shape, buffered_park, sparse = FALSE) |>
               as.vector())
    
    # Find years with no fires in the vicinity of the park
    missing_years <- setdiff(min_year:max_year,
                             fire_polygons_park |>
                               st_drop_geometry() |>
                               pull(FIREYEAR))
    
    # Slightly different processing depending on if all years had fire
    if (length(missing_years) > 0) {
      # Create multipolygon for each missing year
      multipoly_sf <- st_sf(FIREYEAR = missing_years, Shape = ocean_polygon)
      
      # Bind fake fire polygons to actual (fake will be off map canvas but let tmap function)
      extra_fire_polygons_park <- fire_polygons_park |>
        filter(FIREYEAR %in% min_year:max_year) |>
        bind_rows(multipoly_sf |>
                    mutate(FIREYEAR = as.factor(FIREYEAR)))
    } else {
      extra_fire_polygons_park <- fire_polygons_park |>
        filter(FIREYEAR %in% min_year:max_year)
    }
    # Explicitly set FIREYEAR to factor
    extra_fire_polygons_park <- extra_fire_polygons_park |>
      mutate(FIREYEAR = as.factor(FIREYEAR))
  }
  
  # Create animation
  { if (park == "KLGO") { # kind of ridiculous workaround to correct fig size of KLGO
    tm_shape(vua_animation_data_bubble |>
               filter(UNITCODE == park) |>
               st_minimum_bounding_circle()) } } +
    tm_shape(vua_animation_data_bubble |> # vua polygons
               filter(UNITCODE == park)) + 
    tm_polygons(col_alpha = .3,
                lwd = .5,
                col = "gray25",
                fill = "gray",
                fill_alpha = 0.4) +
    {if (number_fires > 0) { # fire polygons
      tm_shape(fire_polygons_park) +
        tm_polygons(fill = 'orange',
                    fill_alpha = 0.5,
                    col = 'orange',
                    col_alpha = 0) + 
      tm_shape(extra_fire_polygons_park) +
        tm_polygons(fill = 'red',
                    fill_alpha = 0.5,
                    col = 'red',
                    col_alpha = 0) +
        tm_animate(frames = "FIREYEAR",
                   fps = 1)
    }} +
    tm_shape(vua_animation_data_bubble |> # visitor use count bubbles
               filter(UNITCODE == park) |>
               st_point_on_surface()) + 
    tm_bubbles(size = paste0(min_year:max_year),
               size.scale = tm_scale_continuous(values.scale = 3),
               size.legend = tm_legend("Total Reported User Days",
                                       position = tm_pos_out("right", "center")),
               fill = "gray91",
               size.free = FALSE,
               fill_alpha = 0.7,
               col_alpha = 1,
               lwd = 1,
               col = "white") +
    tm_add_legend(title = "Burned Areas",
                  type = "polygons",
                  labels = c("Fire Perimeters 1994-2024", "Fire in Displayed Year"),
                  fill = c('orange', 'red'),
                  fill_alpha = 0.5,
                  col_alpha = 0,
                  position = tm_pos_out("right", "center"))+
    tm_animate(fps = 1) +
    tm_basemap("Esri.WorldTopoMap")
}

create_commercial_popularity_map <- function(visitor_use_export, vua_geom, year, basemap) {
  # Select the top 7 visitor use activities 
  most_popular_activities <- visitor_use_export |>
    filter(AccountYear == year) |>
    group_by(CommercialActivity) |>
    summarize(tot_user_days = sum(UserDays)) |>
    arrange(desc(tot_user_days)) |>
    filter(row_number() < 8) |>
    pull(CommercialActivity)
  
  # Find the most popular activity in each VUA
  # If the activity is not among the most common, set to Other
  vua_popular_activities_year <- visitor_use_export |>
    filter(AccountYear == year) |>
    group_by(UnitCode, Location, CommercialActivity) |>
    summarise(UserDays = sum(UserDays), .groups = "drop_last") |>
    slice_max(UserDays, with_ties = FALSE) |>
    ungroup() |>
    mutate(CommercialActivity = if_else(CommercialActivity %in% most_popular_activities,
                                        CommercialActivity,
                                        "Other"))
  
  # Join visitation data (top CommercialActivity) to vua geometries
  vua_activity_data <- vua_geom |>
    left_join(vua_popular_activities_year,
              by = join_by(UNITCODE == UnitCode, # redundant but safe 
                           Name == Location))
  
  # Join visitation data (total UserDays) to vua geometries
  vua_map_data_year <- vua_geom |>
    left_join(visitor_use_export |>
                filter(AccountYear == year) |>
                group_by(UnitCode, Location) |>
                summarise(UserDays = sum(UserDays), .groups = "drop"),
              by = join_by(UNITCODE == UnitCode, # redundant but safe 
                           Name == Location))
  
  tm_shape(vua_activity_data |>
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
    tm_shape(vua_map_data_year |>
               st_point_on_surface(),
             name = "Total Reported User Days") +
    tm_bubbles(size = "UserDays",
               size.scale = tm_scale_continuous(values.scale = 3),
               size.legend = tm_legend("Total Reported User Days",
                                       # bg.color = 'gray80',
                                       fill_alpha = 1,
                                       bg.alpha = 0.9,
                                       col = 'black',
                                       lwd = 0.5
               ),
               fill = "white",
               fill_alpha = 0.5,
               col_alpha = 0.9,
               lwd = 0.25,
               col = "white",
               id = "Name") +
    tm_basemap(basemap) +
    tm_title(paste0("Commercial Activity and Visitor Use in ", year))
}

# Handy function that we will call in the following function
not_all_na <- function(x) any(!is.na(x))

create_monthly_choropleth_animation <- function(vua_animation_monthly_data, park) {
  # Filter for data within park of interest
  vua_animation_monthly_data_park <- vua_animation_monthly_data |>
    filter(UNITCODE == park) |>
    select(where(not_all_na))
  
  # Select only the columns that correspond to year-month combinations and sort them
  cols <- vua_animation_monthly_data_park |>
    st_drop_geometry() |>
    select(matches(month.name)) |>
    colnames()
  
  # Create animation
  { if (park == "KLGO") { # kind of ridiculous workaround to correct fig size of KLGO
    tm_shape(vua_animation_monthly_data_park |> 
               filter(UNITCODE == park) |>
               st_minimum_bounding_circle()) } } +
    tm_shape(vua_animation_monthly_data_park) +
    tm_polygons(fill = cols,
                fill.legend = tm_legend("Total Reported User Days",
                                        position = tm_pos_out("right", "center")),
                fill.scale = tm_scale_continuous(values = "brewer.reds",
                                                 label.na = "No Commercial Use"),
                fill.free = FALSE,
                fill_alpha = 0.7,
                col_alpha = .5,
                lwd = 1,
                lwd.legend = tm_legend_hide(),
                col = "#CDC9C9") +
    tm_animate(fps = 1) +
    tm_basemap("Esri.WorldTopoMap")
}
