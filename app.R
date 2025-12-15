library(shiny)
library(bs4Dash)
library(bslib)
library(DT)
library(sf)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(ggspatial)
library(scatterpie)
library(plotly)
library(stringr)

## loading data
gpkg <- "kodiak_analysis_layers.gpkg"
kodiak               <- st_read(gpkg, "kodiak")
kodiak_crop          <- st_read(gpkg, "kodiak_crop")
points_spatial_kodiak<- st_read(gpkg, "awc_points_kodiak")
fish_stat            <- st_read(gpkg, "fish_stat")
lakes_touch_kodiak   <- st_read(gpkg, "lakes_touch_kodiak")
streams_touch_kodiak <- st_read(gpkg, "streams_touch_kodiak")
search_data<-fish_stat%>%
  st_drop_geometry(fish_stat)%>%
  mutate(
    NAME = str_replace_all(NAME, "[\\*\"']", "")
  )%>%
  select(SPECIES,CYCLE,NAME,TYPE,QUAD,Index_ID,AWC_CODE,X_COORD,Y_COORD)
kDK_list <- paste0("KDK-", sprintf("%02d", 1:15))
fish_list<-unique(fish_stat$SPECIES)
fish_cycle<-unique(fish_stat$CYCLE)
### plot
smaple_plot_map<-function(point_data=points_spatial_kodiak,poly_data=kodiak){
  pts_coords <- st_coordinates(point_data)
  pts_df <- data.frame(lon = pts_coords[,1],
                       lat = pts_coords[,2],
                       lbl  = paste0("Lng: ", round(pts_coords[,1],2),
                                     "\tLat: ", round(pts_coords[,2],2)))
  sample_plot<-leaflet() %>%
    addProviderTiles(providers$OpenStreetMap) %>%
    addPolygons(data = poly_data, color = "red", weight = 1, fill = FALSE) %>%
    addCircleMarkers(data = pts_df, lng = ~lon, lat = ~lat,
                     radius = 1, color = "blue",
                     label = ~lbl,)
}

plot_bar_polygons <- function(fish_sf=fish_stat, id_col = "Index_ID", group_col = "SPECIES") {
  
  stat_df <- fish_sf %>%
    st_drop_geometry() %>%
    count(.data[[id_col]], .data[[group_col]], name = "n")
  
  gg <- ggplot(stat_df, aes(x = .data[[group_col]], y = n)) +
    geom_col(aes(fill = .data[[group_col]])) +
    facet_wrap(as.formula(paste("~", id_col)), nrow = 5) +
    theme_minimal() +
    labs(x = group_col, y = "Count", title = paste("Distribution of", group_col)) +
    scale_y_log10()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  return(ggplotly(gg)%>% layout( height = 1000))
}

plot_pie_by_polygon <- function(fish_sf=fish_stat,kodiak_crop=kodiak_crop,poly_sf  = kodiak,id_col= "Index_ID" ,group_col= "SPECIES",r = 0.10) {
  stat_df <- fish_sf %>%
    st_drop_geometry() %>%
    count(.data[[id_col]], .data[[group_col]])
  
  centers <- st_point_on_surface(poly_sf)
  coords <- st_coordinates(centers)
  
  centers_df <- poly_sf %>%
    st_drop_geometry() %>%
    mutate(
      cx = coords[, 1],
      cy = coords[, 2]
    ) %>%
    select(all_of(id_col), cx, cy)
  
  pie_df <- stat_df %>%
    pivot_wider(
      names_from  = .data[[group_col]],
      values_from = n,
      values_fill = 0
    ) %>%
    left_join(centers_df, by = id_col)
  
  pie_cols <- setdiff(names(pie_df), c(id_col, "cx", "cy"))
  
  ggplot() +
    geom_sf(data = kodiak_crop, fill = "lightblue", color = "blue", size = 0.1) +
    geom_sf(data = poly_sf, fill = NA, color = "red", alpha=0.3,size = 0.1) +
    geom_scatterpie(
      data = pie_df,
      aes(x = cx, y = cy, r = r),
      cols = pie_cols,
      alpha=0.4
    ) +
    coord_sf() +
    theme_minimal()+
    theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
    labs(fill = group_col)
}
fit_travel <- function(data = fish_stat, fish = "Chum Salmon") {
  data_filtered <- data[data$SPECIES == fish, , drop = FALSE]
  p <- ggplot() +
    geom_sf(data = kodiak_crop, fill = "lightblue", color = "blue", size = 0.2) +
    geom_sf(data = kodiak, color = "red", alpha = 0.1, size = 0.5) +
    geom_sf(data = lakes_touch_kodiak, color = "blue", size = 1.5) +
    geom_sf(data = streams_touch_kodiak, color = "blue", size = 2) +
    geom_sf(data = data_filtered,aes(color = CYCLE,text = paste0("Species: ", SPECIES, "<br>Cycle: ", CYCLE)),
            alpha = 0.9, size = 0.5) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = -45, hjust = 1, vjust = 1)) +
    coord_sf()
  ggplotly(p, tooltip = "text")
}
sample_plot<-smaple_plot_map()
p_species_pie <- plot_pie_by_polygon(fish_sf=fish_stat,kodiak_crop=kodiak_crop,poly_sf  = kodiak,id_col= "Index_ID" ,group_col= "SPECIES",r = 0.10)
p_cycle_pie <- plot_pie_by_polygon(fish_sf= fish_stat ,kodiak_crop=kodiak_crop, poly_sf = kodiak,id_col   = "Index_ID",group_col = "CYCLE",r = 0.10)
p_species_bar <- plot_bar_polygons(fish_stat, group_col = "SPECIES")
p_cycle_bar   <- plot_bar_polygons(fish_stat, group_col = "CYCLE")
#fit_travel(fish = "Chum Salmon")


my_theme <- bs_theme(
  version =4,
  bootswatch = "cosmo",
  primary = "lightgreen",
  secondary = "red",
  base_font = font_google("Roboto")
)


# Define UI for application that draws a histogram
ui <- bs4DashPage(
  title="Fish Cycle in Kodiak Island",
  header = bs4DashNavbar(
    title = tagList(icon("chart-pie"), "Project Dashboard")),
  sidebar=bs4DashSidebar(
    skin="light",
    bs4SidebarMenu(
      bs4SidebarMenuItem("Introduction",tabName = "page1",icon = icon("info-circle")),
      bs4SidebarMenuItem("Field Analysis",tabName = "page2",icon = icon("map")),
      bs4SidebarMenuItem("Track Dedection",tabName = "page3",icon = icon("map")),
      bs4SidebarMenuItem("Table",tabName = "page4",icon=icon("table"))
    )
  ),
  footer = bs4DashFooter(left  = "Course Project – Fish Observation in Kodiak Island",right = "Author: Shuxin Qian"
  ),
  body=bs4DashBody(
    tags$head(bs_theme_dependencies(my_theme)),
    bs4TabItems(
      bs4TabItem(
        tabName = "page1",
        fluidRow(
          bs4Card(
            title = "Project Introduction",
            width = 12,
            HTML("
              <p>Kodiak Island, Alaska, is famous for its rich fisheries, especially Pacific Salmon. Understanding where these fish live and how they move through different life stages is key for protecting them and managing fisheries sustainably.</p>
              <p>This project uses 2,078 sampling points across lakes and streams. The data records fish species like Coho, Sockeye, Chum, Pink, and King Salmon, as well as Dolly Varden and Sheefish, along with life stages such as parr, resident, and smolt. While exact fish counts are not available, the number of sampling points helps estimate fish distribution and habitat use. The main goals are to explore fish movement across life stages and support fisheries management by mapping species distribution.</p>
              <p>All data come from the Alaska Department of Fish and Game:  https://www.adfg.alaska.gov/sf/SARR/AWC/index.cfm?ADFG=maps.dataFiles</p>
            ")
          ),
          bs4Card(
            title = "Sampling Map",
            width=12,
            leafletOutput(outputId = "sample_map",height = 800)
          )
        )
      ),
      bs4TabItem(
        tabName = "page2",
        fluidRow(
          radioButtons(
            "page2_choice",
            label = NULL,
            choices = c("By Species" = "species", "By Cycle" = "cycle"),
            selected = "species"
          )
        ),
        fluidRow(
          bs4Card(width=12,title = "Map", plotOutput("page2_map",height = 800)),
          bs4Card(width=12,title = "Stats", plotlyOutput("page2_bar",height = 1000))
        )
      ),
      bs4TabItem(
        tabName = "page3",
        fluidRow(
          bs4Card(
            width=12,
            title = "Filter",
            selectInput( 
              "page3_species", 
              label="Species", 
              choices = fish_list, 
              selected = "Chum Salmon" 
            )
          )
        ),
        fluidRow(
          bs4Card(
            width=12,
            title = "Fish Travel Diagram",
            plotlyOutput("fish_travel_plot", height = 1000) 
          )
        )
      ),
      bs4TabItem(
        tabName = "page4",
        fluidRow(
          bs4Card(
            width=12,
            title = "Fish Data in Kodiak Island",
            textInput("table_search","Search"),
            DTOutput("data_table")
          )
        )
      )
    )
  )
)



# Define server logic required to draw a histogram
server <- function(input, output,session) {
  output$sample_map <-renderLeaflet({sample_plot})
  output$page2_map <- renderPlot({
    if(input$page2_choice == "species") {
      p_species_pie
    } else {
      p_cycle_pie
    }
  })
  output$page2_bar <- renderPlotly({
    if(input$page2_choice == "species") {
      ggplotly(p_species_bar)
    } else {
      ggplotly(p_cycle_bar)
    }
  })
  output$fish_travel_plot<-renderPlotly({
    fit_travel(fish=input$page3_species)
  })
  output$data_table <- renderDT({
    filtered <- if (input$table_search == "") {
      search_data
    } else {
      search_data %>%
        filter(if_any(everything(), ~ grepl(input$table_search, as.character(.), ignore.case = TRUE)))
    }
    datatable(filtered, options = list(pageLength = 10, scrollX = TRUE))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
