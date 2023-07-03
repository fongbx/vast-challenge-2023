# LOAD LIBRARIES -----------------------------------------------
pacman::p_load(shiny, shinyWidgets, shinythemes, shinydashboard,
               bslib, tidyverse, lubridate, tidygraph, ggraph,
               visNetwork, igraph, plotly, RColorBrewer,
               DT, patchwork, zoo, bsicons, wesanderson)

# LOAD DATA ---------------------------------------------------

## all nodes and edges
all_nodes <- read_csv("appdata/all_nodes.csv")
all_edges_year <- read_csv("appdata/all_edges_year.csv")
all_edges_year_month <- read_csv("appdata/all_edges_year_month.csv")

## exporter bundle
exporter_nodes <- read_csv("appdata/exporter_nodes_and_partners.csv")
exporter_edges <- read_csv("appdata/exporter_edges_year.csv")

## importer bundle
importer_nodes <- read_csv("appdata/importer_nodes_and_partners.csv")
importer_edges <- read_csv("appdata/importer_edges_year.csv")

## intermediary bundle
intermediary_nodes <-
  read_csv("appdata/intermediary_nodes_and_partners.csv")
intermediary_edges <- read_csv("appdata/intermediary_edges_year.csv")

## redflag bundle
redflag_nodes <- read_csv("appdata/redflag_nodes_and_partners.csv")
redflag_edges <- read_csv("appdata/redflag_edges_year.csv")

# DATA PREPARATION FOR SHIPPING ROUTE PANEL  --------------------------
mapped_data <- all_edges_year %>%
  left_join(all_nodes %>% select(id, shpcountry), by = c("source" = "id")) %>%
  rename(source_country = shpcountry) %>%
  left_join(all_nodes %>% select(id, rcvcountry), by = c("target" = "id")) %>%
  rename(target_country = rcvcountry)

mapped_data_year_month <- all_edges_year_month %>%
  left_join(all_nodes %>% select(id, shpcountry), by = c("source" = "id")) %>%
  rename(source_country = shpcountry) %>%
  left_join(all_nodes %>% select(id, rcvcountry), by = c("target" = "id")) %>%
  rename(target_country = rcvcountry)

aggregated_data <- mapped_data %>%
  group_by(Year, source_country, target_country) %>%
  summarise(shipping_frequency = sum(weights))

aggregated_data_year_month <- mapped_data_year_month %>%
  group_by(Year, Month, source_country, target_country) %>%
  summarise(shipping_frequency = sum(weights))

arranged_data <- aggregated_data %>%
  group_by(Year) %>%
  slice_max(shipping_frequency, n = 50)

arranged_data_top_5 <- arranged_data %>%
  arrange(Year, desc(shipping_frequency)) %>%
  slice_max(order_by = shipping_frequency, n = 5)

data <- aggregated_data_year_month %>%
  inner_join(
    arranged_data_top_5,
    by = c("source_country" = "source_country", "target_country" = "target_country")
  ) %>%
  select(Year.x,
         Month,
         source_country,
         target_country,
         shipping_frequency.x) %>%
  mutate(YearMonth = paste(Year.x, sprintf("%02d", Month), sep = "-"))

# DEFINE THEME ---------------------------------------------------
mytheme <- bs_theme(
  bg = "#FFF",
  fg = "#1e3c60",
  primary = "#9ACEEB",
  secondary = "#0072B2",
  success = "#009E73",
  base_font = font_google("Inter"),
  code_font = font_google("JetBrains Mono"),
  font_scale = 0.9
)

# DEFINE VALUE BOXES FOR ENTITY INVESTIGATION PANEL  --------------
vbs <- list(
  value_box(
    fill = TRUE,
    title = "Total Trade Partners",
    value = span(textOutput("total_trade_partners"),
                 style = "font-size: 150%;"),
    showcase = icon("handshake-simple", class = "fa-3x"),
    theme_color = "secondary",
    width = 1/4,
  ),
  value_box(
    fill = TRUE,
    title = "Total Trade Frequency",
    value = span(textOutput("total_trade_freq"),
                 style = "font-size: 150%;"),
    showcase = icon("ship", class = "fa-3x"),
    theme_color = "secondary",
    width = 1 / 4
  ),
  value_box(
    fill = TRUE,
    title = "Total Traded Weight (kg)",
    value = span(textOutput("total_trade_wt"),
                 style = "font-size: 150%;"),
    showcase = icon("scale-balanced", class = "fa-3x"),
    theme_color = "secondary",
    width = 1 / 4
  ),
  value_box(
    fill = TRUE,
    title = "Total Traded Value (USD)",
    value = span(textOutput("total_trade_value"),
                 style = "font-size: 150%;"),
    showcase = icon("sack-dollar", class = "fa-3x"),
    theme_color = "secondary",
    width = 1 / 4
  )
)

#####################################################
#                        UI                         #
#####################################################

ui <- page_navbar(
  theme = mytheme,
  title = "Oceanus Watch",
  
  # USER GUIDE PANEL  -------------------------------------------------------
  
  nav_panel("User Guide", icon = icon("circle-info")),
  
  # ENTITY HEATMAP PANEL  ---------------------------------------------------
  
  nav_panel(
    "Entity Overview",
    icon = icon("chart-simple"),
    layout_sidebar(
      border = FALSE,
      fillable = TRUE,
      sidebar = sidebar(
        title = "Input Controls",
        width = 340,
        
        # Sidebar Filter 1
        awesomeRadio(
          inputId = "bundleoverview",
          label = "Select a Bundle:",
          choices = c("Exporters",
                      "Importers",
                      "Intermediaries"),
          selected = "Exporters"
        ),
        
        # Sidebar Filter 2
        sliderInput(
          "node_range",
          "Select the range of entities to view:",
          min = 1,
          max = 100,
          value = c(1, 100),
          step = 5
        )
      ),
      
      # Mainpanel Heatmap Plot
      card(
        full_screen = TRUE,
        card_body(
          
          # Display header
          h4(strong(textOutput("entityoverviewheader"))),
          p("Uncover entities with unexpected trade pattern variations over time using the heatmaps below. 
          Filter by", tags$i(strong("Bundle")), "and selected", tags$i(strong("Entity Range")), "for specific insights."),
          
          # Display plot
          plotlyOutput("entityheatmap")
        )
      )
    )
  ),
  
  # ENTITY INVESTIGATION PANEL  --------------------------------------------
  
  nav_panel(
    "Entity Investigation",
    icon = icon("eye"),
    layout_sidebar(
      border = FALSE,
      fillable = TRUE,
      sidebar = sidebar(
        title = "Input Controls",
        width = 340,
        
        accordion(
          accordion_panel(
            "All Tabs",
            # Sidebar Filter 1
            awesomeRadio(
              inputId = "bundleinvestigation",
              label = "Select a Bundle:",
              choices = c(
                "Exporters",
                "Importers",
                "Intermediaries",
                "Red Flagged Entities"
              ),
              selected = "Exporters"
            ),
            
            # Sidebar Filter 2
            pickerInput(
              inputId = "idselection",
              label = "Select an Entity:",
              choices = NULL,
              options = list(`live-search` = TRUE),
            )
          ),
          accordion_panel(
            "Entity Partners Tab",
            # add picker input for partners
            pickerInput(
              inputId = "partnerselection",
              label = "Select Partners (multiple):",
              choices = NULL,
              multiple = TRUE,
              options = list(`actions-box` = TRUE,
                             `live-search` = TRUE))
          )
        )
      ),
      navset_card_tab(
        full_screen = TRUE,
        
        # Tab 1 - Entity Details
        nav_panel(
          title = "Entity Details",
          
          # Display header
          h4(strong(textOutput("entitydetailsheader"))),
          p("Analyse key statistics and associated partners of a specific entity over the years
          using the network graphs and bar charts below. Filter by", tags$i(strong("Bundle")), "and", tags$i(strong("Entity.")),
            "for targeted insights."),
          
          # Display value boxes
          layout_columns(fill = FALSE, !!!vbs),
          
          # Display network graphs
          layout_columns(
            width = 1 / 2,
            navset_card_tab(
              title = "Network",
              nav_panel("Overall", visNetworkOutput("networkoverall")),
              nav_panel("2028", visNetworkOutput("network2028")),
              nav_panel("2029", visNetworkOutput("network2029")),
              nav_panel("2030", visNetworkOutput("network2030")),
              nav_panel("2031", visNetworkOutput("network2031")),
              nav_panel("2032", visNetworkOutput("network2032")),
              nav_panel("2033", visNetworkOutput("network2033")),
              nav_panel("2034", visNetworkOutput("network2034"))
            ),
            
            # Display barplot
            card(
              card_header("Statistics over the years"),
              card_body(plotlyOutput("barplot"))
            )
          )
        ),
        
        # Tab 2 - Entity Partners
        nav_panel(
          title = "Entity Partners",
          min_height = 450,
          
          # Display header
          h4(strong(textOutput("entitypartnersheader"))),
          p("Observe the trade frequency between the selected", tags$i(strong("Entity")),
            "and its partners in the line chart below to uncover their
            business relationship patterns. Filter on specific", tags$i(strong("Partners")), "for targeted insights."),
          
          # Display plot
          plotlyOutput("lineplot")
        )
      )
    )
  ),
  
  # COUNTRY ROUTE PANEL  ---------------------------------------------------
  
  nav_panel(
    "Country Routes",
    icon = icon("flag"),
    
    layout_sidebar(
      border = FALSE,
      fillable = TRUE,
      sidebar = sidebar(
        title = "Input Controls",
        width = 340,
        
        accordion(
          
          accordion_panel(
            "All Tabs",
            # Sidebar Filter 1
            selectInput(
              inputId = "Year",
              label = "Select Year:",
              choices = unique(arranged_data$Year),
              selected = unique(arranged_data$Year)[1]
            )
          ),
          
          accordion_panel(
            "Selected Country Route Tab",
            
            # Sidebar Filter 2 and 3
            uiOutput("source_country_dropdown"),
            uiOutput("target_country_dropdown")
          )
        )
      ),
      
      # Mainpanel Plots
      navset_card_tab(
        full_screen = TRUE,
        nav_panel(
          "Trade Network Overview",
          
          # Display header
          h4(strong(textOutput("countryrouteheader"))),
          p("Explore the trade frequency of country routes by filtering on",
            tags$i(strong("Year.")), "Countries with bigger nodes represent more export routes,
            and thicker line widths indicate higher shipment frequency."),
          
          # Display plot
          plotOutput("country_network")
        ),
        nav_panel(
          "Selected Country Route",
          
          
          # Display header and plot
          h4(strong(textOutput("selcountryrouteheader"))),
          p("Investigate specific country routes by filtering on", tags$i(strong("Source Country")),
            "and", tags$i(strong("Target Country")), "based on the top 5 shipping route frequencies.
                The trade frequency of the chosen shipping route by selected", tags$i(strong("Year")), "is shown."),
          
          plotOutput("plots"),
          # Display header and table
          h4(strong(textOutput("selcountryentheader"))),
          p("The following entities, ranked in descending order of total trade frequency,
                frequently ship on the chosen shipping route."),
          DTOutput("mytable")
        )
      )
    )
  )
)

#####################################################
#                     SERVER                        #
#####################################################
server <- function(input, output, session) {
  # ENTITY HEATMAP PANEL  ---------------------------------------------------
  
  # output entity overview header based on selection
  output$entityoverviewheader <- renderText({
    paste("Trade Frequency of Top", input$node_range[1],
          "to", input$node_range[2], input$bundleoverview, "over time")
  })
  
  # update node_range in slider
  observeEvent(input$bundleoverview, {
    if (input$bundleoverview == "Exporters") {
      max_num <- 100
    }
    else if (input$bundleoverview == "Importers") {
      max_num <- 100
    }
    else if (input$bundleoverview == "Intermediaries") {
      max_num <- 66
    }
    
    updateSliderInput(
      session,
      inputId = "node_range",
      max = max_num,
      value = c(1, max_num)
    )
  })
  
  # plot entity heatmap
  output$entityheatmap <- renderPlotly({
    # filter out top exporter nodes or importer nodes
    if (input$bundleoverview == "Exporters") {
      nodes_check <- exporter_nodes %>%
        filter(top_exporters == "Yes") %>%
        select(-in_deg_centrality, -betweenness_centrality) %>%
        arrange(desc(out_deg_centrality))
    } else if (input$bundleoverview == "Intermediaries") {
      nodes_check <- intermediary_nodes %>%
        filter(top_intermediaries == "Yes") %>%
        select(-in_deg_centrality, -out_deg_centrality) %>%
        arrange(desc(betweenness_centrality))
    } else if (input$bundleoverview == "Importers") {
      nodes_check <- importer_nodes %>%
        filter(top_importers == "Yes") %>%
        select(-out_deg_centrality, -betweenness_centrality) %>%
        arrange(desc(in_deg_centrality))
    }
    
    nodes_check <- nodes_check %>%
      slice(input$node_range[1]:input$node_range[2])
    
    # extract edges with top nodes as source or target
    if (input$bundleoverview == "Exporters") {
      nodes_heatmap <- all_edges_year_month %>%
        filter(source %in% nodes_check[['id']]) %>%
        rename(id = source) %>%
        group_by(id, Year, Month) %>%
        summarise(
          totalfrequency = sum(weights),
          totalweightkg = sum(totalweightkg),
          totalvalueofgoodsusd = sum(totalvalueofgoodsusd)
        ) %>%
        ungroup()
    } else if (input$bundleoverview == "Importers") {
      nodes_heatmap <- all_edges_year_month %>%
        filter(target %in% nodes_check[['id']]) %>%
        rename(id = target) %>%
        group_by(id, Year, Month) %>%
        summarise(
          totalfrequency = sum(weights),
          totalweightkg = sum(totalweightkg),
          totalvalueofgoodsusd = sum(totalvalueofgoodsusd)
        ) %>%
        ungroup()
    } else if (input$bundleoverview == "Intermediaries") {
      intermediary_as_source <- all_edges_year_month %>%
        filter(source %in% nodes_check[['id']]) %>%
        rename(id = source) %>%
        group_by(id, Year, Month) %>%
        summarise(
          frequency = sum(weights),
          totalweightkg = sum(totalweightkg),
          totalvalueofgoodsusd = sum(totalvalueofgoodsusd)
        ) %>%
        ungroup()
      intermediary_as_target <- all_edges_year_month %>%
        filter(target %in% nodes_check[['id']]) %>%
        rename(id = target) %>%
        group_by(id, Year, Month) %>%
        summarise(
          frequency = sum(weights),
          totalweightkg = sum(totalweightkg),
          totalvalueofgoodsusd = sum(totalvalueofgoodsusd)
        ) %>%
        ungroup()
      
      nodes_heatmap <-
        rbind(intermediary_as_source, intermediary_as_target) %>%
        filter(id %in% nodes_check[['id']]) %>%
        group_by(id, Year, Month) %>%
        summarise(
          totalfrequency = sum(frequency),
          totalweightkg = sum(totalweightkg),
          totalvalueofgoodsusd = sum(totalvalueofgoodsusd)
        ) %>%
        ungroup()
    }
    
    # Create date column
    nodes_heatmap$Date <-
      as.yearmon(paste(nodes_heatmap$Year, nodes_heatmap$Month), "%Y %m")
    
    # Create heatmap
    heatmap_plot <-
      ggplot(nodes_heatmap, aes(x = Date, y = id, fill = totalfrequency)) +
      geom_tile(colour = "White") +
      xlab(label = "Month") +
      labs(fill = "Frequency") +
      scale_fill_distiller(palette = "Spectral") +
      theme_classic() +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(angle = 0),
        axis.text.y = element_text(size = 7),
        plot.margin = margin(20, 20, 20, 120),
        legend.position = "bottom",
        legend.key.height = unit(0.3, "cm"),
        legend.key.width = unit(1.5, "cm")
      )
  })
  
  # ENTITY INVESTIGATION PANEL  ---------------------------------------------
  
  # output entity details header based on selection
  output$entitydetailsheader <- renderText({
    paste("Analysis of", input$idselection)
  })
  
  # output entity partners header based on selection
  output$entitypartnersheader <- renderText({
    paste("Trade Frequency between", input$idselection, "and Partners over the years")
  })
  
  # nodes to use based on bundle selection
  bundle_nodes <- reactive({
    req(input$bundleinvestigation)
    
    df <- if (input$bundleinvestigation == "Exporters") {
      exporter_nodes %>%
        rename(nodes_of_interest = top_exporters)
    } else if (input$bundleinvestigation == "Importers") {
      importer_nodes %>%
        rename(nodes_of_interest = top_importers)
    } else if (input$bundleinvestigation == "Intermediaries") {
      intermediary_nodes %>%
        rename(nodes_of_interest = top_intermediaries)
    } else if (input$bundleinvestigation == "Red Flagged Entities") {
      redflag_nodes %>%
        rename(nodes_of_interest = redflag_nodes) %>%
        arrange(id)
    }
    df
  })
  
  # edges to use based on bundle selection
  bundle_edges <- reactive({
    req(input$bundleinvestigation)
    df <- if (input$bundleinvestigation == "Exporters") {
      exporter_edges
    } else if (input$bundleinvestigation == "Importers") {
      importer_edges
    } else if (input$bundleinvestigation == "Intermediaries") {
      intermediary_edges
    } else if (input$bundleinvestigation == "Red Flagged Entities") {
      redflag_edges
    }
    df
  })
  
  # update ids in dropdown selection
  observeEvent(bundle_nodes(), {
    select_nodes <- bundle_nodes() %>%
      filter(nodes_of_interest == "Yes")
    choices <- unique(select_nodes$id)
    updatePickerInput(
      session,
      inputId = "idselection",
      choices = stringr::str_trunc(choices, width = 35),
      selected = NULL
    )
  })
  
  # extract edges with selected entity on ends
  selected_edges <- reactive({
    req(input$idselection)
    
    df <- bundle_edges() %>%
      filter(source == input$idselection |
               target == input$idselection) %>%
      mutate(
        year = as.integer(Year),
        export_import = case_when(source == input$idselection ~ "Exporting",
                                  TRUE ~ "Importing"),
        partner = case_when(export_import == "Exporting" ~ target,
                            TRUE ~ source)
      )
    
    df
  })
  
  # extract nodes of trades
  selected_co_and_partners <- reactive({
    req(input$idselection)
    
    source_partners_id <- selected_edges() %>%
      select(source) %>%
      rename(id = source)
    
    target_partners_id <- selected_edges() %>%
      select(target) %>%
      rename(id = target)
    
    df <- rbind(source_partners_id, target_partners_id) %>%
      distinct() %>%
      mutate(selected_company = case_when(id == input$idselection ~ "Yes",
                                          TRUE ~ "No")) %>%
      left_join(bundle_nodes(), by = "id")
    
    df
  })
  
  # update partner ids in dropdown selection
  observeEvent(selected_edges(), {
    select_nodes_partners <- selected_edges() %>%
      arrange(partner)
    choices <- unique(select_nodes_partners$partner)
    updatePickerInput(
      session,
      inputId = "partnerselection",
      choices = stringr::str_trunc(choices, width = 35),
      selected = NULL
    )
  })
  
  # extract edges with selected entity and partners
  selected_partner_edges <- reactive({
    req(input$partnerselection)
    
    original_data <- selected_edges() %>%
      filter(partner %in% input$partnerselection) %>%
      select(partner,
             year,
             weights,
             totalweightkg,
             totalvalueofgoodsusd)
    
    start_year <- 2028
    end_year <- 2034
    
    all_combinations <-
      expand.grid(partner = unique(input$partnerselection),
                  year = start_year:end_year)
    
    df <- left_join(all_combinations, original_data,
                    by = c("partner", "year")) %>%
      mutate(
        weights = ifelse(is.na(weights), 0, weights),
        totalweightkg = ifelse(is.na(totalweightkg), 0, totalweightkg),
        totalvalueofgoodsusd = ifelse(is.na(totalvalueofgoodsusd), 0, totalvalueofgoodsusd)
      )
    
    df
  })
  
  # function to plot network plot
  plot_network <- function(nodes, edges) {
    # colour palette
    sw_colors <- colorRampPalette(brewer.pal(3, "RdBu"))(3)
    
    # customise edges for plotting
    edges_updated <- edges %>%
      rename(from = source,
             to = target) %>%
      mutate(
        width = weights / max(weights) * 10,
        # width of edge
        title = paste0(
          "Frequency: ",
          format(weights, big.mark = ",", scientific = FALSE),
          "<br>Weight: ",
          format(totalweightkg, big.mark = ",", scientific = FALSE),
          "<br>Value: ",
          format(totalvalueofgoodsusd, big.mark = ",", scientific = FALSE)
        ),
        # tooltip
        arrows = "to",
        # set arrow for each edge
        color = "#0085AF" # color of edge
      )
    
    # customise nodes for plotting
    nodes_updated <- nodes %>%
      mutate(selected_company.type = ifelse(selected_company == "Yes", sw_colors[1], sw_colors[2])) %>%
      mutate(
        shape = "dot",
        # customise shape of nodes
        title = id,
        # tooltip when hover over
        size = 40,
        # set size of nodes
        color.border = "#013848",
        # border colour of nodes
        color.background = selected_company.type,
        # background colour of nodes
        color.highlight.background = "#FF8000" # background colour of nodes when highlighted
      )
    
    # function for visNetwork
    visNetwork(nodes_updated,
               edges_updated,
               height = "100%",
               width = "100%") %>%
      visIgraphLayout() %>%
      visOptions(highlightNearest = list(enabled = T, degree = 1)) %>%
      visLayout(randomSeed = 123)
  }
  
  # plot network graphs
  output$networkoverall <- renderVisNetwork({
    plot_network(selected_co_and_partners(), selected_edges()) %>%
      return
  })
  
  output$network2028 <- renderVisNetwork({
    edges <- selected_edges() %>%
      filter(year == 2028)
    nodes <- selected_co_and_partners() %>%
      filter(id %in% edges$source | id %in% edges$target)
    plot_network(nodes, edges) %>%
      return
  })
  
  output$network2029 <- renderVisNetwork({
    edges <- selected_edges() %>%
      filter(year == 2029)
    nodes <- selected_co_and_partners() %>%
      filter(id %in% edges$source | id %in% edges$target)
    plot_network(nodes, edges) %>%
      return
  })
  
  output$network2030 <- renderVisNetwork({
    edges <- selected_edges() %>%
      filter(year == 2030)
    nodes <- selected_co_and_partners() %>%
      filter(id %in% edges$source | id %in% edges$target)
    plot_network(nodes, edges) %>%
      return
  })
  
  output$network2031 <- renderVisNetwork({
    edges <- selected_edges() %>%
      filter(year == 2031)
    nodes <- selected_co_and_partners() %>%
      filter(id %in% edges$source | id %in% edges$target)
    plot_network(nodes, edges) %>%
      return
  })
  
  output$network2032 <- renderVisNetwork({
    edges <- selected_edges() %>%
      filter(year == 2032)
    nodes <- selected_co_and_partners() %>%
      filter(id %in% edges$source | id %in% edges$target)
    plot_network(nodes, edges) %>%
      return
  })
  
  output$network2033 <- renderVisNetwork({
    edges <- selected_edges() %>%
      filter(year == 2033)
    nodes <- selected_co_and_partners() %>%
      filter(id %in% edges$source | id %in% edges$target)
    plot_network(nodes, edges) %>%
      return
  })
  
  output$network2034 <- renderVisNetwork({
    edges <- selected_edges() %>%
      filter(year == 2034)
    nodes <- selected_co_and_partners() %>%
      filter(id %in% edges$source | id %in% edges$target)
    plot_network(nodes, edges) %>%
      return
  })
  
  # generate output for value boxes
  output$total_trade_partners <-
    renderText(length(unique(selected_edges()$partner)))
  
  output$total_trade_freq <- renderText(format(
    sum(selected_edges()$weights, na.rm = TRUE),
    big.mark = ",",
    scientific = FALSE
  ))
  
  output$total_trade_wt <- renderText(format(
    sum(selected_edges()$totalweightkg, na.rm = TRUE),
    big.mark = ",",
    scientific = FALSE
  ))
  
  output$total_trade_value <- renderText(format(
    sum(selected_edges()$totalvalueofgoodsusd, na.rm = TRUE),
    big.mark = ",",
    scientific = FALSE
  ))
  
  # plot line plot with partners
  output$barplot <- renderPlotly({
    selected_edges_grouped <- selected_edges() %>%
      group_by(year, export_import) %>%
      summarise(
        tradefreq = sum(weights, na.rm = TRUE),
        tradewt = sum(totalweightkg, na.rm = TRUE),
        tradevalue = sum(totalvalueofgoodsusd, na.rm = TRUE)
      ) %>%
      ungroup()
    
    plot_ly(
      data = selected_edges_grouped,
      x = ~ year,
      y = ~ tradefreq,
      color = ~ export_import,
      type = 'bar',
      hoverinfo = "y"
    ) |>
      
      # Generate plot, x-axis, and y-axis title
      layout(
        xaxis = list(title = "Year"),
        yaxis = list(title = "Trade Frequency"),
        
        theme(plot.title = element_text(hjust = 0.5, size = 18)),
        
        # Create dropdown menus to allow selection of parameters on y-axis
        updatemenus = list(
          list(
            type = "dropdown",
            xanchor = "left",
            yanchor = "top",
            x = 0.04,
            y = 0.95,
            buttons = list(
              list(
                method = "update",
                args = list(list(
                  y = list(selected_edges_grouped$tradefreq)
                ),
                list(yaxis = list(title = "Trade Frequency"))),
                label = "Trade Frequency"
              ),
              list(
                method = "update",
                args = list(list(y = list(
                  selected_edges_grouped$tradewt
                )),
                list(yaxis = list(title = "Traded Weight"))),
                label = "Traded Weight"
              ),
              list(
                method = "update",
                args = list(list(
                  y = list(selected_edges_grouped$tradevalue)
                ),
                list(yaxis = list(title = "Traded Value"))),
                label = "Traded Value"
              )
            )
          )
        )
      )
  })
  
  # plot lineplot with partners
  output$lineplot <- renderPlotly({
    p <- ggplot(
      data = selected_partner_edges(),
      aes(
        x = year,
        y = weights,
        color = partner,
        label1 = totalweightkg,
        label2 = totalvalueofgoodsusd,
      )
    ) +
      geom_point(
        aes(
          text = paste0(
            "Frequency: ",
            format(weights, big.mark = ",", scientific = FALSE),
            "<br>Weight: ",
            format(totalweightkg, big.mark = ",", scientific = FALSE),
            "<br>Value: ",
            format(totalvalueofgoodsusd, big.mark = ",", scientific = FALSE)
          )
        )
      ) +
      geom_line() +
      labs(y = "Trade Frequency") +
      theme(axis.title.x = element_blank())
    
    # wrap plot in ggplotly for interactivity
    ggplotly(p,
             tooltip = "text") %>% 
      layout(legend = list(
        orientation = "h",
        x = 0,
        y = -0.2
      ))
  })
  
  # COUNTRY ROUTE PANEL  ----------------------------------------------------
  
  # output country route header based on selection
  output$countryrouteheader <- renderText({
    paste("Country-Level Trade Network in", input$Year)
  })
  
  # output selected country route header based on selection
  output$selcountryrouteheader <- renderText({
    paste("Trade Frequency from", input$source_country,
          "to", input$target_country, "in", input$Year)
  })
  
  # output selected country route entity header based on selection
  output$selcountryentheader <- renderText({
    paste("Entities frequently shipping from", input$source_country,
          "to", input$target_country)
  })
  
  output$source_country_dropdown <- renderUI({
    req(input$Year)
    
    top_pairs <- arranged_data_top_5 %>%
      filter(Year == input$Year) %>%
      select(source_country, target_country)
    
    selectInput("source_country",
                "Select Source Country:",
                choices = unique(top_pairs$source_country))
  })
  
  output$target_country_dropdown <- renderUI({
    req(input$Year, input$source_country)
    
    top_pairs <- arranged_data_top_5 %>%
      filter(Year == input$Year, source_country == input$source_country) %>%
      select(target_country)
    
    selectInput("target_country",
                "Select Target Country:",
                choices = unique(top_pairs$target_country))
  })
  
  output$country_network <- renderPlot({
    # filter data for selected year
    country_links_selected_year <- mapped_data %>%
      filter(Year == input$Year) %>%
      group_by(source_country, target_country) %>%
      summarise(weight = sum(weights),
                weightkg = sum(totalweightkg)) %>%
      ungroup()
    
    # create nodes for selected year
    distinct_shpcountry <- country_links_selected_year %>%
      distinct(source_country) %>%
      mutate(country = source_country) %>%
      select(country)
    
    distinct_rcvcountry <- country_links_selected_year %>%
      distinct(target_country) %>%
      mutate(country = target_country) %>%
      select(country)
    
    country_nodes_selected_year <-
      unique(rbind(distinct_shpcountry, distinct_rcvcountry))
    
    # create graph for selected year
    country_graph_selected_year <-
      tbl_graph(nodes = country_nodes_selected_year,
                edges = country_links_selected_year,
                directed = TRUE)
    
    # measure directed out-degree centrality and save as a column
    V(country_graph_selected_year)$out_degree <-
      degree(country_graph_selected_year, mode = "out")
    
    set.seed(1234)
    g <- country_graph_selected_year %>%
      activate(edges) %>%
      ggraph(layout = "linear",
             circular = TRUE) +
      geom_edge_fan(
        aes(width = weight,
            color = input$Year),
        alpha = .6,
        arrow = arrow(length = unit(2, 'mm')),
        show.legend = FALSE
      ) +
      scale_edge_width(range = c(0.1, 4)) +
      geom_node_point(
        aes(size = out_degree),
        color = "grey20",
        alpha = .7,
        show.legend = FALSE
      ) +
      geom_node_text(aes(label = ifelse(
        out_degree > quantile(out_degree, .75), country, ""
      )),
      size = 5,
      repel = TRUE) +
      theme(
        plot.title = element_text(size = 25),
        plot.subtitle = element_text(size = 15),
        plot.margin = margin(r = 10, l = 10)
      )
    g
  })
  
  output$plots <- renderPlot({
    req(input$Year, input$source_country, input$target_country)
    
    pair_data <- aggregated_data_year_month %>%
      filter(
        Year == input$Year,
        source_country == input$source_country,
        target_country == input$target_country
      ) %>%
      mutate(Date = as.Date(paste(
        Year, sprintf("%02d", Month), "01", sep = "-"
      )))
    
    ggplot(pair_data, aes(x = Date, y = shipping_frequency)) +
      geom_line(color = "steelblue", size = 0.8) +
      theme_classic() +
      labs(y = "Trade frequency") +
      scale_x_date(date_breaks = "1 month", date_labels = "%b-%Y")
  })
  
  output$mytable <- renderDT({
    req(input$Year, input$source_country, input$target_country)
    
    filtered_data <- mapped_data_year_month %>%
      filter(
        Year == input$Year,
        source_country == input$source_country,
        target_country == input$target_country
      )
    
    aggregated_data <- filtered_data %>%
      group_by(source, target) %>%
      summarise(total_weight = sum(weights)) %>%
      arrange(desc(total_weight))
    
    datatable(aggregated_data,
              options = list(
                scrollY = "200px"),
              colnames = c("Source Entity", "Target Entity",
                           "Total Trade Frequency")
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
