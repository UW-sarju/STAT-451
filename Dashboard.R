library(shiny)
library(tidyverse)
library(treemapify)

ui <- navbarPage(
  title = "Stat 451 Project Part 3",
  
  tabPanel("Property Type Efficiency",
           fluidPage(
             wellPanel(fluidRow(
               column(width = 3,
                      checkboxInput("top_elec", label = "Show Top Electricity Usage", value = TRUE),
                      "This plot shows the average electricity usage for property types
              in Seattle from 2024. You can choose to toggle between the top 10
              highest and top ten lowest electricity using property types.
              Values are normalized by building square footage. This gives
              insight into which properties it might be best to focus on for
              more effiiently utalizing electricity."
               ),
               column(width = 9, plotOutput(outputId = "elec_plot1"))
             )),
             wellPanel(fluidRow(
               column(width = 3,
                      checkboxInput("top_ghg", label = "Show Top Greenhouse Gas Usage", value = TRUE),
                      "This plot shows average greenhouse gas emitions per building
              area for property types in Seattle from 2024. You can choose to
              toggle between the top 10 and lowest 10 property types. This gives
              insight into which properties it might be best to focus on for
              reducing greenhouse gas emissions."
               ),
               column(width = 9, plotOutput(outputId = "ghg_plot1"))
             )),
             wellPanel(fluidRow(
               column(width = 3,
                      checkboxInput("top_ghg_elec", label = "Show Top Greenhouse Gas per Electricity Usage", value = TRUE),
                      "This plot shows the average greenhouse gas emissions per unit
              of electricity used by property type in Seattle from 2024.
              You can toggle between the top 10 and bottom 10 property types.
              This gives insight into which properties use electricity most
              efficiently in terms of greenhouse gas emissions of electricity."
               ),
               column(width = 9, plotOutput(outputId = "ghg_elec_plot1"))
             ))
           )
  ),
  
  # New tab for Emissions Trends  
  tabPanel("Emissions & Compliance Trends",
           fluidPage(
             wellPanel(
               fluidRow(
                 column(width = 3,
                        h4("GHG Emissions Intensity Over Time"),
                        sliderInput("year_range_emissions",
                                    "Select Year Range:",
                                    min = 2015,
                                    max = 2024,
                                    value = c(2015, 2024),
                                    step = 1,
                                    sep = ""),
                        checkboxInput("show_outliers",
                                      "Include outliers (beyond 99th percentile)",
                                      value = FALSE),
                        p("This visualization shows the trend in greenhouse gas emissions intensity
              for buildings in Seattle. Both mean and median values
              are displayed to show central tendencies while accounting for outliers.")
                 ),
                 column(width = 9,
                        plotOutput(outputId = "emissions_trend_plot", height = "500px")
                 )
               )
             ),
             wellPanel(
               fluidRow(
                 column(width = 3,
                        h4("Building Compliance Rate"),
                        sliderInput("year_range_compliance",
                                    "Select Year Range:",
                                    min = 2015,
                                    max = 2024,
                                    value = c(2015, 2024),
                                    step = 1,
                                    sep = ""),
                        numericInput("compliance_threshold",
                                     "Compliance Threshold (%):",
                                     value = 90,
                                     min = 0,
                                     max = 100,
                                     step = 5),
                        p("This chart displays the percentage of buildings meeting compliance
              standards each year. The dashed line represents the compliance threshold.")
                 ),
                 column(width = 9,
                        plotOutput(outputId = "compliance_plot", height = "500px")
                 )
               )
             )
           )
  ),
  
  # Third tab - Energy vs Age + Treemap  
  tabPanel("Energy Usage by Building Age and Type",
           fluidPage(
             # First row: Energy vs Building Age
             fluidRow(
               column(width = 3,
                      selectInput("building_type",
                                  "Choose EPA Property Type:",
                                  choices = NULL),
                      selectInput("plot_type",
                                  "Plot Type:",
                                  choices = c("Stacked Bar by Decade", "Smoothed Line by Year"))
               ),
               column(width = 9,
                      wellPanel(
                        h4("Annual Energy Consumption (kBtu) Proportions vs Age of Building"),
                        plotOutput("energy_age_plot", height = "400px")
                      )
               )
             ),
             
             # Second row: Treemap
             fluidRow(
               column(width = 3,
                      selectInput("decade",
                                  "Decade:",
                                  choices = NULL)
               ),
               column(width = 9,
                      wellPanel(
                        h4("Annual Energy Consumption (kBtu) of Top 10 EPA Property Types"),
                        plotOutput("treemap_plot", height = "500px")
                      )
               )
             )
           )
  )
)

server <- function(input, output, session) {
  df <- read.csv("Building_Energy_Benchmarking_Data__2015-Present.csv")
  
  df_2024 <- df %>% 
    filter(DataYear == 2024)
  
  agg <- df_2024 %>% 
    group_by(EPAPropertyType) %>%
    summarise(
      avg_ghg = mean(TotalGHGEmissions, na.rm = TRUE),
      avg_elec = mean(Electricity.kWh., na.rm = TRUE),
      avg_ghg_per_elec = avg_ghg / avg_elec,
      avg_ghg_per_area = mean(GHGEmissionsIntensity, na.rm = TRUE),
      avg_elec_per_area = avg_elec / mean(PropertyGFABuildings),
      total_ghg = sum(TotalGHGEmissions, na.rm = TRUE),
      n = n()
    )
  
  # Prepare data for emissions visualizations
  df_emissions_full <- df %>%
    select(DataYear, GHGEmissionsIntensity, EPAPropertyType) %>%
    filter(!is.na(GHGEmissionsIntensity),
           GHGEmissionsIntensity > 0,
           !is.na(EPAPropertyType),
           EPAPropertyType != "")
  
  # Original plots
  output$elec_plot1 <- renderPlot({
    if (input$top_elec) {
      temp <- agg %>% 
        arrange(desc(avg_elec_per_area)) %>% 
        head(10)
    } else {
      temp <- agg %>% 
        arrange(avg_elec_per_area) %>% 
        head(10)
    }
    
    plot_elec <- temp %>% 
      ggplot(aes(y = reorder(EPAPropertyType, avg_elec_per_area), x = avg_elec_per_area)) +
      geom_col(fill = "darkblue", color = "black") + 
      xlim(0, 175) +
      theme_bw() +
      theme(
        axis.text.y = element_text(color = "black", size = 11)
      )
    
    if (input$top_elec) {
      plot_elec <- plot_elec + labs(
        title = "Property Types with the 10 Highest Average Electricity Usage per Building Area",
        subtitle = "Data for Seattle, WA in 2024",
        x = "Average Electeicity usageper Area (kWh per sq ft)",
        y = "Property Type",
        caption = "Source: Seattle Office of Sustainability and Environment\n(https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)"
      )
    } else {
      plot_elec <- plot_elec + labs(
        title = "Property Types with the 10 Lowest Average Electricity Usage per Building Area",
        subtitle = "Data for Seattle, WA in 2024",
        x = "Average Electeicity usageper Area (kWh per sq ft)",
        y = "Property Type",
        caption = "Source: Seattle Office of Sustainability and Environment\n(https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)"
      )
    }
    
    print(plot_elec)
  })
  
  output$ghg_plot1 <- renderPlot({
    if (input$top_ghg) {
      temp <- agg %>% 
        arrange(desc(avg_ghg_per_area)) %>% 
        head(10)
    } else {
      temp <- agg %>% 
        arrange(avg_ghg_per_area) %>% 
        head(10)
    }
    
    plot_ghg <- temp %>% 
      ggplot(aes(y = reorder(EPAPropertyType, avg_ghg_per_area), x = avg_ghg_per_area)) +
      geom_col(fill = "darkgreen", color = "black") + 
      xlim(0, 14) +
      theme_bw() +
      theme(
        axis.text.y = element_text(color = "black", size = 11)
      )
    
    if (input$top_ghg) {
      plot_ghg <- plot_ghg + labs(
        title = "Property Types with the 10 Highest Average Greenhouse Gas \nEmissions per Building Area",
        subtitle = "Data for Seattle, WA in 2024",
        x = "Average GHG per Area (kg co2 equivalent per sq ft)",
        y = "Property Type",
        caption = "Source: Seattle Office of Sustainability and Environment\n(https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)"
      )
    } else {
      plot_ghg <- plot_ghg + labs(
        title = "Property Types with the 10 Lowest Average Greenhouse Gas \nEmissions per Building Area",
        subtitle = "Data for Seattle, WA in 2024",
        x = "Average GHG per Area (kg co2 equivalent per sq ft)",
        y = "Property Type",
        caption = "Source: Seattle Office of Sustainability and Environment\n(https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)"
      )
    }
    
    print(plot_ghg)
  })
  
  output$ghg_elec_plot1 <- renderPlot({
    if (input$top_ghg_elec) {
      temp <- agg %>% 
        arrange(desc(avg_ghg_per_elec)) %>% 
        head(10)
    } else {
      temp <- agg %>% 
        arrange(avg_ghg_per_elec) %>% 
        head(10)
    }
    
    plot_ghg_elec <- temp %>% 
      ggplot(aes(y = reorder(EPAPropertyType, avg_ghg_per_elec), x = avg_ghg_per_elec)) +
      geom_col(fill = "darkgreen", color = "black") + 
      xlim(0, 0.0012) +
      theme_bw() +
      theme(
        axis.text.y = element_text(color = "black", size = 11)
      )
    
    if (input$top_ghg_elec) {
      plot_ghg_elec <- plot_ghg_elec + labs(
        title = "Property Types with the 10 Highest Average Greenhouse Gas \nEmissions per Electricity Usage",
        subtitle = "Data for Seattle, WA in 2024",
        x = "Average Greenhouse Gas Emissions per Electricity Usage (kg co2 equivalent per kWh)",
        y = "Property Type",
        caption = "Source: Seattle Office of Sustainability and Environment\n(https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)"
      )
    } else {
      plot_ghg_elec <- plot_ghg_elec + labs(
        title = "Property Types with the 10 Lowest Average Greenhouse Gas \nEmissions per Electricity Usage",
        subtitle = "Data for Seattle, WA in 2024",
        x = "Average Greenhouse Gas Emissions per Electricity Usage (kg co2 equivalent per kWh)",
        y = "Property Type",
        caption = "Source: Seattle Office of Sustainability and Environment\n(https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)"
      )
    }
    
    print(plot_ghg_elec)
  })
  
  # Interactive emissions trend plot
  output$emissions_trend_plot <- renderPlot({
    # Filter by year range
    df_filtered <- df_emissions_full %>%
      filter(DataYear >= input$year_range_emissions[1],
             DataYear <= input$year_range_emissions[2])
    
    # Handle outliers
    if (!input$show_outliers) {
      threshold <- quantile(df_filtered$GHGEmissionsIntensity, 0.99, na.rm = TRUE)
      df_filtered <- df_filtered %>%
        filter(GHGEmissionsIntensity <= threshold)
    }
    
    # Calculate summary statistics
    emissions_by_year <- df_filtered %>%
      group_by(DataYear) %>%
      summarise(
        mean_emissions = mean(GHGEmissionsIntensity, na.rm = TRUE),
        median_emissions = median(GHGEmissionsIntensity, na.rm = TRUE),
        count = n(),
        sd_emissions = sd(GHGEmissionsIntensity, na.rm = TRUE)
      )
    
    # Create plot
    ggplot(emissions_by_year, aes(x = DataYear)) +
      geom_line(aes(y = mean_emissions, color = "Mean"),
                linewidth = 1.2) +
      geom_point(aes(y = mean_emissions, color = "Mean"),
                 size = 4) +
      geom_line(aes(y = median_emissions, color = "Median"),
                linewidth = 1, linetype = "dashed") +
      geom_point(aes(y = median_emissions, color = "Median"),
                 size = 3, shape = 15) +
      geom_text(aes(y = mean_emissions,
                    label = sprintf("%.2f", mean_emissions)),
                vjust = -1.5, size = 3.5, fontface = "bold") +
      scale_color_manual(values = c("Mean" = "#e74c3c", "Median" = "#3498db")) +
      scale_x_continuous(breaks = emissions_by_year$DataYear) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.1))) +
      labs(
        title = "GHG Emissions Intensity Trend",
        x = "Year",
        y = "GHG Emissions Intensity",
        color = "Metric",
        caption = "Source: Seattle Office of Sustainability and Environment\n(https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color = "gray30"),
        axis.title = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 10),
        legend.position = "top",
        legend.title = element_text(face = "bold")
      )
  })
  
  # Interactive compliance plot
  output$compliance_plot <- renderPlot({
    # Start with full dataset
    df_compliance <- df %>%
      filter(!is.na(ComplianceStatus), ComplianceStatus != "")
    
    # Filter by year range
    df_compliance <- df_compliance %>%
      filter(DataYear >= input$year_range_compliance[1],
             DataYear <= input$year_range_compliance[2])
    
    # Calculate compliance rate
    compliance_rate <- df_compliance %>%
      group_by(DataYear) %>%
      summarise(
        total = n(),
        compliant = sum(ComplianceStatus == "Compliant"),
        compliance_rate = (compliant / total) * 100
      )
    
    # Create plot
    ggplot(compliance_rate, aes(x = factor(DataYear), y = compliance_rate)) +
      geom_col(fill = "#27ae60", width = 0.7, alpha = 0.9) +
      geom_hline(yintercept = input$compliance_threshold, 
                 linetype = "dashed", color = "gray40", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.1f%%", compliance_rate)),
                vjust = -0.5, size = 3.5, fontface = "bold", color = "gray20") +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10),
                         expand = expansion(mult = c(0, 0.02))) +
      labs(
        title = "Building Compliance Rate Over Time",
        subtitle = paste("Dashed line indicates", input$compliance_threshold, "% threshold"),
        x = "Year",
        y = "Compliance Rate (%)",
        caption = "Source: Seattle Office of Sustainability and Environment\n(https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5, margin = margin(b = 5)),
        plot.subtitle = element_text(size = 10, hjust = 0.5, color = "gray40", margin = margin(b = 10)),
        axis.title = element_text(face = "bold"),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10)),
        panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 0.5)
      )
  })
  
  #Third tab - stuff & more stuff
  filtered <- df %>% 
    select(BuildingName, BuildingType, TaxParcelIdentificationNumber, Address,
           ZipCode, Latitude, Longitude, Neighborhood, CouncilDistrictCode,
           YearBuilt, NumberofFloors, NumberofBuildings, PropertyGFABuildings,
           EPAPropertyType, LargestPropertyUseType, LargestPropertyUseTypeGFA,
           SteamUse.kBtu., Electricity.kBtu., NaturalGas.kBtu.) %>%
    mutate(across(everything(), ~ na_if(str_trim(.), "")))
  
  numeric_cols <- c("ZipCode", "Latitude", "Longitude", "CouncilDistrictCode",
                    "YearBuilt", "NumberofFloors", "NumberofBuildings",
                    "PropertyGFABuildings", "LargestPropertyUseTypeGFA",
                    "Electricity.kBtu.", "NaturalGas.kBtu.", "SteamUse.kBtu.")
  
  filtered <- filtered %>%
    mutate(across(all_of(numeric_cols),
                  ~ as.numeric(gsub(",", "", .))))
  
  wrangled <- filtered %>%
    drop_na(YearBuilt, EPAPropertyType) %>%
    mutate(across(c("Electricity.kBtu.", "SteamUse.kBtu.", "NaturalGas.kBtu."),
                  ~ replace_na(., 0))) %>%
    mutate(TotalEnergy.kBtu = NaturalGas.kBtu. + SteamUse.kBtu. + Electricity.kBtu.,
           PropNaturalGas = NaturalGas.kBtu. / TotalEnergy.kBtu,
           PropSteamUse   = SteamUse.kBtu. / TotalEnergy.kBtu,
           PropElectricity = Electricity.kBtu. / TotalEnergy.kBtu) %>%
    filter(TotalEnergy.kBtu > 0) %>%
    mutate(Decade = floor(YearBuilt/10) * 10,
           DecadeLabel = paste0(Decade, "s"))
  
  energy_long <- wrangled %>%
    pivot_longer(
      cols = c(PropNaturalGas, PropSteamUse, PropElectricity,
               NaturalGas.kBtu., SteamUse.kBtu., Electricity.kBtu.),
      names_to = "EnergyType",
      values_to = "Value"
    )
  
  top_types <- wrangled %>%
    count(EPAPropertyType, sort = TRUE) %>%
    slice_head(n = 10) %>%
    pull(EPAPropertyType)
  
  prop_long <- energy_long %>%
    filter(EnergyType %in% c("PropNaturalGas", "PropSteamUse", "PropElectricity")) %>%
    mutate(EnergyType = recode(EnergyType,
                               PropNaturalGas = "Natural Gas",
                               PropSteamUse = "Steam",
                               PropElectricity = "Electricity")) %>%
    filter(EPAPropertyType %in% top_types)
  
  
  # UPDATE DROPDOWNS
  updateSelectInput(session, "building_type",
                    choices = sort(unique(prop_long$EPAPropertyType)),
                    selected = top_types[1])
  
  updateSelectInput(session, "decade",
                    choices = sort(unique(prop_long$Decade)),
                    selected = sort(unique(prop_long$Decade))[1])
  
  # ENERGY VS AGE PLOT (Bar + Line plots)
  
  output$energy_age_plot <- renderPlot({
    df <- prop_long %>% filter(EPAPropertyType == input$building_type)
    
    if (input$plot_type == "Stacked Bar by Decade") {
      plot_df <- df %>%
        group_by(Decade, EnergyType) %>%
        summarise(TotalEnergy = sum(Value, na.rm = TRUE), .groups = "drop") %>%
        group_by(Decade) %>%
        mutate(Prop = TotalEnergy / sum(TotalEnergy))
      
      ggplot(plot_df, aes(x = factor(Decade), y = Prop, fill = EnergyType)) +
        geom_col(position = "stack") +
        geom_text(aes(label = scales::percent(Prop, accuracy = 1)),
                  position = position_stack(vjust = 0.5), size = 5) +
        labs(x = "Building Decade",
             y = "Proportional Energy",
             fill = "Energy Type",
             title = paste("Energy Mix by Age —", input$building_type)) +
        theme_minimal(base_size = 14)
      
    } else {
      ggplot(df, aes(x = YearBuilt, y = Value, color = EnergyType)) +
        geom_smooth(method = "loess", se = FALSE, linewidth = 1.1) +
        labs(x = "Year Built",
             y = "Energy Proportion",
             color = "Energy Type",
             title = paste("Energy vs Age —", input$building_type)) +
        theme_minimal(base_size = 14)
    }
  })
  
  # TREEMAP PLOT
  
  output$treemap_plot <- renderPlot({
    df_decade <- prop_long %>% filter(Decade == input$decade)
    
    all_combo <- expand.grid(EPAPropertyType = top_types,
                             EnergyType = c("Natural Gas", "Steam", "Electricity"))
    
    prop_building <- df_decade %>%
      group_by(EPAPropertyType, EnergyType) %>%
      summarise(TotalEnergy = sum(Value, na.rm = TRUE), .groups = "drop")
    
    merged <- all_combo %>%
      left_join(prop_building, by = c("EPAPropertyType", "EnergyType")) %>%
      mutate(TotalEnergy = replace_na(TotalEnergy, 0))
    
    ggplot(merged,
           aes(area = 1,
               fill = ifelse(TotalEnergy == 0, "No data", EnergyType),
               subgroup = EPAPropertyType,
               label = ifelse(TotalEnergy == 0, "", paste0(EnergyType, "\n", scales::percent(TotalEnergy, accuracy = 1))))) +
      geom_treemap() +
      geom_treemap_subgroup_border(color = "white", size = 2) +
      geom_treemap_subgroup_text(place = "center", grow = TRUE, colour = "black",
                                 fontface = "bold", min.size = 4) +
      scale_fill_manual(values = c("Natural Gas" = "#d95f02",
                                   "Steam"       = "#7570b3",
                                   "Electricity" = "#1b9e77",
                                   "No data"     = "gray85")) +
      labs(title = paste("Energy Mix Treemap —", input$decade)) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui=ui, server=server)
