library(tidyverse)

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
               h4("GHG Emissions Intensity Over Time"),
               p("This visualization shows the trend in greenhouse gas emissions intensity 
          from 2015 to 2024 for buildings in Seattle. Both mean and median values 
          are displayed to show central tendencies while accounting for outliers. 
          The data excludes extreme outliers beyond the 99th percentile for better 
          visualization."),
               plotOutput(outputId = "emissions_trend_plot", height = "500px")
             ),
             wellPanel(
               h4("Building Compliance Rate"),
               p("This chart displays the percentage of buildings meeting compliance 
          standards each year. The dashed line at 90% represents a key threshold 
          for evaluating overall compliance performance across Seattle's building stock."),
               plotOutput(outputId = "compliance_plot", height = "500px")
             )
           )
  )
)

server <- function(input, output) {
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
  
  # Prepare data for new visualizations
  df_emissions <- df %>%
    select(DataYear, GHGEmissionsIntensity) %>%
    filter(!is.na(GHGEmissionsIntensity), 
           GHGEmissionsIntensity > 0) %>%
    filter(GHGEmissionsIntensity <= quantile(GHGEmissionsIntensity, 0.99, na.rm = TRUE))
  
  emissions_by_year <- df_emissions %>%
    group_by(DataYear) %>%
    summarise(
      mean_emissions = mean(GHGEmissionsIntensity, na.rm = TRUE),
      median_emissions = median(GHGEmissionsIntensity, na.rm = TRUE),
      count = n(),
      sd_emissions = sd(GHGEmissionsIntensity, na.rm = TRUE)
    )
  
  compliance_rate <- df %>%
    filter(!is.na(ComplianceStatus), ComplianceStatus != "") %>%
    group_by(DataYear) %>%
    summarise(
      total = n(),
      compliant = sum(ComplianceStatus == "Compliant"),
      compliance_rate = (compliant / total) * 100
    )
  
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
  
  # New plots for the second tab
  output$emissions_trend_plot <- renderPlot({
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
        title = "GHG Emissions Intensity Trend (2015-2024)",
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
  
  output$compliance_plot <- renderPlot({
    ggplot(compliance_rate, aes(x = factor(DataYear), y = compliance_rate)) +
      geom_col(fill = "#27ae60", width = 0.7, alpha = 0.9) +
      geom_hline(yintercept = 90, linetype = "dashed", color = "gray40", linewidth = 0.5) +
      geom_text(aes(label = sprintf("%.1f%%", compliance_rate)),
                vjust = -0.5, size = 3.5, fontface = "bold", color = "gray20") +
      scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 10), 
                         expand = expansion(mult = c(0, 0.02))) +
      labs(
        title = "Building Compliance Rate Over Time",
        subtitle = "Dashed line indicates 90% threshold",
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
}

shinyApp(ui=ui, server=server)