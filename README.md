# Seattle Building Energy Analysis Project

## Team Members
- Sarju Patel
- Xing Yu Ren
- Carter Lembo

## Project Overview

This project analyzes Seattle's Building Energy Benchmarking data to investigate energy consumption patterns, greenhouse gas emissions, and the effectiveness of Seattle's mandatory energy disclosure policy. We examine data from 2015 to 2024, encompassing 34,381 observations across non-residential and multifamily buildings in Seattle.

## Research Questions

### 1. Policy Effectiveness Analysis
**Has Seattle's energy benchmarking policy successfully reduced building emissions over time since 2015?**

This investigation examines whether mandatory energy disclosure policies drive behavioral changes in building management. We focus on two key aspects:

- **Emissions Trends**: Aggregate average emissions intensity across all buildings for each year to assess the overall trend in Seattle's building sector from 2015 to 2024
- **Compliance Patterns**: Track compliance rates over time to determine whether building owners are increasingly meeting reporting requirements

**Primary outcome variables:**
- `GHGEmissionsIntensity` (emissions per square foot)
- `ComplianceStatus` (analyzed by DataYear)

### 2. Property Type Efficiency Analysis
**Which property types are the least efficient in terms of electricity usage and greenhouse gas emissions in 2024?**

This analysis investigates which types of properties (hotels, offices, retail, etc.) use the most electricity and produce the most greenhouse gas emissions per square foot, providing standardized comparisons across different building sizes.

**Key metrics:**
- Electricity usage per square foot: `Electricity(kWh)` / `PropertyGFATotal`
- GHG emissions per square foot: `TotalGHGEmissions` / `PropertyGFATotal`
- Property classification: `EPAPropertyType`

### 3. Historical Energy Consumption Patterns
**How does the year a building was constructed relate to its pattern of energy consumption, and how do these relationships differ across building types?**

This investigation explores how building age correlates with energy consumption patterns, examining the proportions of different energy sources (natural gas, steam, and electricity) used annually.

## Data Source

**Dataset:** Building Energy Benchmarking Data, 2015-Present

**Provider:** City of Seattle, Office of Sustainability and Environment

**Data Links:**
- Primary source: [Seattle Open Data Portal](https://cos-data.seattle.gov/Built-Environment/Building-Energy-Benchmarking-Data-2015-Present/teqw-tu6e/about_data)
- Alternative source: [Data.gov](https://catalog.data.gov/dataset/building-energy-benchmarking-data-2015-present)

**Dataset Description:**
This comprehensive dataset contains energy performance metrics for buildings required to comply with Seattle's benchmarking ordinance. It includes information on energy consumption, greenhouse gas emissions, building characteristics, and compliance status for non-residential and multifamily buildings in Seattle.


## Research Methodology

### Emissions Trend Analysis
1. Calculate annual average emissions intensity across all buildings
2. Analyze year-over-year changes from 2015 to 2024

### Compliance Analysis
1. Calculate annual compliance rates
2. Identify patterns in compliance behavior over time
3. Assess whether compliance improves as policy matures

### Property Type Efficiency Analysis
1. Filter data for 2024 observations
2. Calculate electricity usage per square foot by property type
3. Calculate GHG emissions per square foot by property type
4. Rank property types by efficiency metrics
5. Visualize results to identify least efficient property types

### Historical Energy Pattern Analysis
1. Compute energy consumption mix for each building
2. Analyze relationship between `YearBuilt` and energy source proportions
3. Facet analysis by building type to control for use-case differences
4. Identify trends in energy infrastructure adoption over time

### Getting Started
Follow these simple steps to use the Building Energy Benchmarking Dashboard:
Step 1: Download the Data File

- Navigate to the repository on GitHub
- Locate the CSV file: Building_Energy_Benchmarking_Data__2016.csv
- Click on the file to view it
- Click the "Download" button (or "Raw" button, then right-click and "Save As...")
- Save the CSV file to your local computer

Step 2: Run the Dashboard

- Download the Dashboard.R file from the repository
- Open Dashboard.R in RStudio (or your preferred R environment)
- Make sure you have the required R packages installed (see Prerequisites below)
- Update the file path in the code to point to where you saved the CSV file
- Run the application by clicking "Run App" in RStudio or executing the script

