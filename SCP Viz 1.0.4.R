# The list of packages we'll require. Consider it our shiny toolbox.
required_packages <- c("shiny", "shinythemes", "plotly","FactoMineR", "factoextra", "imputeTS", "readxl", "ggplot2", "ggpubr", "readr", "dplyr", "tidyr", "gridExtra", "DT", "janitor")

# If any package is missing, let's install it on-the-fly.
for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  # Load the package into our R session.
  library(pkg, character.only = TRUE)
}

# Crafting the user interface for our app.
ui = navbarPage(
  
  # Naming our app. Make sure to pick a cool name, or else it won't work.
  title = "SCP Viz 1.0.4",
  
  theme = shinytheme("flatly"),
  
  # Laying out the main structure: a sidebar for inputs and a main panel for outputs.
  tabPanel("Main",
           sidebarLayout(
             
             # The sidebar: where users tell us what they want. They are SO picky. 
             sidebarPanel(
               # A space for users to provide their own CSV.
               fileInput("data_upload", "Upload CSV file", accept = NULL),
               # An option to address those pesky missing values.
               checkboxInput("impute_zeros", "Would you like to impute N/A's?", value = FALSE),
               # What do we do if there are missing values? Here are some methods.
               conditionalPanel(
                 condition = "input.impute_zeros == true",
                 radioButtons("imputation_method", "Choose imputation method:",
                              choices = c("Replace with zeros" = "zeros",
                                          "Background Noise" = "background_noise")),
                 selected = "zeros"),
               # If they chose 'background noise', let them adjust the level.
               conditionalPanel(
                 condition = "input.imputation_method == 'background_noise'",
                 sliderInput("background_noise_level", "Choose Background Noise Level:", min = 0, max = 10^4, value = 0)
               ),
               # Getting specifics for sample columns and proteins.
               textInput("sample_keyword", "Text used for quan columns, e.g. Abundance, Intensity, PG.Quantity", value = ""),
               selectInput("protein_col", "Header for Protein ID Column", choices = NULL),
               textInput("protein", "Enter Protein ID", value = ""),
               # Specifying the number of conditions.
               numericInput("num_conditions", "Number of Conditions (For sub-conditions within a primary condition, separate with the word 'or')", value = 1, min = 1),
               uiOutput("conditions_input"),
               # A big ol' button to execute the user's wishes.
               submitButton("Unleash the Analysis"),
             ),
             
             # The main panel: our space to deliver results. Don't mess this up.
             mainPanel(
               # Organizing our outputs with tabs for clarity.
               tabsetPanel(
                 # Each tab is a different view or analysis of the data.
                 # Simple table to summarize the data.
                 tabPanel("Summary Table",
                          radioButtons("summary_choice", "Choose Summary Type:",
                                       choices = c("Group" = "group", "Individual" = "individual")),
                          DTOutput("summary_table")),
                 # Just showcasing the columns they selected.
                 tabPanel("Selected Columns", DTOutput("selected_columns")),
                 # Who doesn't love a good PCA plot?
                 tabPanel("PCA Plot", 
                          radioButtons("log2_choice_pca", "Choose Scale:",
                                       choices = c("Original" = "original", "Log2" = "log2")),
                          plotlyOutput("pca_plot"),
                          # If they found something interesting, let them take the data home with them. 
                          uiOutput("download_pca_plot_button_ui")),
                 # Classic bar plot for comparisons.
                 tabPanel("Bar Plot", 
                          radioButtons("log2_choice_bar", "Choose Scale:",
                                       choices = c("Original" = "original", "Log2" = "log2")),
                          plotlyOutput("bar_plot"),
                          # If they found something interesting, let them take the data home with them. 
                          uiOutput("download_bar_plot_button_ui")),
                 # Box plots provide a deeper look into data distribution.
                 tabPanel("Box Plot", 
                          radioButtons("log2_choice_box", "Choose Scale:",
                                       choices = c("Original" = "original", "Log2" = "log2")),
                          plotOutput("box_plot")),
                 # Histograms for frequency distributions.
                 tabPanel("Histogram", 
                          radioButtons("log2_choice_hist", "Choose Scale:",
                                       choices = c("Original" = "original", "Log2" = "log2")),
                          sliderInput("bin_height", "Choose Number of Bins:", min = 5, max = 250, value = 50),
                          plotlyOutput("hist_plot")),
                 # Density plots for a smooth view of distributions.
                 tabPanel("Density Plot", 
                          radioButtons("log2_choice_dens", "Choose Scale:",
                                       choices = c("Original" = "original", "Log2" = "log2")),
                          plotOutput("density_plot")),
                 # And violins, because they're elegant (and useful!). Unlike viola plots, ew. 
                 tabPanel("Violin Plot",
                          radioButtons("log2_choice_violin", "Choose Scale:",
                                       choices = c("Original" = "original", "Log2" = "log2")), 
                          plotlyOutput("violin_plot"))
               )
             )
           )
  ),
  
  tabPanel("User Guide",
           
           tags$h2("SCP Viz User Guide"),
           tags$p("Welcome to SCP Viz, an interactive data visualization platform tailored for analyzing and visualizing Single Cell Proteomics (SCP) data. Our tool is designed to be compatible with diverse analytical software, including Proteome Discoverer, Spectronaut, and FragPipe."),
           
           tags$h3("1. Uploading Dataset:"),
           tags$ul(
             tags$li("Upload your SCP data file through the ", tags$b("Upload CSV File"), " button in the UI.")
           ),
           
           tags$h3("2. Imputation:"),
           tags$ul(
             tags$li("Activate the imputation option if you wish to impute missing values."),
             tags$li("Update your selection by clicking the ", tags$b("Unleash the Analysis"), " button."),
             tags$li("Choose whether to replace missing values with zeros or assign an arbitrary background noise level."),
             tags$li("Click ", tags$b("Unleash the Analysis"), " after each modification to apply the changes.")
           ),
           
           tags$h3("3. Quan Column Input:"),
           tags$ul(
             tags$li("Once the dataset is uploaded, specify the name of the quantification column in your output file, e.g., `Abundance`, `Intensity`, or `PG.Quantity`.")
           ),
           
           tags$h3("4. Protein Id Column:"),
           tags$ul(
             tags$li("Select the appropriate column header for the Protein ID, e.g., `Accession` or `Protein ID`.")
           ),
           
           tags$h3("5. Protein ID of Interest:"),
           tags$ul(
             tags$li("Enter the specific Protein ID you are interested in.")
           ),
           
           tags$h3("6. Experiment Conditions:"),
           tags$ul(
             tags$li("Define the number of experimental conditions and enter them as they are labeled in the sample names."),
             tags$li("Use `or` to separate multiple names representing a single condition."),
             tags$li("Update your selection by clicking ", tags$b("Unleash the Analysis"), " when adjusting the number of conditions.")
           ),
           
           tags$h3("Tabs:"),
           tags$ul(
             tags$li(tags$b("Summary Table:"), " Provides a detailed summary of the groups being compared."),
             tags$li(tags$b("Selected Columns:"), " Confirm that the intended columns are included."),
             tags$li(tags$b("Individual Plots:"), " PCA, Bar, Box, Histogram, and Violin Plots are available.")
           ),
           
           tags$h3("Interacting with Graphs:"),
           tags$ul(
             tags$li("Graphs are interactive, allowing for enhanced insights via downloading, zooming, panning, selecting, auto-scaling, tooltips, and legend interaction.")
           ),
           
           tags$h3("Data Transformation:"),
           tags$ul(
             tags$li("Opt for a Log2 transformed scale for a refined view of the dataset.")
           ),
           
           tags$h3("Plot Selection:"),
           tags$ul(
             tags$li("Select points of interest on the PCA and Bar plots, hit ", tags$b("Unleash the Analysis"), ", then click ", tags$b("Download CSV"), " to generate a CSV containing the Protein IDs and selected samples for immediate analysis.")
           )
  ),
  
  tabPanel("Test Data",
           h3("Test Data"),  # Header
           p("Please find the test data below:"),  # Intro text
           
           tags$h3("Data Set 1"),
           tags$ul(
             tags$li(
               a("Single-cell proteomic and transcriptomic analysis of macrophage heterogeneity using SCoPE2", 
                 href = "https://drive.google.com/drive/folders/1tAS9qXcinPi7xx0T34hJCuSSPjGAXmWy?usp=drive_link", 
                 target = "_blank"
               )
             ),
             tags$li(
               "Data was originally generated by:",
               tags$ul(  # Changed from 'ui' to 'ul'
                 tags$li("Specht, H., Emmott, E., Petelski, A.A. et al. Single-cell proteomic and transcriptomic analysis of macrophage heterogeneity using SCoPE2. Genome Biol 22, 50 (2021).")
               )
             ),
             tags$li(
               "To use this data, please use the following instructions:",
               tags$ul(  # This starts a new nested list
                 tags$li("Text for Quan Columns: 'Cell'"),
                 tags$li("Header for Protein ID: 'Accession'"),
                 tags$li("Enter Protein ID: 'Enter your protein accession of interest'"),
                 tags$li("Number of Conditions: '2'"),
                 tags$li(  # This 'li' will contain another nested list
                   "For the conditions, use the following names:",
                   tags$ul(
                     tags$li("Condition 1: 'sc_m0', which is the macrophage"),
                     tags$li("Condition 2: 'sc_u', which is the monocyte")
                     
                   )
                 )
               )
             )
           ),
           
           tags$h3("Data Set 2"),
           tags$ul(
             tags$li(
               a("Rapid non-uniform adaptation to conformation-specific KRASG12C inhibition", 
                 href = "https://drive.google.com/drive/folders/1jkzYTPYdcoOzyI3jpwvM_4GEeqRpFGmF?usp=share_link", 
                 target = "_blank"
               )
             ),
             tags$li(
               "Data was originally generated by:",
               tags$ul(  # Changed from 'ui' to 'ul'
                 tags$li("Xue JY, Zhao Y, Aronowitz J, Mai TT, Vides A, Qeriqi B, Kim D, Li C, de Stanchina E, Mazutis L, Risso D, Lito P. Rapid non-uniform adaptation to conformation-specific KRAS(G12C) inhibition. Nature. 2020 Jan;577(7790):421-425. doi: 10.1038/s41586-019-1884-x. Epub 2020 Jan 8. PMID: 31915379; PMCID: PMC7308074.")
               )
             ),
             tags$li(
               "To use this data, please use the following instructions:",
               tags$ul(  # This starts a new nested list
                 tags$li("Text for Quan Columns: 'H358'"),
                 tags$li("Header for Protein ID: 'Accession'"),
                 tags$li("Enter Protein ID: 'Enter your protein accession of interest'"),
                 tags$li("Number of Conditions: '4'"),
                 tags$li(  # This 'li' will contain another nested list
                   "For the conditions, use the following names:",
                   tags$ul(
                     tags$li("Condition 1: 'H358_A', which is the control sample"),
                     tags$li("Condition 2: 'H358_B', which is the 2 hour AMG510 treatment time point"),
                     tags$li("Condition 3: 'H358_C', which is the 24 hour AMG510 treatment time point"),
                     tags$li("Condition 4: 'H358_D', which is the 72 hour AMG510 treatment time point")
                   )
                 )
               )
             )
           ), 
           
           tags$h3("Data Set 3"),
           tags$ul(
             tags$li(
               a("Metabolomic, proteomic and single cell proteomic analysis of cancer cells treated with the KRASG12D inhibitor MRTX1133", 
                 href = "https://drive.google.com/drive/folders/1xt9Y0Xi2kAfGnt72r6xUR_aGI_4qlxrj?usp=share_link", 
                 target = "_blank"
               )
             ),
             tags$li(
               "Data was originally generated by:",
               tags$ul(  # Changed from 'ui' to 'ul'
                 tags$li("Orsburn BC. Metabolomic, proteomic and single cell proteomic analysis of cancer cells treated with the KRASG12D inhibitor MRTX1133. bioRxiv [Preprint]. 2023 Sep 13:2023.03.23.533981. doi: 10.1101/2023.03.23.533981. PMID: 36993160; PMCID: PMC10055375.")
               )
             ),
             tags$li(
               "To use this data, please use the following instructions:",
               tags$ul(  # This starts a new nested list
                 tags$li("Text for Quan Columns: 'Intensity'"),
                 tags$li("Header for Protein ID: 'protein_id'"),
                 tags$li("Enter Protein ID: 'Enter your protein accession of interest'"),
                 tags$li("Number of Conditions: '2'"),
                 tags$li(  # This 'li' will contain another nested list
                   "For the conditions, use the following names:",
                   tags$ul(
                     tags$li("Condition 1: 'Control or Ctrl', which is the control sample"),
                     tags$li("Condition 2: 'Treated', which is the treated sample")
                   )
                 )
               )
             )
           )
  )
 
)

# The 'server' is the backbone of our shiny app. It's where the magic happens!
server = function(input, output, session) {
  # Adjusting settings to allow large data uploads.
  options(shiny.maxRequestSize = Inf)
  
  # Dynamically reading the uploaded data and cleaning the column names.
  data <- reactive({
    # Ensure the data upload input is present.
    req(input$data_upload)
    filepath <- input$data_upload$datapath
    fileext <- tools::file_ext(filepath)
    
    # Read the uploaded file depending on its extension.
    if (fileext == "csv") {
      file <- read_csv(filepath)
    } else if (fileext == "tsv") {
      file <- read_delim(filepath, delim = '\t')# Read TSV properly
      write_csv(file, "newfile.csv")
      file <- read_csv("newfile.csv")
    } else if (fileext %in% c("xlsx", "xls")) {
      file <- read_excel(filepath)
      # Convert the Excel file to a CSV for processing
      write_csv(file, "newfile.csv")
      file <- read_csv("newfile.csv")
    } else {
      stop("Invalid file type") # Handle unsupported formats.
    }
    
    # Clean up column names for better usability.
    janitor::clean_names(file)
  })
  
  # Once we upload the data, dynamically update the dropdown menu in the UI.
  observeEvent(input$data_upload, {
    file <- data()
    updateSelectInput(session, "protein_col", choices = names(file))
  })
  
  # Select the appropriate columns from the uploaded CSV.
  selected_columns <- reactive({
    req(input$sample_keyword, input$data_upload)
    selected <- grepl(input$sample_keyword, names(data()), ignore.case = TRUE)
    data.frame(column_name = names(data())[selected])
  })
  
  # Display the selected columns in the UI.
  output$selected_columns <- renderTable({
    selected_columns()
  })
  
  # Organize the data to get it ready for visualization.
  organized_data <- reactive({
    req(input$protein_col)
    data() %>% 
      select(input$protein_col, contains(input$sample_keyword))
  })
  
  # For some visuals, we need the data in a dataframe format.
  df <- reactive({
    data.frame(organized_data())
  })
  
  # Convert our data to a 'long' format which is preferable for certain visualizations.
  df_long <- reactive({
    selected_col <- input$protein_col
    print(paste("Selected column:", selected_col))
    long_data <- df() %>%
      pivot_longer(cols = -all_of(selected_col), names_to = "Samples", values_to = "Intensity")
    
    # Check and handle missing data based on user's preference.
    if (input$impute_zeros) { 
      if (input$imputation_method == "zeros") {
        long_data <- long_data %>%
          mutate(Intensity = ifelse(is.na(Intensity), 0, Intensity))
      } else if (input$imputation_method == "background_noise") {
        long_data <- long_data %>%
          mutate(Intensity = ifelse(is.na(Intensity), input$background_noise_level, Intensity))
      }
    }
    
    long_data
  })
  
  # Filtering the long dataframe based on the protein selection by the user.
  df_filtered <- reactive({
    req(input$protein)
    col_name <- input$protein_col
    df_long() %>%
      filter((!!as.name(col_name)) == input$protein)
  })
  
  # Render input fields dynamically based on the number of conditions specified by the user.
  output$conditions_input <- renderUI({
    lapply(1:input$num_conditions, function(i) {
      textInput(inputId = paste0("condition_", i), label = paste0("Condition ", i), value = "")
    })
  })
  
  # This function identifies the sample type based on user-defined conditions.
  get_sample_type <- function(Samples, num_conditions, input) {
    conditions <- sapply(1:num_conditions, function(i) {
      
      # Convert condition input to lowercase and then split by "or", 
      # and remove any leading/trailing spaces
      condition_terms <- trimws(unlist(strsplit(tolower(input[[paste0("condition_", i)]]), split = "or", fixed = TRUE)))
      
      # Check if any of the terms match the Samples string
      any(sapply(condition_terms, function(x) grepl(paste0(x), Samples, ignore.case = TRUE)))
    })
    condition_names <- paste0("condition_", seq_along(conditions))
    matched_condition <- match(TRUE, conditions, nomatch = NA)
    
    if (!is.na(matched_condition)) {
      return(input[[paste0("condition_", matched_condition)]]) # If a match is found, return the matched condition name.
    }
    return(NA) # If no match is found, return NA.
  }
  
  # Extend the filtered data to include color assignments based on sample group.
  df_filtered_with_color <- reactive({
    df_filtered() %>%
      rowwise() %>%
      mutate(Group = get_sample_type(Samples, input$num_conditions, input)) %>%
      ungroup()
  })
  
  # Reactive function to retrieve the unique condition names, omitting any NA values.
  condition_names <- reactive({
    unique(na.omit(df_filtered_with_color()$Group))
  })
  
  # Generate a summary table to give an overview of the data based on the user's choice of grouping.
  output$summary_table <- renderDT({
    if (input$summary_choice == "group") {
      summary_data <- df_filtered_with_color() %>%
        group_by(Group) %>%
        summarise(
          min = min(Intensity, na.rm = TRUE),
          max = max(Intensity, na.rm = TRUE),
          median = round(median(Intensity, na.rm = TRUE), 2),
          mean = round(mean(Intensity, na.rm = TRUE), 2),
          SD = round(sd(Intensity, na.rm = TRUE), 2),
          n_total = length(Intensity),
          n_missing = sum(is.na(Intensity)),
          pct_missing = round(sum(is.na(Intensity))/length(Intensity), 3),
          n_values = sum(!is.na(Intensity))
        )
    } else { # For individual summaries.
      summary_data <- df_filtered() %>%
        group_by(Samples) %>%
        summarise(
          Abundance = sum(Intensity, na.rm = TRUE) # Sum up the Intensity for each sample as an example.
        )
    }
    datatable(summary_data) # Display the constructed summary data in a table format.
  })
  
  # Display the columns selected by the user in a table for confirmation.
  output$selected_columns <- renderDT({
    selected_columns()
  })
  
  #Now lets make the PCA plot
  # Create a reactive function for data transformation (wide format) and grouping.
  # The data is converted from long to wide format and potentially undergoes log2 transformation.
  df_wide_with_color <- reactive({
    # Convert from long to wide format based on user's selected protein column.
    df_wide <- df_long() %>%
      group_by(Samples, !!sym(input$protein_col)) %>%
      slice(1) %>%
      ungroup() %>%
      filter(input$protein_col != "sp") %>%
      pivot_wider(names_from = !!sym(input$protein_col), values_from = Intensity) %>%
      select_if(~ !any(is.na(.)))
    
    # Apply log2 conversion if user chooses to.
    if (input$log2_choice_pca == "log2") {
      df_wide <- df_wide %>%
        mutate(across(-Samples, ~log2(. + 1)))
    }
    
    # Add the Group column to identify sample type.
    df_wide %>%
      mutate(Group = sapply(Samples, function(sample) get_sample_type(sample, input$num_conditions, input)),
             UniqueID = row_number())
  })
  
  # Create a reactive function to compute the PCA results from the transformed data.
  pca_data <- reactive({
    # Remove only the Group column for PCA computation. Keep the Samples column.
    df_for_pca <- df_wide_with_color() %>%
      select(-Group)
    
    # Compute PCA on the data.
    pca_res <- PCA(df_for_pca[, -1], graph = FALSE, ncp = 2)  # Exclude the Samples column for PCA computation
    
    # Construct a dataframe to store the PCA results along with the Group and Samples information.
    df_pca <- data.frame(Samples = df_for_pca$Samples,
                         Dim1 = pca_res$ind$coord[, 1],
                         Dim2 = pca_res$ind$coord[, 2],
                         Group = df_wide_with_color()$Group,
                         UniqueID = df_wide_with_color()$UniqueID)
    df_pca
  })
  
  # Render a plotly PCA plot for the user using the pca_data.
  output$pca_plot <- renderPlotly({
    req(pca_data())
    pca_df <- pca_data()
    
    pca_plot <- ggplot(pca_df, aes(x = Dim1, y = Dim2, color = Group)) +
      geom_point(alpha = 0.7, size = 3, aes(customdata = Samples)) +
      theme_classic(base_size = 20) +
      labs(title = "PCA plot", x = "PC1", y = "PC2")
    
    ggplotly(pca_plot, source = "pcaPlot", tooltip = c("x", "y", "customdata"))
  })
  
  
  #Let's make the PCA plot interactive.
  # Initialize a variable to store points selected interactively in the PCA plot.
  selected_pca_plot_points <- reactiveVal(data.frame())
  
  # Observe any points selected by the user in the PCA plot and update the selected_points variable.
  observe({
    selected_data <- event_data("plotly_selected", source = "pcaPlot")
    
    print("Checking selected data:")  # Debugging line
    print(selected_data)              # Debugging line
    
    if (!is.null(selected_data)) {
      print("Data selected!")  # Debugging line
      
      # Extract the names of the selected samples
      selected_samples <- selected_data$customdata
      
      print("Selected samples:")  # Debugging line
      print(selected_samples)     # Debugging line
      
      # Extract selected rows from df_wide_with_color using the sample names
      selected_rows <- df_wide_with_color() %>%
        filter(Samples %in% selected_samples)
      
      # Update the selected_points reactive value
      selected_pca_plot_points(selected_rows)
    }
  })
  
  
  output$download_pca_plot_button_ui <- renderUI({
    if (!is.null(selected_pca_plot_points()) && nrow(selected_pca_plot_points()) > 0) {
      downloadButton("downloadPCAPlotCSV", "Download Selected Points")
    } else {
      NULL
    }
  })
  
  # Provide the user with a way to download the selected points from the PCA plot as a CSV file.
  output$downloadPCAPlotCSV <- downloadHandler(
    filename = function() "selected_pca_plot_points.csv",
    content = function(file) {
      # Extract selected points from the PCA plot
      selected_points <- selected_pca_plot_points()
      # Filter original dataframe based on selected points and order by Protein ID
      df_selected <- df() %>%
        select(all_of(c(input$protein_col, selected_points$Samples)))
      #arrange(get(input$protein_col)) if you want alphabetical
      # Write to CSV
      write_csv(df_selected, file)
    }
  )
  
  
  # Render a bar plot based on user data and preferences.
  output$bar_plot <- renderPlotly({
    
    # Check if log2 transformation is selected for the bar plot, if yes, then apply the transformation.
    if (input$log2_choice_bar == "log2") {
      plot_data <- df_filtered_with_color() %>%
        mutate(Intensity = log2(Intensity + 1)) # Add 1 to avoid log2(0)
    } else {
      plot_data <- df_filtered_with_color() 
    }
    
    # Generate a bar plot using ggplot2.
    bar_plotly <- ggplot(plot_data, aes(x = Samples, y = Intensity, fill = Group, customdata = Samples)) +
      geom_bar(stat = "identity", position = "dodge", width = 0.7) + # Adjust width here
      facet_grid(cols = vars(Group)) +
      theme_classic(base_size = 12) +
      theme(axis.text.x = element_blank()) +
      labs(y = paste("Intensity"))
    
    interactive_bar_plot <- ggplotly(bar_plotly, source = "barPlot", tooltip = c("x", "y"))
    print(interactive_bar_plot)
  })
  
  #Let's make the bar plot interactive.
  # Initialize a variable to store points selected interactively in the bar plot.
  selected_bar_plot_points <- reactiveVal(data.frame())
  
  # Observe any points selected by the user in the PCA plot and update the selected_points variable.
  observe({
    selected_data <- event_data("plotly_selected", source = "barPlot")
    
    print("Checking selected data for bar plot:")  # Debugging line
    print(selected_data)  # Debugging line
    
    if (!is.null(selected_data)) {
      print("Data selected in bar plot!")  # Debugging line
      
      # Extract the names of the selected samples
      selected_samples <- selected_data$customdata
      
      print("Selected samples in bar plot:")  # Debugging line
      print(selected_samples)  # Debugging line
      
      # Extract selected rows from df_wide_with_color using the sample names
      selected_rows <- df_wide_with_color() %>%
        filter(Samples %in% selected_samples)
      
      # Update the selected_points reactive value
      selected_bar_plot_points(selected_rows)
      print(selected_bar_plot_points)
    }
  })
  
  output$download_bar_plot_button_ui <- renderUI({
    if (!is.null(selected_bar_plot_points()) && nrow(selected_bar_plot_points()) > 0) {
      downloadButton("downloadBarPlotCSV", "Download Selected Points")
    } else {
      NULL
    }
  })
  
  # Provide the user with a way to download the selected points from the Bar Plot as a CSV file.
  output$downloadBarPlotCSV <- downloadHandler(
    filename = function() "selected_bar_plot_points.csv",
    content = function(file) {
      # Extract selected points from the bar plot
      selected_points <- selected_bar_plot_points()
      # Filter original dataframe based on selected points and order by Protein ID
      df_selected <- df() %>%
        select(all_of(c(input$protein_col, selected_points$Samples))) 
      # arrange(get(input$protein_col)) if you want alphabetical
      # Write to CSV
      write_csv(df_selected, file)
    }
  )
  
  # Render a box plot for data visualization.
  output$box_plot <- renderPlot({
    
    # Apply log2 transformation if selected for the box plot.
    if (input$log2_choice_box == "log2") {
      plot_data <- df_filtered_with_color() %>%
        mutate(Intensity = log2(Intensity + 1)) # Add 1 to avoid log2(0)
    } else {
      plot_data <- df_filtered_with_color()
    }
    
    # Define comparisons for the statistical test.
    my_comparisons <- list(
      c(condition_names()[1], condition_names()[2]) # Adjust these to match the actual values in your Group variable
    )
    
    # Generate a box plot using ggplot2.
    box_plot <- ggplot(plot_data, aes(x = Group, y = Intensity, fill = Group)) +
      geom_boxplot(width = 0.5) +
      geom_jitter(width = 0.2) +
      theme_classic(base_size = 14) +
      labs(x = "Group", y = paste("Intensity", unique(df_filtered()$protein_id))) +
      ggpubr::stat_compare_means(
        comparisons = my_comparisons,
        method = "t.test",
      )
    
    # Display the box plot.
    print(box_plot)
  })
  
  # Render an interactive histogram for data visualization.
  output$hist_plot <- renderPlotly ({
    # Set the bin height based on user input.
    bin_height <- input$bin_height
    
    # Apply log2 transformation if selected for the histogram.
    if (input$log2_choice_hist == "log2") {
      plot_data <- df_filtered_with_color() %>%
        mutate(Intensity = log2(Intensity + 1)) # Add 1 to avoid log2(0)
    } else {
      plot_data <- df_filtered_with_color()
    }
    
    # Generate a histogram using ggplot2.
    hist_plotly <- ggplot(plot_data, aes(x = Intensity, fill = Group)) +
      geom_histogram(bins = bin_height, color = "black") + # Use both binwidth and bins
      theme_classic(base_size = 14) +
      labs(x = "Intensity", y = "Frequency")
    
    # Convert the ggplot object to a Plotly object for interactivity.
    interactive_hist_plot <- ggplotly(hist_plotly)
    
    # Print the Plotly object to render it interactively
    print(interactive_hist_plot)
  })
  
  # Render the density plot
  output$density_plot <- renderPlot({
    
    # Apply log2 transformation if selected for the density plot.
    if (input$log2_choice_dens == "log2") {
      plot_data <- df_filtered_with_color() %>%
        mutate(Intensity = log2(Intensity + 1)) # Add 1 to avoid log2(0)
    } else {
      plot_data <- df_filtered_with_color()
    }
    
    # Generate a density plot using ggplot2.
    density_plot <- ggplot(plot_data, aes(x = Intensity, fill = Group)) +
      geom_density(alpha = 0.5) + # You can change the alpha to control the transparency of the plot
      theme_classic(base_size = 16) +
      labs(x = "Intensity", y = "Density")
    
    # Display the density plot.
    print(density_plot)
  })
  
  # Render an interactive violin plot.
  output$violin_plot <- renderPlotly({
    # Ensure that the filtered data with the color is available before generating the plot.
    req(df_filtered_with_color())
    
    # Apply log2 transformation if selected for the violin plot.
    if (input$log2_choice_violin == "log2") {
      plot_data <- df_filtered_with_color() %>%
        mutate(Intensity = log2(Intensity + 1)) # Add 1 to avoid log2(0)
    } else {
      plot_data <- df_filtered_with_color()
    }
    
    # Function to compute mean and standard deviation for data points.
    mean_sdl <- function(x, mult = 1) {
      mean <- mean(x, na.rm = TRUE)
      sdl <- sd(x, na.rm = TRUE) * mult
      data.frame(y = mean, ymin = mean - sdl, ymax = mean + sdl)
    }
    
    # Generate a violin plot using ggplot2.
    p_violin <- ggplot(plot_data, aes(x = Group, y = Intensity, fill = Group)) +
      geom_violin() +
      stat_summary(fun.data = mean_sdl, fun.args = list(mult = 1),
                   geom = "pointrange", color = "black",
                   shape = 18, size = 0.75,
                   position = position_dodge(width = 0.9)) +
      labs(title = "Violin Plot",
           x = "Conditions",
           y = ifelse(input$log2_choice == "log2", "Log2 Intensity", "Intensity")) +
      theme_minimal() +
      theme(
        text = element_text(size = 14),                # Overall text size
        axis.title = element_text(size = 14),          # Axis title size
        axis.text = element_text(size = 14),           # Axis text (ticks) size
        plot.title = element_text(size = 14, hjust = 0.5) # Plot title size
      )
    
    # Convert the ggplot object to a Plotly object
    interactive_violin_plot <- ggplotly(p_violin)
    
    # Print the Plotly object to render it interactively
    print(interactive_violin_plot)
  })
}

# Run the app ----

shinyApp(ui = ui, server = server)


