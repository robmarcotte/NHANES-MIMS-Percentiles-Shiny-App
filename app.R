library(shiny)
library(devtools)
# install_github('brightantwiboasiako/wwadapercentiles', force = T)
library(wwadapercentiles)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(shinyWidgets)
library(ggthemes)
library(english)

# Load package reference datasets into memory globally so the plots render immediately
data("tac_data", package = "wwadapercentiles")
data("mims_data", package = "wwadapercentiles")

# Define UI
ui <- fluidPage(
  setBackgroundColor("seashell"),
  
  # Application title
  titlePanel("NHANES Activity Monitor Percentiles"),
  
  # Implement tabs to separate the original interactive tool and the added batch processing tool
  tabsetPanel(
    tabPanel("Individual Calculator",
             br(),
             sidebarLayout(
               sidebarPanel(
                 numericInput('age_input', label = 'Age (years):', value = 40, min = 0, max = 105, step = 1),
                 radioButtons('sex_input', label = 'Biological Sex:', choices = list('Male' = 1,'Female' = 2), selected = 1, inline = T),
                 radioButtons('metric_input', label = 'Activity Monitor Metric:', choices = list('Activity Counts' = 1,'MIMS Units' = 2), selected = 1, inline = T),
                 numericInput('activity_input', label = 'Total Activity Monitor Output:', value = 500000, min = 0, max = Inf, step = 1000)
               ),
               mainPanel(
                 htmlOutput('percentile_message'),
                 tags$head(tags$style("#percentile_message{font-size: 20px}")),
                 plotOutput('activityplot'),
                 br(),
                 htmlOutput('reference_title'),
                 htmlOutput('reference1'),
                 htmlOutput('reference2'),
                 htmlOutput('reference3')
               )
             )
    ),
    
    # Added tab for batch processing a summary dataset
    tabPanel("Batch CSV Processing",
             br(),
             sidebarLayout(
               sidebarPanel(
                 h4("Upload Summary Data"),
                 p("Upload a .csv file. The file MUST contain the following column headers exactly:"),
                 tags$ul(
                   tags$li(tags$b("id")),
                   tags$li(tags$b("age"), "must be between 6 and 85 inclusive"),
                   tags$li(tags$b("sex"), " (coded exactly as 'M' or 'F')"),
                   tags$li(tags$b("metric"), " (coded exactly as 'Counts' or 'MIMS')"),
                   tags$li(tags$b("value"), " (the raw numeric output value of either Counts or MIMS)")
                 ),
                 br(),
                 fileInput("uploaded_file", "Choose CSV File", accept = c("text/csv", ".csv")),
                 hr(),
                 uiOutput("download_ui")
               ),
               mainPanel(
                 h4("Preview of Output Data (Top 10 rows)"),
                 p("Your processed dataset with computed percentiles will preview below once uploaded:"),
                 tableOutput("preview_table"),
                 
                 #  Added Footnotes and References Block
                 hr(),
                 tags$p(tags$em(tags$b("Metric Specifications Note:")), style = "font-size: 13px; color: #555;"),
                 tags$ul(
                   tags$li(style = "font-size: 12px; color: #555;", 
                           tags$b("Counts:"), " Represent Vertical Axis Counts gathered from a Waist-Worn device configuration."),
                   tags$li(style = "font-size: 12px; color: #555;", 
                           tags$b("MIMS Units:"), " Represent summary metrics gathered from a Triaxial, Non-Dominant Wrist-Worn device configuration.")
                 ),
                 br(),
                 htmlOutput('batch_reference_title'),
                 htmlOutput('batch_reference1'),
                 htmlOutput('batch_reference2'),
                 htmlOutput('batch_reference3')
               )
             )
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Original App Observers and Outputs
  observeEvent(input$metric_input,{
    updateNumericInput(session, inputId = 'activity_input', value = ifelse(input$metric_input == 1, 
                                                                           ifelse(input$sex_input == 1, 290000, 240000),
                                                                           ifelse(input$sex_input == 1, 14000, 15000)))
  })
  
  observeEvent(input$sex_input,{
    updateNumericInput(session, inputId = 'activity_input', value = ifelse(input$metric_input == 1, 
                                                                           ifelse(input$sex_input == 1, 290000, 240000),
                                                                           ifelse(input$sex_input == 1, 14000, 15000)))
  })
  
  output$activityplot <- renderPlot({
    if(input$metric_input == 1){
      sex = ifelse(input$sex_input == 1, 'M', 'F')
      if(input$age_input <=19){
        plot_data = tac_data %>% gather(X5:X97, key = 'Percentile', value = 'Total_Activity') %>% dplyr::filter(Age < 20, Sex == sex) 
      } else {
        plot_data = tac_data %>% gather(X5:X97, key = 'Percentile', value = 'Total_Activity') %>% dplyr::filter(Age >= 20, Sex == sex)
      }
      
      user_percentile = wwadapercentiles::mims(y = input$activity_input, age = input$age_input, sex = ifelse(input$sex_input == 1, 'M', 'F'))
      
      if(input$sex_input == 1){
        if(input$age_input >= 20){
          tac_values = c(150000, 180000, 250000, 350000, 475000, 590000, 675000, 730000)
        } else {
          tac_values = c(390000, 430000, 500000, 600000, 725000, 850000, 930000, 990000)
        }
      } else {
        if(input$age_input >= 20){
          tac_values = c(150000, 175000, 220000, 265000, 325000, 390000, 435000, 475000)
        } else {
          tac_values = c(345000, 375000, 430000, 510000, 610000, 700000, 775000, 835000)
        }
      }
      
      perc_labs = data.frame(Age = ifelse(input$age_input >= 20, 19, 5.8),
                             labels = c(5, 10, 25, 50, 75, 90, 95, 97),
                             Total_Activity = tac_values)
      
      ggplot(data = plot_data, aes(x = Age, y = Total_Activity, group = Percentile)) + 
        geom_line(color = 'black', linewidth = 1.2) + # changed from 'size' to 'linewidth' due to deprecation in ggplot2 version
        geom_point(inherit.aes = FALSE, data = data.frame(x = ifelse(input$age_input >= 80, 80, input$age_input), y = input$activity_input), 
                   aes(x = x, y = y), size = 3, color = 'navy blue') +
        theme(axis.text.x = element_text(angle = 60, vjust = 0.5))+ 
        labs(x = 'Age (years)', y = 'Daily Activity Counts (x1000)') +
        scale_y_continuous(breaks = seq(0, 1000000, by = 100000), labels = seq(0, 1000, by = 100)) +
        annotate('text', x = perc_labs$Age, y = perc_labs$Total_Activity, label = paste('bold(', perc_labs$label,')', sep = ''), parse = T)+ theme_calc() +
        theme(axis.title = element_text(size = 14), axis.text = element_text(size =12))
      
    } else {
      sex = ifelse(input$sex_input == 1, 'M', 'F')
      if(input$age_input <=19){
        plot_data = mims_data %>% gather(X3:X97, key = 'Percentile', value = 'Total_Activity') %>% mutate(Age = as.numeric(Age)) %>% dplyr::filter(Age < 20, Sex == sex) 
      } else {
        plot_data = mims_data %>% gather(X3:X97, key = 'Percentile', value = 'Total_Activity')  %>% mutate(Age = as.numeric(Age)) %>% dplyr::filter(Age >= 20, Sex == sex)
      }
      
      user_percentile = wwadapercentiles::mims(y = input$activity_input, age = input$age_input, sex = ifelse(input$sex_input == 1, 'M', 'F'))
      
      if(input$sex_input == 1){
        if(input$age_input >= 20){
          mims_values = c(7500, 8500, 9750, 11750, 14000, 16500, 18750, 20250, 21250)
        } else {
          mims_values = c(13500, 14500, 15500, 17000, 18500, 20500, 22000, 23000, 24000)
        }
      } else {
        if(input$age_input >= 20){
          mims_values = c(8000, 9000, 10000, 12000, 14000, 16500, 18500, 20000, 21500)
        } else {
          mims_values = c(13000, 14000, 15500, 17000, 19000, 20500, 22500, 23500, 24500)
        }
      }
      
      perc_labs = data.frame(Age = ifelse(input$age_input >= 20, 19, 2.8),
                             labels = c(3, 5, 10, 25, 50, 75, 90, 95, 97),
                             Total_Activity = mims_values)
      
      ggplot(data = plot_data, aes(x = Age, y = Total_Activity, group = Percentile)) + 
        geom_line(color = 'black', linewidth = 1.2) + 
        # Exact original placement with inherit.aes = FALSE added to prevent aesthetic mapping crash
        geom_point(inherit.aes = FALSE, data = data.frame(x = ifelse(input$age_input >= 80, 80, input$age_input), y = input$activity_input), 
                   aes(x = x, y = y), size = 3, color = 'navy blue') +
        theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + 
        labs(x = 'Age (years)', y = 'Daily MIMS-units (x1000)') +
        scale_y_continuous(breaks = seq(5000, 27000, by = 2000), labels = seq(5, 27, by = 2)) +
        annotate('text', x = perc_labs$Age, y = perc_labs$Total_Activity, label = perc_labs$label, parse = T) + theme_calc()+
        theme(axis.title = element_text(size = 14), axis.text = element_text(size =12))
    }
  })
  
  output$percentile_message = renderText({
    if(input$metric_input == 1){
      user_percentile = wwadapercentiles::tac(y = input$activity_input, age = input$age_input, sex = ifelse(input$sex_input == 1, 'M', 'F'))
    } else {
      user_percentile = wwadapercentiles::mims(y = input$activity_input, age = input$age_input, sex = ifelse(input$sex_input == 1, 'M', 'F'))
    }
    message = paste('Your activity levels place you in the <B>', user_percentile*100, str_trunc(english::ordinal(user_percentile*100), 2, side = 'left', ellipsis = ''),' percentile for your age and sex!</B>', sep = '')
    return(message)
  })
  
  # --- References for the Individual Tab ---
  output$reference_title = renderText({ paste('<B>References:</B>') })
  output$reference1 = renderText({ paste('<B>1.</B> Wolff-Hughes DL, Fitzhugh EC, Bassett DR, Churilla JR. Waist-Worn Actigraphy: Population-Referenced Percentiles for Total Activity Counts in U.S. Adults. J Phys Act Health. 2015 Apr;12(4):447-53. doi: 10.1123/jpah.2013-0464. Epub 2014 Jun 4. PMID: 24905055.') })
  output$reference2 = renderText({ paste('<B>2.</B> Wolff-Hughes DL, Bassett DR, Fitzhugh EC. Population-referenced percentiles for waist-worn accelerometer-derived total activity counts in U.S. youth: 2003 - 2006 NHANES. PLoS One. 2014 Dec 22;9(12):e115915. doi: 10.1371/journal.pone.0115915. PMID: 25531290; PMCID: PMC4274159.') })
  output$reference3 = renderText({ paste('<B>3.</B> Belcher BR, Wolff-Hughes DL, Dooley EE, Staudenmayer J, Berrigan D, Eberhardt MS, Troiano RP. U.S. Population-referenced Percentiles for Wrist-Worn Accelerometer-derived Activity. Med Sci Sports Exerc. 2021 Jun 11. doi: 10.1249/MSS.0000000000002726. Epub ahead of print. PMID: 34115727.') })
  
  # --- Batch CSV Processing Logic ---
  processed_data <- reactive({
    req(input$uploaded_file)
    df <- read.csv(input$uploaded_file$datapath, stringsAsFactors = FALSE)
    names(df) <- tolower(names(df))
    
    required_cols <- c("id", "age", "sex", "metric", "value")
    if(!all(required_cols %in% names(df))) {
      showNotification("Error: CSV must contain 'id', 'age', 'sex', 'metric', and 'value' columns.", type = "error")
      return(NULL)
    }
    
    percentiles <- numeric(nrow(df))
    for(i in 1:nrow(df)) {
      row_sex <- toupper(trimws(df$sex[i])) 
      row_metric <- tolower(trimws(df$metric[i])) 
      
      if (row_metric %in% c("counts", "tac", "activity counts")) {
        percentiles[i] <- wwadapercentiles::tac(y = df$value[i], age = df$age[i], sex = row_sex)
      } else if (row_metric %in% c("mims", "mims units")) {
        percentiles[i] <- wwadapercentiles::mims(y = df$value[i], age = df$age[i], sex = row_sex)
      } else {
        percentiles[i] <- NA 
      }
    }
    
    # Appends values rounded cleanly to whole integers
    df$calculated_percentile <- round(percentiles * 100, 1)
    return(df)
  })
  
  output$preview_table <- renderTable({
    req(processed_data())
    head(processed_data(), 10)
  })
  
  output$download_ui <- renderUI({
    req(processed_data())
    downloadButton("download_csv", "Download Processed CSV File", class = "btn-success")
  })
  
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("Batch_NHANES_Percentiles_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(processed_data(), file, row.names = FALSE)
    }
  )
  
  # References for the Batch Tab
  output$batch_reference_title = renderText({ paste('<B>References:</B>') })
  output$batch_reference1 = renderText({ paste('<B>1.</B> Wolff-Hughes DL, Fitzhugh EC, Bassett DR, Churilla JR. Waist-Worn Actigraphy: Population-Referenced Percentiles for Total Activity Counts in U.S. Adults. J Phys Act Health. 2015 Apr;12(4):447-53. doi: 10.1123/jpah.2013-0464. Epub 2014 Jun 4. PMID: 24905055.') })
  output$batch_reference2 = renderText({ paste('<B>2.</B> Wolff-Hughes DL, Bassett DR, Fitzhugh EC. Population-referenced percentiles for waist-worn accelerometer-derived total activity counts in U.S. youth: 2003 - 2006 NHANES. PLoS One. 2014 Dec 22;9(12):e115915. doi: 10.1371/journal.pone.0115915. PMID: 25531290; PMCID: PMC4274159.') })
  output$batch_reference3 = renderText({ paste('<B>3.</B> Belcher BR, Wolff-Hughes DL, Dooley EE, Staudenmayer J, Berrigan D, Eberhardt MS, Troiano RP. U.S. Population-referenced Percentiles for Wrist-Worn Accelerometer-derived Activity. Med Sci Sports Exerc. 2021 Jun 11. doi: 10.1249/MSS.0000000000002726. Epub ahead of print. PMID: 34115727.') })
  
}

# Run the application 
shinyApp(ui = ui, server = server)