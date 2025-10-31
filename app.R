#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

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


# Define UI for application that draws a histogram
ui <- fluidPage(
    setBackgroundColor("seashell"),
    
    # Application title
    titlePanel("NHANES Activity Monitor Percentiles"),
    sidebarLayout(sidebarPanel(numericInput('age_input', label = 'Age (years):', value = 40, min = 0, max = 105, step = 1),
                               radioButtons('sex_input', label = 'Biological Sex:', choices = list('Male' = 1,'Female' = 2), selected = 1, inline = T),
                               radioButtons('metric_input', label = 'Activity Monitor Metric:', choices = list('Activity Counts' = 1,'MIMS Units' = 2), selected = 1, inline = T),
                               numericInput('activity_input', label = 'Total Activity Monitor Output:', value = 500000, min = 0, max = Inf, step = 1000)),
                  mainPanel(htmlOutput('percentile_message'),
                            
                            tags$head(tags$style("#percentile_message{font-size: 20px}")),
                            
                            plotOutput('activityplot'),
                            br(),
                            htmlOutput('reference_title'),
                            htmlOutput('reference1'),
                            htmlOutput('reference2'),
                            htmlOutput('reference3'))),
    fluidRow(column(width = 12))
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    
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
            # Filter for age-group and gender-specific percentile curve data
            if(input$age_input <=19){
                plot_data = tac_data %>% gather(X5:X97, key = 'Percentile', value = 'Total_Activity') %>% dplyr::filter(Age < 20, Sex == sex) 
            } else {
                plot_data = tac_data %>% gather(X5:X97, key = 'Percentile', value = 'Total_Activity') %>% dplyr::filter(Age >= 20, Sex == sex)
                
            }
            
            user_percentile = wwadapercentiles::mims(y = input$activity_input, age = input$age_input, sex = ifelse(input$sex_input == 1, 'M', 'F'))
            
            if(input$sex_input == 1){
                if(input$age_input >= 20){
                    tac_values = c(150000, 180000, 250000, 350000, 475000, 590000, 675000, 730000) # adult males
                } else {
                    tac_values = c(390000, 430000, 500000, 600000, 725000, 850000, 930000, 990000) # youth males
                }
            } else {
                if(input$age_input >= 20){
                    tac_values = c(150000, 175000, 220000, 265000, 325000, 390000, 435000, 475000) # adult females
                } else {
                    tac_values = c(345000, 375000, 430000, 510000, 610000, 700000, 775000, 835000) # youth females
                }
            }
            
            perc_labs = data.frame(Age = ifelse(input$age_input >= 20, 19, 5.8),
                                   labels = c(5, 10, 25, 50, 75, 90, 95, 97),
                                   Total_Activity = tac_values)
            
            # TAC curve
            ggplot(data = plot_data, aes(x = Age, y = Total_Activity, group = Percentile)) + 
                geom_line(color = 'black', size = 1.2) + #geom_smooth(color = 'black', se = F) + 
                geom_point(inherit.aes = F, aes(x = ifelse(input$age_input>=80, 80, input$age_input), y = input$activity_input), size = 3, color = 'navy blue') +
                theme(axis.text.x = element_text(angle = 60, vjust = 0.5))+ 
                labs(x = 'Age (years)', y = 'Daily Activity Counts (x1000)') +
                scale_y_continuous(breaks = seq(0, 1000000, by = 100000), labels = seq(0, 1000, by = 100)) +
                annotate('text', x = perc_labs$Age, y = perc_labs$Total_Activity, label = paste('bold(', perc_labs$label,')', sep = ''), parse = T)+ theme_calc() +
                theme(axis.title = element_text(size = 14), axis.text = element_text(size =12))
            
            
            
        } else {
            
            sex = ifelse(input$sex_input == 1, 'M', 'F')
            # Filter for age-group and gender-specific percentile curve data
            if(input$age_input <=19){
                plot_data = mims_data %>% gather(X3:X97, key = 'Percentile', value = 'Total_Activity') %>% mutate(Age = as.numeric(Age)) %>% dplyr::filter(Age < 20, Sex == sex) 
            } else {
                plot_data = mims_data %>% gather(X3:X97, key = 'Percentile', value = 'Total_Activity')  %>% mutate(Age = as.numeric(Age)) %>% dplyr::filter(Age >= 20, Sex == sex)
                
            }
            # MIMS curve data
            user_percentile = wwadapercentiles::mims(y = input$activity_input, age = input$age_input, sex = ifelse(input$sex_input == 1, 'M', 'F'))
            
            if(input$sex_input == 1){
                if(input$age_input >= 20){
                    mims_values = c(7500, 8500, 9750, 11750, 14000, 16500, 18750, 20250, 21250) # adult males
                } else {
                    mims_values = c(13500, 14500, 15500, 17000, 18500, 20500, 22000, 23000, 24000) # youth males
                }
            } else {
                if(input$age_input >= 20){
                    mims_values = c(8000, 9000, 10000, 12000, 14000, 16500, 18500, 20000, 21500) # adult females
                } else {
                    mims_values = c(13000, 14000, 15500, 17000, 19000, 20500, 22500, 23500, 24500) # youth females
                }
            }
            
            perc_labs = data.frame(Age = ifelse(input$age_input >= 20, 19, 2.8),
                                   labels = c(3, 5, 10, 25, 50, 75, 90, 95, 97),
                                   Total_Activity = mims_values)
            
            ggplot(data = plot_data, aes(x = Age, y = Total_Activity, group = Percentile)) + 
                geom_line(color = 'black', size = 1.2) + #geom_smooth(color = 'black', se = F) + 
                geom_point(inherit.aes = F, aes(x = ifelse(input$age_input>=80, 80, input$age_input), y = input$activity_input), size = 3,color = 'navy blue') +
                theme(axis.text.x = element_text(angle = 60, vjust = 0.5)) + 
                labs(x = 'Age (years)', y = 'Daily MIMS-units (x1000)') +
                scale_y_continuous(breaks = seq(5000, 27000, by = 2000), labels = seq(5, 27, by = 2)) +
                annotate('text', x = perc_labs$Age, y = perc_labs$Total_Activity, label = perc_labs$label, parse = T) + theme_calc()+
                theme(axis.title = element_text(size = 14), axis.text = element_text(size =12))
            
        }
    })
    
    output$percentile_message = renderText({
        if(input$metric_input == 1){
            # User percentile for TAC
            user_percentile = wwadapercentiles::tac(y = input$activity_input, age = input$age_input, sex = ifelse(input$sex_input == 1, 'M', 'F'))
            
        } else {
            # User percentile for MIMS
            user_percentile = wwadapercentiles::mims(y = input$activity_input, age = input$age_input, sex = ifelse(input$sex_input == 1, 'M', 'F'))
            
        }
        message = paste('Your activity levels place you in the <B>', user_percentile*100, str_trunc(english::ordinal(user_percentile*100), 2, side = 'left', ellipsis = ''),' percentile for your age and sex!</B>', sep = '')
        return(message)
        
    })
    
    output$reference_title = renderText(paste('<B>References:</B>'))
    
    output$reference1 = renderText({
        message = paste('<B>1.</B> Wolff-Hughes DL, Fitzhugh EC, Bassett DR, Churilla JR. Waist-Worn Actigraphy: Population-Referenced Percentiles for Total Activity Counts in U.S. Adults. J Phys Act Health. 2015 Apr;12(4):447-53. doi: 10.1123/jpah.2013-0464. Epub 2014 Jun 4. PMID: 24905055.', sep = '')
        return(message)
        
    })
    output$reference2 = renderText({
        message = paste('<B>2.</B> Wolff-Hughes DL, Bassett DR, Fitzhugh EC. Population-referenced percentiles for waist-worn accelerometer-derived total activity counts in U.S. youth: 2003 - 2006 NHANES. PLoS One. 2014 Dec 22;9(12):e115915. doi: 10.1371/journal.pone.0115915. PMID: 25531290; PMCID: PMC4274159.', sep = '')
        return(message)
        
    })
    output$reference3 = renderText({
        message = paste('<B>3.</B> Belcher BR, Wolff-Hughes DL, Dooley EE, Staudenmayer J, Berrigan D, Eberhardt MS, Troiano RP. U.S. Population-referenced Percentiles for Wrist-Worn Accelerometer-derived Activity. Med Sci Sports Exerc. 2021 Jun 11. doi: 10.1249/MSS.0000000000002726. Epub ahead of print. PMID: 34115727.', sep = '')
        
        return(message)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
