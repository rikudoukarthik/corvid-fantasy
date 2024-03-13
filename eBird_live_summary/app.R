library(shiny)
library(curl)
library(jsonlite)
library(tidyverse)
library(lubridate)
library(glue)
library(writexl)
library(rebird)
library(rclipboard)


source("token.R") # Get an eBird API token and assign it to object myebirdtoken
source("functions.R")
source("get_ebird_taxonomy.R")

# # local run
# source("eBird_live_summary/token.R")
# source("eBird_live_summary/functions.R")
# ebd_tax <- read_csv("eBird_live_summary/eBirdTaxonomy.csv")


# Define UI for app ----

ui <- fluidPage(
  
  # App title 
  titlePanel("Hello eBirder!"),
  
  helpText("Generate a summary of number of observers, checklists, and species reported for the selected date and region (country/state)."),
  
  # Sidebar layout with input and output definitions 
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: Date selection (value always yyyy-mm-dd, even if display format different)
      helpText(h4("Select the start and end dates of interest")),
      
      dateInput(inputId = "event_date_start", 
                label = "Start date", 
                value = today()),
      
      dateInput(inputId = "event_date_end", 
                label = "End date", 
                value = today()),
      
      # Input: Region (admin unit) code
      helpText(h4("Select the administrative unit code for the region of interest")),
      helpText("Summary will include selection and all admin. units one level below selection"),
      selectizeInput(inputId = "region_code", 
                     choices = c("Choose one" = "", 
                                 get_admin_codes("IN")),
                     selected = "", label = "Admin. unit"), 
      # # if IN, option to get 2nd admin unit summaries also
      # conditionalPanel(
      #   condition = "input.region_code == 'IN'", 
      #   checkboxInput(inputId = "deep_admin", 
      #                 label = "Generate summary for districts also (in addition to states)?",
      #                 value = FALSE, width = NULL)
      # ),
      checkboxInput(inputId = "text_req", label = "Generate textual summary?", 
                    value = FALSE, width = NULL),
      
      
      
      # Input: Event code for save file name
      helpText(h4("Provide a short event code")),
      helpText("This will be used in the file name and textual summary."),
      textInput(inputId = "event_code", 
                value = glue("GBBC_{today() %>% year()}"), label = "Event code"), 

      conditionalPanel(
        condition = "input.text_req == true && input.event_date_start == input.event_date_end", 
        helpText(h4("Provide day of event")),
        helpText("Which Day of the event have you chosen?"),
        numericInput(inputId = "event_day", label = NULL,
                     value = 1, step = 1, min = 1, max = 4, width = "10%")
      ),
      
    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "Summary",
          
          # Input: download button 
          helpText(h4("Download your eBird summary!")),
          
          
          downloadButton("downloadData", "Download",
                         label = "Summary (.xlsx)"),
          
          
          # Display generated text
          
          conditionalPanel(
            condition = "input.text_req == true && input.region_code != ''", 
            helpText(h4("Textual summary of selected admin. unit:")),
            
            rclipboardSetup(),
            textOutput("generated_text"),
            # Button to copy text
            uiOutput("clip")
          )
          
        ),
        
        tabPanel(
          "Top regions",
          
          # Input: download button 
          helpText(h4("Download summary bar chart of top regions")),
          
          
          downloadButton("downloadchart1", "Download",
                         label = "Bar chart (.png)"),
          
        ),
        
        tabPanel(
          "About",
          
          h2("About"),
          p("This tool is built to generate summaries of eBirding in a specific region for a specific date(s). It was created and is currently maintained for Bird Count India by Karthik Thrikkadeeri and Praveen J."),
          p("The code for this Shiny app can be found", 
            a("here", .noWS = "after",
              href = "https://github.com/rikudoukarthik/corvid-fantasy/tree/main/eBird_live_summary"),
            ". Please report any bugs or feature requests there."),
          p("Inspiration taken from", 
            a("birdsurveycrunch", href = "https://paintedstork.shinyapps.io/birdsurveycrunch/",
              .noWS = "after"),
            "."),
          p("Last update: ", lubridate::today())
        ),
        
      )
      
    
  )),
  
)


# Define server logic ----

server <- function(input, output) {

  # dates (single or multi)
  event_date <- reactive ({
    if (input$event_date_start == input$event_date_end) {
      input$event_date_start
    } else {
      seq(input$event_date_start, input$event_date_end, by = "days")
    }
  })
  
  event_day <- reactive ({
    input$event_day
  })

  region_info <- reactive ({
    get_admin_names(input$region_code)
  })

  
  # get list of species
  spec_list_adm1 <- reactive ({
    get_admin_codes(input$region_code, hi_arch = FALSE) %>%
      gen_spec_list(dates = event_date())
  })

  spec_list_adm2 <- reactive ({
    get_admin_codes(input$region_code, hi_arch = TRUE) %>%
      gen_spec_list(dates = event_date()) %>%
      filter(REGION != input$region_code)
  })

  # get participation stats
  
  basic_summary_adm1 <- reactive ({
    get_admin_codes(input$region_code, hi_arch = FALSE) %>%
      gen_part_summ(dates = event_date(), list_spec = spec_list_adm1())
  })

  basic_summary_adm2 <- reactive ({
    get_admin_codes(input$region_code, hi_arch = TRUE) %>%
      gen_part_summ(dates = event_date(), list_spec = spec_list_adm2()) %>%
      filter(REGION != input$region_code)
  })
  
  # textual summary
  text_summary_adm1 <- reactive ({
    if (input$text_req == TRUE) {
      basic_summary_adm1() %>% 
        mutate(TEXT = gen_textual_summ(SPECIES, OBSERVERS, CHECKLISTS, 
                                       event_code = input$event_code, 
                                       event_day = event_day())) %>% 
        dplyr::select(-c(SPECIES, OBSERVERS, CHECKLISTS))
    } else {
      NULL
    }
  })

  
  # ebird-style bar chart
  ebird_barchart_adm2 <- reactive ({
    basic_summary_adm2() %>% 
      gen_ebird_barchart()
  })
  
    
  # Downloadable .xlsx of selected dataset 
  output$downloadData <- downloadHandler(
    
    filename = function(){glue("{input$event_code}_summary.xlsx")},
    content = function(file) {
      
      withProgress(message = "Calculating summaries", value = 0, {
        
        Sys.sleep(1)

        data3 <- spec_list_adm1()    
        incProgress(0.25)
        data1 <- basic_summary_adm1()
        incProgress(0.25)
        data4 <- spec_list_adm2()    
        incProgress(0.25)
        data2 <- basic_summary_adm2()    
        incProgress(0.25)

        if (!is.null(text_summary_adm1())) {
          
          data1_text <- text_summary_adm1()
          
          write_xlsx(list("Summary (overall)" = data1,
                          "Summary (subregions)" = data2,
                          "Species list (overall)" = data3,
                          "Species list (subregions)" = data4,
                          "Summary text (overall)" = data1_text), 
                     file)
          
        } else {
          
          write_xlsx(list("Summary (overall)" = data1,
                          "Summary (subregions)" = data2,
                          "Species list (overall)" = data3,
                          "Species list (subregions)" = data4), 
                     file)
          
        }
        
      })
      
    }
    
  )
  
  # Downloadable .png of ebird bar charts 
  output$downloadchart1 <- downloadHandler(
    
    filename = function(){glue("{input$event_code}.png")},
    content = function(file) {

      ggsave(file, plot = ebird_barchart_adm2(), device = "png",
             width = 13, units = "in", dpi = 300,
             height = min(8 * (n_distinct(basic_summary_adm2() %>% pull(REGION.NAME)))/14,
                          10))
      
    }
      
  )
  
  
  # Display generated text based on input when download button is clicked
  output$generated_text <- renderText({
    req(text_summary_adm1())
    
    text_summary_adm1() %>%
      pull(TEXT)
  })
  
  # Add clipboard buttons
  output$clip <- renderUI({
    req(text_summary_adm1())

    rclipButton(
      inputId = "copy_text",
      label = "Copy",
      clipText = text_summary_adm1() %>% pull(TEXT), 
      icon = icon("clipboard"),
      tooltip = "Click me... I dare you!",
      placement = "top",
      options = list(delay = list(show = 800, hide = 100), trigger = "hover")
    )
  })

}

# Define app ----

shinyApp(ui = ui, server = server)

