

library(shiny)
library(bslib)
library(tidyverse)
library(gridExtra)
library(ggplot2)
library(rsconnect)
library(shinyWidgets)
library(plotly)
library(lubridate)
library(DT)

df <-read.csv("data/working_shiny_file.csv")
shinyOptions(bootstrapTheme=bslib::bs_theme())
df <- df %>% 
  mutate(
    ChronicAbsenteeismEligibleCumulativeEnrollment = as.numeric(ChronicAbsenteeismEligibleCumulativeEnrollment),
    ChronicAbsenteeismCount = as.numeric(ChronicAbsenteeismCount)
  ) %>% 
  mutate_if(is.character, utf8::utf8_encode)


ui <- fluidPage(
  theme = "bootstrap.css",
  
  navbarPage(
    "RAISE Chronic Absenteeism Control Chart Dashboard",
    id = "main_navbar",
    fluid = TRUE,
    theme= "bootstrap.css",
    tabPanel(
      "Case Study Generator",
      "This interactive dashboard allows you to explore and analyze chronic absenteeism data for your school or district. By selecting a school or district, you can generate a comprehensive PDF report that highlights key trends and insights into attendance patterns.",
      br(),br(),
      "Chronic absenteeism—defined as missing a significant portion of the school year—can impact student success and school performance. This dashboard is designed to help educators, administrators, and stakeholders identify areas of concern, monitor progress, and inform strategies to improve attendance.",
      br(), br(),
      br(),
      'Case Study Generator: School-level',
      pickerInput("school_name_cs", "School: ",
                  choices = unique(df$School.Name),
                  options = list(`actions-box` = TRUE,
                                 `live-search` = TRUE),
                  selected = "High Tech High International",
                  multiple = FALSE),
      downloadButton("report_school", "Generate Report"),
      br(),
      br(),
      br(),
      "Case Study Generator: District-level",
      br(),
      pickerInput("district_name_cs", "District: ",
                  choices = unique(df$District.Name),
                  options = list(`actions-box` = TRUE,
                                 `live-search` = TRUE),
                  selected = "San Diego Unified",
                  multiple = FALSE),
      downloadButton("report_district", "Generate Report")
    )
    ,
    tabPanel(
      "School Auto-Comparison",
      sidebarLayout(
      sidebarPanel(
                    pickerInput("school_name_a", "School: ",
                                choices = unique(df$School.Name),
                                options = list(`actions-box` = TRUE,
                                               `live-search` = TRUE),
                                selected = "High Tech High International",
                                multiple = FALSE),
                    pickerInput("school_year_a", "Academic Year: ",
                                choices = sort(unique(df$Academic.Year)),
                                options = list(`actions-box` = TRUE),
                                selected = "2023-24",
                                multiple = FALSE),
                    pickerInput("reporting_category_a", "Student Group: ",
                                choices = unique(df$Reporting.Category),
                                options = list(`actions-box` = TRUE),
                                selected = "All Students",
                                multiple = FALSE)
        ,
        width = 3,
        tags$div(class="header", checked=NA,
                 tags$p("Instructions on how to use this page: "),
                 tags$br(),
                 tags$p("Select a school from the list. Up to 20 additional schools will be selected as a comparison group. You can also use the filters to select the academic year visualized on the top chart, and a student population subset."),
                 tags$br(),
                 tags$p("Comparison schools are schools in the same county as the one you selected, sharing a similar % of SED Students"),
                 tags$br(),
                 tags$p("The line chart at the bottom shows only the data for the school you selected."))
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("school_view", height = "300%"),
        plotlyOutput("school_run_chart")
        ,width=9
                            
      )
    )),
    tabPanel(
      "School Manual Selection",
      sidebarLayout(
      sidebarPanel(
          
                    uiOutput("school_filter"),
                    pickerInput("school_year_c", "Academic Year: ",
                                choices = sort(unique(df$Academic.Year)),
                                options = list(`actions-box` = TRUE),
                                selected = "2023-24",
                                multiple = FALSE),
                    pickerInput("reporting_category_c", "Student Group: ",
                                choices = unique(df$Reporting.Category),
                                options = list(`actions-box` = TRUE),
                                selected = "All Students",
                                multiple = FALSE),
                    uiOutput("ss_filter"),
                    uiOutput("size_filter"),
                    uiOutput("county_filter"),
                    uiOutput("eil_filter"),
                    uiOutput("district_filter_s")
        ,
        width = 3,
        tags$div(class="header", checked=NA,
                 tags$p("Instructions on how to use this page: "),
                 tags$br(),
                 tags$p("Select up to 20 schools from the list. You can also use the filters to select the academic year visualized on the top chart, and a student population subset."),
                 tags$br(),
                 tags$p("Other filters will affect which schools are shown in the 'School' drop-down menu. For example: Selecting San Diego Unified as a district, will make it so that only San Diego Unified schools are shown in the School drop-down menu."),
                 tags$br(),
                 tags$p("The line chart at the bottom shows the combined data for all the schools you selected."))
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("school_free", height = "300%"),
        plotlyOutput("school_run_chart_free")
        ,width = 9
      )
    )
  ),
  tabPanel(
    "District Manual Selection",
    sidebarLayout(
      sidebarPanel(
                    uiOutput("district_filter"),
                              pickerInput("school_year_d", "Academic Year: ",
                                          choices = unique(df$Academic.Year),
                                          options = list(`actions-box` = TRUE),
                                          selected = "2023-24",
                                          multiple = TRUE),
                              pickerInput("reporting_category_d", "Student Group: ",
                                          choices = unique(df$Reporting.Category),
                                          options = list(`actions-box` = TRUE),
                                          selected = "All Students",
                                          multiple = TRUE),
        uiOutput("county_filter_d_f")
        ,
        width = 3,
        tags$div(class="header", checked=NA,
                 tags$p("Instructions on how to use this page: "),
                 tags$br(),
                 tags$p("Select up to 20 districts from the list. You can also use the filters to select the academic year visualized on the top chart, and a student population subset."),
                 tags$br(),
                 tags$p("The line chart at the bottom shows the combined data for all the districts you selected."))
      ),
      # Show a plot of the generated distribution
      mainPanel(
        plotlyOutput("district_view", height = "300%"),
        plotlyOutput("district_run_chart")
        ,width=9
        
      )
    ))  
  
)
)
        
       
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Reactive Filters ----
  
    # Case Study (School)
    output$report_school <- downloadHandler(
      filename="NCIE School Absenteeism Report.pdf",
      content = function(file) {
      params <- list(
        school = input$school_name_cs,
        df = df
      )  
        rmarkdown::render("report.Rmd", 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    # Case Study (District)
    output$report_district <- downloadHandler(
      filename="NCIE District Absenteeism Report.pdf",
      content = function(file) {
        params <- list(
          district = input$district_name_cs,
          df = df
        )  
        rmarkdown::render("report_district.Rmd", 
                          output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv())
        )
      }
    )
  
    # School Chart
    output$school_view <- renderPlotly({
        income_bracket <- df %>% filter(
          School.Name %in% input$school_name_a,
          Academic.Year %in% input$school_year_a,
          Reporting.Category %in% input$reporting_category_a
        ) %>% select(
          pct_ss_bucket
        )
      
        df <- df %>% 
          filter(
            Reporting.Category %in% input$reporting_category_a,
            Academic.Year %in% input$school_year_a,
            DASS != "Yes"
          )
      
        df.chart <- df %>%
            filter(
              School.Name %in% input$school_name_a
            )
        
        comparison_selection <- df %>% filter(
          County.Name %in% unique(df.chart$County.Name) &
          EILName %in% unique(df.chart$EILName),
          pct_ss_bucket %in% income_bracket)
        
        if(nrow(comparison_selection) < 15) {
          additional_income <- case_when(
            income_bracket == "0% - 10%" ~ c("0% - 10%" ,"10% - 20%", "20% - 30%"),
            income_bracket == "10% - 20%" ~ c("0% - 10%", "10% - 20%", "20% - 30%"),
            income_bracket == "20% - 30%" ~ c("10% - 20%", "20% - 30%", "30% - 40%"),
            income_bracket == "30% - 40%" ~ c("20% - 30%", "30% - 40%", "40% - 50%"),
            income_bracket == "40% - 50%" ~ c("30% - 40%", "40% - 50%", "50% - 60%"),
            income_bracket == "50% - 60%" ~ c("40% - 50%", "50% - 60%", "60% - 70%"),
            income_bracket == "60% - 70%" ~ c("50% - 60%", "60% - 70%", "70% - 80%"),
            income_bracket == "70% - 80%" ~ c("60% - 70%", "70% - 80%", "80% - 90%"),
            income_bracket == "80% - 90%" ~ c("70% - 80%", "80% - 90%", "90% - 100%"),
            income_bracket == "90% - 100%" ~ c("70% - 80%", "80% - 90%", "90% - 100%")
          )
          
          comparison_selection <- df %>% filter(
            County.Name %in% unique(df.chart$County.Name),
            EILName %in% unique(df.chart$EILName),
            pct_ss_bucket %in% additional_income)
        }
          
          comparison_num <- if (nrow(comparison_selection) < 19) nrow(comparison_selection) else 19
          set.seed(123)
          comparison_selection <- comparison_selection[sample(1:nrow(comparison_selection), comparison_num, replace=FALSE), ]
          cat("Comparison: ",nrow(comparison_selection), "\n")
          df.chart <- rbind(
            df.chart,
            comparison_selection
          )
        
        
        df.chart <- df.chart %>% 
          mutate(
            ca_pct = round(100 * ChronicAbsenteeismCount/ChronicAbsenteeismEligibleCumulativeEnrollment, 2)
          )
        
        q1 <- qicharts2::qic(
            x=reorder(
              School.Name, 
              ChronicAbsenteeismEligibleCumulativeEnrollment),
            y = ChronicAbsenteeismCount,
            n= ChronicAbsenteeismEligibleCumulativeEnrollment,
            data = df.chart,
            chart = 'p',
            title = "Chronic Absenteeism by School",
            xlab = "School",
            x.angle = 45,
            point.size = 1,
            show.labels = T
        )
        ggplotly(q1)
    })
    
    # School Run Chart
    output$school_run_chart <- renderPlotly({
      df <- df %>% 
        filter(
          Reporting.Category %in% input$reporting_category_a,
          DASS != "Yes"
        )
      
      df.chart <- df %>%
        filter(
          School.Name %in% input$school_name_a
        ) %>% 
        mutate(
          ca_pct = round(100*(ChronicAbsenteeismCount/ChronicAbsenteeismEligibleCumulativeEnrollment),2)
        )
      
      q1 <- qicharts2::qic(
        x=factor(Academic.Year),
        y = ChronicAbsenteeismCount,
        n = ChronicAbsenteeismEligibleCumulativeEnrollment,
        data = df.chart,
        chart = 'p',
        freeze = 3,
        title = "Chronic Absenteeism Over Time (Selected School)",
        xlab = "School Year",
        x.angle = 45,
        point.size = 1,
        show.labels = T
      ) + geom_line()
      q1
      ggplotly(q1)
    })
    
    # District Chart
    output$district_view <- renderPlotly({
    
      df.chart <- df %>% 
        filter(
          District.Name %in% input$district,
          Reporting.Category %in% input$reporting_category_d,
          Academic.Year %in% input$school_year_d,
          DASS != "Yes"
        ) %>% 
        group_by(District.Name) %>% 
        summarize(
          enrollment = sum(ChronicAbsenteeismEligibleCumulativeEnrollment, na.rm=T),
          ca = sum(ChronicAbsenteeismCount, na.rm=T),
          county = first(County.Name)
        )
      
      q1 <- qicharts2::qic(
        x=reorder(
          District.Name, 
          enrollment),
        y = ca,
        n= enrollment,
        data = df.chart,
        chart = 'pp',
        title = "Chronic Absenteeism by District (P-Prime Chart)",
        xlab = "District",
        x.angle = 45,
        point.size = 1,
        show.labels = T
      ) 
      q1
      ggplotly(q1)
      
    })
    
    # District Run Chart
    output$district_run_chart <- renderPlotly({
      
      df.chart <- df %>% 
        filter(
          Reporting.Category %in% input$reporting_category_d,
          DASS != "Yes",
          District.Name %in% input$district
        ) %>% 
        group_by(Academic.Year) %>% 
        summarize(
          enrollment = sum(ChronicAbsenteeismEligibleCumulativeEnrollment, na.rm=T),
          ca = sum(ChronicAbsenteeismCount, na.rm=T)
        ) 
      
      
      
      q1 <- qicharts2::qic(
        x = factor(Academic.Year),
        y = ca,
        n = enrollment,
        data = df.chart,
        chart = 'pp',
        freeze = 3,
        title = "Chronic Absenteeism Over Time",
        xlab = "School Year",
        x.angle = 45,
        point.size = 1,
        show.labels = T
      ) + geom_line()
      q1
      ggplotly(q1)
    })
   
    # School Selection
    output$school_free <- renderPlotly({
      df <- df %>% filter(
        Academic.Year %in% input$school_year_c,
        Reporting.Category %in% input$reporting_category_c,
        DASS != "Yes"
      )
      
      df.chart <- df %>% 
        filter(
          School.Name %in% input$school_name_c
        )
      
      q1 <- qicharts2::qic(
        x=reorder(
          School.Name, 
          ChronicAbsenteeismEligibleCumulativeEnrollment),
        y = ChronicAbsenteeismCount,
        n= ChronicAbsenteeismEligibleCumulativeEnrollment,
        data = df.chart,
        chart = 'p',
        title = "Chronic Absenteeism by School",
        xlab = "School",
        x.angle = 45,
        point.size = 1,
        show.labels = T
      ) 
      ggplotly(q1)
    })
    
    # School Selection Run Chart
    output$school_run_chart_free <- renderPlotly({
      df <- df %>% filter(
        Reporting.Category %in% input$reporting_category_c,
        DASS != "Yes"
      )
      
      df.chart <- df %>% 
        filter(
          School.Name %in% input$school_name_c
        ) %>%
        group_by(Academic.Year) %>% 
        summarize(
          ca_count = sum(ChronicAbsenteeismCount, na.rm=T),
          ca_enr = sum(ChronicAbsenteeismEligibleCumulativeEnrollment, na.rm=T)
        ) %>% 
        mutate(
          ca_pct = round(100*(ca_count/ca_enr),2)
        )
      
      q1 <- qicharts2::qic(
        x=factor(Academic.Year),
        y = ca_count,
        n = ca_enr,
        data = df.chart,
        chart = 'p',
        title = "Chronic Absenteeism over Time (Combined)",
        xlab = "School Year",
        freeze = 3,
        x.angle = 45,
        point.size = 1,
        show.labels = T
      ) + geom_line()
      q1
      ggplotly(q1)
    })
    
    
   # Reactive Filters (School) ----
    output$school_filter <- renderUI({
      pickerInput(inputId = "school_name_c", 
                  "Select Schools:",
                  choices = var_school_name(), 
                  selected = c("High Tech High International",
                               "High Tech High Media Arts",
                               "High Tech High",
                               "High Tech High Chula Vista",
                               "High Tech High North County"),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                  `live-search` = TRUE))
    })
    
    output$district_filter <- renderUI({
      pickerInput(inputId = "district", 
                  "Select Districts:",
                  choices = var_district_name(), 
                  selected = c("San Diego Unified",
                               "San Francisco Unified",
                               "Los Angeles Unified"),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `live-search` = TRUE))
    })
    
    output$district_filter_s <- renderUI({
      pickerInput(inputId = "district_s", 
                  "Select Districts:",
                  choices = sort(unique(df$District.Name)), 
                  selected = unique(df$District.Name),
                  multiple = T,
                  options = list(`actions-box` = TRUE,
                                 `live-search` = TRUE))
    })
    
    output$ss_filter <- renderUI({
      pickerInput("ss_category_c", "% of SED students: ",
                  choices = sort(unique(df$pct_ss_bucket_actual)),
                  options = list(`actions-box` = TRUE),
                  selected = unique(df$pct_ss_bucket_actual),
                  multiple = TRUE)
    })
    
    output$size_filter <- renderUI({
      pickerInput("size_category_c", "School Size: ",
                  choices = c(
                    "100 - 200", "200 - 400", "400 - 600", "600 - 800",
                    "800 - 1000", "1000 - 1250", "1250 - 1500", "1500 - 1750",
                    "1750 - 2000", "2000 - 2500", "2500 - 3000", "3000+"
                  ),
                  options = list(`actions-box` = TRUE),
                  selected = unique(df$num_ss_bucket),
                  multiple = TRUE)
    })
    
    output$county_filter <- renderUI({
      pickerInput("county_filter_c", "County: ",
                  choices = sort(unique(df$County.Name)),
                  options = list(`actions-box` = TRUE,
                                 `live-search` = TRUE),
                  selected = unique(df$County.Name),
                  multiple = TRUE)
    })
    
    output$eil_filter <- renderUI({
      pickerInput("eil_filter_c", "School Level: ",
                  choices = sort(unique(df$EILName)),
                  options = list(`actions-box` = TRUE),
                  selected = unique(df$EILName),
                  multiple = TRUE)
    })
    
    #district
    output$county_filter_d_f <- renderUI({
      pickerInput("county_filter_d", "County: ",
                  choices = sort(unique(df$County.Name)),
                  options = list(`actions-box` = TRUE,
                                 `live-search` = TRUE),
                  selected = unique(df$County.Name),
                  multiple = TRUE)
    })
    
    output$eil_filter_d_f <- renderUI({
      pickerInput("eil_filter_d", "School Level: ",
                  choices = sort(unique(df$EILName)),
                  options = list(`actions-box` = TRUE),
                  selected = unique(df$EILName),
                  multiple = TRUE)
    })
    
    ss_category <- reactive({
      if (is.null(input$ss_category_c)) unique(df$pct_ss_bucket_actual) else input$ss_category_c
    })
    
    size_category <- reactive({
      if(is.null(input$size_category_c)) unique(df$num_ss_bucket) else input$size_category_c
    })
    
    county_category <- reactive({
      if(is.null(input$county_filter_c)) unique(df$County.Name) else input$county_filter_c
    })
    
    eil_category <- reactive({
      if(is.null(input$eil_filter_c)) unique(df$EILName) else input$eil_filter_c
    })
    
    county_category_d <- reactive({
      if(is.null(input$county_filter_d)) unique(df$County.Name) else input$county_filter_d
    })
    
    district_category <- reactive({
      if(is.null(input$district_s)) unique(df$District.Name) else input$district_s
    })
    
    var_school_name <- reactive({
      df %>% filter(
        pct_ss_bucket_actual %in% ss_category(),
        num_ss_bucket %in% size_category(),
        County.Name %in% county_category(),
        EILName %in% eil_category(),
        District.Name %in% district_category()
      ) %>% pull(School.Name) %>% unique()
    })
    
    var_district_name <- reactive({
      df %>% filter(
        County.Name %in% county_category_d()
      ) %>% pull(District.Name) %>% unique()
    })
    
    
    
#----
}

# Run the application 
shinyApp(ui = ui, server = server)


#tags$div(class="header", checked=NA,
#         tags$p("Instruction on how to use this page: Select a school from the list. Up to 30 additional schools will be selected as a comparison group. You can also use the filters to select the academic year visualized on the top chart, and a student population subset."),
#         tags$p("Comparison schools are schools in the same county as the one you selected, sharing a similar % of SED Students"))





