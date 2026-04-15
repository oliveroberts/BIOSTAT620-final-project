library(ipumsr)
library(dplyr)
library(haven)
library(survey)
library(srvyr)
library(tidyverse)
library(ggplot2)
library(tidyr)
library(knitr)
library(kableExtra)
library(plotly)
library(shiny)

NHIS <- readRDS("NHIS.rds")

sex_choices  <- sort(unique(na.omit(NHIS$sexcat)))
age_choices  <- sort(unique(na.omit(NHIS$agecat)))
race_choices <- sort(unique(na.omit(NHIS$race_eth)))

svy_design <- svydesign(
  id = ~PSU,
  strata = ~STRATA,
  weight = ~SAMPWEIGHT,
  data = NHIS,
  nest = TRUE)

product_labels <- c(
  smk_current = "Current Cigarette Use",
  smk_ever = "Ever Cigarette Use",
  ecig_current = "Current E-Cigarette Use",
  ecig_ever = "Ever E-Cigarette Use",
  cigar_current = "Current Cigar Use",
  cigar_ever = "Ever Cigar Use",
  smkless_current = "Current Smokeless Tobacco Use",
  smkless_ever = "Ever Smokeless Tobacco Use",
  pipe_current = "Current Pipe Use",
  pipe_ever = "Ever Pipe Use")

make_label <- function(product, subgroup) {
  paste0(product, ", ", subgroup)}

clean_text <- function(x) {
  x <- as.character(x)
  x <- gsub("\\(|\\)", "", x)
  x <- gsub(",\\s*1$", "", x)
  trimws(x)}

ui <- navbarPage(
  "NHIS Tobacco Dashboard",
  tabPanel("Subgroup Trends",
           sidebarLayout(
             sidebarPanel(
               selectInput("product1",
                           "Tobacco/Nicotine Product Use",
                           choices = setNames(names(product_labels), product_labels),
                           multiple = TRUE),
               selectInput("group",
                           "Stratify by",
                           choices = c(
                             "Race" = "race_eth",
                             "Sex" = "sexcat",
                             "Age" = "agecat")),
               uiOutput("subgroups_ui")),
             mainPanel(
               plotlyOutput("plot1", height = "600px")))),
  tabPanel("Group Comparison",
           sidebarLayout(
             sidebarPanel(
               selectInput("product2",
                           "Tobacco/Nicotine Product Use",
                           choices = setNames(names(product_labels), product_labels),
                           multiple = TRUE),
               h4("Group A"),
               selectInput("race_A", "Race",
                           race_choices,
                           multiple = TRUE),
               selectInput("sex_A", "Sex",
                           sex_choices,
                           multiple = TRUE),
               selectInput("age_A", "Age",
                           age_choices,
                           multiple = TRUE),
               hr(),
               h4("Group B"),
               selectInput("race_B", "Race",
                           race_choices,
                           multiple = TRUE),
               selectInput("sex_B", "Sex",
                           sex_choices,
                           multiple = TRUE),
               selectInput("age_B", "Age",
                           age_choices,
                           multiple = TRUE)),
             mainPanel(
               plotlyOutput("plot2", height = "600px")))))

server <- function(input, output) {
  output$subgroups_ui <- renderUI({
    req(input$group)
    choices <- switch(input$group,
                      race_eth = race_choices,
                      sexcat = sex_choices,
                      agecat = age_choices)
    selectInput("subgroups",
                "Select Groups",
                choices = choices,
                multiple = TRUE,
                selected = choices)})
  
  data1 <- reactive({
    req(input$product1, input$subgroups)
    group_var <- input$group
    design_sub <- subset(
      svy_design,
      get(group_var) %in% input$subgroups)
    bind_rows(lapply(input$product1, function(var) {
      df <- svyby(
        as.formula(paste0("~", var)),
        as.formula(paste0("~YEAR + ", group_var)),
        design_sub,
        svymean,
        vartype = "ci",
        na.rm = TRUE)
      
      df$YEAR <- as.numeric(as.character(df$YEAR))
      subgroup <- clean_text(as.character(df[[group_var]]))
      df <- df %>%
        mutate(
          Prevalence = .data[[var]] * 100,
          CI_Lower = ci_l * 100,
          CI_Upper = ci_u * 100,
          
          Product = product_labels[[var]],
          Subgroup = subgroup,
          
          Label = make_label(Product, Subgroup)) %>%
        mutate(Label = clean_text(Label))
      df}))
  })
  
  output$plot1 <- renderPlotly({
    df <- data1()
    ggplotly(
      ggplot(df,
             aes(x = YEAR, y = Prevalence,
                 color = Label, group = Label)) +
        
        geom_line(linewidth = 1) +
        geom_point() +
        
        geom_ribbon(aes(ymin = CI_Lower,
                        ymax = CI_Upper,
                        fill = Label),
                    alpha = 0.15,
                    color = NA,
                    show.legend = FALSE) +
        labs(x = "Year", y = "Prevalence %") +
        theme_minimal(base_size = 14))})
  
  data2 <- reactive({
    req(input$product2)
    compute_group <- function(race, sex, age) {
      design_sub <- subset(
        svy_design,
        race_eth %in% race &
          sexcat %in% sex &
          agecat %in% age)
      bind_rows(lapply(input$product2, function(var) {
        df <- svyby(
          as.formula(paste0("~", var)),
          ~YEAR,
          design_sub,
          svymean,
          vartype = "ci",
          na.rm = TRUE)
        df$YEAR <- as.numeric(as.character(df$YEAR))
        subgroup <- paste(race, sex, age, collapse = " ")
        subgroup <- clean_text(subgroup)
        
        df <- df %>%
          mutate(
            Prevalence = .data[[var]] * 100,
            CI_Lower = ci_l * 100,
            CI_Upper = ci_u * 100,
            
            Product = product_labels[[var]],
            Label = make_label(Product, subgroup)) %>%
          mutate(Label = clean_text(Label))
        df}))}
    bind_rows(
      compute_group(input$race_A, input$sex_A, input$age_A),
      compute_group(input$race_B, input$sex_B, input$age_B))
  })
  
  output$plot2 <- renderPlotly({
    
    df <- data2()
    ggplotly(
      ggplot(df,
             aes(x = YEAR, y = Prevalence,
                 color = Label, group = Label)) +
        
        geom_line(linewidth = 1) +
        geom_point() +
        
        geom_ribbon(aes(ymin = CI_Lower,
                        ymax = CI_Upper,
                        fill = Label),
                    alpha = 0.15,
                    color = NA,
                    show.legend = FALSE) +
        
        labs(x = "Year", y = "Prevalence %") +
        theme_minimal(base_size = 14))})
}

shinyApp(ui, server)
