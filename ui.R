library(shiny)
library(tidyverse)
library(readxl)
library(plotly)
library(scales)
library(shinyWidgets)
library(shinycssloaders)
library(DT)
library(shinydashboard)
library(shinyWidgets)
library(shinycssloaders)
library(shinyFeedback)
library(shinyfullscreen)
library(xfun)

anova_one_way_UI <- function(id) {
  ns <- NS(id)
  tabPanel(
    'One way',
    icon = icon('square-poll-vertical'),
    sidebarLayout(
      sidebarPanel(
        width = 3,
        actionBttn(
          ns('test_data'),
          label = 'Show default example',
          style = 'float',
          block = TRUE,
          color = 'primary',
          size  = 'xs'
        ),
        br(),
        numericInput(
          ns('n_col'),
          'Choose the col',
          width = '110px', value = 2, min = 2, max = 7
        ),
        tabsetPanel(
          id = ns('tabset'),
          type = 'pills',
          tabPanel(
            'Box plot',
            hr(style = 'border-color: #cccccc;'),
            fluidRow(
              column(
                width = 6,
                prettyRadioButtons(
                  ns('id_order_fun'),
                  'Ordered by',
                  choices    = c('Default', 'Mean', 'Median', 'Sd'),
                  status     = 'primary',
                  animation  = 'smooth'
                )
              ),
              column(
                width = 6,
                tabsetPanel(
                  id = ns('tabset_order'),
                  type = 'hidden',
                  tabPanel('empty'),
                  tabPanel(
                    'order',
                    h5(p(strong('Order')), style = "margin-top: 4px;"),
                    div(
                      style = "margin-top: -5px;",
                      prettyCheckbox(
                        ns('id_order'),
                        label     = 'Descending',
                        value     = FALSE,
                        status    = 'primary',
                        animation = 'smooth'
                      )
                    )
                  )
                )
              )
            ),
            textInput(ns('title_name'), label = 'Graphic title'),
            textInput(ns('name_axis_x'), label = 'X axis title'),
            textInput(ns('name_axis_y'), label = 'Y axis title')
          ),
          tabPanel(
            'Statistical analysis',
            hr(style = 'border-color: #cccccc;'),
            prettyCheckboxGroup(
              ns('inferential_test'),
              'Select test',
              choices    = c('Normality', 'Homoscedasticity', 'ANOVA', 'Tukey'),
              status     = 'primary',
              animation  = 'smooth'
            )
          )
        )
      ),
      mainPanel(
        width = 9,
        fluidRow(
          column(
            width = 11,
            actionBttn(
              inputId = ns('analyze'),
              label = 'Analyze',
              style = 'simple',
              size = 'sm',
              color = 'primary'
            ),
            br(), br(), br(),
            tableOutput(ns('empty')),
            map(
              c('Normality', 'Homoscedasticity', 'ANOVA', 'Tukey'),
              ~ tableOutput(ns(.x))
            ),
            plotlyOutput(ns('plot'))
          ),
          column(
            width = 1,
            div(
              style = 'display: flex; justify-content: flex-end;' ,
              actionBttn(
                inputId = ns('info_button_one_way_anova'),
                label   = '',
                icon    = icon('info-circle'),
                style   = 'jelly'
              )
            )
          )
        )
      )
    )
  )
}

fluidPage(
  br(),
  anova_one_way_UI('id')
)
