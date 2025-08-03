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
          id = ns('tabset'),type = 'pills',

          tabPanel(
            'Box plot',
            br(),
            prettyRadioButtons(
              ns('id_order_fun'),
              'Ordered by',
              choices    = c('Default', 'Mean', 'Median', 'Sd'),
              status     = 'primary',
              icon       = icon('check'),
              animation  = 'smooth',
              inline     = TRUE
            ),
            radioGroupButtons(
              ns('id_order'),
              'Order',
              selected = character(0),
              choices = c(
                `<i class='fa fa-sort-up'></i> Ascending ` = 'Ascending',
                `<i class='fa fa-sort-down'></i> Descending ` = 'Descending'
              ),
              justified = TRUE,
            ),
            br(),
            textInput(
              ns('title_name'), label = 'Graphic title'
            ),
            textInput(
              ns('name_axis_y'), label = 'Y axis title'
            )
          ),
          tabPanel('Statistical analysis',
                   br(),
                   radioButtons(
                     ns('selection'), 'Select test',
                     choices = c('Normality', 'Homoscedasticity', 'ANOVA', 'all'),
                     inline = T
                   )
          )
        )
      ),
      mainPanel(
        actionBttn(
          inputId = ns('go'),
          label = 'Analyze',
          style = 'simple',
          size = 'sm',
          color = 'primary'
        ),
        br(),

        # actionBttn(
        #   inputId = ns('info_button_one_way_anova'),
        #   label   = '',
        #   icon    = icon('info-circle'),
        #   style   = 'jelly'
        # )


        verbatimTextOutput(ns('t_anova')),
        plotlyOutput(ns('plot'))
      )
    )
  )
}


fluidPage(
  br(),
  anova_one_way_UI('id')
)
