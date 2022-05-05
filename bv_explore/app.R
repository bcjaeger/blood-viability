#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

setwd("../")

source("packages.R")

for(f in list.files("R/", full.names = TRUE)) source(f)

key_data <- read_csv('bv_explore/key.csv',
                     col_names = TRUE,
                     col_types = cols(variable = col_character(),
                                      type = col_character(),
                                      label = col_character(),
                                      outcome = col_logical(),
                                      exposure = col_logical(),
                                      subset = col_logical(),
                                      group = col_logical()))

key_list <- key_data |>
  table.glue::as_inline(
    tbl_variables = 'variable',
    tbl_values = setdiff(names(key_data), c('variable'))
  )

input_width = '97.5%'


tar_load(names = c(bv_xgb_fits_pax,
                   bv_xgb_fits_pbmc))

x_vars <- bv_xgb_fits_pax$shap_importance$name[1:20]

# UI ----

ui <- shinyUI(

  fluidPage(

    introjsUI(),

    # title ----
    introBox(
      titlePanel("Blood viability"),
      data.step = 1,
      data.intro = "This is an application to explore blood viability in motrpac"
    ),

    # sidebar ----
    sidebarLayout(
      sidebarPanel(

        introBox(
          actionButton("help",
                       "Press for instructions",
                       icon = icon("question"),
                       width = '95%')
        ),

        br(),

        pickerInput(
          inputId = 'variable_x',
          label = 'Select a variable for the x axis',
          choices = x_vars,
          selected =  x_vars[1],
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1),
          width = input_width
        ),

        sliderInput(
          inputId = 'bounds_x',
          label = 'Select x boundaries for the figure',
          min = -10,
          max = 10,
          value = c(-5, 5)
        ),

        pickerInput(
          inputId = 'variable_color',
          label = 'Select a color variable',
          choices = c('None'),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1),
          width = input_width
        ),

        pickerInput(
          inputId = 'variable_facet',
          label = 'Select facet variable(s)',
          choices = c('None'),
          selected = NULL,
          multiple = TRUE,
          options = pickerOptions(maxOptions = 1),
          width = input_width
        ),

        actionButton('do_compute',
                     label = 'Compute')


      ),

      mainPanel(

        tabsetPanel(

          tabPanel(
            "PAX SHAP",
            plotOutput('fig_pax_shap',
                       brush = "pax_shap_brush"),
            DTOutput('pax_shap_data')
          ),

          tabPanel(
            "PAX descriptive",
            # plotOutput('fig_pax_desc')
          )


        )

      )
    )
  )
)

# Server ----

is_used <- function(x){
  if(is.null(x)) return(FALSE)
  !(is_empty(x) |x == 'None')
}

server = function(input, output, session) {

  observeEvent(input$variable_x, {

    variable_color_inputs <- key_data |>
      filter(exposure,
             variable %in% x_vars,
             variable != input$variable_x) |>
      select(label, variable) |>
      deframe()

    updatePickerInput(
      session = session,
      inputId = 'variable_color',
      choices = variable_color_inputs,
      selected = character()
    )

    # browser()


    if(key_list[[input$variable_x]]$type == 'ctns'){

      updateSliderInput(
        session = session,
        inputId = 'bounds_x',
        min = min(bv_xgb_fits_pax$shap_data[[input$variable_x]],
                  na.rm = TRUE),
        max = max(bv_xgb_fits_pax$shap_data[[input$variable_x]],
                  na.rm = TRUE),
        value = quantile(bv_xgb_fits_pax$shap_data[[input$variable_x]],
                         probs = c(.25, .75),
                         na.rm = TRUE)
      )


    }


  })

  output$fig_pax_shap <- renderPlot({

    fig_base <- ggplot(data = bv_xgb_fits_pax$shap_data)

    fig_geom <- ifelse(key_list[[input$variable_x]]$type == 'ctns',
                       'point',
                       'boxplot')

    fig_mapped <- fig_base +
      aes_string(x = input$variable_x,
                 y = paste(input$variable_x, 'contrib', sep = '_'))

    if(is_used(input$variable_color)){

      if(fig_geom %in% c('point')){
        fig_mapped <- fig_base +
          aes_string(x = input$variable_x,
                     y = paste(input$variable_x, 'contrib', sep = '_'),
                     col = input$variable_color)
      }

      if(fig_geom %in% c("boxplot")){
        fig_mapped <- fig_base +
          aes_string(x = input$variable_x,
                     y = paste(input$variable_x, 'contrib', sep = '_'),
                     fill = input$variable_color)
      }

    }

    pd <- ifelse(is_used(input$variable_color),
                 position_jitterdodge,
                 position_identity)


    out <- switch(
      fig_geom,
      'point' = fig_mapped +
        geom_point(position = pd()) +
        scale_x_continuous(limits = c(input$bounds_x)),
      'boxplot' = fig_mapped + geom_boxplot()
    )

    out

  }) |>
    bindEvent(input$do_compute)

  output$pax_shap_data <- renderDataTable({

    bv_xgb_fits_pax$shap_data |>
      mutate(outcome = bv_xgb_fits_pax$y, .before = 1) |>
      brushedPoints(input$pax_shap_brush) |>
      select(outcome,
             any_of(x_vars),
             any_of(paste(x_vars, 'contrib', sep = '_'))) |>
      datatable(
        options = list(
          lengthMenu = list(c(5,15,20),c('5','15','20')),
          pageLength = 10,
          columnDefs=list(list(className='dt-center',targets="_all"))
        ),
        filter = "top",
        selection = 'multiple',
        style = 'bootstrap',
        class = 'cell-border stripe',
        rownames = FALSE
      )

  })

  # instructions ----
  # when user asks for instructions, this triggers
  observeEvent(
    input$help, {

      if(is.null(input$dataset)){
        updatePickerInput(
          session = session,
          inputId = 'dataset',
          selected = 'racs'
        )
      }

      introjs(session,
              options = list("nextLabel"="Next",
                             "prevLabel"="Previous",
                             "skipLabel"="Skip"))

    }
  )

}

shinyApp(ui = ui, server = server)

