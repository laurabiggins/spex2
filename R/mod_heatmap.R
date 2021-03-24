#!! This is not generic - it's been tweaked to look reasonable with the FemExpression 
# data in the selected_data() reactive expression

#' histogram UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
# mod_histogram_ui <- function(id){
#   ns <- NS(id)
#   tagList(
#  
#   )
# }
mod_heatmap_ui <- function(id, meta_sum){
  
  ns <- NS(id)
  
  tagList(
    wellPanel(
      id = ns("panel"),
      fluidRow(
        column(2, downloadButton(ns("download_png"), "png")),
        column(2, downloadButton(ns("download_pdf"), "pdf")),
        column(3, offset = 5,numericInput(ns("plot_height"), "plot height", 500))
        #numericInput(ns("plot_width"), "plot width", 700),
      ),
      shinycssloaders::withSpinner(
        plotOutput(ns("plot"), width = "100%", height = "500"), 
        image = "bioinf1.gif", 
        image.width = 100
      )
    ),
    br(),

    br(),
    br(),
    #actionButton(inputId = ns("browser"), "browser"), 
    
    #if we want the downloaded plot to be the window size
    # for height we make the user adjust it, as it won't resize with the window
    tags$script(
      "var myWidth = 0;
      $(document).on('shiny:connected', function(event) {
        myWidth = $(window).width();
        Shiny.onInputChange('heatmap-shiny_width', myWidth);
      });
      $(window).resize(function(event) {
         myWidth = $(window).width();
         Shiny.onInputChange('heatmap-shiny_width', myWidth);
      });"
    )
  )
}


#' histogram Server Function
#'
#' @noRd 
mod_heatmap_server <- function(id, dataset, meta_sum, metadata, of_interest,
                               sample_name_col, prefix = "", session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      heatmap_options <- reactiveValues(
        annot_col = tibble::column_to_rownames(metadata, "sample_name")#,
        #annot_row = tibble::column_to_rownames(of_interest[[1]], "gene")
      )
      
      # row_annotations <- reactive({
      #   tib <- dplyr::filter(
      #     of_interest[[1]], 
      #     gene %in% rownames(selected_data())
      #   )
      #   tibble::column_to_rownames(tib, "gene")
      # })
      
      # this has to be in a reactive expression because the dataset 
      # passed to mod_heatmap_server is a reactive expression
      selected_data <- reactive({
        
        genes_of_interest <- dplyr::pull(of_interest[[1]], gene)
        filtered_meta <- dplyr::filter(metadata, class %in% c("AA", "DA", "OEA"))
        selected_samples <- dplyr::pull(filtered_meta, sample_name_col)
        # we're working with a matrix so can't do dplyr
        matrix_columns <- colnames(dataset) %in% selected_samples
        dataset[rownames(dataset) %in% genes_of_interest, matrix_columns]
      })
      
      heatmap_obj <- reactive({
        pheatmap::pheatmap(
          selected_data(),
          scale = "row",
          annotation_col = heatmap_options$annot_col,
          #annotation_row = row_annotations(),
          silent = TRUE
        )
      })
      
      output$plot <- renderPlot({
        
        req(input$plot_height)
        req(!is.na(input$plot_height))

        plot(heatmap_obj()$gtable)
      }, 
      height = function(x) input$plot_height
      )
        
      output$download_png <- downloadHandler(
        filename = function() {
          paste0("heatmap.png")
        },
        content = function(file) {
          ggplot2::ggsave(
            file, 
            heatmap_obj()$gtable, 
            device = "png",
            width = input$shiny_width/4, ## 1pixel ~ 0.26mm at 96 dpi. it's ~0.35 at 72dpi
            #height = input$shiny_height/4,
            #width = input$plot_width*0.35,
            height = input$plot_height*0.35,  
            units = "mm"
          )
        }
      )
       
      output$download_pdf <- downloadHandler(
        filename = function() {
          paste0("heatmap.pdf")
        },
        content = function(file) {
          ggplot2::ggsave(
            file, 
            heatmap_obj(), 
            device = "pdf",
            width = input$shiny_width/4,
            #width = input$plot_width*0.35,
            height = input$plot_height*0.35,  
            units = "mm"
          )
        }
      )    
      observeEvent(input$browser, browser())
    }
  )
}

# option for scaling 
# if nrow > 100? just plot first 100/500 rows, and write an error message

# annot_col1 <- metadata %>%
#   select(sample_name, type) %>%
#   tibble::column_to_rownames("sample_name")

# cluster by correlation
# clustering_distance_rows = "correlation"
