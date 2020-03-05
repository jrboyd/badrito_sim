#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(ggplot2)
source("functions.R")
library(cowplot)
library(magick)
library(BiocFileCache)
library(shinycssloaders)

bfcif = ssvRecipes::bfcif

main_suits = c("H", "D", "S", "C")
all_suits = c(main_suits, setdiff(LETTERS, main_suits))

input = list()
input$selCardRange = c(2, 6)
input$num1 = 4
input$num2 = 4
input$num3 = 4
input$num4 = 3
input$num5 = 2
input$num6 = 1
input$selSuits = main_suits
input$numPlayerCount = 4
input$selHandSize = c(5, 8)
input$numSims = "1,000"
num_sims = as.numeric(gsub(",", "", input$numSims))
max_hand = max(input$selHandSize)
hand_range = input$selHandSize
num_players = input$numPlayerCount
deck_dt = generate_deck(get_value_weights(input$selCardRange, input), input$selSuits)
master_seed = 0

# Define UI for application that draws a histogram
ui <- fluidPage(
    tags$style(HTML("
        input[type=number] {
              -moz-appearance:textfield;
        }
        input[type=number]::{
              -moz-appearance:textfield;
        }
        input[type=number]::-webkit-outer-spin-button,
        input[type=number]::-webkit-inner-spin-button {
              -webkit-appearance: none;
              margin: 0;
        }
    ")),
    # Application title
    titlePanel("BadRito simulation"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("numSeed", "Seed", value = 0),
            numericInput("numPlayerCount", "Player Count", value = 4),
            selectInput(inputId = "selSuits", label = "Card Suits", choices = all_suits, selected = main_suits, multiple = TRUE),
            sliderInput("selHandSize",
                        "Hand Size Start and Final",
                        min = 1,
                        max = 10,
                        value = c(5, 8)),
            sliderInput("selCardRange", "Card Value Range", min = 1, max = 15, value = c(2, 6)),
            selectInput("numSims", "Simulations", choices = c("100", "1,000", "10,000", "100,000", "1,000,000"), selected = "100", multiple = FALSE),
            uiOutput("copiesPerCard")
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            htmlOutput("description"),
            shinycssloaders::withSpinner(plotOutput("fraction_plot", width = "300px", height = "240px")),
            fluidRow(column(width = 4, htmlOutput("exGood")),
                     column(width = 4, htmlOutput("exBad")))
            
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$description <- renderUI({
        req(rdeck_dt())
        req(input$selSuits)
        req(input$selCardRange)
        list(
            tags$span(paste(length(input$selSuits) * length(range2seq(input$selCardRange)), "unique cards in deck")),
            tags$br(),
            tags$span(paste("Deck of", nrow(rdeck_dt())))
        )
    })
    output$copiesPerCard = renderUI({
        numins = lapply(range2seq(input$selCardRange), make_num_input, input = input)
        c(list(
            fluidRow(column(width = 2, tags$h3("Card")), column(width = 8, tags$h3("Count"))), 
            tags$hr(),
            numins
        ))
    })
    
    rvalue_weights = reactiveVal()
    rdeck_dt = reactiveVal()
    observe({
        rfraction(NULL)
        rlegal(NULL)
        req(input$selCardRange)
        req(input$selSuits)
        val_w = get_value_weights(input$selCardRange, input)
        rvalue_weights(val_w)
        deck_dt = generate_deck(val_w, input$selSuits)
        showNotification(as.character(nrow(deck_dt)))
        rdeck_dt(deck_dt)
    })
    
    rsim_hands = reactiveVal()
    observe({
        req(input$selCardRange)
        req(input$selSuits)
        req(rdeck_dt())
        req(input$numSims)
        req(input$selHandSize)
        req(input$numPlayerCount)
        req(input$numSeed)
        num_sims = as.numeric(gsub(",", "", input$numSims))
        all_hands_dt = sim_make_hands(rdeck_dt(), num_sims, input$selHandSize, input$numPlayerCount, master_seed = input$numSeed)
        rsim_hands(all_hands_dt)
    })
    
    rlegal = reactiveVal()
    observe({
        req(rsim_hands())
        req(input$selHandSize)
        legal_dt = sim_badrito_test_legal(rsim_hands(), input$selHandSize)
        rlegal(legal_dt)
    })
    
    rfraction = reactiveVal()
    observe({
        req(rlegal())
        rfraction(sim_badrito_report_fraction(rlegal()))
    })
    
    output$fraction_plot = renderPlot({
        req(rfraction())
        frac_dt = rfraction()
        p = ggplot(frac_dt, aes(x = hand_size, y = fraction_legal)) + 
            geom_path() + geom_point() +
            labs(x = "Hand size", y = "Legal fraction of hands", title = "BadRito legal hands")
        ggdraw() +
        draw_image("Coop-memegenerator.net_.jpg", width = .8, x = .1, y = .1, height = .8) +
        draw_plot(p + theme(panel.background = element_blank(), plot.background = element_rect(fill = "#FFFFFFF4")))
        # p
    })
    
    output$exGood = renderUI({
        req(rsim_hands())
        req(rlegal())
        set.seed(input$numSeed)
        hands_list = rsim_hands()
        legal_dt = rlegal()
        sel_legal = legal_dt[legal == TRUE][sample(.N, min(10, .N)),]
        sel_hands = rbindlist(hands_list[sel_legal$hand_id], idcol = "hand_id")
        sel_hands = merge(sel_hands, sel_legal, by = c("hand_id", "player"))
        sel_hands = sel_hands[order(hand_order)]
        sel_hands = sel_hands[hand_order <= hand_size]
        sel_hands = sel_hands[order(value)][order(suit)]
        hand_txt =sel_hands[, paste(paste0(value, suit), collapse = ", "), .(hand_id, player, hand_size)]$V1
        c(list(tags$h2("Legal hands", style="color:green;")), lapply(hand_txt, function(x)list(tags$span(x), tags$br())))
    })
    output$exBad = renderUI({
        req(rsim_hands())
        req(rlegal())
        set.seed(input$numSeed)
        hands_list = rsim_hands()
        legal_dt = rlegal()
        sel_legal = legal_dt[legal == FALSE][sample(.N, min(10, .N)),]
        sel_hands = rbindlist(hands_list[sel_legal$hand_id], idcol = "hand_id")
        sel_hands = merge(sel_hands, sel_legal, by = c("hand_id", "player"))
        sel_hands = sel_hands[order(hand_order)]
        sel_hands = sel_hands[hand_order <= hand_size]
        sel_hands = sel_hands[order(value)][order(suit)]
        hand_txt =sel_hands[, paste(paste0(value, suit), collapse = ", "), .(hand_id, player, hand_size)]$V1
        c(list(tags$h2("Illegal hands", style="color:red;")),lapply(hand_txt, function(x)list(tags$span(x), tags$br())))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
