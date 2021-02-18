
library(shiny)

shinyUI(fluidPage(theme = "wingspan_with_lauren.css",
                  title="Wingspan with Lauren",
    h1("Wingspan"),
    h2("with Lauren"),

    tabsetPanel(type="pills",
        tabPanel("Overall", 
                 h3("Points over Time"),
                 plotlyOutput("all_points_time"),
                 h3("Ranking over Time"),
                 plotlyOutput("rank_time"),
                 h3("Point Percentage per Game"),
                 plotlyOutput("point_pct_time"),
                 h3("Distribution of Points"),
                 plotlyOutput("box_whisker"),
                 h3("Average Point Percentage per Player"),
                 plotlyOutput("point_pct_freq_players")),
        
        tabPanel("By Player", 
                 selectInput("player_comp_select",
                             "Choose a player:",
                             c("Lauren", "Kira", "Julia")),
                 h3("Point Categories over Time"),
                 plotlyOutput("player_comp"))
    ),
    
    p("Fonts:",
    a("Cardenio Modern Bold", href="https://www.dafont.com/cardenio-modern.font"),
    ",",
    a("Bebas Neue Regular", href="https://fonts.google.com/specimen/Bebas+Neue?preview.text_type=custom")
    )
))
