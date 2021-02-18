#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(showtext)
library(plotly)
library(readxl)
library(httr)

theme_set(theme_minimal())

# Load ggplot-friendly font using show_text
font_add("bebas", "www/fonts/BebasNeue-Regular.ttf")
font_add("cardenio", "www/fonts/CardenioModern-Bold.otf")
showtext_auto()

label_ggplotly <- list(
    font = list(
        family = "cardenio",
        size = 15
    )
)

modeBarButtonsToRemove <- c("zoomIn2d", "zoomOut2d", "zoom2d", "pan2d", 
                            "select2d", "lasso2d", "autoScale2d",
                            "hoverClosestCartesian",
                            "hoverCompareCartesian", "toggleSpikelines")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    # Define file locations
    googledrive_url <- "https://docs.google.com/spreadsheets/d/1Ryei71k-te0LNubhoaB_HcQ1mWMH86r-bEghjvOmoNk/export"
    
    # Download excel spreadsheet from URL and read as DF
    GET(googledrive_url, 
        write_disk(tf <- tempfile(fileext = ".xlsx")))
    
    data_df <- read_excel(tf) %>%
        select(-`Date?`) %>%
        mutate(game_id = row_number()) %>%
        pivot_longer(-game_id, names_to="player", values_to="scores", values_drop_na=T) %>%
        extract(scores, into=c("Birds", "Bonus Cards", "End-of-Round Goals", "Eggs", "Food on Cards", "Tucked Cards"),
                regex="(\\d{1,2})B,(\\d{1,2})C,(\\d{1,2})R,(\\d{1,2})E,(\\d{1,2})F,(\\d{1,2})T", remove=T) %>%
        pivot_longer(c(-game_id, -player), names_to="type", values_to="score") %>%
        mutate(score = as.numeric(score))
    
    
    output$box_whisker <- renderPlotly({
        g <- data_df %>%
            rename(Score=score) %>%
        ggplot(aes(y=Score, x=type)) +
            geom_boxplot() +
            coord_flip() +
            labs(x="") +
            theme(text=element_text(family="cardenio", size=15),
                  title=element_text(size=18))
        
        ggplotly(g) %>%
            style(hoverlabel = label_ggplotly) %>% 
            plotly::config(modeBarButtonsToRemove = modeBarButtonsToRemove)
    })
    
    output$all_points_time <- renderPlotly({
        g <- data_df %>%
            group_by(game_id, player) %>%
            summarize(game_score = sum(score)) %>%
            rename(`Game #`=game_id, Score=game_score, Player=player) %>%
            group_by(Player) %>%
            summarize(n_games = n(), `Game #`, Score) %>%
            ungroup() %>%
            arrange(desc(n_games)) %>%
            mutate(Player = factor(Player, unique(Player))) %>%
        ggplot(aes(x=`Game #`, y=Score, color=Player, linetype=Player, shape=Player)) +
            geom_line(size=1, alpha=.8) +
            geom_point(size=2.5, alpha=.8) +
            theme(text=element_text(family="cardenio", size=15),
                  title=element_text(size=18)) +
            scale_color_manual(values=rep(c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE',
                                            '#882255', '#44AA99', '#999933', '#AA4499'), 2)) +
            scale_linetype_manual(values=c(rep("solid", 9), rep("11", 9))) + 
            scale_shape_manual(values=c(rep(16, 9), rep(15, 9)))
        
        ggplotly(g, tooltip=c("x", "y", "shape"),
                 dynamicTicks = T) %>%
            style(hoverlabel = label_ggplotly) %>% 
            layout(legend = list(orientation = 'h', x = .5, y = -.2, 
                                 yanchor="top", xanchor="center"),
                   xaxis = list(title = list(standoff=10))) %>%
            plotly::config(modeBarButtonsToRemove = modeBarButtonsToRemove)
    })
    
    output$rank_time <- renderPlotly({
        g <- data_df %>%
            group_by(game_id, player) %>%
            summarize(game_score = sum(score)) %>%
            group_by(game_id) %>%
            summarize(rank = min_rank(desc(game_score)), player, game_score) %>%
            rename(Rank=rank, Score=game_score, Player=player, `Game #`=game_id) %>%
            group_by(Player) %>%
            summarize(n_games = n(), Rank, Score, `Game #`) %>%
            ungroup() %>%
            arrange(desc(n_games)) %>%
            mutate(Player = factor(Player, unique(Player))) %>%
        ggplot(aes(x=`Game #`, y=Rank, color=Player, linetype=Player, shape=Player)) +
            geom_line(size=1, alpha=.8) +
            geom_point(size=2.5, alpha=.8) +
            theme(text=element_text(family="cardenio", size=15),
                  title=element_text(size=18)) +
            scale_color_manual(values=rep(c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE',
                                            '#882255', '#44AA99', '#999933', '#AA4499'), 2)) +
            scale_linetype_manual(values=c(rep("solid", 9), rep("11", 9))) + 
            scale_shape_manual(values=c(rep(16, 9), rep(15, 9)))
        
        ggplotly(g, tooltip=c("x", "y", "shape"),
                 dynamicTicks = T) %>%
            style(hoverlabel = label_ggplotly) %>% 
            layout(legend = list(orientation = 'h', x = .5, y = -.2, 
                                 yanchor="top", xanchor="center"),
                   xaxis = list(title = list(standoff=10)),
                   yaxis = list(autorange = "reversed")) %>%
            plotly::config(modeBarButtonsToRemove = modeBarButtonsToRemove)
    })
    
    output$point_pct_time <- renderPlotly({
        g <- data_df %>%
            group_by(game_id, player) %>%
            summarize(game_score = sum(score), type, score) %>%
            group_by(game_id) %>%
            summarize(rank = dense_rank(desc(game_score)), game_score, score, type) %>%
            filter(rank == 1) %>%
            mutate(percent_score = score / game_score * 100) %>%
            rename(`% of Score`=percent_score, Category=type, `Game #`=game_id) %>%
        ggplot(aes(x=`Game #`, y=`% of Score`, fill=Category)) +
            geom_col(position=position_stack()) +
            theme(text=element_text(family="cardenio", size=15),
                  title=element_text(size=18)) +
            scale_fill_manual(values=rep(c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE',
                                            '#882255', '#44AA99', '#999933', '#AA4499'), 2))
            
        ggplotly(g, tooltip=c("x", "y", "fill"),
                 dynamicTicks = T) %>%
            style(hoverlabel = label_ggplotly) %>% 
            layout(legend = list(orientation = 'h', x = .5, y = -.2, 
                                 yanchor="top", xanchor="center"),
                   xaxis = list(title = list(standoff=10))) %>%
            plotly::config(modeBarButtonsToRemove = modeBarButtonsToRemove)
    })
    
    output$point_pct_freq_players <- renderPlotly({
        g <- data_df %>%
            group_by(player) %>%
            filter(length(unique(game_id)) > 1) %>%
            group_by(game_id, player) %>%
            summarize(game_score = sum(score), type, score) %>%
            group_by(player, type) %>%
            summarize(type_score = sum(score)) %>%
            group_by(player) %>%
            mutate(tot_game_score = sum(type_score),
                   percent_type = type_score / tot_game_score) %>%
            rename(`% of Average Score`=percent_type, Category=type, Player=player) %>%
        ggplot(aes(x=Player, y=`% of Average Score`, fill=Category)) +
            geom_col(position = position_stack()) +
            theme(text=element_text(family="cardenio", size=15),
                  title=element_text(size=18)) +
            scale_fill_manual(values=rep(c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE',
                                           '#882255', '#44AA99', '#999933', '#AA4499'), 2))
        
        ggplotly(g, tooltip=c("x", "y", "fill"),
                 dynamicTicks = T) %>%
            style(hoverlabel = label_ggplotly) %>% 
            layout(legend = list(orientation = 'h', x = .5, y = -.2, 
                                 yanchor="top", xanchor="center"),
                   xaxis = list(title = list(standoff=10))) %>%
            plotly::config(modeBarButtonsToRemove = modeBarButtonsToRemove)
        
    })
    
    output$player_comp <- renderPlotly({
        g <- data_df %>%
            filter(player==input$player_comp_select) %>%
            group_by(type) %>%
            mutate(game_id = row_number()) %>%
            rename(Score=score, Category=type, `Game #`=game_id) %>%
        ggplot(aes(x=`Game #`, y=Score, fill=Category)) +
            geom_col(position = position_dodge()) +
            scale_x_continuous(labels=scales::number_format(accuracy=1)) +
            theme(text=element_text(family="cardenio", size=15),
                  title=element_text(size=18)) +
            scale_fill_manual(values=rep(c('#CC6677', '#332288', '#DDCC77', '#117733', '#88CCEE',
                                           '#882255', '#44AA99', '#999933', '#AA4499'), 2))
        
        ggplotly(g, tooltip=c("x", "y", "fill"),
                 dynamicTicks = T) %>%
            style(hoverlabel = label_ggplotly) %>% 
            layout(legend = list(orientation = 'h', x = .5, y = -.2, 
                                 yanchor="top", xanchor="center"),
                   xaxis = list(title = list(standoff=10))) %>%
            plotly::config(modeBarButtonsToRemove = modeBarButtonsToRemove)
    })

})
