library(shiny)
library(shinycssloaders)
library(shinythemes)
library(tidyverse)
library(readr)
library(rsconnect)

options(spinner.size=0.3)
options(spinner.type = 4)
options(spinner.color = "#DCA137")
c25 <- c(
    "dodgerblue2", "#E31A1C",# red
    "green4",
    "#6A3D9A", # purple
    "#FF7F00", # orange
    "black", "gold1",
    "skyblue2", "#FB9A99", # lt pink
    "palegreen2",
    "#CAB2D6", # lt purple
    "#FDBF6F", # lt orange
    "gray70", "khaki2",
    "maroon", "orchid1", "deeppink1", "blue1", "steelblue4",
    "darkturquoise", "green1", "yellow4", "yellow3",
    "darkorange4", "brown",
    "#FDF7E3", "#DEA0FD", '#B8DEE6'
)
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

data <- read_csv("self_others.csv") %>% 
    mutate(value = factor(value),
           self_or_others = case_when(self_or_others == 1 ~ "Others",
                                      self_or_others == 2 ~ "Self")) %>%
    filter(value != "Did not specify" & value != "NaN")
demo <- read_csv("demographics.csv") %>%
    group_by(question) %>%
    rowid_to_column("q_id") %>%
    filter(value != "Did not specify") %>%
    mutate(per=round(100*n/sum(n),2))
age_income <- read_csv("age_income_zip.csv") 
long_survey <- read_csv("long_survey_hm.csv")

ui <- fluidPage(
    tags$head(tags$style("
                         iframe {
                           width: 100%;
                         }
                         .aspect-ratio-map {
                           position: relative;
                           overflow: hidden;
                           width: 100%;
                           padding-top: 56.25%;
                         }
                         .aspect-ratio-map iframe {
                           position: absolute;
                           width: 100%;
                           height: 100%;
                           left: 0;
                           top: 0;
                           border: 0;
                         }
                         .aspect-ratio-corr {
                          position: relative;
                          padding-top: 56.25%;
                          height: 70vmax;
                         }
                         .aspect-ratio-corr iframe {
                           overflow: hidden;
                           position: absolute;
                           width: 100%;
                           height: 100%;
                           left: 0;
                           top: 0;
                           border: 0;
                         }
                         .corr-row {
                          padding: 1% 0;
                          margin: 1% 0;
                         }
                         .map-pad {
                          margin: 1% 0 1%;
                         }
                         .blue-map-pad {
                          margin: 1% 0 1%;
                         }
                         ")),
    theme = shinytheme("spacelab"),
    h1(tags$a(href = "https://sites.google.com/view/life-after-covid19/", "Life After: A Survey of Beliefs About the World After COVID-19"), align = 'center'),
    h4("A", tags$a(href = "http://www.combine.umd.edu/network-epidemiology/", "#NET-COVID"), "project by: Philip Nguyen, Rory Gilchrist, José R. Nicolás-Carlock, Samantha Sherrill", align = 'center'),
    br(),
    wellPanel(style = "padding: 1% 0 1%;",
              fluidRow(width = 12,
                       column(tags$div(class = "aspect-ratio-map", class = "blue-map-pad", htmlOutput("map")), align = 'center', width = 6, offset = 3)),
              br(),
              fluidRow(class = "map-pad",
                       width = 12,
                       column(imageOutput("us_map", width = '100%', height = '100%') %>% withSpinner(), align = 'center', width = 6, offset = 3)),
    ),
    fluidRow(width = 12,
             h2(align = 'center', tags$span(style = 'color:#3399F3', "Demographics")),
             br()),
    br(),
    fluidRow(width = 12,
             column(h4("What is the highest level of education you have completed?", align = 'center'),
                    plotOutput('education') %>% withSpinner(),
                    width = 5,
                    offset = 1),
             column(h4("What is your current employment status?", align = 'center'),
                    plotOutput('employment') %>% withSpinner(),
                    width = 5,
                    offset = -1)
             ),
    br(),
    fluidRow(width = 12,
             column(h4("In what country do you currently live?", align = 'center'),
                    plotOutput('country') %>% withSpinner(),
                    width = 5,
                    offset = 1),
             column(h4("What is your race or ethnicity?", align = 'center'),
                    plotOutput('race') %>% withSpinner(),
                    width = 5,
                    offset = 0)
    ),
    br(),
    fluidRow(width = 12,
             column(h4("What is your marital status?", align = 'center'),
                       plotOutput('marital') %>% withSpinner(),
                    width = 5,
                    offset = 1),
             column(h4("What is your gender?", align = 'center'),
                    plotOutput('gender') %>% withSpinner(),
                    width = 5,
                    offset = 0)
    ),
    br(),
    fluidRow(width = 12,
             column(h4("What is your age?", align = 'center'),
                    plotOutput('age') %>% withSpinner(),
                    width = 5,
                    offset = 1),
             column(h4("What is your annual income level? (U.S. normalized)", align = 'center'),
                    plotOutput('income') %>% withSpinner(),
                    width = 5,
                    offset = 0)
    ),
    br(),
    wellPanel(style = "padding: 0 0 45px;",
              fluidRow(h2(align = 'center', "Select a question in the list below to view the corresponding graph."),
              h2(align = 'center', "Each question has a", tags$span(style = "color:#DCA137", "self-oriented"), "and", tags$span(style = "color:#999999", "other-oriented"), "perspective, though only the self-oriented questions are listed below."),
                 width = 12),
        br(),
        br(),
        fluidRow(
            column(DT::dataTableOutput('items') %>% withSpinner(),
                        width = 4, 
                        offset = 1),
            column(plotOutput("plot", width = '100%', height = 525) %>% withSpinner(), 
                        br(),
                        fluidRow(column(h5("All survey questions were answered on a 5-point Likert scale, where:"), width = 12,align="center")),
                        fluidRow(column(h5("1 = disagree/much less/much worse"), width = 12, align="center")),
                        fluidRow(column(h5("5 = agree/much more/much better"), width = 12, align="center")),
                   width = 6,
                   offset = -1), width = 12)),
    fluidRow(
        h3(tags$span(style = "color:#3399F3", "Individual Response Matrix"), align = 'center'),
        column(align = "center",
               style = "padding: 1% 0 1%;",
               plotOutput('data_mtx', height ='100%', width = '100%') %>% withSpinner(),
               width = 12),
        width = 12),
    
    br(),
    wellPanel(style = "padding: 1% 0 1%; margin: 1% 0 1%;",
              fluidRow(
                  h3(tags$span(style ="color:#3399F3","Aggregated Average Responses"), align = "center"),
                  br(),
                  column(h4(tags$span(style = "color:#3399F3", "Together"), align = 'center'),
                         br(),
                         plotOutput("avg_response", height = '100%', width = '100%') %>% withSpinner(),
                         width = 4, offset = 0, align = "center"),
                  column(h4(tags$span(style = "color:#DCA137", "Self"), align = 'center'),
                         br(),
                         plotOutput("avg_response_self", height = '100%', width = '100%') %>% withSpinner(),
                         width = 4, offset = 0, align = "center"),
                  column(h4(tags$span(style ="color:#999999","Others"), align = "center"),
                         br(),
                         plotOutput('avg_response_others', height = '100%', width = '100%') %>% withSpinner(),
                         width = 4, offset = 0, align = "center"),
                  width = 12, align = "center")
        ),
    fluidRow(width = 8,
             h3(tags$span(style = "color:#3399F3", 'Average Self vs. Other'), align = "center"),
             column(tags$div(class = "aspect-ratio-corr", htmlOutput('corr')), align = "center", width = 8, offset = 2)),
    br(),
    fluidRow(
        column(
            h3(tags$span(style = "color:#3399F3", '"If you would like, please share how the COVID-19 pandemic has personally affected you."')),
            br(),
            imageOutput("personally_affected") %>% withSpinner(), 
            width = 12, align = "center"),
        width = 12),
    fluidRow(
        column(
            h3(tags$span(style = "color:#3399F3", '"What\'s the first thing you will do once quarantine is fully lifted and there is no risk of another wave of infection?"')),
            br(),
            imageOutput("done") %>% withSpinner(),          
            width = 12, align = "center"),
            width = 12),
    br(),
    br()
    )

server <- function(input, output) {
    
    output$map <- renderUI({
        tags$iframe(style = "border:0; overflow: hidden;",
                    scrolling = "no",
                    src = 'https://datastudio.google.com/embed/reporting/43210988-35ec-4ad5-bceb-dca4508f269a/page/j10MB')
     })
     
    output$corr <- renderUI({
        tags$iframe(style = "border:0; overflow: hidden;",
                    scrolling = "no",
                    src = 'https://datastudio.google.com/embed/reporting/b1292d52-e951-4d06-9f3b-684e6fae6ccd/page/7TJNB'
                    )
    })
    
     output$us_map <- renderImage({
         list(src = "US_zipcode_map.png",
              contentType = 'image/png',
              height = '100%',
              width = '100%',
              alt = "")
     }, deleteFile = F)
    
    output$education <- renderPlot({
        demo %>%
            filter(id == 2 & value != "Did not specify") %>%
            ggplot(aes(x = fct_reorder(value, per), y = per, fill = as.factor(value))) +
            geom_col(color = "black") +
            theme_minimal(base_size = 18) +
            theme(legend.title = element_blank(),
                  legend.position = "none",
                  text = element_text(size=16),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette) +
            scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            coord_flip()
    })
    
    output$employment <- renderPlot({
        demo %>%
            filter(id == 3 & value != "Did not specify") %>%
            ggplot(aes(x = fct_reorder(value, per), y = per, fill = as.factor(value))) +
            geom_col(color = "black") +
            theme_minimal(base_size = 18) +
            theme(legend.title = element_blank(),
                  legend.position = "none",
                  text = element_text(size=16),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette) +
            scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            coord_flip()
    })
    
    output$age <- renderPlot({
        age_income %>%
            ggplot(aes(x = `What is your age?`)) +
            geom_histogram(bins = 15, fill = "#DCA137", color = "black", aes(y = (..count..)/sum(..count..))) + 
            theme_minimal(base_size = 18) +
            theme(legend.position = "none",
                  text = element_text(size=16),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette) +
            scale_y_continuous(labels = scales::percent) +
            scale_x_continuous(breaks = c(0, 20, 40, 60, 80, 100))
    })
    
    output$income <- renderPlot({
        age_income %>%
            ggplot(aes(x = us_currency)) +
            geom_histogram(bins = 15, fill = "#DCA137", color = "black", aes(y = (..count..)/sum(..count..))) + 
            theme_minimal(base_size = 18) +
            theme(legend.position = "none",
                  text = element_text(size=16),
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette) +
            scale_y_continuous(labels = scales::percent) +
            scale_x_continuous(labels = scales::dollar, breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000))
    })
    
    output$country <- renderPlot({
        demo %>%
            filter(id == 1 & value != "Did not specify" & n > 2) %>% 
            ggplot(aes(x = fct_reorder(value, per), y = per, fill = as.factor(value))) +
            geom_col(color = "black") + 
            theme_minimal(base_size = 18) +
            theme(
                legend.position = "none",
                text = element_text(size=16),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette) +
            scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            coord_flip()
    })
    
    output$race <- renderPlot({
        demo %>%
            filter(id == 6) %>%
            ggplot(aes(x = fct_reorder(value, per), y = per, fill = as.factor(value))) +
            geom_col(color = "black") + 
            theme_minimal(base_size = 18) +
            theme(
                legend.position = "none",
                text = element_text(size=16),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette) +
            scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            coord_flip()
    })
    
    output$marital <- renderPlot({
        demo %>%
            filter(id == 5 & value != "Did not specify") %>%
            ggplot(aes(x = fct_reorder(value, per), y = per, fill = as.factor(value))) +
            geom_col(color = "black") + 
            theme_minimal(base_size = 18) +
            theme(
                legend.position = "none",
                text = element_text(size=16),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette) +
            scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            coord_flip()
    })
    
    output$gender <- renderPlot({
        demo %>%
            filter(id == 4 & value != "Did not specify") %>%
            ggplot(aes(x = fct_reorder(value, per), y = per, fill = as.factor(value))) +
            geom_col(color = "black") + 
            theme_minimal(base_size = 18) +
            theme(
                legend.position = "none",
                text = element_text(size=16),
                axis.title.x = element_blank(),
                axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette) +
            scale_y_continuous(labels = scales::percent_format(scale = 1)) +
            coord_flip()
    })
    
    output$personally_affected <- renderImage({
    list(src = "wordCloud_PersonallyAffected.png",
         contentType = 'image/png',
         width = 500,
         height = 400,
         alt = "")
        }, deleteFile = F)
    
    output$avg_response <- renderPlot({
        long_survey %>%
            filter(q_id != 31) %>%
            group_by(q_condensed, self_or_others, q_id) %>% 
            summarize(mean_value = mean(as.double(value), na.rm = T)) %>%
            arrange(q_id) %>%
            ggplot(aes(x = fct_reorder(q_condensed, mean_value), y = mean_value, fill = factor(self_or_others))) +
            geom_col(color = 'black') +
            theme_minimal(base_size = 14) +
            scale_y_continuous(expand = c(0, 0.5),
                               breaks = c(1, 2, 3, 4, 5),
                               labels = c('Disagree\nMuch Less\nMuch Worse',
                                          'Somewhat Disagree\nLess\nWorse',
                                          'Neutral\nSame',
                                          'Somewhat Agree\nMore\nBetter',
                                          'Agree\nMuch More\nMuch Better')) +
            geom_hline(yintercept=3, 
                       linetype="dashed", 
                       color = "black", 
                       size=1) +
            coord_flip(ylim = c(1, 5)) +
            theme(text = element_text(size = 14),
                  legend.position = "none",
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank()) +
            scale_fill_manual(values = cbPalette)
    }, width = "auto", height = 600)
    
    output$avg_response_self <- renderPlot({
        long_survey %>%
            filter(q_id != 31 & q_word != 'Extroverted' & q_word != 'Greatly Affected Me' & q_word != 'Introverted') %>%
            group_by(q_condensed, self_or_others, q_id) %>% 
            summarize(mean_value = mean(as.double(value), na.rm = T)) %>%
            arrange(q_id) %>%
            filter(self_or_others == 2) %>%
            ggplot(aes(x = fct_reorder(q_condensed, mean_value), y = mean_value, fill = q_condensed)) +
            geom_col(color = 'black') +
            scale_fill_manual(values = c25) +
            theme_minimal(base_size = 14) +
            scale_y_continuous(expand = c(0, 0.5),
                               breaks = c(1, 2, 3, 4, 5),
                               labels = c('Disagree\nMuch Less\nMuch Worse',
                                          'Somewhat Disagree\nLess\nWorse',
                                          'Neutral\nSame',
                                          'Somewhat Agree\nMore\nBetter',
                                          'Agree\nMuch More\nMuch Better')) +
            geom_hline(yintercept=3, 
                       linetype="dashed", 
                       color = "black", 
                       size=1) +
            coord_flip(ylim = c(1, 5)) +
            theme(text = element_text(size = 14),
                  legend.position = "none",
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank())
    }, width = "auto", height = 550)
    
    output$avg_response_others <- renderPlot({
        long_survey %>%
            filter(q_id != 31 & q_word != 'Greatly Affected Closest') %>%
            group_by(q_condensed, self_or_others, q_id) %>% 
            summarize(mean_value = mean(as.double(value), na.rm = T)) %>%
            arrange(q_id) %>%
            filter(self_or_others == 1) %>%
            ggplot(aes(x = fct_reorder(q_condensed, mean_value), y = mean_value, fill = q_condensed)) +
            geom_col(color = 'black') +
            theme_minimal(base_size = 14) +
            scale_fill_manual(values = c25) +
            scale_y_continuous(expand = c(0, 0.5),
                               breaks = c(1, 2, 3, 4, 5),
                               labels = c('Disagree\nMuch Less\nMuch Worse',
                                          'Somewhat Disagree\nLess\nWorse',
                                          'Neutral\nSame',
                                          'Somewhat Agree\nMore\nBetter',
                                          'Agree\nMuch More\nMuch Better')) +
            geom_hline(yintercept=3, 
                       linetype="dashed", 
                       color = "black", 
                       size=1) +
            coord_flip(ylim = c(1, 5)) +
            theme(text = element_text(size = 14),
                  legend.position = "none",
                  axis.title.x = element_blank(),
                  axis.title.y = element_blank())
    }, width = "auto", height = 550)

    output$data_mtx <- renderPlot({
        long_survey %>% 
            filter(q_id != 31 & !is.na(value)) %>%
            arrange(q_id) %>%
            ggplot(aes(x = factor(id), 
                       y = fct_reorder(q_condensed, q_id, .desc = T), 
                       fill = factor(value, 
                                     levels = c(5,4,3,2,1),
                                     labels = c('Agree/Much More/Much Better',
                                                'Somewhat Agree/More/Better',
                                                'Neutral/Same',
                                                'Somewhat Disagree/Less Worse',
                                                'Disagree/Much Less/Much Worse'
                                               )))) +
            geom_tile() +
            ylab('') +
            xlab('') +
            theme_minimal(base_size = 13) +
            theme(axis.text.x = element_text(size = 13),
                  legend.title = element_blank(),
                  legend.text = element_text(size = 14)) +
            scale_x_discrete(breaks = c(50,100,150,200,250)) +
            scale_fill_manual(values = c("#FAF954", "#C8C248","#57BDB9", "#447CF5", "#3728A3")) 
    }, width = 1850, height = 550)
    
    output$done <- renderImage({
        list(src = "wordCloud_FirstThing.png",
             contentType = 'image/png',
             width = 500,
             height = 400,
             alt = "")
    }, deleteFile = F)
    
    output$items <- DT::renderDataTable({
        data %>% 
            arrange(q_id) %>%
            filter(self_or_others == "Self" & question != "I see myself as introverted or reserved." & 
                       question != "I see myself as extroverted or outgoing." &
                       question != "The COVID-19 pandemic has greatly affected my daily life.") %>%
            rename('List of Questions' = question) %>%
            distinct(`List of Questions`)
    },
    server = FALSE, 
    selection = 'single')
    
    outVar = reactive({
        return(input$items_rows_selected)
    })
    
    observe({
        if(length(input$items_rows_selected == 0)){
            output$plot <- renderPlot({
                data %>%
                    arrange(q_id) %>%
                    filter(q_id == outVar()) %>%
                    ggplot(aes(x = value, y = per, fill = as.factor(question))) +
                    geom_col(color = "black", position = "dodge") +
                    theme_minimal(base_size = 16) +
                    theme(legend.title = element_blank(),
                          legend.position = "top",
                          legend.direction = "vertical",
                          text = element_text(size=16),
                          axis.title.x = element_blank(),
                          axis.title.y = element_blank()) +
                    scale_y_continuous(labels = scales::percent_format(scale = 1)) +
                    scale_fill_manual(values = cbPalette)
                })
        } else {
            output$plot <- renderPlot({
                    data %>%
                        arrange(q_id) %>%
                        mutate(q_id_word = paste0(q_id,'. ',q_word)) %>%
                        filter(q_id != 30 & question != "I see myself as introverted or reserved." &
                                   question != "I see myself as extroverted or outgoing." &
                                   question != "The COVID-19 pandemic has greatly affected my daily life.") %>%
                        ggplot(aes(x = value, y = per, fill = as.factor(self_or_others))) +
                        geom_col(color = "black", position = "dodge") +
                        theme_minimal(base_size = 16) +
                        theme(legend.title = element_blank(),
                              legend.position = "right",
                              legend.direction = "vertical",
                              text = element_text(size=16),
                              axis.title.x = element_blank(),
                              axis.title.y = element_blank()) +
                        facet_wrap(~fct_reorder(q_id_word,q_id)) +
                        scale_fill_manual(values = cbPalette) +
                        scale_y_continuous(labels = scales::percent_format(scale = 1))})
            }
    })

}

shinyApp(ui = ui, server = server)