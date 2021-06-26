#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

data1 <- readr::read_csv("./data/data1.csv")

txt70 <- data1 %>%
    filter(week >= as.Date("2021-01-01")) %>%
    #filter(week <= as.Date("2021-06-12")) %>% 
    mutate(over70 = if_else(age_range %in% c("70-79","80-89", ">90"), "70+", "<70")) %>% 
    group_by(week, over70) %>%
    summarise(total_deaths = sum(total_deaths)) %>% 
    spread(c(over70), total_deaths) %>% 
    mutate(position = `70+`/(`<70` + `70+`),
           text = as.character(round(position*100, 1)))

txt80 <- data1 %>%
    filter(week >= as.Date("2021-01-01")) %>%
    #filter(week <= as.Date("2021-06-12")) %>% 
    mutate(over80 = if_else(age_range %in% c("80-89", ">90"), "80+", "<80")) %>% 
    group_by(week, over80) %>%
    summarise(total_deaths = sum(total_deaths)) %>% 
    spread(c(over80), total_deaths) %>% 
    mutate(position = `80+`/(`<80` + `80+`),
           text = as.character(round(position*100, 1)))

txt90 <- data1 %>%
    filter(week >= as.Date("2021-01-01")) %>%
    #filter(week <= as.Date("2021-06-12")) %>% 
    mutate(over90 = if_else(age_range == ">90" , "90+", "<90")) %>% 
    group_by(week, over90) %>%
    summarise(total_deaths = sum(total_deaths)) %>% 
    spread(c(over90), total_deaths) %>% 
    mutate(position = `90+`/(`<90` + `90+`),
           text = as.character(round(position*100, 1)))

pl_dec_rel <- data1 %>%
    filter(week >= as.Date("2021-01-01")) %>% 
    #filter(week <= as.Date("2021-06-12")) %>% 
    mutate(age_range =case_when(
        age_range %in% 
            c("0-9", "10-19", "20-29", "30-39", "40-49") ~ "< 50",
        age_range == "50-59" ~ "50-59",
        age_range == "60-69" ~ "60-69",
        age_range == "70-79" ~ "70-79",
        age_range == "80-89" ~ "80-89",
        age_range == ">90" ~ "90+"
    )) %>% 
    ggplot(mapping = aes(x = week, y = total_deaths, fill=age_range))  +
    geom_col(position = "fill") + scale_fill_brewer(palette= "YlGnBu") +
    ggthemes::theme_few() + 
    xlab("Settimana") +
    ylab("% decessi")+
    labs(fill = "Fascia d'età") +
    scale_y_continuous(labels = scales::percent)  +
    geom_text(inherit.aes = FALSE,data = txt80,
              mapping = aes(x = week, y = position + 0.03,
                            label = text),
              size =4.1, 
              col="white")+
    geom_text(inherit.aes = FALSE,data = txt90,
              mapping = aes(x = week, y = position + 0.03,
                            label = text),
              size =4.1,
              col="white") +  geom_text(inherit.aes = FALSE,data = txt70,
                                        mapping = aes(x = week, y = position + 0.03,
                                                      label = text),
                                        size =4.1,
                                        col="white")

#pl_dec_rel


pl_dec_ass <- data1 %>%
    filter(week >= as.Date("2021-01-01")) %>% 
    #filter(week <= as.Date("2021-06-11")) %>% 
    
    mutate(age_range =case_when(
        age_range %in% 
            c("0-9", "10-19", "20-29", "30-39", "40-49") ~ "< 50",
        age_range == "50-59" ~ "50-59",
        age_range == "60-69" ~ "60-69",
        age_range == "70-79" ~ "70-79",
        age_range == "80-89" ~ "80-89",
        age_range == ">90" ~ "90+"
    )) %>% 
    ggplot(mapping = aes(x = week, y = total_deaths, fill=age_range))  +
    geom_bar(stat = "identity") + scale_fill_brewer(palette= "YlGnBu") +
    ggthemes::theme_few() + 
    xlab("Settimana") +
    ylab("N. decessi")+
    labs(fill = "Fascia d'età")

counts <- data1 %>% 
    #filter(week <= as.Date("2021-06-12")) %>% 
    group_by(week) %>% summarise(deaths = sum(total_deaths))

pl_dec_ass <- pl_dec_ass +  geom_text(inherit.aes = FALSE,data = counts,
                        mapping = aes(x = week, y = deaths + 100,
                                      label = deaths),
                        size =4.1,
                        col="black")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Distributione per età dei decessi COVID-19 in Italia per settimana di notifica"),
    plotOutput('p.ass'),
    hr(),
    plotOutput('p.rel')
    )


# Define server logic required to draw a histogram
server <- function(input, output) {

    output$p.ass <- renderPlot(pl_dec_ass)
    output$p.rel <- renderPlot(pl_dec_rel)
}

# Run the application 
shinyApp(ui = ui, server = server)
