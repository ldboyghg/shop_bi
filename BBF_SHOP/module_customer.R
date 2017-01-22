# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 客单价趋势
kdUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(class='toolbar',
                 column(3, dateRangeInput(ns('dt'), '购买时间', Sys.Date()-90, Sys.Date()-1)),
                 column(3, radioButtons(ns('freq'), '频率', choices = c('按月'='month','按周'='week', '按天'='day'), inline = TRUE))
        ),
        fluidRow(
            column(12, plotlyOutput(ns('p')))
        )
    )
}
kd <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    freq <- reactive({
        req(input$freq)
        input$freq
    })
    df <- reactive({
        
        shop_code_ <- username 
        
        
        if(freq()=='month') {
            prefix_sql <- 'substring(dt,1,7)'
        } else if(freq()=='week') {
            prefix_sql <- 'weekofyear(dt)'
        } else if(freq()=='day') {
            prefix_sql <- 'substring(dt,1,10)'
        } else {
            prefix_sql <- 'substring(dt,1,7)'
        }
        sql <- paste0("
                      select " ,prefix_sql, " as dt1, sum(SUM_PRICE) /count(DISTINCT oid) AS m
                      FROM
                      (
                      select case
                      when PAY_STATUS = 1 then  date(payDates)
                      when PAY_TYPE = 10002 then date(POST_DATE_STR)
                      end dt , oid,
                      sum_price
                      from tb_porder 
                      where 1 = 1
                      and shop_code =   '",shop_code_,"'
                      and HANDLE_STATUS not in (5, 7) 
                      and (PAY_STATUS = 1 or PAY_TYPE = 10002)
                      ) a
                      WHERE 1=1
                      #and date(dt) >= '20161001'
                      #and date(dt) <= '20161031'
                      AND	date(dt) >= '",dt()[1],"'
                      AND date(dt) <= '",dt()[2],"'
                      GROUP BY dt1
                      ")
        x <- dbGetQuery0('ecommerce', sql) %>% mutate(dt = as.factor(dt1))
        if(freq()=='day') {
            x$dt <- as.Date(x$dt)
        }
        x
    })
    
    output$p <- renderPlotly({
        g <- df() %>% 
            ggplot(aes(dt, m))+geom_bar(aes(fill=3), stat='identity')+labs(x='客单价趋势')+theme(legend.position='none')
        #ggplot()+geom_point(aes(dt, m, color=2))+geom_smooth()+labs(x='客单价趋势')+theme(legend.position='none')
        ggplotly(g)
    })
}