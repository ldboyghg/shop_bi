# 小时销售曲线
salesHourUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        box(title = '日销售曲线', status = 'primary', solidHeader = T, width = 12,
            fluidRow(class='toolbar',
                     column(3, dateInput(ns('dt'), '成单时间', Sys.Date()-1))
            ),
            fluidRow(
                column(8, plotlyOutput(ns('trend1')) ),
                column(4,dataTableOutput(ns('tb1')) )
            )
        )
    )
}
salesHour <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    df <- reactive({
        
        shop_code_ <- username 
        
        sql <- paste0("
                      select DATE_FORMAT(b.dt,'%H') h,round(sum(sum_price)) amount2 , count(oid) oid2 
                      from (
                      select 
                      case
                      when PAY_STATUS = 1 then
                      payDates
                      when PAY_TYPE = 10002 then
                      POST_DATE_STR
                      end dt , sum_price ,oid 
                      from tb_porder a
                      where
                      shop_code =   '",shop_code_,"'
                      and HANDLE_STATUS NOT IN (5, 7) 
                      and  (PAY_STATUS = 1 or PAY_TYPE = 10002 ) -- 货到付款
                      ) b
                      where 1 = 1 
                      and date(dt) = '",dt(),"'
                      group by DATE_FORMAT(b.dt,'%H')
                      order by DATE_FORMAT(b.dt,'%H') 
                      ")
        x <- dbGetQuery0('ecommerce', sql)
        x1<-x%>%
            rename(`小时`=h , `销售额`=amount2,`订单数量`=oid2)
        x1
    })
    
    
    output$trend1 <- renderPlotly({
        x1 <- df()
        plot_ly(x1, x = ~`小时`) %>%
            add_lines(y = ~`销售额`)%>% add_markers(y = ~`销售额`)
        # plot_ly(x1, x = ~x1[,1]) %>%
        #      add_lines(y = ~x1[,2]) %>% add_markers(y = ~x1[,2])
    })
    output$tb1 <- DT::renderDataTable({
        datatable(
            df(), 
            rownames = FALSE,
            selection = 'multiple',
            extensions = list(Scroller=list()),
            options = list(
                searching=FALSE,
                lengthChange = TRUE,
                pageLength = 24
                
            )
        )
    })
    
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~












# #------------------------地域销售概况---REmap----------------
salesMapUI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-30, end = Sys.Date()-1)) ,
                 # column(2, selectInput(ns('field'), '指标', choices =setNames( c("oids", "mids", "amount",'quantity') , c('订单数','客户数','销售额','销售量') ) ) )
                # column(2, selectInput(ns('field'), '指标',  c("订单数" = "oids","客户数" = "mids","销售额" = "amount","销售量" = "quantity") ) )  
                column(2, selectInput(ns('field'), '指标',  c("订单数" ,"客户数","销售额" ,"销售量") ) )  
                 
                
        ),
        fluidRow(
            column(4,dataTableOutput(ns('tbl2'))),
            column(8,eChartOutput(ns('map')))
            #column(8,eChartOutput(ns('map')) ,plotOutput(ns('map1')))
            #column(8,eChartOutput(ns('map')) ,REmapOutput(ns('map1')))
            
        )
    )
}
salesMap <- function(input, output, session , username) {
    
    #付款日期值获取
    pay_date <- reactive({
        req(input$pay_date)
        input$pay_date
    }) 
    
    
    field <- reactive({
                req(input$field)
                input$field
            })
    
    
    df1 <- reactive({
        
        shop_code_ <- username 
        
        
        sql <-
            paste0("
                   select  case when x.PROVINCE_NAME like '%内蒙古%' then '内蒙古'
                   when x.PROVINCE_NAME like '%宁夏%' then '宁夏'
                   when x.PROVINCE_NAME like '%广西%' then '广西'
                   when x.PROVINCE_NAME like '%西藏%' then '西藏'
                   when x.PROVINCE_NAME like '%新疆%' then '新疆'
                   when x.PROVINCE_NAME like '%青海%' then '青海'
                   else replace(replace(x.PROVINCE_NAME,'省',''),'市','') end province_name ,
                   ifnull(y.oids ,0) oids,
                   ifnull(y.mids ,0) mids,
                   ifnull(y.amount ,0) amount,
                   ifnull(y.quantity ,0) quantity
                   from bbf_shop.tb_param_PROVINCE_NAME x
                   left join (
                   SELECT
                   t.PROVINCE_NAME ,
                   count(distinct t.oid) oids ,
                   count(distinct t.mid) mids ,
                   round(sum(b.amount),0) amount ,
                   sum(b.quantity) quantity
                   FROM
                   (select oid , mid ,
                   case when PROVINCE_NAME like '%内蒙古%' then '内蒙古自治区'
                   when PROVINCE_NAME like '%宁夏%' then '宁夏回族自治区'
                   when PROVINCE_NAME like '%广西%' then '广西壮族自治区'
                   when PROVINCE_NAME like '%西藏%' then '西藏自治区'
                   when PROVINCE_NAME like '%新疆%' then '新疆维吾尔自治区'
                   when PROVINCE_NAME like '%青海%' then '青海省'
                   else PROVINCE_NAME end PROVINCE_NAME ,
                   PAY_STATUS,payDates ,HANDLE_STATUS,PAY_TYPE , POST_DATE_STR from
                   ecommerce.tb_porder
                   where shop_code =  '",shop_code_,"')t
                   JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
                   WHERE
                   1 = 1
                   AND  HANDLE_STATUS NOT IN (5, 7)
                   AND (
                   (
                   PAY_STATUS = 1
                   AND date(payDates) >=  '",pay_date()[1],"'
                   AND date(payDates) <=  '",pay_date()[2],"'
                   )
                   OR (
                   PAY_TYPE = 10002
                   AND date(POST_DATE_STR) >=  '",pay_date()[1],"'
                   AND date(POST_DATE_STR) <=  '",pay_date()[2],"'
                   )
                   )
                   group by t.PROVINCE_NAME )  y
                   on x.PROVINCE_NAME = y.PROVINCE_NAME
                   ")
        dd1 <- dbGetQuery0('ecommerce',sql)
        
        dd1 <- dd1 %>%  rename(`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity)
         
    }) 
    
    
    output$tbl2 <- renderDataTable({
        datatable(
            #df1() %>%  rename(`省份`=province_name,`订单数`=oids,`客户数`=mids,`销售额`=amount,`销售量`=quantity),
            df1() %>%  rename(`省份`=province_name),
            escape = FALSE,
            rownames = FALSE,  
            options = list( 
                searching=FALSE, 
                pageLength = 20,
                lengthChange = TRUE
                
            )
        ) 
    })
    
    
    output$map <- renderEChart({
                # info(field())
                # 
                # country<-df()$country
                # value<-df()$field()
                # #x <- as.data.frame(country,value)
                # out = REmap::remapC(df()[,1:2] ,maptype = "china",color = 'skyblue')
                # plot(out)
        
        #mapData <- head(mapTestData_chs, 5)
        eMap(df1(), namevar=~province_name, datavar = ~get(field()) )
        #eMap(mapData, namevar=~province_name, datavar = ~val1 + val2)
  })

    
    
    
    # output$map1 <- renderREmap({
    #     remap(demoC,title = "REmap",subtitle = "theme:Dark")
    # })renderPlotly
    
    # output$map1 <- renderREmap({
    #     out = remap(demoC,title = "REmap: Demo DATA",
    #                 subtitle = "theme:Dark")
    #     plot(out)
    # })
} 




 