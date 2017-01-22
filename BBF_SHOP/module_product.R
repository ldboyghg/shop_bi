# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 热卖商品
topProductUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-1, end = Sys.Date()-1)),
                 column(2, downloadButton(ns('btn_export'), '导出商品汇总表', class = 'btn_nowrap'))
                 #             column(2, downloadButton(ns('btn_export_detail'), '导出所选商品的订单明细', class = 'btn_nowrap'))
        ) ,
        fluidRow(
            DT::dataTableOutput(ns('tbl'))
        )
    )
}
topProduct <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    
    df <- reactive({
        
        shop_code_ <- username 
        
        
        dbGetQuery0('ecommerce', paste0("
                                        SELECT  	a.pid,	a.name,	a.proname, a.proPzwh,b.merchant_name,
                                        c.ctitle0,	c.ctitle1,	c.ctitle2,
                                        a.quantity,	a.amount ,	a.guige,	a.bcbilv,	a.commission,
                                        b.mid as account
                                        FROM
                                        tb_porder_line a
                                        INNER JOIN tb_porder b 
                                        ON a.oid = b.oid and b.shop_code =   '",shop_code_,"'
                                        INNER JOIN tb_product_catalogbase c ON c.pid = a.pid
                                        WHERE 1=1
                                        and b.HANDLE_STATUS NOT IN (5, 7)
                                        AND (
                                        (
                                        b.PAY_STATUS = 1
                                        AND date(b.payDates) >= '",dt()[1],"'
                                        AND date(b.payDates) <= '",dt()[2],"'
                                        )
                                        OR (
                                        b.PAY_TYPE = 10002
                                        AND date(b.POST_DATE_STR) >= '",dt()[1],"'
                                        AND date(b.POST_DATE_STR) <= '",dt()[2],"'
                                        )
                                        )
                                        "))
    })
    #and date(dt)>='",dt()[1],"' and date(dt)<='",dt()[2],"'
    df2 <- reactive({
        x <- df() %>% 
            group_by(pid) %>% 
            mutate(name=substring(name,1,10)) %>% 
            summarise(
                name=max(name),
                proname = max(proname),
                proPzwh=max(proPzwh),
                merchant_name=max(merchant_name),
                pcnt = n_distinct(pid),
                ctitle0 = max(ctitle0),
                ctitle1 = max(ctitle1),
                ctitle2 = max(ctitle2),
                quantity = sum(quantity),
                amount = sum(amount),
                guige = max(guige),
                bcbilv = max(bcbilv),
                commission = sum(commission),
                account = n_distinct(account)
            ) %>% 
            arrange(desc(amount)) %>% 
            ungroup() %>% 
            # mutate(name=ifelse(nchar(proname)==0, name, paste(name, proname, sep=' / '))) %>% 
            select(
                `pid`=pid,
                `商品名称`=name,
                `通用名称` = proname,
                `销售量` = quantity,
                `销售额` = amount,
                `批准文号`=proPzwh,
                `商家名称` = merchant_name,
                `商品个数` = pcnt,
                `一级分类` = ctitle0,
                `二级分类` = ctitle1,
                `三级分类` = ctitle2,
                `购买人数(排重)` = account,
                `规格` = guige,
                `分成比率` = bcbilv,
                `分成总额` = commission
            )
      
    })
    
    # export
    output$btn_export <- downloadHandler(paste('bbf-data-callcenter-',Sys.Date(),'.csv',sep=''), content = function(file) {
        if(TRUE){
            tmp <- df2()[input$tbl_rows_all, , drop = FALSE]
            write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
        } else {
            write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
        }
        
    })
    
    
    output$tbl <- DT::renderDataTable({
        datatable(
            df2(),
            escape = FALSE,
            rownames = FALSE,
            selection = 'multiple',
            #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
            extensions = list(Scroller=list()),
            options = list(
                autoWidth = TRUE,
                columnDefs = list(list(width = '50px', targets = c(1))),
                searching=TRUE,
                deferRender=TRUE,
                scrollY=200,
                scrollX=TRUE,
                scrollCollapse = TRUE,
                order = list(list(11, 'desc')),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
                pageLength = 200,
                lengthChange = FALSE,
                initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
                fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
            )
        )
    })
}






# 品类优化
proudctOptimizeUI <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(
            column(3, dateRangeInput(ns('pay_date'), '查询期付款时间',  start = Sys.Date()-1, end = Sys.Date()-1)),
            column(3, dateRangeInput(ns('pay_date1'), '对比期付款时间',  start = Sys.Date()-2, end = Sys.Date()-2)),
            #column(3, numericInput(ns("kpi1"), 'UV',value = 0 , step = 10000)),
            column(2, sliderInput(ns("sharerat"),label = "占有率", min = 0.0001, max = 0.5, value = c(0.0001, 0.2)  )),
            column(2, sliderInput(ns("riserat"),label = "增长率",min = -0.9999, max = 5, value = c(-0.001, 2) ))
            
        ),
        
        fluidRow(
            column(4,dataTableOutput(ns('tbl'))),
            column(8,plotlyOutput(ns('plot')))
        )
        
    )
}
proudctOptimize <- function(input, output, session,username) {
    #付款日期值获取
    pay_date <- reactive({
        req(input$pay_date)
        input$pay_date
    })
    #对比期付款日期值获取
    pay_date1 <- reactive({
        req(input$pay_date1)
        input$pay_date1
    })
    #增长率
    riserat <- reactive({
        req(input$riserat)
        input$riserat
    })
    
    #占有率
    sharerat <- reactive({
        req(input$sharerat)
        input$sharerat
    })
    
    #and date(dt) >= '",pay_date()[1],"'
    #and date(dt) <= '",pay_date()[2],"'
    # AND date(POST_DATE_STR) >= '",pay_date()[1]-30,"'
    # AND date(POST_DATE_STR) <= '",pay_date()[2]-30,"'
    
    
    
    df <- reactive({
        
        shop_code_ <- username 
        
        sql <-
            paste0("  select ctitle2, amount1 , amount2 from (
                   select  x.ctitle2 , ifnull(x.amount ,0) amount1, ifnull(y.amount ,0) amount2 from (
                   SELECT 
                   c.ctitle2 ,
                   count(distinct t.oid) oids ,
                   count(distinct t.mid) mids ,
                   sum( b.amount ) amount ,
                   sum( b.quantity )quantity 
                   FROM
                   ecommerce.tb_porder t
                   JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
                   join my_product c on b.pid = c.pid 
                   WHERE
                   1 = 1
                   and t.shop_code =   '",shop_code_,"'
                   and HANDLE_STATUS NOT IN (5, 7)
                   AND (
                   (
                   PAY_STATUS = 1
                   AND date(payDates) >= '",pay_date()[1],"'
                   AND date(payDates) <= '",pay_date()[2],"'
                   )
                   OR (
                   PAY_TYPE = 10002
                   AND date(POST_DATE_STR) >= '",pay_date()[1],"'
                   AND date(POST_DATE_STR) <= '",pay_date()[2],"'
                   )
                   )
                   group by c.ctitle2 ) x
                   
                   left join 
                   (
                   SELECT 
                   c.ctitle2 ,
                   count(distinct t.oid) oids ,
                   count(distinct t.mid) mids ,
                   sum( b.amount ) amount ,
                   sum( b.quantity )quantity 
                   FROM
                   ecommerce.tb_porder t
                   JOIN ecommerce.tb_porder_line b ON t.oid = b.oid
                   join my_product c on b.pid = c.pid 
                   WHERE
                   1 = 1
                   and t.shop_code =   '",shop_code_,"'
                   and HANDLE_STATUS NOT IN (5, 7)
                   AND (
                   (
                   PAY_STATUS = 1
                   AND date(payDates) >=  '",pay_date1()[1],"'
                   AND date(payDates) <=  '",pay_date1()[2],"'
                   )
                   OR (
                   PAY_TYPE = 10002
                   AND date(POST_DATE_STR) >=  '",pay_date1()[1],"'
                   AND date(POST_DATE_STR) <= '",pay_date1()[2],"'
                   )
                   )
                   group by  c.ctitle2 ) y
                   on  x.ctitle2 = y.ctitle2
            ) z  
                   order by amount1  desc 
                   limit 1,100
                   ")
        
        dd <- dbGetQuery0('ecommerce',sql)
        
        df <- dd %>% 
            mutate(
                sum_amount = round(sum(amount1)),
                amount_rat=ifelse(sum_amount==0 ,0 ,round(amount1/sum_amount,4)),
                rise_rat=ifelse(amount2==0,0,round((amount1-amount2)/amount2,4))
            ) %>% select(-sum_amount,-amount2) 
        
        
    })
    
    
    
    output$tbl <- renderDataTable({ 
        
        datatable(
            df()%>%rename(`三级类目`=ctitle2,`销售额`=amount1,`销售额占比`=amount_rat,`增长率`=rise_rat),
            escape = FALSE,
            rownames = FALSE,
            selection = 'none',
            #filter = 'top',
            extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
            options = list(
                #autoWidth = TRUE,
                columnDefs = list(list(width = '200px', targets = c(0))),
                searching=FALSE,
                deferRender=FALSE,
                scrollY=200,
                scrollX=TRUE,
                scrollCollapse = TRUE,
                order = list(list(2, 'desc')),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
                pageLength = 500,
                lengthChange = FALSE,
                initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
                fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
            )
        )
    })
    
    
    output$plot <- renderPlotly({
        #ePoints(df(), ~amount_rat, ~rise_rat, series = ~ctitle2)
        #
        # highchart() %>%
        #   hc_title(text = "Scatter chart with size and color") %>%
        #   hc_add_series_scatter(df()$amount_rat, df()$rise_rat)
        
        
        df1 <- subset(df() ,df()$amount_rat<= quantile(df()$amount_rat,  probs = c(95)/100) &  df()$rise_rat<=quantile(df()$rise_rat,  probs = c(95)/100))
        
        
        
        df1 <- subset(df1 , df1$rise_rat >= riserat()[1] &  df1$rise_rat<= riserat()[2] )
        
        df1 <- subset(df1 , df1$amount_rat >= sharerat()[1] &  df1$amount_rat<= sharerat()[2] )
        
        df1 <- df1 %>% rename(`占有率`=amount_rat,`增长率`=rise_rat)
        
        ggplot(df1, aes(x = 占有率, y = 增长率)) +
            # 散点图函数
            geom_point() +
            # 文本函数：aes参数中：y将原有纵轴值向上偏移，label设置绑定文本
            # 将y轴偏移的目的是为了让文本展示在样本点上方而不是中间
            #geom_text(aes( label = ctitle2   ))+
            geom_vline(size = 1, colour = "green", xintercept = mean(df1$占有率)) +
            geom_hline(size = 1, colour = "blue", yintercept = mean(df1$增长率)) +
            geom_text(aes( label = paste(ctitle2,'\n',"占有率:",占有率,",","增长率:",增长率)   ))
    })
}







# 商品通用名排名 
pronameProductUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-1, end = Sys.Date()-1)) 
        ),
        fluidRow(
            DT::dataTableOutput(ns('tbl'))
        )
    )
}
pronameProduct <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    
    df <- reactive({
        
        # shop_code_ <- username 
        
       dd <- dbGetQuery0('ecommerce', paste0("
                                        SELECT
                                        coalesce(c.proname,'未知') proname ,
                                        -- c.ctitle0,	c.ctitle1,	c.ctitle2,
                                        count(distinct a.NAME) as name_counts,
                                        count(distinct a.pid) as pid_counts,
                                        COUNT(DISTINCT b.merchant_name) as merchant_counts,
                                        sum(a.quantity) as quantity,
                                        round(sum(a.amount)) as amounts,	
                                        sum(a.commission) as commission,
                                        count(distinct b.mid) as mid_counts
                                        FROM
                                        tb_porder_line a
                                        INNER JOIN tb_porder b 
                                        ON a.oid = b.oid 
                                        -- INNER JOIN tb_product_catalogbase c ON c.pid = a.pid
                                        INNER JOIN my_product c ON c.pid = a.pid
                                        WHERE 1=1
                                        and b.HANDLE_STATUS NOT IN (5, 7)
                                        AND (
                                        (
                                        b.PAY_STATUS = 1
                                        AND date(b.payDates) >= '",dt()[1],"'
                                        AND date(b.payDates) <= '",dt()[2],"'
                                        )
                                        OR (
                                        b.PAY_TYPE = 10002
                                        AND date(b.POST_DATE_STR) >= '",dt()[1],"'
                                        AND date(b.POST_DATE_STR) <= '",dt()[2],"'
                                        )
                                        )
                                        group by coalesce(c.proname,'未知')
                                        "))
  
     
          df <- dd  %>% 
            # select(
            #     `通用名称` = proname,
            #     # `一级分类` = ctitle0,
            #     # `二级分类` = ctitle1,
            #     # `三级分类` = ctitle2,
            #     `商品名数` = name_counts,
            #     `pid数` = pid_counts,
            #     `商家数` = merchant_counts,
            #     `购买人数` = mid_counts,
            #     `销售额` = amounts,
            #     `销售量` = quantity,
            #     `分成总额` = commission
            # )  %>%
                mutate(
                # `商家数占比` = dd[,4]/sum(dd[,4]),
                # `销售量占比` = dd[,5]/sum(dd[,5]),
                # `销售额占比` = dd[,6]/sum(dd[,6]),
                # `购买人数占比` = dd[,8]/sum(dd[,8])
                    a =  round(dd[,4]/sum(dd[,4]),4),
                    b= round(dd[,5]/sum(dd[,5]),4),
                    c= round(dd[,6]/sum(dd[,6]),4),
                    d = round(dd[,8]/sum(dd[,8]),4)
              )   %>% 
             select(`通用名称` = proname,`商家数占比` = a ,`销售量占比` = b ,`销售额占比` = c ,`购买人数占比` = d )  
             # arrange(desc('销售额占比'))
        
    })
    
    
    
    output$tbl <- DT::renderDataTable({
        datatable(
             df() %>% arrange(desc(销售额占比)),
            escape = FALSE,
            rownames = FALSE,
            #selection = 'multiple',
            #extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
            extensions = list(Scroller=list()),
            options = list(
                autoWidth = TRUE,
                columnDefs = list(list(width = '30px', targets = c(1))),
                searching=TRUE,
                deferRender=TRUE,
                scrollY=200,
                scrollX=TRUE,
                scrollCollapse = TRUE,
                #order = list(list(3, 'desc')),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
                pageLength = 50,
                lengthChange = FALSE,
                initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
                fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
            )
        )%>% formatPercentage(c('商家数占比','销售量占比','销售额占比','购买人数占比'), 2) 
    })
    }









# 商品类型排名 
bcnameRankUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('dt'), '付款时间', start = Sys.Date()-30, end = Sys.Date()-1)) 
                 
        ),
        fluidRow(
           column(5,  DT::dataTableOutput(ns('tbl'))) ,
           column(7,  plotlyOutput(ns('p'))) 
        )
    )
}
bcnameRank <- function(input, output, session ) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    
    df <- reactive({ 
        
        dd <- dbGetQuery0('ecommerce', paste0("
                                             select b.BCNAME bcname , sum(b.amount) amount  from tb_porder a
                                              join tb_porder_line b
                                              on a.oid = b.oid
                                              where 1 = 1
                                              AND (
                                              (
                                              a.PAY_STATUS = 1
                                              AND date(a.payDates) >= '",dt()[1],"'
                                              AND date(a.payDates) <= '",dt()[2],"'
                                              )
                                              OR (
                                              a.PAY_TYPE = 10002
                                              AND date(a.POST_DATE_STR) >= '",dt()[1],"'
                                              AND date(a.POST_DATE_STR) <= '",dt()[2],"'
                                              )
                                              )
                                              group by b.BCNAME 
                                              "))
        
        
        df <- dd  %>% 
        mutate(
            a =  round(dd[,2]/sum(dd[,2]),4)
           ) %>% arrange(desc(a)) %>%
            select(`商品类型` = bcname ,`销售额占比` = a)
        
    })
    
    
    
    
    df2 <- reactive({
        #s <- ifelse(is.null(input$tbl_rows_selected), input$tbl_rows_all, input$tbl_rows_selected)
#        bcname <- paste(df()$`商品类型`[input$tbl_rows_selected], collapse="','")
        #info(input$tbl_rows_selected)
        #bcname <- df()[input$tbl_rows_selected,1]
        bcname <- ifelse(nchar(df()[input$tbl_rows_selected,1]) < 1 ,'肝炎用药',df()[input$tbl_rows_selected,1])
        
        sql <- paste0("SELECT
                	DATE_FORMAT(dt, '%Y%m%d') dt ,
                        sum(amount) amount
                        FROM
                        (
                        SELECT
                        CASE
                        WHEN a.PAY_STATUS = 1 THEN
                        a.payDates
                        WHEN a.PAY_TYPE = 10002 THEN
                        a.POST_DATE_STR
                        END dt,
                        b.BCNAME bcname,
                        b.amount
                        FROM
                        tb_porder a
                        JOIN tb_porder_line b ON a.oid = b.oid
                        WHERE
                        1 = 1
                        AND (
                        (
                        a.PAY_STATUS = 1
                        AND date(a.payDates) >= '",dt()[1],"'
                        AND date(a.payDates) <= '",dt()[2],"'
                        )
                        OR (
                        a.PAY_TYPE = 10002
                        AND date(a.POST_DATE_STR) >= '",dt()[1],"'
                        AND date(a.POST_DATE_STR) <= '",dt()[2],"'
                        )
                        )
                        ) x
                        WHERE
                        BCNAME  in ('",bcname,"') 
                        GROUP BY
                        DATE_FORMAT(dt, '%Y%m%d')")
        
        dbGetQuery0('ecommerce', sql) 
    })
    
    
    
    output$tbl <- DT::renderDataTable({
        datatable(
            df(),
            escape = FALSE,
            rownames = TRUE,
            selection = 'single', 
            options = list(
                autoWidth = TRUE,
                columnDefs = list(list(width = '30px', targets = c(1))),
                #searching=TRUE,
                deferRender=TRUE,
                scrollY=200,
                scrollX=TRUE,
                scrollCollapse = TRUE,
                pageLength = 50,
                #lengthChange = FALSE,
                 initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-200)+'px !important');}"),
                 fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-200;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
            )
        )%>% formatPercentage(c('销售额占比'), 2) 
    })
    
    
    
     
    
    
    output$p <- renderPlotly({
        
        #bcname <- df()[input$tbl_rows_selected,1]
        bcname <- ifelse(nchar(df()[input$tbl_rows_selected,1]) < 1 ,'肝炎用药',df()[input$tbl_rows_selected,1])
        
        mytheme <- theme(plot.title=element_text(face="bold.italic",
                                                 size="14", color="brown"),
                         axis.title=element_text(face="bold.italic",
                                                 size=10, color="brown"),
                         axis.text=element_text(face="bold", size=9,
                                                color="darkblue"),
                         panel.background=element_rect(fill="white",
                                                       color="darkblue"),
                         panel.grid.major.y=element_line(color="grey",
                                                         linetype=1),
                         panel.grid.minor.y=element_line(color="grey",
                                                         linetype=2),
                         panel.grid.minor.x=element_blank(),
                         axis.text.x=element_text(angle = 315,vjust = 0.5,hjust = 0.5),
                         legend.position="none")
        
        
        ggplot(df2() %>% mutate(dt=substr(dt,5,8))  ,aes(dt, amount), fill='blue')+
            #geom_bar(aes(fill=`渠道`),stat="identity",position="dodge",width=0.8)+
            geom_bar(stat="identity",position="dodge",width=0.8, fill = 'steelblue')+
            labs(title= bcname , x="日期", y="销售额")+
            mytheme
    })
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 销售品种数趋势

saleskuTrendsUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        box(title = '销售品种数趋势', status = 'primary', solidHeader = T, width = 12,
            fluidRow(class='toolbar',
                     column(4, dateRangeInput(ns('dt'), '支付日期', start = Sys.Date()-8 , end = Sys.Date()-1))
            )
            ,
            fluidRow(
                navbarPage("趋势图与表!",
                           tabPanel("Plot", 
                                    fluidRow(
                                        column(2,radioButtons(ns('field'), '指标',
                                                              choices = c('通用商品名数'
                                                                          ,'商品名数'
                                                                          ,'pid数'
                                                                          ,'销售量'
                                                                          ,'销售额'
                                                                          ,'成单数'
                                                                          ,'购买人数'
                                                                          ,'客单价')
                                        )
                                        ),
                                        column(10,plotlyOutput(ns('trend1')))
                                    )
                                    
                           )
                           ,
                           tabPanel("Summary",dataTableOutput(ns('daysales'))   )
                )
            )
            
            
        )
    )
}


saleskuTrends <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    field <- reactive({
        req(input$field)
        input$field
    })
    
    # 总销售额、总订单数、平均销售额
    #可以根据指标画曲线，用plotly作图
    
    df <- reactive({
        
        shop_code_ <- username 
        
        sql<-paste0("
                    select date(dt) as paydate,
                    count(distinct proname) as proname_counts,
                    count(distinct NAME) as name_counts,
                    count(distinct pid) as pid_counts,
                    -- COUNT(DISTINCT merchant_name) as merchant_counts,
                    sum(quantity) as quantity,	
                    ROUND(sum(amount)) as amounts,	
                    count(distinct oid) as oid_counts,	
                    count(distinct mid) as mid_counts,
                    ROUND(sum(amount)/	count(distinct oid)) as oid_pay
                    from (
                    select a.pid,a.name,a.proname,a.oid,a.quantity,a.amount,b.dt,b.merchant_name,b.mid
                    from tb_porder_line a
                    INNER JOIN 
                    (
                    SELECT 
                    case
                    when PAY_STATUS = 1 then payDates
                    when PAY_TYPE = 10002 then POST_DATE_STR
                    end dt,
                    oid,
                    merchant_name,
                    mid
                    FROM  tb_porder
                    where 1 = 1
                    and shop_code =   '",shop_code_,"'
                    and HANDLE_STATUS not in (5, 7)
                    and (PAY_STATUS = 1 or  PAY_TYPE = 10002)
                    ) b
                    ON a.oid = b.oid
                    WHERE 1=1
                    and date(b.dt)>='",dt()[1],"' 
                    and date(b.dt)<='",dt()[2],"'
                    ) x
                    group by date(dt) 
                    ")
        dbGetQuery0('ecommerce', sql)%>% 
            mutate(paydate = as.Date(paydate)) %>%
            select(
                `日期` = paydate,
                `通用商品名数` = proname_counts,
                `商品名数` = name_counts,
                `pid数` = pid_counts,
                # `商家数` = merchant_counts,
                `销售量` = quantity,
                `销售额` = amounts,
                `成单数` = oid_counts,
                `购买人数` = mid_counts,
                `客单价` = oid_pay
            )
    })
    # AND date(b.dt) >= '2016-10-18'
    # AND date(b.dt) <= '2016-10-31'
    output$daysales <- DT::renderDataTable({
        datatable(
            df(), 
            rownames = FALSE,
            selection = 'multiple',
            extensions = list(Scroller=list()),
            options = list(
                searching=TRUE,
                lengthChange = TRUE
            )
        )
    })
   
    
    output$trend1 <- renderPlotly({
        x<-df()
        p <- x%>% ggplot(aes_string(x[,1], field()))+geom_point(aes(color='2'))+geom_line(aes(color='4'))
        ggplotly(p)
    })
    
} 

 





#------------------------省份城市分析-------------------
provinceCityUI <- function(id) {
    
    ns <- NS(id)
    
    tagList(
        fluidRow(class='toolbar',
                 box(
                     title = '城市分析', status = 'primary', solidHeader = T, width = 12,
                     column(4, dateRangeInput(ns('pay_date'), '付款时间',  start = Sys.Date()-30, end = Sys.Date()-1))
                 )
        ),
        fluidRow(
            column(6,dataTableOutput(ns('tbl2')))
        )
    )
}
provinceCity <- function(input, output, session , username) {
    
    #付款日期值获取
    pay_date <- reactive({
        req(input$pay_date)
        input$pay_date
    }) 
    
     
    
    df1 <- reactive({
        
        shop_code_ <- username 
        
        
        sql <-
            paste0("
                    select coalesce(city,'未知') city, 
                        count(DISTINCT oid) as count_orders,
                        count(DISTINCT mid) as count_mids,
                        sum(sum_price) as      sum_price 
                 from (
                          SELECT
                            	CASE
                            WHEN PAY_STATUS = 1 THEN
                            	payDates
                            WHEN PAY_TYPE = 10002 THEN
                            	POST_DATE_STR
                            END dt,
                             oid,
                             mid,
                             sum_price,
                            replace(REPLACE(REPLACE(city_name,CHAR(10),''),CHAR(13),''),char(9),'') as city
                            FROM
                            	tb_porder
                            WHERE
                            	1 = 1
                            and shop_code =   '",shop_code_,"'
                            AND HANDLE_STATUS NOT IN (5, 7)
                            AND (
                            	(
                            		PAY_STATUS = 1
                            		AND date(payDates) BETWEEN '",pay_date()[1],"'  AND '",pay_date()[2],"' 
                            	)
                            	OR (
                            		PAY_TYPE = 10002
                            		AND date(POST_DATE_STR) BETWEEN '",pay_date()[1],"'  AND '",pay_date()[2],"' 
                            	)
                            )
    --                             and city_name is not NULL 
                            ) x
                             group by coalesce(city,'未知') 
                    ")
        dd1 <- dbGetQuery0('ecommerce',sql)
        
        df1 <- dd1 %>%mutate(
            #oid_rat = round( dd[,2]/sum(dd[,2]))
            sum_price = round(sum_price),
            oid_pay = round(dd1[,4]/dd1[,2])
        ) %>% rename(`城市`=city , `购买单数`=count_orders,`购买人数`=count_mids,`销售额`=sum_price,`客单价`=oid_pay)
    }) 
    
    
    output$tbl2 <- renderDataTable({
        datatable(
            df1() %>% arrange(desc(销售额) ),
            escape = FALSE,
            rownames = FALSE,
            selection = 'none',
            #filter = 'top',
            extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
            options = list(
                #   autoWidth = TRUE,
                #   columnDefs = list(list(width = '200px', targets = c(0))),
                searching=TRUE,
                #   deferRender=TRUE,
                #   scrollY=200,
                #   scrollX=TRUE,
                #   scrollCollapse = TRUE,
                #   order = list(list(0, 'asc')),
                #   language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
                pageLength = 20,
                lengthChange = TRUE
                #   initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
                #   fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
                # 
            )
        ) 
    })
    
    
} 



 










