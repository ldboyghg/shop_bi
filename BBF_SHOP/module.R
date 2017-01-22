# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# # 仪表盘
# dashboardUI <- function(id) {
#     ns <- NS(id)
#     
#     tagList(
#         fluidRow(class='toolbar', infoBoxOutput(ns('box1_1'), width = 3)  ) ,
#         fluidRow(class='toolbar',
#                  column(4, dateRangeInput(ns('dt'), '成交日期',  start = Sys.Date()-30, end = Sys.Date()-1))
#         ),
#     
#     fluidRow(
#         column(7, highchartOutput(ns('trend1')) ),
#         column(5, dataTableOutput(ns('tbl')) )
#     )
#     )
# }
# dashboard <- function(input, output, session, username) {
#     dt <- reactive({
#         req(input$dt)
#         input$dt
#     })
#     
#     df <- reactive({
# 
#     shop_code_ <- username 
#     sql <- paste0(" select merchant_name  from tb_merchant where merchant_code =   '",shop_code_,"'   ")
#     dbGetQuery0('ecommerce', sql)
#     }) 
#     
#     output$box1_1 <- renderInfoBox({
#         infoBox(
#             "商家", paste0(df()), icon = icon("list"),
#             color = "purple"
#         )
#     })
#     
#      
#     df1 <- reactive({
#         
#         shop_code_ <- username 
#         
#         sql <- paste0("
#                       SELECT
#                       CASE
#                       WHEN PAY_STATUS = 1 THEN
#                       DATE_FORMAT(payDates,'%Y%m%d')
#                       WHEN HANDLE_STATUS NOT IN (5, 7)
#                       AND PAY_TYPE = 10002 THEN
#                       DATE_FORMAT(POST_DATE_STR,'%Y%m%d')
#                       END dt,
#                       case 
#                       WHEN PAY_STATUS = 1 THEN
#                       1
#                       WHEN PAY_TYPE = 10002 THEN
#                       2
#                       END type ,
#                       count(t.oid) oids ,
#                       count(distinct t.mid) mids ,
#                       sum(sum_price) sum_price
#                       FROM
#                       tb_porder t
#                       WHERE
#                       1 = 1
#                       and shop_code =   '",shop_code_,"'  
#                       AND  HANDLE_STATUS NOT IN (5, 7)
#                       AND (
#                       (
#                       PAY_STATUS = 1
#                       AND date(payDates) >= '",dt()[1],"'
#                       AND date(payDates) <= '",dt()[2],"'
#                       )
#                       OR (
#                       PAY_TYPE = 10002
#                       AND date(POST_DATE_STR) >= '",dt()[1],"'
#                       AND date(POST_DATE_STR) <= '",dt()[2],"'
#                       )
#                       )
#                       group by 
#                       CASE
#                       WHEN PAY_STATUS = 1 THEN
#                       DATE_FORMAT(payDates,'%Y%m%d')
#                       WHEN PAY_TYPE = 10002 THEN
#                       DATE_FORMAT(POST_DATE_STR,'%Y%m%d')
#                       END ,
#                       case 
#                       WHEN PAY_STATUS = 1 THEN
#                       1
#                       WHEN HANDLE_STATUS NOT IN (5, 7)
#                       AND PAY_TYPE = 10002 THEN
#                       2
#                       END 
#                       ")
#         
#         #info(sql)
#         
#         x <- dbGetQuery0('ecommerce', sql)
#         
#         x
#     })
#     
#      
#     output$tbl <- DT::renderDataTable({
#         datatable(
#             df1(), 
#             rownames = FALSE,
#             selection = 'multiple',
#             extensions = list(Scroller=list()),
#             options = list(
#                 searching=FALSE,
#                 lengthChange = TRUE
#             )
#         )
#     })
#     
#     
#     output$trend1 <- renderHighchart({
#         hc <- highchart() %>%
#             hc_xAxis(categories = subset(df1(),type==1)$dt) %>%
#             hc_add_series(name = "货到付款", data = subset(df1(),type==2)$sum_price) %>%
#             hc_add_series(name = "在线支付", data = subset(df1(),type==1)$sum_price) %>%
#             hc_legend(align = "right", verticalAlign = "top",layout = "vertical", x = 0, y = 100) %>%
#             hc_title(text = "<b>付款方式趋势</b>",margin = 20, align = "center",style = list(color = "blue", useHTML = TRUE)) %>%
#             hc_credits(enabled = TRUE, text = "www.lonk.tomy.site", href = "http://jkunst.com")%>%
#             hc_tooltip(crosshairs = TRUE, backgroundColor = "#FCFFC5", shared = TRUE, borderWidth = 5) %>%
#             hc_exporting(enabled = TRUE) # enable exporting option
# 
#         hc %>% hc_add_theme(hc_theme_flat())
#     })
#     
#     
# }


# 仪表盘 网站整体指标
dashboardUI <- function(id) {
ns <- NS(id)

tagList(
    box(
        title = '网站整体指标', status = 'primary', solidHeader = T, width = 12,
        fluidRow(class='toolbar',
                 column(4, dateRangeInput(ns('dt'), '支付日期', start = Sys.Date()-7 , end = Sys.Date()-1))
        )
    ),
    br(),
    br(),
    br(),
    fluidRow(
        DT::dataTableOutput(ns('web_total1_sale'))
    ),
    
    fluidRow(
        box(column(12,dygraphOutput(ns('trend1')))),
        box(column(12,dygraphOutput(ns('trend2'))))
    ),

    fluidRow(
        box(column(12,dygraphOutput(ns('trend3'))))
    )
)

}


dashboard <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    
    # 总销售额、总订单数、平均销售额
    df <- reactive({
        
        shop_code_ <- username
        
        sql<-paste0("
                    select * from (
                    select count(distinct pid) dx_pids,
                    sum(quantity) quantity
                    from (select case
                    when PAY_STATUS = 1 then
                    payDates
                    when PAY_TYPE = 10002 then
                    POST_DATE_STR
                    end dt ,
                    a.oid,
                    b.pid ,
                    b.quantity
                    from tb_porder a
                    join tb_porder_line b on a.oid = b.oid
                    where 1 = 1
                    and a.shop_code =  '",shop_code_,"'  
                    and HANDLE_STATUS not in (5, 7) 
                    and (PAY_STATUS = 1 or PAY_TYPE = 10002) -- 货到付款
                    ) a
                    where 1 = 1
                    and date(dt) >=  '",dt()[1],"'
                    and date(dt) <=  '",dt()[2],"'
                    #and date(dt) >= '2016-03-01' 
                    #and date(dt) <= '2016-03-31'
                    ) x ,
                    (
                    select count(distinct a.pid ) jy_pids from tb_product_info a
                    where 1 = 1 
                    and a.shop_code =  '",shop_code_,"'  
                    and alive not in (0 ,2 )
                    #and POST_DATE <= '2016-09-31'
                    and POST_DATE <= '",dt()[2],"' 
                    ) y  ,
                    (
                    SELECT
                    sum(sum_price) AS sales_total,
                    count(DISTINCT OID) AS count_orders,
                    avg(sum_price) AS sales_avg
                    FROM
                    (
                    select case
                    when PAY_STATUS = 1 then	payDates
                    when PAY_TYPE = 10002 then POST_DATE_STR
                    end dt ,
                    oid,sum_price
                    from tb_porder 
                    where 1 = 1
                    and shop_code =   '",shop_code_,"'  
                    and HANDLE_STATUS not in (5, 7)
                    and (PAY_STATUS = 1 or PAY_TYPE = 10002) -- 货到付款
                    ) c
                    WHERE
                    1 = 1
                    And date(dt) >= '",dt()[1],"'
                    And date(dt) <= '",dt()[2],"'
                    #and date(dt) >= '2016-10-01' 
                    #and date(dt) <= '2016-10-30'
                    ) z
                    
                    ")
        dbGetQuery0('ecommerce', sql)%>%
            mutate(
                #oid_rat = round( dd[,2]/sum(dd[,2]))
                sales_total = round(sales_total),
                sales_avg = round(sales_avg)
            )%>%
            select(
                `销售额` = sales_total,
                `订单数量` = count_orders,
                `客单价` = sales_avg,
                `动销pid商品数` = dx_pids,
                `销售商品件数` = quantity,
                `在售pid商品数` = jy_pids
            )
    })
    
    output$web_total1_sale <- DT::renderDataTable({
        datatable(
            df(), 
            rownames = FALSE,
            selection = 'multiple',
            extensions = list(Scroller=list()),
            options = list(
                searching=FALSE,
                lengthChange = FALSE
            )
            
        ) 
    })
    
    # 返回：日期+ 指标
    df3 <- reactive({
         
        shop_code_ <- username 
        
        sql<-paste0("
                    select  date(dt) as dt1,sum(sum_price) as sales_total,count(DISTINCT OID ) as count_orders,
                    avg(sum_price) as sales_avg
                    from
                    (
                    select case
                    when PAY_STATUS = 1 then	payDates
                    when PAY_TYPE = 10002 then POST_DATE_STR
                    end dt ,
                    oid,sum_price
                    from tb_porder
                    where 1 = 1
                    and shop_code =   '",shop_code_,"'
                    and HANDLE_STATUS not in (5, 7)
                    and (PAY_STATUS = 1 or PAY_TYPE = 10002) -- 货到付款
                    ) c
                    where 1=1
                    and date(dt)>='",dt()[1],"'
                    and date(dt)<='",dt()[2],"'
                    #and date(dt) >= '2016-10-01'
                    #and date(dt) <= '2016-10-30'
                    group by dt1
                    ")

        dbGetQuery0('ecommerce', sql) %>%
            mutate(dt1 = as.Date(dt1))
    })

    # 做曲线图:销售额曲线

    output$trend1 <- renderDygraph({
        x <- df3() %>% select(dt1,sales_total)
        x <- as.xts(x[, 2], x[, 1])
        dygraph(x,"销售额趋势图")%>%
            dyRangeSelector(height = 20, strokeColor = "")
    })
    # 做曲线图:订单数量曲线  count_orders
    output$trend2 <- renderDygraph({
        x <- df3() %>% select(dt1,count_orders)
        x <- as.xts(x[, 2], x[, 1])
        dygraph(x,"订单量趋势图")%>%
            dyRangeSelector(height = 20, strokeColor = "")
    })

    # 做曲线图：客单价曲线 sales_avg
    output$trend3 <- renderDygraph({
        x <- df3() %>% select(dt1,sales_avg)
        x <- as.xts(x[, 2], x[, 1])
        dygraph(x,"平均客单价趋势图")%>%
            dyRangeSelector(height = 20, strokeColor = "")
    })

    
} 







# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 帐号表
userTableUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        column(
            4,
            titlePanel("账号表"),
            helpText(icon('warning'), br(), '1. 此表由开发更新', br(), ''),
            div(
                actionButton(ns("btn_save"), "保 存", icon = icon('save')),
                actionButton(ns("btn_reload"), "刷 新", icon = icon('refresh'))
            )
        ),
        column(
            8,
            rHandsontableOutput(ns("hot"))
        )
    )
}
userTable <- function(input, output, session) {
    values = list()
    setHot = function(x) {
        values[["hot"]] <<- x
    }
    
    observeEvent(input$btn_save, {
        if (!is.null(values[["hot"]])) {
            dbGetQuery0('bbf_shop', "delete from bbf_user")
            dbWriteTable0('bbf_shop', 'bbf_user', values[["hot"]])
            info('更新完成')
        }
    })
    observeEvent(input$btn_reload, {
        reset(('hot'))
    })
    
    output$hot = renderRHandsontable({
        if (!is.null(input$hot)) {
            df = hot_to_r(input$hot)
        } else {
            df = dbReadTable0('bbf_shop', 'bbf_user')
        }
        
        setHot(df)
        rhandsontable(df) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
}
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 菜单表
menuTableUI <- function(id) {
    ns <- NS(id)
    
    fluidPage(
        column(
            4,
            titlePanel("菜单表"),
            helpText(icon('warning'), br(), '1. 此表由开发更新', br(), '2. 菜单名更新会导致菜单无法访问'),
            div(
                actionButton(ns("btn_save"), "保 存", icon = icon('save')),
                actionButton(ns("btn_reload"), "刷 新", icon = icon('refresh'))
            )
        ),
        column(
            8,
            rHandsontableOutput(ns("hot"))
        )
    )
}
menuTable <- function(input, output, session) {
    values = list()
    setHot = function(x) {
        values[["hot"]] <<- x
    }
    
    observeEvent(input$btn_save, {
        if (!is.null(values[["hot"]])) {
            dbGetQuery0('bbf_shop', "delete from bbf_menu")
            dbWriteTable0('bbf_shop', 'bbf_menu', values[["hot"]])
            info('更新完成')
        }
    })
    observeEvent(input$btn_reload, {
        reset(('hot'))
    })
    output$hot = renderRHandsontable({
        if (!is.null(input$hot)) {
            df = hot_to_r(input$hot)
        } else {
            df = dbReadTable0('bbf_shop', 'bbf_menu') %>% arrange(menu1)
        }
        
        setHot(df)
        rhandsontable(df) %>%
            hot_table(highlightCol = TRUE, highlightRow = TRUE)
    })
}


# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 密码重置
resetPasswordUI <- function(id) {
    ns <- NS(id)
    
    tagList(
        h3('修改密码'),
        textInput(ns('old_pwd'), '原密码', value = ''),
        textInput(ns('new_pwd'), '新密码', value = ''),
        actionButton(ns('btn_reset_password'), '修 改', icon=icon('refresh'))
    )
}
resetPassword <- function(input, output, session, username) {
    old_pwd <- reactive({
        req(input$old_pwd)
        input$old_pwd
    })
    new_pwd <- reactive({
        req(input$new_pwd)
        input$new_pwd
    })
    observeEvent(input$btn_reset_password, {
        password <- as.character(isolate(input$old_pwd))
        user <- dbGetQuery0('bbf_shop', paste0("SELECT * FROM bbf_user WHERE status=1 AND username='",username,"' AND password='",digest::digest(paste0(username, password),'md5'),"'"))
        if (nrow(user) == 1){
            dbGetQuery0('bbf_shop', paste0("update bbf_user set password='",digest::digest(paste0(username, new_pwd()),'md5'),"' where username='", username,"'"))
            info('密码修改成功')
            reset('old_pwd')
            reset('new_pwd')
        } else {
            info('原密码不匹配')
        }
    })
}