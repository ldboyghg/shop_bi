# 界面框架
library(shiny)
library(shinydashboard)
# javascript与R交互
library(shinyjs)
# 浏览器localstorage保存登录信息
library(shinyStore)
# 透视表
library(dplyr)
library(tidyr)
# 日期处理
library(lubridate)
library(xts)
# 做图
library(ggplot2)
library(plotly)
#library(d3funnel)
library(forecast)
library(dygraphs)
library(showtext)
library(directlabels)
library(recharts)
library(REmap)
#library(treemap)
#library(d3treeR)
#library(viridis)
library(highcharter)
# 数据表格
library(DT)
library(rhandsontable)
# 加密
library(PKI)
# 数据库读写
library(DBI)
# csv读取
library(readr)

source('global.R', encoding='utf-8')

# colVis is used to hide the col that don't need
js_code <- '
        shinyjs.colVis = function(col) {xtbl.columns()[0].filter(function(x) { return col.indexOf(x) < 0 }).forEach(function(i){xtbl.column(i).visible(0)})}
        '

# prepare data
#stuff <- dbGetQuery0('bbf_shop', paste0("SELECT * FROM bbf_stuff"))
# localstorage加密key
pub_key <- PKI.load.key(file="./data/bbf.key.pub")
priv_key <- PKI.load.key(file="./data/bbf.key")

# 定义一个登录面板，后面多个判断逻辑重复使用
ui_login <- dashboardBody(
    wellPanel(
        id='loginpanel',
        textInput("username", icon('user', class='fa-fw'), placeholder='username', value = ''),
        passwordInput("password", icon('lock', class='fa-fw'), value = ''),
        tags$br(),
        actionButton("btn_login",label="登 录", icon=icon('sign-in')),
        actionButton("btn_reset",label="重 置", icon=icon('undo'), style='margin-left:20px;'),
        style = "width:500px; margin-top: 100px; margin-bottom: 400px; margin-left: auto; margin-right: auto;"
    )
)

# ui
ui = dashboardPage(
    header = dashboardHeader(
        title = img(src='./image/logo.png', width='150px;'),
        titleWidth = 180,
        dropdownMenu(type = 'messages', badgeStatus = 'success'),
        dropdownMenu(type='tasks', badgeStatus = 'warning')
    ),
    sidebar = dashboardSidebar(
        # 引入shinyjs
        useShinyjs(),
        extendShinyjs(text = js_code),
        # 初始化localStorage
        initStore("store", "shinyStore-bbf", priv_key),
        # ~~~~~~~~~~~~~~~~~~~~~~~~
        width = 180,
        uiOutput("userpanel"),
        sidebarMenu(id="tabs",
                    sidebarMenuOutput('sidebarmenu')
        )
    ),
    body = uiOutput('page'),
    title = '八百方商家经营分析系统',
    skin = 'red'
)
# server
server = function(input, output, session) {
    # put module here to avoid restart shiny server when modified module
    source('module.R', encoding='utf-8', local = TRUE)
    source('module_sales.R', encoding='utf-8', local = TRUE)
    source('module_product.R', encoding='utf-8', local = TRUE)
    source('module_customer.R', encoding='utf-8', local = TRUE)
    #财务结算---分账统计
    source('module_settle.R', encoding='utf-8', local = TRUE)
    #username <- as.character(isolate(input$username))
    

    # 登录验证逻辑
    observeEvent(input$btn_login, {
        username <- as.character(isolate(input$username))
        password <- as.character(isolate(input$password))
        user <- dbGetQuery0('bbf_shop', paste0("SELECT * FROM bbf_user WHERE status=1 AND username='",username,"' AND password='",digest::digest(paste0(username, password),'md5'),"'"))
        if (nrow(user) == 1){
            key <- pub_key
            # key <- NULL
            updateStore(session, "username", username, encrypt=key)
            #session5天效期
            expired = as.numeric(Sys.time())+5*24*60*60
            updateStore(session, "expired", expired, encrypt=key)
        } else {
            info('登录失败,请检查用户名和密码.')
            reset('password')
        }

    })
    # 重置按钮
    observeEvent(input$btn_reset, {
        reset('username')
        reset('password')
    })
    # 检测是否填写了登录信息
    observe({
        toggleState("btn_login", !is.null(input$username) && input$username != "" && !is.null(input$password) && input$password != "")
    })
    # 退出登录时清空session
    observeEvent(input$al_logout, {
        key <- pub_key
        # key <- NULL
        updateStore(session, "username", NULL, encrypt=key)
        # 退出登录则减去1天
        expired = as.numeric(Sys.time())-24*60*60
        updateStore(session, "expired", expired, encrypt=key)
    })
    # menu click
    onclick('msg1', {
        info('done')
    })

    # 登录验证后刷新UI
    observe({
        # 如果localstorage中的expired为空，表示第一次访问
        # 如果曾经访问过则不为空（不管session是否已过期）
        if (is.null(input$store$expired)){
            output$sidebarmenu <- renderMenu({
                sidebarMenu(
                    menuItem('请先登录', tabName = 'tn_null', icon = icon('dashboard')),
                    .list = list()
                )
            })
            output$page <- renderUI({
                ui_login
            })
        } else{
            # 如果localstorage中的expired存在且未过期则渲染菜单
            if (as.numeric(Sys.time()) < input$store$expired){
                # 验证usename：localstorage中的username跟数据库比对
                sql <- paste0("select * from bbf_user where username='", input$store$username, "' and status=1")
                uu <- dbGetQuery0('bbf_shop', sql)
                if (nrow(uu)==1) {
                    # 权限控制在这里实现
                    sql <- paste0("SELECT * FROM bbf_menu WHERE status=1 and privilege like '%", input$store$username, "%' order by orders")
                    menu <- dbGetQuery0('bbf_shop', sql)
                    if(nrow(menu)==0) {
                        menu0 <- TRUE
                        menu <- data.frame(menu1='权限受限', menu2='权限受限', status=1, orders=1, privilege='')
                    } else {
                        menu0 <- FALSE
                    }
                    msg <- apply(menu %>% arrange(orders) %>% group_by(menu1) %>% summarise(status=min(status)), 1, function(row1){
                        msg1 <- apply(menu %>% filter(menu1==row1[['menu1']], status==1), 1, function(row2){
                            menuSubItem(row2[['menu2']], tabName = paste0('tn_', row2[['menu2']]))
                        })
                        menuItem(
                            text = row1[['menu1']],
                            tabName =paste0('tn_', row1[['menu1']]),
                            icon = icon('th'),
                            .list=msg1
                        )
                    })
                    output$sidebarmenu <- renderMenu({
                        sidebarMenu(
                            menuItem(
                                text = '仪表盘',
                                tabName = ifelse(menu0, 'null', 'tn_dashboard'),
                                icon = icon('dashboard')
                            ),
                            .list = msg
                        )
                    })
                    
                    output$page <- renderUI({
                        dashboardBody(
                            includeCSS('./www/style.css'),
                            tabItems(
                                #             menu2             module       moduleID
                                #           ↓↓↓↓↓↓↓↓↓         ↓↓↓↓↓↓↓↓↓↓↓  ↓↓↓↓↓↓↓↓↓↓↓↓↓
                                tabItem('tn_dashboard',       dashboardUI('mid_dashboard')),
                                tabItem('tn_账号管理',        userTableUI('mid_user_table')),
                                tabItem('tn_系统菜单',        menuTableUI('mid_menu_table')),
                                tabItem('tn_reset_password',  resetPasswordUI('mid_reset_password')),
                                
                                
                                ##########销售分析#########
                                tabItem('tn_日小时销售',        salesHourUI('sales_hour')),
                                tabItem('tn_地域销售概况',        salesMapUI('mid_salesMap')),
                                
                                
                                
                                
                                
                                
                                
                                
                                #########商品分析############
                                tabItem('tn_热卖商品',        topProductUI('mid_top_product')),
                                tabItem('tn_品类优化',        proudctOptimizeUI('mid_proudctOptimize')),
                                tabItem('tn_商品通用名排名',        pronameProductUI('mid_proname_product')),
                                tabItem('tn_商品类型排名',        bcnameRankUI('mid_bcname_rank')),
                                tabItem('tn_销售品种数趋势',        saleskuTrendsUI('mid_sale_sku_Trends')),
                                tabItem('tn_省份城市分析',        provinceCityUI('mid_province_city')),
                                
                                
                                
                                
                                
                                #########用户分析############
                                tabItem('tn_客单价',          kdUI('mid_kd')),
                                
                                
                                
                                
                                
                                
                                
                                
                                
                                
                                
                                
                                ############财务结算############
                                ### 在线支付 ，不包含跨月退款
                                tabItem('tn_在线支付1',          fztj1UI('mid_fztj1')),
                                ### 在线支付-跨月退款
                                tabItem('tn_在线支付跨月退款',          fztj2UI('mid_fztj2')),
                                ###货到付款
                                tabItem('tn_货到付款',          fztj3UI('mid_fztj3'))
                                
                            )
                        )
                    })
                } else {
                    key <- pub_key
                    # key <- NULL
                    updateStore(session, "username", NULL, encrypt=key)
                    # 退出登录则减去1天
                    expired = as.numeric(Sys.time())-24*60*60
                    updateStore(session, "expired", expired, encrypt=key)
                }
            # 如果localstorage中的expired存在且已过期
            } else {
                output$sidebarmenu <- renderMenu({
                    sidebarMenu(
                        menuItem('请先登录', tabName = 'tn_null', icon = icon('dashboard')),
                        .list = list()
                    )
                })
                output$page <- renderUI({
                    ui_login
                })
            }
        }
    })
    # 初始化用户面板
    observe({
        output$userpanel = renderUI({
            # 如果expired为空则显现未登录状态
            # 如果当前时间小于expired说明session有效，现实登录状态
            if (is.null(input$store$expired)){
                sidebarUserPanel('Offline', image = "image/head.png")
            } else if (as.numeric(Sys.time()) < input$store$expired) {
                sidebarUserPanel(
                    a(input$store$username, href='#shiny-tab-tn_reset_password', `data-toggle`='tab', `data-value`='tn_reset_password'),
                    subtitle = actionLink('al_logout', 'Logout', icon=icon('sign-out')),
                    image = "image/head.png"
                )
            }
        })
    })
    
    
     
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # 监控面板
    callModule(module = dashboard,             id = 'mid_dashboard', input$store$username )
    # 重置密码
    callModule(module = resetPassword,         id = 'mid_reset_password', input$store$username )  # module的input与server的input不同，不能相互访问
  
    # 菜单管理
    callModule(module = menuTable,             id = 'mid_menu_table')
    # 用户管理
    callModule(module = userTable,             id = 'mid_user_table')
    
    
    
    
    
    
    
    ###############销售分析###############
    #日小时销售
    callModule(module = salesHour,       id = 'sales_hour', input$store$username )
    #地域销售概况
    callModule(module =  salesMap,       id = 'mid_salesMap' ,input$store$username)    
    
    
    
    
    
    
    
    
    
    ################商品分析##############
    # 热卖商品
    callModule(module = topProduct,            id = 'mid_top_product', input$store$username)
    #品类优化
    callModule(module = proudctOptimize,            id = 'mid_proudctOptimize', input$store$username)
    #tn_商品通用名排名
    callModule(module =  pronameProduct,       id = 'mid_proname_product' , input$store$username )
    #商品类型排名
    callModule(module =  bcnameRank,       id = 'mid_bcname_rank' )
    #tn_销售品种数趋势
    callModule(module =  saleskuTrends,       id = 'mid_sale_sku_Trends', input$store$username )
    #省份城市分析
    callModule(module = provinceCity,        id = 'mid_province_city', input$store$username)
    
    
    
     
    
    
    
    ################用户分析##############
    # 客单价
    callModule(module = kd,                    id = 'mid_kd' , input$store$username )
    
    
    
    
    ################财务结算##############
    # 在线支付 ，不包含跨月退款
    callModule(module = fztj1,                    id = 'mid_fztj1' , input$store$username )
    # 在线支付-跨月退款
    callModule(module = fztj2,                    id = 'mid_fztj2' , input$store$username )
    ###货到付款
    callModule(module = fztj3,                    id = 'mid_fztj3' , input$store$username ) 
    
  
    
    isolate({updateTabItems(session, "tabs", "tn_dashboard")})
}
# 入口
shinyApp(ui, server)
