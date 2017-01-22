# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 部门日报测试   报表异常
deptDailyReportUI1 <- function(id) {
    ns <- NS(id)
    tagList(
        fluidRow(class='toolbar',
                 column(2, selectInput(ns('dept'), '部门', choices = c('客户发展部', '客户增值部'))),
                 column(2, selectInput(ns('team'), '分组', choices = c('全部', '发展1', '发展2', '发展3'))),
                 column(2, dateInput(ns('dt'), '日期', value = Sys.Date()-1, max = Sys.Date()-1), min='2016-03-01'),
                 column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'), actionButton(ns('btn_format_dt'), '精简', class = 'btn_nowrap', icon = icon('th')))
        ),
        fluidRow(
            column(12, DT::dataTableOutput(ns('tbl')))
        )
    )
}
deptDailyReport1 <- function(input, output, session) {
    dept <- reactive({
        req(input$dept)
        input$dept
    })
    team <- reactive({
        req(input$team)
        if(input$team=='全部') {
            c('发展1', '发展2', '发展3', '增值1')
        } else{
            input$team
        }
    })
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    
    observeEvent(input$dept, {
        if (dept()=='客户发展部') {
            updateSelectInput(session, 'team', choices = c('全部', '发展1', '发展2', '发展3'))
        }
        else if(dept()=='客户增值部') {
            updateSelectInput(session, 'team', choices = c('全部', '增值1'))
        }
    })
    
    df <- reactive({
        stuff <- dbGetQuery0('bbf_shiny', 'select * from bbf_stuff')
        stuff <- stuff %>% filter(team %in% team())
        
        sql <- paste0("SELECT orderno, substring(post_date_str, 1, 10) as dt, actual_pay as pay, isback, payname, pay_status,mid,adminaccount,handle_status FROM tb_porder WHERE isback in (1, 5) AND substring(post_date_str,1,10)>='",dt(),"'")
        dd <- dbGetQuery0('ecommerce', sql) %>%
            mutate(
                dt = as.character(dt),
                payname = ifelse(payname %in% c('货到付款'), '货到付款', '在线支付'),
                pay_status = ifelse(payname=='货到付款', 1, pay_status)
            )
        # isback==1 发展订单（支付+下单）
        # isback==5 增值订单（支付+下单）
        # 在线＋到付
        dd1 <- dd %>% 
            filter(handle_status %in% c(0, 1, 4, 6, 99), pay_status==1, !is.na(payname)) %>% 
            group_by(dt, adminaccount, payname) %>%
            summarise(
                pay=sum(pay),
                cnt=n_distinct(orderno)
            ) %>% 
            rename(金额=pay, 单数=cnt) %>% 
            gather(variable, value, -(dt:payname)) %>%
            unite(temp, payname, variable) %>%
            spread(temp, value) %>% 
            mutate(
                `总成交_金额` = sum(`在线支付_金额`, `货到付款_金额`, na.rm=TRUE),
                `总成交_单数` = sum(`在线支付_单数`, `货到付款_单数`, na.rm=TRUE)
            ) %>% 
            select(`在线支付_单数`, `货到付款_单数`, `总成交_单数`, `在线支付_金额`, `货到付款_金额`, `总成交_金额`)
        # 下单总额
        dd2 <- dd %>% 
            group_by(dt, adminaccount) %>%
            summarise(
                pay=sum(pay),
                cnt=n_distinct(orderno)
            ) %>% 
            rename(`下单金额`=pay, `下单数`=cnt)
        
        dd3 <- dd2 %>%
            left_join(dd1, by=c('dt'='dt', 'adminaccount'='adminaccount'))
        #fix na issue
        dd3[is.na(dd3)] <- 0
        dd3 <- dd3 %>% 
            mutate(
                `未付款金额` = `下单金额`-`总成交_金额`,
                `未付款单量` = `下单数`-`总成交_单数`,
                `订单确认%` = `总成交_金额`/`下单金额`
            )
        
        ##update by 20160629 按黄文宇需求调整报表字段
        dd3 <- dd3  %>%
            select(dt,adminaccount,`下单金额`,`在线支付_金额`,`货到付款_金额`,`总成交_金额`,`未付款金额`,`下单数`,`在线支付_单数`,`货到付款_单数`,`总成交_单数`,`未付款单量`,`订单确认%`
            )
        
        
        
        # 呼叫中心
        cc <- dbGetQuery0('bbf_shiny', paste0("SELECT * FROM bbf_cc_call_report_dept WHERE dt='", dt(), "'"))
        # 以stuff表筛选部门后作为合并表的基础，这样可防止漏掉记录
        #cc <- cc %>% rename(stuff_id=`工号`) %>% left_join(stuff, by='stuff_id') %>% rename(`工号`=stuff_id)
        cc <- cc %>% rename(name=`座席`, stuff_id=`工号`)
        cc <- stuff %>% left_join(cc, by=c('name'='name', 'stuff_id'='stuff_id'))
        cc <- cc %>%
            filter(dept == dept()) %>%
            mutate(
                dt = ifelse(is.na(dt), as.character(dt[!is.na(dt)][1]), as.character(dt)),
                `呼入通话平均时长` = hour(hms(`呼入通话平均时长`))*60+minute(hms(`呼入通话平均时长`))+second(hms(`呼入通话平均时长`))/60,
                `呼入通话总时长` = hour(hms(`呼入通话总时长`))+minute(hms(`呼入通话总时长`))/60+second(hms(`呼入通话总时长`))/3600,
                `呼入总数` = as.numeric(`呼入总数`),
                `呼入接通数` = as.numeric(`呼入接通数`),
                `外呼通话平均时长` = hour(hms(`外呼通话平均时长`))*60+minute(hms(`外呼通话平均时长`))+second(hms(`外呼通话平均时长`))/60,
                `外呼通话总时长` = hour(hms(`外呼通话总时长`))+minute(hms(`外呼通话总时长`))/60+second(hms(`外呼通话总时长`))/3600,
                `外呼总数` = as.numeric(`外呼总数`),
                `外呼成功数` = as.numeric(`外呼成功数`)
            ) %>% 
            group_by(`dt`, `name`) %>%
            summarise(
                `stuff_id`         = max(stuff_id),
                `name_py`          = max(name_py),
                `呼入总数`         = sum(`呼入总数`, na.rm=TRUE),
                `呼入接通数`       = sum(`呼入接通数`, na.rm=TRUE),
                `呼入接通率`       = `呼入接通数`/`呼入总数`,
                `呼入通话平均时长` = mean(`呼入通话平均时长`, na.rm=TRUE),
                `呼入通话总时长`   = sum(`呼入通话总时长`, na.rm=TRUE),
                `外呼总数`         = sum(`外呼总数`, na.rm=TRUE),
                `外呼成功数`       = sum(`外呼成功数`, na.rm=TRUE),
                `外呼接通率`       = `外呼成功数`/`外呼总数`,
                `外呼通话平均时长` = mean(`外呼通话平均时长`, na.rm=TRUE),
                `外呼通话总时长`   = sum(`外呼通话总时长`, na.rm=TRUE),
                `总通时(H)`        = `呼入通话总时长`+`外呼通话总时长`
            ) %>% 
            select(dt, name, `name_py`, `工号`=stuff_id, `呼入`=`呼入总数`, `呼入接通数`, `呼入接通率`, `呼入平均通时(M)`=`呼入通话平均时长`, `呼入通时(H)`=`呼入通话总时长`, `外呼`=`外呼总数`, `外呼接通数`=`外呼成功数`, `外呼接通率`, `外呼平均通时(M)`=`外呼通话平均时长`, `外呼通时(H)`=`外呼通话总时长`, `总通时(H)`)
        
        
        
        # 小能
        xn <- dbGetQuery0('bbf_shiny', paste0("SELECT * FROM bbf_xn_daily where dt='",dt(),"'"))
        # xn <- xn %>% rename(name=`用户_商户`) %>% left_join(stuff, by='name') %>% rename(`用户_商户`=name)
        xn <- xn %>% rename(name=`用户_商户`)
        xn <- stuff %>% left_join(xn, by='name')
        xn <- xn %>% 
            filter(dept == dept()) %>% 
            group_by(name) %>% 
            mutate(
                dt = ifelse(is.na(dt), as.character(dt[!is.na(dt)][1]), as.character(dt)),
                `有效咨询%` = `有效咨询`/`咨询总量`,
                `无效咨询%` = `无效咨询`/`咨询总量`
            ) %>% 
            select(dt, name, `有效咨询`=`有效咨询`, `有效咨询%`, `无效咨询`=`无效咨询`, `无效咨询%`, `咨询总量`=`咨询总量`, `首次响应时间`, `平均响应时间`, `满意度`)
        
        df <- cc %>% 
            left_join(dd3, by=c('dt'='dt', 'name_py'='adminaccount')) %>%
            left_join(xn, by=c('dt'='dt', 'name'='name')) %>% 
            rename(`日期`=dt, `座席`=name) %>%
            select(-name_py)
        
        # fixed na issue
        df[is.na(df)] <- 0
        df <- df %>%
            mutate(
                `满意度` = as.numeric(sub('%', '', `满意度`))/100,
                `转化率` = ifelse(`呼入接通数`+`外呼接通数`+`咨询总量`==0, 0, `总成交_单数`/(`呼入接通数`+`外呼接通数`+`咨询总量`))
            )
        df[is.na(df)] <- 0
        df %>% 
            add_row(
                `日期`            = '合计',
                `座席`            = '',
                `工号`            = 8888,
                `呼入`            = sum(df$`呼入`),
                `呼入接通数`      = sum(df$`呼入接通数`),
                `呼入接通率`      = sum(df$`呼入接通数`)/sum(df$`呼入`),
                `呼入平均通时(M)` = mean(df$`呼入平均通时(M)`),
                `呼入通时(H)`     = sum(df$`呼入通时(H)`),
                `外呼`            = sum(df$`外呼`),
                `外呼接通数`      = sum(df$`外呼接通数`),
                `外呼接通率`      = sum(df$`外呼接通数`)/sum(df$`外呼`),
                `外呼平均通时(M)` = mean(df$`外呼平均通时(M)`),
                `外呼通时(H)`     = sum(df$`外呼通时(H)`),
                `总通时(H)`       = sum(df$`总通时(H)`),
                `下单金额`        = sum(df$`下单金额`),
                
                `在线支付_金额`   = sum(df$`在线支付_金额`),
                `货到付款_金额`   = sum(df$`货到付款_金额`),
                `总成交_金额`     = sum(df$`总成交_金额`),
                `未付款金额`      = sum(df$`未付款金额`),
                
                ##update by 20160629 按黄文宇需求调整报表字段
                `下单数`          = sum(df$`下单数`),
                `在线支付_单数`   = sum(df$`在线支付_单数`),
                `货到付款_单数`   = sum(df$`货到付款_单数`),
                `总成交_单数`     = sum(df$`总成交_单数`),
                
                `未付款单量`      = sum(df$`未付款单量`),
                `订单确认%`       = sum(df$`总成交_金额`)/sum(df$`下单金额`),
                `有效咨询`        = sum(df$`有效咨询`),
                `有效咨询%`       = sum(df$`有效咨询`)/sum(df$`咨询总量`),
                `无效咨询`        = sum(df$`无效咨询`),
                `无效咨询%`       = sum(df$`无效咨询`)/sum(df$`咨询总量`),
                `咨询总量`        = sum(df$`咨询总量`),
                `首次响应时间`    = as.integer(mean(df$`首次响应时间`)),
                `平均响应时间`    = as.integer(mean(df$`平均响应时间`)),
                `满意度`          = mean(df$`满意度`, na.rm=TRUE),
                `转化率`          = ifelse(sum(df$`呼入接通数`)+sum(df$`外呼接通数`)+sum(df$`咨询总量`)==0, 0, sum(df$`总成交_单数`)/(sum(df$`呼入接通数`)+sum(df$`外呼接通数`)+sum(df$`咨询总量`)))
            )
        
    })
    # 精简列
    observeEvent(input$btn_format_dt, {
        #js$colVis(0, 1, 2, 4, 9, 18, 19, 20, 21, 22, 29, 30, 31, 33)
        #因为黄文宇调整字段，所有精简列也需要调整字段顺序
        js$colVis(0, 1, 2, 4, 9, 22, 15, 16, 17, 18, 29, 30, 31, 33)
    })
    output$tbl <- DT::renderDataTable({
        datatable(
            df(),
            escape = FALSE,
            rownames = FALSE,
            selection = 'none',
            #extensions = c('FixedColumns', 'Scroller'),
            extensions = list(FixedColumns = list(leftColumns = 2), Scroller=list()),
            # save instance of this table to global window, so then we can use it for colVis
            callback = JS("window.xtbl = table;"),
            options = list(
                autoWidth = TRUE,
                columnDefs = list(list(width = '60px', targets = c(0, 1))),
                searching=TRUE,
                search = list(regex = TRUE, caseInsensitive = FALSE),
                deferRender=TRUE,
                scrollY=200,
                scrollX=TRUE,
                scrollCollapse = TRUE,
                order = list(list(2, 'asc')),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
                pageLength = 100,
                lengthChange = FALSE,
                initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
                fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
            )  # options end
        ) %>%  # datatable end
            formatRound(c('外呼平均通时(M)', '外呼通时(H)', '呼入平均通时(M)', '呼入通时(H)', '总通时(H)', '总成交_金额', '未付款金额'), 2) %>% 
            formatPercentage(c('呼入接通率','外呼接通率', '满意度', '有效咨询%', '无效咨询%', '订单确认%', '转化率'), 2)
    })
    
    output$btn_export <- downloadHandler(paste('bbf-data-dept-',Sys.Date(),'.csv',sep=''), content = function(file) {
        if(TRUE){
            tmp <- df()[input$tbl_rows_all, , drop = FALSE]
            write.csv(tmp, file, fileEncoding='gbk',row.names=FALSE)
        } else {
            write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
        }
        
    })
}





# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 转单报表测试
transferReportUI1 <- function(id){
    ns <- NS(id)
    
    tagList(
        fluidRow(class='toolbar',
                 column(1, selectInput(ns('dt_type'), '日期类型', choices = c('支付'='post_date', '转单'='create_date', '承接'='accept_date', '审核'='check_date'))),
                 column(2, dateRangeInput(ns('dt'), '日期', language='zh_CN', start = Sys.Date()-months(1), end = Sys.Date())),
                 column(1, numericInput(ns('create_hour'), '转单>=X天', value = -1)),
                 column(1, numericInput(ns('check_hour'), '审核>=X天', value = -1)),
                 column(1, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
        ),
        #sbr(),br(),
        fluidRow(
            DT::dataTableOutput(ns('tbl'))
        )
    )
}
transferReport1 <- function(input, output, session) {
    dt_type <- reactive({
        req(input$dt_type)
        input$dt_type
    })
    
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    
    df <- reactive({
        # 计算转单时间改下单时间为支付时间，避免商家使用客户支付晚了作为借口
        #20160804 添加interval_day1,interval_day2两个字段
        sql <- paste0("
                      SELECT
                      cast(a.orderno as char) as orderno,
                      case when length(paydates)>5 then paydates else b.post_date_str end as post_date,
                      a.create_date,
                      case when length(paydates)>5 then DATEDIFF( a.create_date , paydates) 
                      else DATEDIFF(a.create_date , b.post_date_str)
                      end as interval_day1,
                      a.accept_date,
                      a.check_date,
                      DATEDIFF(a.check_date , a.accept_date)  interval_day2,
                      a.status,
                      a.old_merchant,
                      a.new_merchant,
                      a.fail_reason,
                      a.check_status,
                      a.check_user,
                      b.payname,
                      b.actual_pay,
                      b.name,
                      b.mobile,
                      b.account
                      FROM
                      tb_porder_transfer a
                      INNER JOIN tb_porder b ON a.orderno = b.orderno
                      ")
        tmp <- dbGetQuery0('ecommerce', sql)
        tmp <- tmp %>%
            # 日期跟NULL做difftime操作会产生NULL
            mutate(
                post_date = as.POSIXct(post_date),
                payname = ifelse(payname %in% c('货到付款'), '是', '否'),
                #`转单用时` = as.integer(ifelse(is.na(create_date) | is.na(post_date), -1, as.numeric(difftime(create_date, post_date, units = 'day')))),
                `转单用时` = as.numeric(interval_day1),
                #`审核用时` = as.integer(ifelse(is.na(check_date) | is.na(accept_date), -1, as.numeric(difftime(check_date, accept_date, units = 'day')))),
                `审核用时` = as.numeric(interval_day2),
                post_date = as.Date(post_date, '%Y-%m-%d'),
                create_date = as.Date(create_date, '%Y-%m-%d'),
                accept_date = as.Date(accept_date, '%Y-%m-%d'),
                check_date = as.Date(check_date, '%Y-%m-%d'),
                `收货人信息` = paste(name, mobile, sep='/'),
                status = c('审核中', '未承接', '转单成功','转单失败')[status+1]  # 0平台审核中,1未承接，2转单成功,3转单失败
            ) %>%
            group_by(orderno) %>% 
            arrange(orderno, create_date) %>% 
            mutate(`第几次转单`=seq_along(orderno)) %>% 
            # 以下过滤会导致部分记录丢失：平台取消转单的记录是没有承接和审核时间的
            filter_(
                paste0('`转单用时` >= ', input$create_hour),
                paste0('`审核用时` >= ', input$check_hour),
                paste0(dt_type(), ' >= "', dt()[1], '"'),
                paste0(dt_type(), ' <= "', dt()[2], '"')
            ) %>% 
            select(订单编号=orderno, 到付=payname,`收货人信息`, `第几次转单`, `审核人`=check_user, `转单用时(天)`=`转单用时`, `审核用时(天)`=`审核用时`,支付=post_date, 转单=create_date, 承接=accept_date, 审核=check_date, 原商家=old_merchant, 承接商家=new_merchant, 订单金额=actual_pay, 审核状态=status, 原因=fail_reason)
    })
    
    output$btn_export <- downloadHandler(paste('bbf-data-transfer-',Sys.Date(),'.csv',sep=''), content = function(file) {
        if(TRUE){
            tmp <- df()[input$tbl_rows_all, , drop = FALSE] %>% ungroup() %>% mutate(`订单编号`=paste0("'", as.character(`订单编号`)))
            write.csv(tmp, file, fileEncoding='gbk', row.names=FALSE)
        } else {
            write.csv('无数据下载权限', file, fileEncoding='gbk', row.names = FALSE, col.names = FALSE)
        }
        
    })
    
    output$tbl <- DT::renderDataTable({
        datatable(
            df(),
            escape = FALSE,
            rownames = FALSE,
            selection = 'none',
            #filter = 'top',
            #extensions = c('FixedColumns', 'Scroller'),
            extensions = list(FixedColumns = list(leftColumns = 1), Scroller=list()),
            options = list(
                autoWidth = TRUE,
                columnDefs = list(
                    list(width = '120px', targets = c(0)),
                    list(targets = c(2,4,5,6,7,8,9,12), searchable = FALSE)
                ),
                searching=TRUE,
                deferRender=TRUE,
                scrollY=200,
                scrollX='100%',
                scrollCollapse = TRUE,
                order = list(list(0, 'asc')),
                language = list(url = '//cdn.datatables.net/plug-ins/1.10.7/i18n/Chinese.json'),
                pageLength = 200,
                lengthChange = FALSE,
                initComplete = JS("function(settings, json) {$('.dataTables_scrollBody').attr('style', 'overflow: auto; width: 100%;height: '+($(window).height()-210)+'px !important');}"),
                fnDrawCallback = JS('function (oSettings, json) {oSettings.oScroll.sY = $(window).height()-210;$.each(oSettings.aoColumns, function(index, value){value.asSorting = ["desc", "asc"]})}')
            )  # options end
        ) %>%  # datatable end
            formatRound(c('转单用时(天)', '审核用时(天)'), 0)
    })
}









# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 小能及呼叫中心数据上传
csvUploadUI1 <- function(id) {
    ns <- NS(id)
    
    tagList(
        h2('上传小能csv文件'),
        selectInput(ns('type'), '类型', choices = c('请选择', '小能'), selected = '请选择'),
        fileInput(ns("csvfile"), 'CSV文件', accept = c('text/csv', 'text/comma-separated-values','text/tab-separated-values','text/plain', '.csv','.txt')),
        actionButton(ns('upload'), '保 存', icon = icon('upload'))
    )
}
csvUpload1 <- function(input, output, session) {
    type <- reactive({
        req(input$type)
        input$type
    })
    csvfile <- reactive({
        req(input$csvfile)
        enable('upload')
        input$csvfile
    })
    # 监听类型选择下拉框动作
    observe({
        # 选择了类型才显示选择文件按钮以及将上传按钮改为可用状态
        if(type() == '请选择') {
            disable('csvfile')
            disable('upload')
        } else {
            enable('csvfile')
        }
        logjs(csvfile()$datapath)
        logjs(csvfile()$name)
    })
    
    # 监听上传按钮点击动作
    observeEvent(input$upload, {
        #req(input$csvfile)
        # 如果未上传文件就点击了上传按钮, 直接浏览器弹窗提示
        if(is.null(csvfile())){
            info('还没选择要上传的CSV文件')
            return(NULL)
        }
        # 读取上传的文件
        # 切记dbWriteTable是不支持dpyr的tbl_df格式 需要转换为data.frame
        if(type() == '小能') {
            
            
            
            tmp <- read_csv(csvfile()$datapath, col_types = list(col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(),col_character(), col_character())) %>%
                #20160720新版本的字段名名称与数据库中的字段名称有出入，需要修改
                #rename(`被转接咨询`=`转入咨询量`,`质检咨询量`=`已质检量`)
                
                #update by gonghg  20160906表头有做修改
                # 导出报表删除了   平均咨询用时 , 平均消息条数, 当天内下单 3个字段
                
                rename(`被转接咨询`=`转入咨询`,`质检咨询量`=`已质检量`,`未评分`=`未评价次数`) 
                
            #x <- as.data.frame(tmp)
            
            #info(x$dt)
            info(tmp$dt[1])
                # delete record whith dt=tmp$dt
            dbGetQuery0('bbf_shiny', paste0("DELETE FROM bbf_xn_daily_test WHERE dt='",tmp$dt[1],"'"))
            dbWriteTable0(db = 'bbf_shiny', tbl = 'bbf_xn_daily_test', df = as.data.frame(tmp))
            
            dbGetQuery0( 'bbf_shiny', paste0("
            insert into bbf_xn_daily(用户_商户,                                          咨询总量,
                                          有效咨询,                                          无效咨询,
                                          接通率,                                          首次响应时间,
                                          平均响应时间,                                          留言总量,
                                          已处理留言,                                          留言处理率,
                                          _24小时处理量,                                          _24小时处理率,
                                          添加CRM,                                          被转接咨询,
                                          已总结,                                         未总结,                             登录时长,
                                          在线时长,                                          忙碌时长,
                                          离开时长,                                          已购买咨询,
                                          订单金额,                                          支付订单,
                                          咨询下单用户,                                          有效咨询用户,
                                          支付率,                                          满意度,
                                          非常满意,                                          满意,
                                          一般,                                          不满意,
                                          很不满意,                                          未评分,
                                          质检咨询量,                                          优,
                                          良,                                          差,合格,dt)
            select *  from bbf_xn_daily_test WHERE dt='",tmp$dt[1],"' ")
            )
        }
        # 重置类型选择以及文件选择菜单
        # # 浏览器弹窗提示更新成功
         reset('type')
         reset('csvfile')
         info('数据更新成功.')
    })
}

