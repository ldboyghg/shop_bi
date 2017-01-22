# 分账统计-----------在线支付(不含跨月退款)
fztj1UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        helpText(icon('warning'), '在线支付(不含跨月退款)'),
        fluidRow(class='toolbar',
                 column(2, selectInput(ns('dt'), '发货时间', choices =  substr(as.character(seq(Sys.Date()-days(180) , by='month', length.out = 12)), 1, 7), selected = substr(Sys.Date()-months(1),1,7))),
                 column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
        ), 
        fluidRow(
            DT::dataTableOutput(ns('tbl'))
        )
    )
}
fztj1 <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    
    df <- reactive({
        
        shop_code_ <- username 
         
        
        dd <- dbGetQuery0('ecommerce', paste0("
                        select distinct oid , orderno, BATCH_OID, send_month, POST_DATE_STR , payDates , payName,
          HANDLE_STATUS ,iscps ,merchant_name , pid , name ,
                                              '11' aa,#商家货号
                                              ischufang ,ctitle0 ,ctitle2 ,specification ,QUANTITY ,
                                              SHOP_price ,AMOUNT,
                                              round(share_rat*SUM_PRICE ,4 )+deliver_share SUM_PRICE , 
                                              round(share_rat*ACTUAL_PAY ,4 ) ACTUAL_PAY ,
                                              round(share_rat*tot_amount , 4 ) tot_amount ,
                                              'bb' bb ,#是否修改商品金额
                                              deliver_share DEL_TYPE_COST, 
                                              '111' cc, # 是否修改运费 
                                              round(share_rat*shopCouponAmount , 4 ) shopCouponAmount ,
                                              coupon_share couponAmount,
                                              score_share score_to_cost,
                                              cut_amount,
                                              send_time_str,
                                              dualstatus ,
                                              tk_share tkje ,
                                              tksj ,
                                              refundcontent ,
                                              yongjinlv ,
                                              round(case 
                                              when tk_flag = 0 then AMOUNT * yongjinlv      #不退款
                                              when tk_flag = 1 then (AMOUNT -tk_share ) * yongjinlv      #退款  
                                              end ,4) serv_fee , 
                                              round(case   
                                              when tk_flag = 0 then actual_pay_share + coupon_share + score_share - ( AMOUNT*yongjinlv )     #不退款   应付商家款 = 到账金额+红包+积分-服务费  
                                              when tk_flag = 1 and tk_type = 1   #退款  且 只退运费   不扣减服务费    
                                              then actual_pay_share + coupon_share + score_share - deliver_share - ( AMOUNT*yongjinlv )        
                                              when tk_flag = 1 and tk_type = 0   #退款 
                                              then actual_pay_share + if( coupon_use_flag = 1 ,coupon_share ,0) + if( score_use_flag = 1 ,score_share ,0)  - tk_share - ((amount-  tk_share )*yongjinlv )  
                                              end ,4) yf_shop_fee 
          from (
          SELECT
          ##订单详情
          tbp.oid ,concat('''',tbp.orderno) orderno ,tbp.BATCH_OID,DATE_FORMAT(tbp.send_time_str, '%m') send_month, tbp.POST_DATE_STR , tbp.payDates,tbp.payName,
          CASE
          WHEN HANDLE_STATUS = 0 THEN
          '待处理'
          WHEN HANDLE_STATUS = 1 THEN
          '正在配货'
          WHEN HANDLE_STATUS = 4 THEN
          '交易成功'
          WHEN HANDLE_STATUS = 5 THEN
          '交易取消'
          WHEN HANDLE_STATUS = 6 THEN
          '已收货'
          WHEN HANDLE_STATUS = 7 THEN
          '交易关闭'
          WHEN HANDLE_STATUS = 97 THEN
          '已退货'
          WHEN HANDLE_STATUS = 99 THEN
          '已发货）'
          END HANDLE_STATUS,
          case when tbcp.oid is not null then '是' else '否' end iscps,
          tbp.merchant_name ,
          tbpl.pid , 
          tbpl.name ,
          '1111111111' aa , #商品货号 
          ##商品
          CASE
          WHEN p.ischufang = 0 THEN
          '非药物'
          WHEN p.ischufang = 1 THEN
          '处方药'
          WHEN p.ischufang = 2 THEN
          '非处方药'
          END AS ischufang ,
          p.ctitle0 ,
          p.ctitle2 ,
          p.specification ,
          ##订单金额
          ifnull(tbpl.QUANTITY,0) QUANTITY,
          -- tbpl.SHOP_CODE,
          p.shop_price,
          ifnull(tbpl.AMOUNT,0) AMOUNT,
          ifnull(tbp.SUM_PRICE,0) SUM_PRICE,
          ifnull(tbp.ACTUAL_PAY,0) ACTUAL_PAY,
          ifnull(tm.tot_amount,0) tot_amount,
          '1111111111' bb ,-- 是否修改商品金额
          ifnull(tbp.DEL_TYPE_COST,0) DEL_TYPE_COST,
          '111111111' cc ,-- 是否修改运费 ,
          improve_cost shopCouponAmount ,
          ifnull(tbp.shopCouponAmount,0) couponAmount,
          #ifnull(tbp.couponAmount,0) couponAmount,
          ifnull(tbp.score_to_cost/100,0) score_to_cost,
          ifnull(tbp.cut_amount,0) cut_amount,
          tbp.send_time_str,
          ##退款
          case 
          when dualstatus = 0 then '申请退款'
          when dualstatus = 1 then '正在退款中'
          when dualstatus = 2 then '退款成功'
          when dualstatus = 3 then '用户取消申请'
          when dualstatus = 4 then '商家不同意退款'
          else '未发生退款'
          end dualstatus ,
          ifnull(pd.amount,0) tkje ,
          dualdates tksj ,
          refundcontent ,
          case when tran.oid  is not null  then f.rate  else tbpl.BCBILV  end yongjinlv ,
          -- 商品分摊比例
          case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end share_rat ,
          -- 退款标记
          case when pd.oid is not null and pd.dualstatus = 2 then 1 else 0 end tk_flag ,
          -- 退款类型
          case when refundcontent like '%仅退运费%' then 1 else 0 end tk_type  ,
          -- 红包使用条件 1 可以使用红包， 0 不能使用红包
          case when ( ifnull(tot_amount,0) - ifnull(pd.amount ,0) > ifnull(tct.consumeamount,0) ) then 1 else 0 end coupon_use_flag ,
          -- 积分使用条件 1 可以使用积分， 0 不能使用积分
          case when ( ifnull(tot_amount,0) - ifnull(pd.amount ,0) > ifnull(tbp.score_to_cost/100,0)  ) then 1 else 0 end score_use_flag ,
          -- 红包分摊金额
          ifnull(tbp.shopCouponAmount,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end )    coupon_share ,
          -- 积分分摊金额
          ifnull(tbp.score_to_cost/100,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end )  score_share ,
          -- 退款分摊金额
          ifnull(pd.amount,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) tk_share ,
          -- 到账金额分摊
          ifnull(tbp.actual_pay,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) actual_pay_share ,
          -- 分摊的运费
          ifnull(tbp.DEL_TYPE_COST,0)* (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) deliver_share ,
          ifnull(tct.consumeamount ,0)  consumeamount
          FROM
          tb_Porder tbp
          left join (select oid from tb_cps_porder group by oid ) tbcp  on tbp.oid = tbcp.oid  # 是否CPS订单
          left join tb_porder_line tbpl on tbp.oid = tbpl.oid -- 订单明细
          left join ecommerce.my_product  p on tbpl.pid = P.pid # and p.shop_code = '",shop_code_,"' -- 商品维度表
          left join (
          SELECT  oid , SUM(AMOUNT) tot_amount
          FROM tb_porder_line
          group by oid 
          ) tm  on tbp.oid = tm.oid   # 商品总额获取
          left join tb_porder_drawback pd on tbp.oid = pd.oid  #退款
          ################服务费率计算#######
          #当发生转单的时候，用承接商家的服务费率来算
          ###################################
          left join (select tr.oid , new_shopcode  from tb_porder_transfer tr
                   join tb_porder_log log on tr.oid = log.oid and tr.new_shopcode = log.shop_code and log.HANDLE_STATUS = 99 # and log.payStatus = 1 
									where   tr.check_status = 1 ) tran on tbp.oid = tran.oid 
          left join TB_MERCHANT_CATALOG_RATE f  on tran.new_shopcode = f.shop_code and f.bcid = p.bcid
          
          #红包使用条件  consumeamount	消费额度
          left join tb_coupon_topic tct on  tbp.couponRecordId = tct.COUPONID
          WHERE
          1 = 1
          and tbp.shop_code =  '",shop_code_,"'
          # and tbp.oid = 1449523
          AND tbp.pay_status = 1 #在线支付
          AND DATE_FORMAT(tbp.send_time_str, '%Y-%m') = '",dt(),"'
          AND tbp.HANDLE_STATUS NOT IN (5, 7) 
          ) x                        
               "))
         
       
        df <- dd %>% select ( 
            `订单号` = oid ,
            `订单编号` = orderno ,
            `订单批号` = BATCH_OID ,
            `月份` = send_month ,
            `下单日期` = POST_DATE_STR ,
            `支付时间` = payDates ,
            `支付方式` = payName ,
            `订单状态` = HANDLE_STATUS ,
            `CPS订单` = iscps ,
            `商家名称` = merchant_name ,
            `商品ID` = pid ,
            `商品名称` = name ,
            `商家货号` =  aa,
            `商品类别` = ischufang ,
            `一级分类` = ctitle0 ,
            `商品分类` = ctitle2 ,
            `规格` = specification ,
            `商品数量` = QUANTITY ,
            `单价` = SHOP_price ,
            `商品小计` = AMOUNT ,
            `订单总额` = SUM_PRICE ,
            `到账金额` = ACTUAL_PAY ,
            `商品总额` = tot_amount ,
            `是否修改商品金额` = bb  ,
            `运费` = DEL_TYPE_COST ,
            `是否修改运费` = cc ,
            `优惠券` = shopCouponAmount ,
            `红包` = couponAmount ,
            `积分抵扣` = score_to_cost ,
            `满立减优惠` = cut_amount ,
            `发货时间` = send_time_str ,
            `退款状态` = dualstatus ,
            `退款金额` = tkje ,
            `退款时间` = tksj ,
            `退款内容` = refundcontent ,
            `扣除服务费用率` = yongjinlv ,
            `服务费用` = serv_fee ,
            `应付商家款` = yf_shop_fee 
        )
    })
    
    
    # export
    output$btn_export <- downloadHandler(paste('bbf-data-callcenter-',Sys.Date(),'.csv',sep=''), content = function(file) {
        if(TRUE){
           write.csv(df() , file, fileEncoding='gbk',row.names=FALSE)
        } else {
            write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
        }

    })
    
    
    
    
    output$tbl <- DT::renderDataTable({
        datatable(
            df() ,
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
        ) 
    })
}









# 分账统计-----------在线支付(跨月退款)
fztj2UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        helpText(icon('warning'), '在线支付跨月退款'),
        fluidRow(class='toolbar',
                 # column(2, selectInput(ns('dt'), '发货时间', choices =  substr(as.character(seq(Sys.Date()-days(180) , by='month', length.out = 12)), 1, 7), selected = substr(Sys.Date()-months(1),1,7))),
                 column(2, selectInput(ns('dt2'), '退款时间', choices =  substr(as.character(seq(Sys.Date()-days(180) , by='month', length.out = 12)), 1, 7), selected = substr(Sys.Date()-months(1),1,7))),
                 column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
        ), 
        fluidRow(
            DT::dataTableOutput(ns('tbl'))
        )
    )
}
fztj2 <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
    
    dt2 <- reactive({
        req(input$dt2)
        input$dt2
    })
    
    df <- reactive({
        
        shop_code_ <- username 
        
        
        dd <- dbGetQuery0('ecommerce', paste0("
                                              select 	oid , orderno, BATCH_OID, send_month, POST_DATE_STR , payDates , payName,
          HANDLE_STATUS ,iscps ,merchant_name , pid , name ,
                                              '11' aa,#商家货号
                                              ischufang ,ctitle0 ,ctitle2 ,specification ,QUANTITY ,
                                              SHOP_price ,AMOUNT,
                                              round(share_rat*SUM_PRICE ,4 )+deliver_share SUM_PRICE , 
                                              round(share_rat*ACTUAL_PAY ,4 ) ACTUAL_PAY ,
                                              round(share_rat*tot_amount , 4 ) tot_amount ,
                                              'bb' bb ,#是否修改商品金额
                                              deliver_share DEL_TYPE_COST, 
                                              '111' cc, # 是否修改运费 
                                              round(share_rat*shopCouponAmount , 4 ) shopCouponAmount ,
                                              coupon_share couponAmount,
                                              score_share score_to_cost,
                                              cut_amount,
                                              send_time_str,
                                              dualstatus ,
                                              tk_share tkje ,
                                              tksj ,
                                              refundcontent ,
                                              yongjinlv ,
                                              -round(tk_share*yongjinlv ,4)serv_fee,  #服务费用
                                              -round((tk_share - (tk_share*yongjinlv)  + if( coupon_use_flag = 1 ,coupon_share ,0)  + if( score_use_flag = 1 ,score_share ,0) ) ,4) yf_shop_fee #跨月退款         退款金额-服务费+红+积分
                                              from (
                                              SELECT
                                              ##订单详情
                                              tbp.oid ,concat('''',tbp.orderno) orderno ,tbp.BATCH_OID,DATE_FORMAT(tbp.send_time_str, '%m') send_month, tbp.POST_DATE_STR , tbp.payDates,tbp.payName,
                                              CASE
                                              WHEN HANDLE_STATUS = 0 THEN
                                              '待处理'
                                              WHEN HANDLE_STATUS = 1 THEN
                                              '正在配货'
                                              WHEN HANDLE_STATUS = 4 THEN
                                              '交易成功'
                                              WHEN HANDLE_STATUS = 5 THEN
                                              '交易取消'
                                              WHEN HANDLE_STATUS = 6 THEN
                                              '已收货'
                                              WHEN HANDLE_STATUS = 7 THEN
                                              '交易关闭'
                                              WHEN HANDLE_STATUS = 97 THEN
                                              '已退货'
                                              WHEN HANDLE_STATUS = 99 THEN
                                              '已发货）'
                                              END HANDLE_STATUS,
                                              case when tbcp.oid is not null then '是' else '否' end iscps,
                                              tbp.merchant_name ,
                                              tbpl.pid ,
                                              tbpl.name ,
                                              '1' aa ,   #商品货号
                                              ##商品
                                              CASE
                                              WHEN p.ischufang = 0 THEN
                                              '非药物'
                                              WHEN p.ischufang = 1 THEN
                                              '处方药'
                                              WHEN p.ischufang = 2 THEN
                                              '非处方药'
                                              END AS ischufang ,
                                              p.ctitle0 ,
                                              p.ctitle2 ,
                                              p.specification ,
                                              ##订单金额
                                              ifnull(tbpl.QUANTITY,0) QUANTITY,
                                              -- tbpl.SHOP_CODE,
                                              p.shop_price,
                                              ifnull(tbpl.AMOUNT,0) AMOUNT,
                                              ifnull(tbp.SUM_PRICE,0) SUM_PRICE,
                                              ifnull(tbp.ACTUAL_PAY,0) ACTUAL_PAY,
                                              ifnull(tm.tot_amount,0) tot_amount,
                                              '1'  bb ,-- 是否修改商品金额
                                              ifnull(tbp.DEL_TYPE_COST,0) DEL_TYPE_COST,
                                              '1' cc ,-- 是否修改运费 ,
                                              improve_cost shopCouponAmount ,
                                              ifnull(tbp.shopCouponAmount,0) couponAmount,
                                              #ifnull(tbp.couponAmount,0) couponAmount,
                                              ifnull(tbp.score_to_cost/100,0) score_to_cost,
                                              ifnull(tbp.cut_amount,0) cut_amount,
                                              tbp.send_time_str,
                                              ##退款
                                              case
                                              when dualstatus = 0 then '申请退款'
                                              when dualstatus = 1 then '正在退款中'
                                              when dualstatus = 2 then '退款成功'
                                              when dualstatus = 3 then '用户取消申请'
                                              when dualstatus = 4 then '商家不同意退款'
                                              else '未发生退款'
                                              end dualstatus ,
                                              ifnull(pd.amount,0) tkje ,
                                              dualdates tksj ,
                                              refundcontent ,
                                              case when tran.oid  is not null  then f.rate  else tbpl.BCBILV  end yongjinlv ,
                                              -- 商品分摊比例
                                              case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end share_rat ,
                                              -- 退款标记
                                              1 tk_flag ,
                                              -- 退款类型
                                              case when refundcontent like '%仅退运费%' then 1 else 0 end tk_type  ,
                                              -- 红包使用条件 1 可以使用红包， 0 不能使用红包
                                              case when ( ifnull(tot_amount,0) - ifnull(pd.amount ,0) > ifnull(tct.consumeamount,0) ) then 1 else 0 end coupon_use_flag ,
                                              -- 积分使用条件 1 可以使用积分， 0 不能使用积分
                                              case when ( ifnull(tot_amount,0) - ifnull(pd.amount ,0) > ifnull(tbp.score_to_cost/100,0)  ) then 1 else 0 end score_use_flag ,
                                              -- 红包分摊金额
                                              ifnull(tbp.shopCouponAmount,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end )    coupon_share ,
                                              -- 积分分摊金额
                                              ifnull(tbp.score_to_cost/100,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end )  score_share ,
                                              -- 退款分摊金额
                                              ifnull(pd.amount,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) tk_share ,
                                              -- 到账金额分摊
                                              ifnull(tbp.actual_pay,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) actual_pay_share ,
                                              -- 分摊的运费
                                              ifnull(tbp.DEL_TYPE_COST,0)* (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) deliver_share ,
                                              ifnull(tct.consumeamount ,0)  consumeamount
                                              FROM
                                              tb_Porder tbp
                                              left join (select oid from tb_cps_porder group by oid ) tbcp  on tbp.oid = tbcp.oid  # 是否CPS订单
                                              left join tb_porder_line tbpl on tbp.oid = tbpl.oid -- 订单明细
                                              left join ecommerce.my_product p on tbpl.pid = P.pid # and p.shop_code = '",shop_code_,"' -- 商品维度表
                                              left join (
                                              SELECT  oid , SUM(AMOUNT) tot_amount
                                              FROM tb_porder_line
                                              group by oid
                                              ) tm  on tbp.oid = tm.oid   # 商品总额获取
                                              join tb_porder_drawback pd on tbp.oid = pd.oid   and pd.dualstatus = 2
                                              and DATE_FORMAT(pd.DUALDATES, '%Y-%m') = '",dt2(),"' #退款
                                              ################服务费率计算#######
                                              #当发生转单的时候，用承接商家的服务费率来算
                                              ###################################
                                              left join (select tr.oid , new_shopcode  from tb_porder_transfer tr
                   join tb_porder_log log on tr.oid = log.oid and tr.new_shopcode = log.shop_code and log.HANDLE_STATUS = 99 and log.payStatus = 1 
                                              where   tr.check_status = 1 ) tran on tbp.oid = tran.oid 

                                              left join TB_MERCHANT_CATALOG_RATE f  on tran.new_shopcode = f.shop_code and f.bcid = p.bcid
                                              
                                              #红包使用条件  consumeamount	消费额度
                                              left join tb_coupon_topic tct on  tbp.couponRecordId = tct.COUPONID
                                              WHERE
                                              1 = 1
                                              and tbp.shop_code = '",shop_code_,"'
                                              AND DATE_FORMAT(tbp.send_time_str, '%Y-%m') =  DATE_FORMAT(DATE_SUB(concat('",dt2(),"','-01'),INTERVAL 1 MONTH),'%Y-%m')
                                              ) x          
                                              ")) 
        
        
        df <- dd %>% select ( 
            `订单号` = oid ,
            `订单编号` = orderno ,
            `订单批号` = BATCH_OID ,
            `月份` = send_month ,
            `下单日期` = POST_DATE_STR ,
            `支付时间` = payDates ,
            `支付方式` = payName ,
            `订单状态` = HANDLE_STATUS ,
            `CPS订单` = iscps ,
            `商家名称` = merchant_name ,
            `商品ID` = pid ,
            `商品名称` = name ,
            `商家货号` =  aa,
            `商品类别` = ischufang ,
            `一级分类` = ctitle0 ,
            `商品分类` = ctitle2 ,
            `规格` = specification ,
            `商品数量` = QUANTITY ,
            `单价` = SHOP_price ,
            `商品小计` = AMOUNT ,
            `订单总额` = SUM_PRICE ,
            `到账金额` = ACTUAL_PAY ,
            `商品总额` = tot_amount ,
            `是否修改商品金额` = bb  ,
            `运费` = DEL_TYPE_COST ,
            `是否修改运费` = cc ,
            `优惠券` = shopCouponAmount ,
            `红包` = couponAmount ,
            `积分抵扣` = score_to_cost ,
            `满立减优惠` = cut_amount ,
            `发货时间` = send_time_str ,
            `退款状态` = dualstatus ,
            `退款金额` = tkje ,
            `退款时间` = tksj ,
            `退款内容` = refundcontent ,
            `扣除服务费用率` = yongjinlv ,
            `服务费用` = serv_fee ,
            `应付商家款` = yf_shop_fee 
        )
    })
    
    
    # export
    output$btn_export <- downloadHandler(paste('bbf-data-callcenter-',Sys.Date(),'.csv',sep=''), content = function(file) {
        if(TRUE){
            write.csv(df() , file, fileEncoding='gbk',row.names=FALSE)
        } else {
            write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
        }
        
    })
    
    
    
    
    output$tbl <- DT::renderDataTable({
        datatable(
            df() ,
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
        ) 
    })
}













# 分账统计-----------货到付款

fztj3UI <- function(id) {
    ns <- NS(id)
    
    tagList(
        helpText(icon('warning'), '货到付款'),
        fluidRow(class='toolbar',
                  column(2, selectInput(ns('dt'), '发货时间', choices =  substr(as.character(seq(Sys.Date()-days(180) , by='month', length.out = 12)), 1, 7), selected = substr(Sys.Date()-months(1),1,7))),
                 column(2, downloadButton(ns('btn_export'), '导出', class = 'btn_nowrap'))
        ), 
        fluidRow(
            DT::dataTableOutput(ns('tbl'))
        )
    )
}
fztj3 <- function(input, output, session,username) {
    dt <- reactive({
        req(input$dt)
        input$dt
    })
     
    
    df <- reactive({
        
        shop_code_ <- username 
        
        # tbp.shop_code = '",shop_code_,"'
        
        dd <- dbGetQuery0('ecommerce', paste0("
                                               
                                             select  
				oid , orderno, BATCH_OID, send_month, POST_DATE_STR , payDates , payName,
                                              HANDLE_STATUS ,iscps ,merchant_name , pid , name ,
                                              '11' aa,#商家货号
                                              ischufang ,ctitle0 ,ctitle2 ,specification ,QUANTITY ,
                                              SHOP_price ,AMOUNT,
                                              round(share_rat*SUM_PRICE ,4 )+deliver_share SUM_PRICE , 
                                              round(share_rat*ACTUAL_PAY ,4 ) ACTUAL_PAY ,
                                              round(share_rat*tot_amount , 4 ) tot_amount ,
                                              'bb' bb ,#是否修改商品金额
                                              deliver_share DEL_TYPE_COST, 
                                              '111' cc, # 是否修改运费 
                                              round(share_rat*shopCouponAmount , 4 ) shopCouponAmount ,
                                              coupon_share couponAmount,
                                              score_share score_to_cost,
                                              cut_amount,
                                              send_time_str,
                                              dualstatus ,
                                              tk_share tkje ,
                                              tksj ,
                                              refundcontent ,
                                              yongjinlv ,
                                              round(case 
                                              when tk_flag = 0 then AMOUNT * yongjinlv      #不退款
                                              when tk_flag = 1 then (AMOUNT -tk_share ) * yongjinlv      #退款  
                                              end ,4) serv_fee , 
                                              round(case   
                                              when tk_flag = 0 then  if( coupon_use_flag = 1 ,coupon_share ,0) + if( score_use_flag = 1 ,score_share ,0) - ( AMOUNT*yongjinlv )     #不退款   应付商家款 = 红包+积分-服务费(AMOUNT * yongjinlv )  
                                              when tk_flag = 1 then  if( coupon_use_flag = 1 ,coupon_share ,0) + if( score_use_flag = 1 ,score_share ,0) - (AMOUNT -tk_share ) * yongjinlv  end , 4 )  yf_shop_fee  #退款   应付商家款 = 红包+积分-服务费(AMOUNT - tk_share) * yongjinlv 
                                              from (
                                              SELECT
                                              ##订单详情
                                              tbp.oid ,concat('''',tbp.orderno) orderno ,tbp.BATCH_OID,DATE_FORMAT(tbp.send_time_str, '%m') send_month, tbp.POST_DATE_STR , tbp.payDates,tbp.payName,
                                              CASE
                                              WHEN tbp.HANDLE_STATUS = 0 THEN
                                              '待处理'
                                              WHEN tbp.HANDLE_STATUS = 1 THEN
                                              '正在配货'
                                              WHEN tbp.HANDLE_STATUS = 4 THEN
                                              '交易成功'
                                              WHEN tbp.HANDLE_STATUS = 5 THEN
                                              '交易取消'
                                              WHEN tbp.HANDLE_STATUS = 6 THEN
                                              '已收货'
                                              WHEN tbp.HANDLE_STATUS = 7 THEN
                                              '交易关闭'
                                              WHEN tbp.HANDLE_STATUS = 97 THEN
                                              '已退货'
                                              WHEN tbp.HANDLE_STATUS = 99 THEN
                                              '已发货）'
                                              END HANDLE_STATUS,
                                              case when tbcp.oid is not null then '是' else '否' end iscps,
                                              tbp.merchant_name ,
                                              tbpl.pid , 
                                              tbpl.name ,
                                              '1111111111' aa , #商品货号 
                                              ##商品
                                              CASE
                                              WHEN p.ischufang = 0 THEN
                                              '非药物'
                                              WHEN p.ischufang = 1 THEN
                                              '处方药'
                                              WHEN p.ischufang = 2 THEN
                                              '非处方药'
                                              END AS ischufang ,
                                              p.ctitle0 ,
                                              p.ctitle2 ,
                                              p.specification ,
                                              ##订单金额
                                              ifnull(tbpl.QUANTITY,0) QUANTITY,
                                              -- tbpl.SHOP_CODE,
                                              p.shop_price,
                                              ifnull(tbpl.AMOUNT,0) AMOUNT,
                                              ifnull(tbp.SUM_PRICE,0) SUM_PRICE,
                                              ifnull(tbp.ACTUAL_PAY,0) ACTUAL_PAY,
                                              ifnull(tm.tot_amount,0) tot_amount,
                                              '1111111111' bb ,-- 是否修改商品金额
                                              ifnull(tbp.DEL_TYPE_COST,0) DEL_TYPE_COST,
                                              '111111111' cc ,-- 是否修改运费 ,
                                              improve_cost shopCouponAmount ,
                                              ifnull(tbp.shopCouponAmount,0) couponAmount,
                                              #ifnull(tbp.couponAmount,0) couponAmount,
                                              ifnull(tbp.score_to_cost/100,0) score_to_cost,
                                              ifnull(tbp.cut_amount,0) cut_amount,
                                              tbp.send_time_str,
                                              ##退款
                                              case 
                                              when dualstatus = 0 then '申请退款'
                                              when dualstatus = 1 then '正在退款中'
                                              when dualstatus = 2 then '退款成功'
                                              when dualstatus = 3 then '用户取消申请'
                                              when dualstatus = 4 then '商家不同意退款'
                                              else '未发生退款'
                                              end dualstatus ,
                                              ifnull(pd.amount,0) tkje ,
                                              dualdates tksj ,
                                              refundcontent ,
                                              case when tran.oid  is not null  then f.rate  else tbpl.BCBILV  end yongjinlv ,
                                              -- 商品分摊比例
                                              case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end share_rat ,
                                              -- 退款标记
                                              case when pd.oid is not null and pd.dualstatus = 2  then 1 else 0 end tk_flag ,
                                              -- 退款类型
                                              case when refundcontent like '%仅退运费%' then 1 else 0 end tk_type  ,
                                              -- 红包使用条件 1 可以使用红包， 0 不能使用红包
                                              case when ( ifnull(tot_amount,0) - ifnull(pd.amount ,0) > ifnull(tct.consumeamount,0) ) then 1 else 0 end coupon_use_flag ,
                                              -- 积分使用条件 1 可以使用积分， 0 不能使用积分
                                              case when ( ifnull(tot_amount,0) - ifnull(pd.amount ,0) > ifnull(tbp.score_to_cost/100,0)  ) then 1 else 0 end score_use_flag ,
                                              -- 红包分摊金额
                                              ifnull(tbp.shopCouponAmount,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end )    coupon_share ,
                                              -- 积分分摊金额
                                              ifnull(tbp.score_to_cost/100,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end )  score_share ,
                                              -- 退款分摊金额
                                              ifnull(pd.amount,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) tk_share ,
                                              -- 到账金额分摊
                                              ifnull(tbp.actual_pay,0) * (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) actual_pay_share ,
                                              -- 分摊的运费
                                              ifnull(tbp.DEL_TYPE_COST,0)* (case when ifnull(tot_amount,0) != 0 then  (ifnull(tbpl.AMOUNT,0)/ifnull(tm.tot_amount,0))  else 0 end ) deliver_share ,
                                              ifnull(tct.consumeamount ,0)  consumeamount
                                              FROM
                                              tb_Porder tbp
                                              left join (select oid from tb_cps_porder group by oid ) tbcp  on tbp.oid = tbcp.oid  # 是否CPS订单
                                              left join tb_porder_line tbpl on tbp.oid = tbpl.oid -- 订单明细
                                              left join my_product  p on tbpl.pid = P.pid  
                                              left join (
                                              SELECT  oid , SUM(AMOUNT) tot_amount
                                              FROM tb_porder_line
                                              group by oid 
                                              ) tm  on tbp.oid = tm.oid   # 商品总额获取
                                              left join tb_porder_drawback pd on tbp.oid = pd.oid  #退款
                                              ################服务费率计算#######
                                              #当发生转单的时候，用承接商家的服务费率来算 
                                              left join (select tr.oid , new_shopcode  from tb_porder_transfer tr
                                              join tb_porder_log log on tr.oid = log.oid and tr.new_shopcode = log.shop_code and log.HANDLE_STATUS = 99 and log.payStatus = 1 
                                              where   tr.check_status = 1 ) tran on tbp.oid = tran.oid 
                                              
                                              left join TB_MERCHANT_CATALOG_RATE f  on tran.new_shopcode = f.shop_code and f.bcid = p.bcid
                                              
                                              #红包使用条件  consumeamount	消费额度
                                              left join tb_coupon_topic tct on  tbp.couponRecordId = tct.COUPONID
                                              WHERE
                                              1 = 1
                                              and tbp.shop_code = '",shop_code_,"'
                                              and tbp.pay_type = 10002 #货到付款
                                              AND DATE_FORMAT(tbp.send_time_str,'%Y-%m')= '",dt(),"'
                                              AND  tbp.HANDLE_STATUS not in (5 , 7 )   
                                              ) x      
                                              ")) 
        
        
        df <- dd %>% select ( 
            `订单号` = oid ,
            `订单编号` = orderno ,
            `订单批号` = BATCH_OID ,
            `月份` = send_month ,
            `下单日期` = POST_DATE_STR ,
            `支付时间` = payDates ,
            `支付方式` = payName ,
            `订单状态` = HANDLE_STATUS ,
            `CPS订单` = iscps ,
            `商家名称` = merchant_name ,
            `商品ID` = pid ,
            `商品名称` = name ,
            `商家货号` =  aa,
            `商品类别` = ischufang ,
            `一级分类` = ctitle0 ,
            `商品分类` = ctitle2 ,
            `规格` = specification ,
            `商品数量` = QUANTITY ,
            `单价` = SHOP_price ,
            `商品小计` = AMOUNT ,
            `订单总额` = SUM_PRICE ,
            `到账金额` = ACTUAL_PAY ,
            `商品总额` = tot_amount ,
            `是否修改商品金额` = bb  ,
            `运费` = DEL_TYPE_COST ,
            `是否修改运费` = cc ,
            `优惠券` = shopCouponAmount ,
            `红包` = couponAmount ,
            `积分抵扣` = score_to_cost ,
            `满立减优惠` = cut_amount ,
            `发货时间` = send_time_str ,
            `退款状态` = dualstatus ,
            `退款金额` = tkje ,
            `退款时间` = tksj ,
            `退款内容` = refundcontent ,
            `扣除服务费用率` = yongjinlv ,
            `服务费用` = serv_fee ,
            `应付商家款` = yf_shop_fee 
        )
    })
    
    
    # export
    output$btn_export <- downloadHandler(paste('bbf-data-callcenter-',Sys.Date(),'.csv',sep=''), content = function(file) {
        if(TRUE){
            write.csv(df() , file, fileEncoding='gbk',row.names=FALSE)
        } else {
            write.csv('无数据下载权限', file, fileEncoding='gbk', row.names=FALSE, col.names=FALSE)
        }
        
    })
    
    
    
    
    output$tbl <- DT::renderDataTable({
        datatable(
            df() ,
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
        ) 
    })
}


 