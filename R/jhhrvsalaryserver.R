

#' Title 预览数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples viewsalaryserver()  
viewsalaryserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_salary,
                        {
                            sql = 'select * from rds_hrv_src_ds_salary'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('费用承担组织',
                                            '个税申报组织',
                                            '银行',
                                            '会计科目',
                                            '高新部门',
                                            '求和项:RD-项目',
                                            '求和项:工资总额',
                                            '固定成本',
                                            '变动成本',
                                            '求和项:社保',
                                            '求和项:公积金',
                                            '计数项:其他',
                                            '求和项:个税',
                                            '求和项:实发金额',
                                            '会计年度',
                                            '会计期间',
                                            '凭证模板名称',
                                            '业务类型',
                                            '单据编号',
                                            '行号',
                                            '单据日期',
                                            '原部门',
                                            '备注期间')
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_salary', data = data)
                            tsui::run_download_xlsx(id = 'dl_salary',data = data,filename = '工资.xlsx')
                            
                            
                 
                          
                        })
    
  })
}



#' Title 后台处理总函数
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples HrvServer()
jhhrvsalaryServer <- function(input,output,session,dms_token) {
  #预览数据
  viewsalaryserver(input,output,session,dms_token)

}
