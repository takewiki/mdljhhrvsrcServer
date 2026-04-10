

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
viewrulevoucherserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_rulevoucher,
                        {
                            sql = 'select * from rds_hrv_src_rule_voucher'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('凭证模板序号',
                                            '凭证模板名称',
                                            '费用承担组织',
                                            '个税申报组织',
                                            '银行',
                                            '业务类型'
                            )
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_rulevoucher', data = data)
                            
                            tsui::run_download_xlsx(id = 'dl_rulevoucher',data = data,filename = '规则表.xlsx')
                            
                            
                 
                          
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
jhhrvrulevoucherServer <- function(input,output,session,dms_token) {
  #预览数据
  viewrulevoucherserver(input,output,session,dms_token)

}
