

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
#' @examples viewtplvoucherserver()  
viewtplvoucherserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_tplvoucher,
                        {
                            sql = 'select * from rds_hrv_src_tpl_voucher'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('凭证模板序号',
                                            '凭证模板名称',
                                            '业务类型',
                                            '行号',
                                            '摘要',
                                            '科目编码',
                                            '科目全名',
                                            '维度序号',
                                            '核算维度',
                                            '取值来源',
                                            '借方金额',
                                            '贷方金额',
                                            '结算方式',
                                            '结算号',
                                            '账簿',
                                            '一级科目是否参与匹配',
                                            '借方金额埋点sql',
                                            '贷方金额买点sql'
                                            )
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_tplvoucher', data = data)
                            
                            tsui::run_download_xlsx(id = 'dl_tplvoucher',data = data,filename = '凭证模版.xlsx')
                 
                          
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
jhhrvtplvoucherServer <- function(input,output,session,dms_token) {
  #预览数据
  viewtplvoucherserver(input,output,session,dms_token)

}
