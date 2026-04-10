

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
viewtploutputvoucherserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_tploutputvoucher,
                        {
                            sql = 'select * from rds_hrv_src_tpl_sys_outputvoucher'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_tploutputvoucher', data = data)
                            
                            tsui::run_download_xlsx(id = 'dl_tploutputvoucher',data = data,filename = '输出凭证.xlsx')
                            
                            
                 
                          
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
jhhrvtploutputvoucherServer <- function(input,output,session,dms_token) {
  #预览数据
  viewtploutputvoucherserver(input,output,session,dms_token)

}
