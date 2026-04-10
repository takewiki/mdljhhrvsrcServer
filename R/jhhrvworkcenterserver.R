

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
#' @examples viewworkcenterserver()  
viewworkcenterserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_workcenter,
                        {
                            sql = 'select * from rds_hrv_src_md_workcenter'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('编码',
                                            '部门名称'
                            )
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_workcenter', data = data)
                            
                            
                            tsui::run_download_xlsx(id = 'dl_workcenter',data = data,filename = '责任中心.xlsx')
                          
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
jhhrvworkcenterServer <- function(input,output,session,dms_token) {
  #预览数据
  viewworkcenterserver(input,output,session,dms_token)

}
