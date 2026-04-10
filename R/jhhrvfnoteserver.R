

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
#' @examples viewfnoteserver()  
viewfnoteserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_fnote,
                        {
                            sql = 'select * from rds_hrv_src_md_fnote'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('组织类型',
                                            '业务类型',
                                            '摘要规范'
                            )
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_fnote', data = data)
                            
                            tsui::run_download_xlsx(id = 'dl_fnote',data = data,filename = '摘要.xlsx')
                            
                            
                 
                          
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
jhhrvfnoteServer <- function(input,output,session,dms_token) {
  #预览数据
  viewfnoteserver(input,output,session,dms_token)

}
