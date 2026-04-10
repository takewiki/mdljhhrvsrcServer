

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
#' @examples viewdeptcomparisonserver()  
viewdeptcomparisonserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_deptcomparison,
                        {
                            sql = 'select * from rds_hrv_src_md_deptcomparison'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('高新部门', '原部门')
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_deptcomparison', data = data)
                            
                            tsui::run_download_xlsx(id = 'dl_deptcomparison',data = data,filename = '新老部门对照.xlsx')
                            
                            
                 
                          
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
jhhrvdeptcomparisonServer <- function(input,output,session,dms_token) {
  #预览数据
  viewdeptcomparisonserver(input,output,session,dms_token)

}
