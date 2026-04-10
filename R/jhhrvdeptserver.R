

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
#' @examples viewdeptserver()  
viewdeptserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_dept,
                        {
                            sql = 'select * from rds_hrv_src_md_dept'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('高新部门_人工费用计提表',
                                            '编码',
                                            '名称',
                                            '部门全称',
                                            '使用组织',
                                            '备注'
                            )
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_dept', data = data)
                            
                            
                            tsui::run_download_xlsx(id = 'dl_dept',data = data,filename = '部门.xlsx')
                            
                            
                 
                          
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
jhhrvdeptServer <- function(input,output,session,dms_token) {
  #预览数据
  viewdeptserver(input,output,session,dms_token)

}
