

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
#' @examples viewacctserver()  
viewacctserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_acct,
                        {
                            sql = 'select * from rds_hrv_src_md_acct'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('编码',
                                            '名称',
                                            '全名',
                                            '余额方向',
                                            '科目类别',
                                            '外币核算',
                                            '核算维度',
                                            '数据状态',
                                            '一级科目'
                                            )
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_acct', data = data)
                            
                            tsui::run_download_xlsx(id = 'dl_acct',data = data,filename = '科目.xlsx')
                            
                            
                 
                          
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
jhhrvacctServer <- function(input,output,session,dms_token) {
  #预览数据
  viewacctserver(input,output,session,dms_token)

}
