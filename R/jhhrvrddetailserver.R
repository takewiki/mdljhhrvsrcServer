

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
viewrddetailserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_rddetail,
                        {
                            sql = 'select * from rds_hrv_src_ds_rddetail'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = 
                              c('序号',
                                '工资类别',
                                '会计年度',
                                '会计期间',
                                '原部门',
                                '高新部门',
                                '姓名',
                                '费用承担组织',
                                '个税申报组织',
                                '单据编号',
                                '项目名称',
                                '研发工资成本'
                                
                              )
                            
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_rddetail', data = data)
                            
                            
                            tsui::run_download_xlsx(id = 'dl_rddetail',data = data,filename = '研发工时.xlsx')
                            
                            
                 
                          
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
jhhrvrddetailServer <- function(input,output,session,dms_token) {
  #预览数据
  viewrddetailserver(input,output,session,dms_token)

}
