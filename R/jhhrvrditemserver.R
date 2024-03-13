

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
#' @examples viewrditemserver()  
viewrditemserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_rditem,
                        {
                            sql = 'select * from rds_hrv_src_md_rditem'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('组织',
                                            'RD-项目（人工费用表格）',
                                            '系统项目名称'
                            )
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_rditem', data = data)
                            
                            
                 
                          
                        })
    
    
#    shanchu1
    shiny::observeEvent(input$btn_hrv_src_delete_rditem,
                        {
                          
                          var_text_hrv_src_rditem_FRDProject_delete=tsui::var_text('text_hrv_src_rditem_FRDProject_delete')
                          
                          if(var_text_hrv_src_rditem_FRDProject_delete()==''){
                            tsui::pop_notice('请输入需要删除的系统项目名称')
                          }
                          else{
                            FRDProject=var_text_hrv_src_rditem_FRDProject_delete()
                            sql =paste0("delete from rds_hrv_src_md_rditem where FRDProject='",FRDProject,"'") 
                            
                            tsda::sql_delete2(token = dms_token,sql_str  = sql)
                            tsui::pop_notice("删除成功")
                            
                          }
                          
                          
                        })
    
  })
}

#' Title 新增数据
#'
#' @param input 输入
#' @param output 输出
#' @param session 会话
#' @param dms_token 口令
#'
#' @return 返回值
#' @export
#'
#' @examples addrditemserver()  
addrditemserver <- function(input,output,session,dms_token) {
  
  
  shiny::observe({
    var_text_hrv_src_rditem_FOrg= tsui::var_text('text_hrv_src_rditem_FOrg')
    var_text_hrv_src_rditem_FRDProjectManual= tsui::var_text('text_hrv_src_rditem_FRDProjectManual')
    var_text_hrv_src_rditem_FRDProject= tsui::var_text('text_hrv_src_rditem_FRDProject')
    shiny::observeEvent(input$btn_hrv_src_add_rditem,
                        {
                         if(var_text_hrv_src_rditem_FOrg()==''){
                           tsui::pop_notice('组织不能为空')
                         }
                         else if(var_text_hrv_src_rditem_FRDProjectManual()==''){
                           tsui::pop_notice('RD-项目不能为空')
                         }
                         else if(var_text_hrv_src_rditem_FRDProject()==''){
                           tsui::pop_notice('系统项目名称不能为空')
                         }
                         else{
                           FOrg=var_text_hrv_src_rditem_FOrg()
                           FRDProjectManual=var_text_hrv_src_rditem_FRDProjectManual()
                           FRDProject=var_text_hrv_src_rditem_FRDProject()
                           sql = paste0("insert into rds_hrv_src_md_rditem values('",FOrg,"','",FRDProjectManual,"','",FRDProject,"')
")
                           tsda::sql_insert2(token = dms_token,sql_str = sql)
                           
                           tsui::pop_notice('研发项目新增成功')
                         }
                         
                          
                          #显示数据
                          
                          
                          
                          
                        })
    
  })
}


#'  后台处理总函数
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
jhhrvrditemServer <- function(input,output,session,dms_token) {
  #预览数据
  viewrditemserver(input,output,session,dms_token)
  addrditemserver(input,output,session,dms_token)

}
