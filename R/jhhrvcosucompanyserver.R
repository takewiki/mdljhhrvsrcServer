

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
#' @examples viewcosucompanyserver()  
viewcosucompanyserver <- function(input,output,session,dms_token) {
  shiny::observe({
    
    shiny::observeEvent(input$btn_hrv_src_view_cosucompany,
                        {
                            sql = 'select * from rds_hrv_src_md_cosucompany'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('项目',
                                            '编码',
                                            '名称') 
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_cosucompany', data = data)
                            
                            
                 
                          
                        })
    
    
    #新增
    shiny::observeEvent(input$btn_hrv_src_add_cosucompany,
                        {
                          var_text_hrv_src_cosucompany_Fitem= tsui::var_text('text_hrv_src_cosucompany_Fitem')
                          var_text_hrv_src_rditem_FNumber= tsui::var_text('text_hrv_src_rditem_FNumber')
                          var_text_hrv_src_cosucompany_FName= tsui::var_text('text_hrv_src_cosucompany_FName')
                          
                          if(var_text_hrv_src_cosucompany_Fitem()==''){
                            tsui::pop_notice('项目不能为空')
                          }
                          else if(var_text_hrv_src_rditem_FNumber()==''){
                            tsui::pop_notice('编码不能为空')
                          }
                          else if(var_text_hrv_src_cosucompany_FName()==''){
                            tsui::pop_notice('名称不能为空')
                          }
                          else{
                            Fitem=var_text_hrv_src_cosucompany_Fitem()
                            FNumber=var_text_hrv_src_rditem_FNumber()
                            FName=var_text_hrv_src_cosucompany_FName()
                            sql = paste0("insert into rds_hrv_src_md_cosucompany values('",Fitem,"','",FNumber,"','",FName,"')
")
                            tsda::sql_insert2(token = dms_token,sql_str = sql)
                            
                            tsui::pop_notice('往来单位新增成功')
                          }
                          
                          
                          
                          
                        })
    
    
    #    shanchu1
    shiny::observeEvent(input$btn_hrv_src_delete_cosucompany,
                        {
                          
                          var_text_hrv_src_rditem_FNumber_delete=tsui::var_text('text_hrv_src_rditem_FNumber_delete')
                          
                          if(var_text_hrv_src_rditem_FNumber_delete()==''){
                            tsui::pop_notice('请输入需要删除的编码')
                          }
                          else{
                            FNumber=var_text_hrv_src_rditem_FNumber_delete()
                            sql =paste0("delete from rds_hrv_src_md_cosucompany where FNumber='",FNumber,"'") 
                            
                            tsda::sql_delete2(token = dms_token,sql_str  = sql)
                            tsui::pop_notice("删除成功")
                            
                          }
                          
                          
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
jhhrvcosucompanyServer <- function(input,output,session,dms_token) {
  #预览数据
  viewcosucompanyserver(input,output,session,dms_token)

}
