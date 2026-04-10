

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
viewdetailserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_detail,
                        {
                            sql = 'select * from rds_hrv_src_ds_detail'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('序号',
                                            '工资类别',
                                            '会计年度',
                                            '会计期间',
                                            '原部门',
                                            '高新部门',
                                            '姓名',
                                            '费用承担组织',
                                            '个税申报组织',
                                            '单项总额',
                                            '研发工资成本',
                                            '非研发工资成本',
                                            'RD-T-202301',
                                            'RD-T-202302',
                                            'RD-T-202303',
                                            'RD-T-202304',
                                            'RD-T-202305',
                                            'RD-S-202301',
                                            'RD-S-202302',
                                            'RD-S-202303',
                                            'RD-S-202304',
                                            'RD-S-202305',
                                            'RD-S-202306',
                                            'RD-S-202307',
                                            'RD-S-202308',
                                            'RD-R-202301',
                                            'RD-R-202302',
                                            'RD-R-202303',
                                            'RD-R-202304',
                                            'RD-R-202305',
                                            'RD-R-202306',
                                            'RD-R-202307',
                                            'RD-R-202308',
                                            '单据编号')
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_detail', data = data)
                            
                            data=data
                            print(data)
                            
                            tsui::run_download_xlsx(id = 'dl_detail',data = data,filename = '工时.xlsx')
                            
                            
                 
                          
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
jhhrvdetailServer <- function(input,output,session,dms_token) {
  #预览数据
  viewdetailserver(input,output,session,dms_token)

}
