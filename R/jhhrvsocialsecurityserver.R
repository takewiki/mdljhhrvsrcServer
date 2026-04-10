

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
viewsocialsecurityserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_socialsecurity,
                        {
                            sql = 'select * from rds_hrv_src_ds_socialsecurity'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('费用承担组织',
                                            '个税申报组织',
                                            '银行',
                                            '高新部门',
                                            '会计科目',
                                            'RD-项目',
                                            '求和项:单位月缴纳养老保险费',
                                            '求和项:单位月缴纳医疗保险费',
                                            '求和项:企业大病医保',
                                            '求和项:异地残障金',
                                            '求和项:异地其他费用',
                                            '求和项:单位月缴纳失业保险费',
                                            '求和项:单位月缴纳工伤保险费',
                                            '求和项:单位月缴纳生育保险费',
                                            '求和项:单位社保合计',
                                            '求和项:单位公积金缴费金额',
                                            '求和项:单位公积金社保合计',
                                            '求和项:个人月缴纳养老保险费',
                                            '求和项:个人月缴纳医疗保险费',
                                            '求和项:个人大病医保',
                                            '求和项:个人月缴纳失业保险费',
                                            '求和项:个人社保缴费合计',
                                            '求和项:个人公积金缴费金额',
                                            '求和项:个人社保公积金合计',
                                            '求和项:社保费用总金额',
                                            '求和项:公积金费用总金额',
                                            '求和项:合计',
                                            '求和项:管理费',
                                            '会计年度',
                                            '会计期间',
                                            '凭证模板名称',
                                            '业务类型',
                                            '单据编号',
                                            '行号',
                                            '日期',
                                            '原部门',
                                            '备注期间')
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_socialsecurity', data = data)
                            
                            tsui::run_download_xlsx(id = 'dl_socialsecurity',data = data,filename = '社保公积金.xlsx')
                            
                            
                 
                          
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
jhhrvsocialsecurityServer <- function(input,output,session,dms_token) {
  #预览数据
  viewsocialsecurityserver(input,output,session,dms_token)

}
