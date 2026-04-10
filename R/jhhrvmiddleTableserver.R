

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
#' @examples viewmiddleTableserver()  
viewmiddleTableserver <- function(input,output,session,dms_token) {

  
  shiny::observe({
    shiny::observeEvent(input$btn_hrv_src_view_middleTable,
                        {
                            sql = 'select * from rds_hrv_src_ds_middleTable'
                            
                            data = tsda::sql_select2(token = dms_token, sql = sql)
                            names(data) = c('单据日期',
                                            '会计年度',
                                            '会计期间',
                                            '单据编号',
                                            '行号',
                                            '凭证模板号',
                                            '凭证模板名称',
                                            '费用申报组织',
                                            '费用承担组织',
                                            '业务类型',
                                            '摘要',
                                            '账簿',
                                            '往来单位名称',
                                            '往来单位编码',
                                            '供应商',
                                            '供应商编码',
                                            '账户名称',
                                            '高新部门',
                                            '科目编码',
                                            '科目全名',
                                            '核算维度',
                                            '部门代码',
                                            '部门名称',
                                            'RD-项目（人工费用表格）',
                                            '系统项目名称',
                                            '责任中心',
                                            '重分类',
                                            '银行账号',
                                            '借方金额',
                                            '贷方金额',
                                            '结算方式',
                                            '结算号',
                                            '责任中心名称',
                                            '重分类名称',
                                            'std新行号',
                                            '状态',
                                            '详细信息',
                                            '源行号',
                                            '标准行号')
                            #显示数据
                            tsui::run_dataTable2(id = 'hrv_src_view_data_middleTable', data = data)
                            
                            
                            tsui::run_download_xlsx(id = 'dl_middleTable',data = data,filename = '中间凭证.xlsx')
                            
                            
                 
                          
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
jhhrvmiddleTableServer <- function(input,output,session,dms_token) {
  #预览数据
  viewmiddleTableserver(input,output,session,dms_token)

}
