#查看是否已经安装R包
if (!requireNamespace("RNeo4j", quietly = TRUE))
  install.packages("RNeo4j")
if (!requireNamespace("dplyr", quietly = TRUE))
  install.packages("dplyr")
if (!requireNamespace("purrr", quietly = TRUE))
  install.packages("purrr")
if (!requireNamespace("visNetwork", quietly = TRUE))
  install.packages("visNetwork")
if (!requireNamespace("igraph", quietly = TRUE))
  install.packages("igraph")
if (!requireNamespace("neo4r", quietly = TRUE))
  install.packages("neo4r")


library(shiny)
library(visNetwork)
#------获取pathwayTable,为了进行选择pathwayID-----------------------------
library(RNeo4j)
graph = startGraph("http://localhost:7474/db/data/", username="neo4j", password="xiaowei")
query_pathwayTable = " match (n:pathway) return n.Description as Pathway_Description, n.ID AS pathway_ID "
pathwayTable <- cypher(graph, query_pathwayTable)
pathwayTable$pathway_ID_Description <- paste0(pathwayTable$pathway_ID,": ", pathwayTable$Pathway_Description) 
###########################################
# ui
###########################################
#=========================================
#fluidPage
#=========================================
ui <- fluidPage(
  
  #设置题目
  titlePanel("Neo4j Time course database Query"),
  
  
  column(12,
         sidebarPanel(
                #选择pathway
                #根据pathwayID进行选择
                selectInput("pathway_ID", "Please select one of pathway ID:",
                            pathwayTable$pathway_ID),
                
                #设置时间选项，多选
                checkboxGroupInput("days", "Please choose days:",
                                   c("3 days" = "day3",
                                     "5 days" = "day5",
                                     "8 days" = "day8",
                                     "only in 3 days"= "only_day3",
                                     "only in 5 days"= "only_day5",
                                     "only in 8 days"= "only_day8")),
                
                
                hr(), #画一条线
                #设置关系中day3weight的选择
                checkboxInput("day3Weight"," Weight in 3 days ?", FALSE),
                #day3weight滑动条
                sliderInput("day3WeightNumber", "Number of WGCNA weight in 3 days:",
                            min = 0, max = 0.5, value = 0),
                
                hr(), #画一条线
                #设置关系中day5weight的选择
                checkboxInput("day5Weight"," Weight in 5 days ?", FALSE),
                #day5weight滑动条
                sliderInput("day5WeightNumber", "Number of WGCNA weight in 5 days:",
                            min = 0, max = 0.6, value = 0),
                
                hr(), #画一条线
                #设置关系中day8weight的选择
                checkboxInput("day8Weight"," Weight in 8 days ?", FALSE),
                #day8weight滑动条
                sliderInput("day8WeightNumber", "Number of WGCNA weight in 8 days:",
                            min = 0, max = 0.55, value = 0),
                
                #总共有多少个节点和多少条边
                strong(textOutput("total_nodes_number")),
                strong(textOutput("total_edges_number")),
                ),
  
  #-------------------------------------------------
          mainPanel(
              
              #显示选择的通路名称
              h2(textOutput("plot_Name") ),
              hr(), #画一条线
              
              tabsetPanel(type = "tabs",
                          tabPanel("visNetwork",                
                                   #是否增加根据基因来选择节点
                                  checkboxInput("nodesIdSelection", "nodes Selection", FALSE),
                                  #是否根据分组筛选节点
                                  checkboxInput("selectedby", "Groups Selection", FALSE),
                    
                                  hr(), #画一条线
                                  #设置输出visNetwork结果
                                  visNetworkOutput("task1_network",  height = "600px")),
                          tabPanel("igraph",
                                   #设置是否需要指定一个图片布局方式
                                   checkboxInput("Select_igraph_layput", "Select igraph layout", FALSE),
                                   #选择布局的方式
                                   selectInput("igraph_layout", label = NULL,
                                               c("randomly" = "randomly",
                                                 "Large Graph" = "Large_Graph",
                                                 "circle" = "circle",
                                                 "sphere" = "sphere",
                                                 "Fruchterman-Reingold" = "Fruchterman-Reingold",
                                                 "Kamada-Kawai" = "Kamada-Kawai",
                                                 "Merging graph" = "Merging_graph",
                                                 "appropriate graph" = "appropriate_graph",
                                                 "Davidson-Harel" = "Davidson-Harel",
                                                 "DrL graph" = "DrL_graph",
                                                 "multidimensional scaling" = "multidimensional_scaling",
                                                 "graphopt" = "graphopt" )),
                                   #选择是否需要做聚类分区
                                   checkboxInput("Selcet_igraph_community", "Selcet igraph community", FALSE),
                                   #选择一种聚类分区的方法
                                   selectInput("igraph_community", label = NULL,
                                               c("edge betweenness" = "edge_betweenness" ,
                                                 "propagating labels" = "propagating_labels" ,
                                                 "greedy optimization of modularity" = "greedy_optimization_of_modularity")),
                                               
                                   #输出igraph图画
                                   plotOutput("task1_igraph", height = "600px")
                                   )
              
              )
          )
  ),
  
  #-------------------------------------------------
  column(12,
         hr(),
         #设置查看节点和边数据的按钮
         actionButton("See_nodesTable", "Nodes informations"),
         actionButton("See_edgesTable", "Edges informations"),
         #设置数据下载按钮
         downloadButton("download_nodeTable", "download nodes information"),
         downloadButton("download_edgeTable", "download edges information")
  ),
  
  #设置数据输出
    h2(textOutput("Nodes_informations")),
    tableOutput('nodeTable'),
  
  
    h2(textOutput("Edges_informations")),
    tableOutput("edgesTable")
  
  
  
  
  
  )


############################################
#server
############################################
server <- function(input, output) {
  
  #####################################
  #输出选择的通路名称
  ####################################
  output$plot_Name <- renderText(
    paste0("genes relationships in ",
           pathwayTable$Pathway_Description[pathwayTable$pathway_ID %in% input$pathway_ID], 
           " over time"))
  #-----------------------------------
  ###################################
  #作图的数据
  ##################################
  task1_data <- reactive({
    #===========================================
    #连接neo4j
    #===========================================
    library(neo4r)
    con <- neo4j_api$new(url = "http://localhost:7474",user = "neo4j", password = "xiaowei")
    con$ping() 
    #===========================================
    #相关数据的输入
    #===========================================
    #--设置pathway选择-------------------------------------
    #pathwayID <- "'mmu04510'"
    #根据界面输入的pathwayID进行编辑好通路ID
    pathwayID <- paste0("'",input$pathway_ID ,"'") 
    
    #-----设置day3weight选择功能----------------------------------
    if (input$day3Weight){
    day3WeightNumber = input$day3WeightNumber
    day3WeightOption <- paste0(" and r2.day3Weight >=", day3WeightNumber)} 
    else{day3WeightOption <- " "}
    
    #-----设置day3weight选择功能----------------------------------
    if (input$day5Weight){
      day5WeightNumber = input$day5WeightNumber
      day5WeightOption <- paste0(" and r2.day5Weight >=", day5WeightNumber)} 
    else{day5WeightOption <- " "}
    
    #-----设置day3weight选择功能----------------------------------
    if (input$day8Weight){
      day8WeightNumber = input$day8WeightNumber
      day8WeightOption <- paste0(" and r2.day8Weight >=", day8WeightNumber)} 
    else{day8WeightOption <- " "}
    
    #--设置时间筛选条件----------------------------------------------
    #days是用来在页面选择day
    #days = c("day3", "day8")
    days = input$days #通过选择后来获取days参数
    #这里是根据days来设置时间筛选条件
    if (length(days) == 0){dayOption = " "}
    if (length(days) ==1){
      if (days == "only_day3"){dayOption = " and r2.relationship =~ '100' "}
      if (days == "only_day5"){dayOption = " and r2.relationship =~ '010' "}
      if (days == "only_day8"){dayOption = " and r2.relationship =~ '001' "}
      
      if (days == "day3"){dayOption = " and r2.relationship =~ '1..' "}
      if (days == "day5"){dayOption = " and r2.relationship =~ '.1.' "}
      if (days == "day8"){dayOption = " and r2.relationship =~ '..1' "}
    }
    if (length(days) == 2){
      if ("day3" %in% days & "day5" %in% days){dayOption = " and r2.relationship =~ '11.' "}
      if ("day5" %in% days & "day8" %in% days){dayOption = " and r2.relationship =~ '.11' "}
      if ("day3" %in% days & "day8" %in% days){dayOption = " and r2.relationship =~ '1.1' "}
    }
    if (length(days) == 3){
      if ("day3" %in% days & "day5" %in% days & "day8" %in% days){dayOption = " and r2.relationship =~ '111' "}
    }
    #------------------------------------------------
    
    #===========================================
    #利用cypher语言查询并获取相关数据
    #===========================================
    
    query = paste0("
                  MATCH (A)-[r1:belong]->(pathway)
                  where pathway.ID = ", pathwayID, 
                                     " with collect(distinct id(A)) AS AID
                  MATCH p = (B)-[r2:WGCNA]->(C)
                  where id(B) in AID and id(C) in AID", dayOption, day3WeightOption, day5WeightOption, day8WeightOption,
                                     "
                  return p ")
    results0 <- neo4r::call_neo4j(con, query=query,type = "graph")
    
    #===========================================
    #数据格式转换
    #===========================================
    # We'll just unnest the properties
    library(dplyr)
    library(purrr)
    
    #解析节点的属性
    results0$nodes <- results0$nodes %>%
      unnest_nodes(what = "properties") 
    
    head(results0$nodes) 
    
    #解析relationship的属性
    results0$relationships <- results0$relationships %>%
      unnest_relationships()  
    head(results0$relationships)
    
    
    #-results0中的relatioship信息中i节点转换成基因名转换一下-----------------------
    #----------------------------
    G <- results0
    #------------------------
    results0$nodes$label =unlist(results0$nodes$label)
    for (i in 1:nrow(results0$nodes)){
      node_id1 <- results0$nodes$id[i]
      if (node_id1 %in% results0$relationships$startNode) {
        results0$relationships$startNode[results0$relationships$startNode %in% node_id1 ] <- results0$nodes$name[i] }
      if (node_id1 %in% results0$relationships$endNode) {
        results0$relationships$endNode[results0$relationships$endNode %in% node_id1 ] <- results0$nodes$name[i] }
    }
    
    #============================================
    #利用visNetwork可视化
    #============================================
    
    #G <- results0
    G$relationships$weight <- paste0("day3weight:", G$relationships$day3Weight,
                                     "; day5weight:", G$relationships$day5Weight,
                                     "; day8weight:", G$relationships$day8Weight)
    G$nodes <- G$nodes %>% 
      select(id = id, label = name) #将基因名称作为节点的名称
    G$relationships <- G$relationships %>%
      select(from = startNode, to = endNode, 
             label = relationship, #将relationship属性作为关系的名称
             title = weight)  #多设置一个title，可以使鼠标移到那条边的时候，显示这条边的信息,这里我设置为weight
    
    GI <- G #这个拷贝是为了igraph作图，因为igraph不需要color这个。
    #根据属性relationship来设置每条边的颜色
    
    #给边指定颜色
    color_rel <- c()
    for (i in 1: nrow(G$relationships)){
      if (G$relationships$label[i] == '000'){color_rel[i] = 'Brown'} #一天都没有
      if (G$relationships$label[i] == '100'){color_rel[i] = 'red'} #day3 红色
      if (G$relationships$label[i] == '010'){color_rel[i] = 'green'} #day5 绿色
      if (G$relationships$label[i] == '001'){color_rel[i] = 'blue'} #day8 蓝色
      if (G$relationships$label[i] == '110'){color_rel[i] = 'yellow'} #day3, day5  黄色
      if (G$relationships$label[i] == '011'){color_rel[i] = 'Aqua'} #day5, day8 亮蓝色
      if (G$relationships$label[i] == '101'){color_rel[i] = 'pink'} #day3, day8 粉色
      if (G$relationships$label[i] == '111'){color_rel[i] = 'black'} #day3, day5, day8
    }
    
    G$relationships$color <- color_rel
    #使线变的平滑
    if (nrow(G$relationships) <20) {G$relationships$smooth = TRUE}  #使线变得平滑，不会是直线，变成一条比较松的线, 前提是边少于30个
    #------------------------------------------------
    #根据属性relationship为每个节点设置分组
    
    group <- c()
    for (i in 1:nrow(G$nodes)){
      if (G$nodes$id[i] %in% G$relationships$from){A <- unique( G$relationships$label[ G$relationships$from %in% G$nodes$id[i] ]) } else{A <- NULL} 
      if (G$nodes$id[i] %in% G$relationships$to){B <- unique( G$relationships$label[ G$relationships$to %in% G$nodes$id[i] ]) } else{B <- NULL}
      AB <- unique(c(A, B))
      group[i] <- paste0( unique(c(A, B)), collapse = "," )
    }
    G$nodes$group <- group
    #===============================================
    #如果大于边10， 就不要label这一列了。
    if (nrow(GI$relationships) > 10){
      GI$relationships <- GI$relationships[,-3]
    }
    #===============================================
    list(results0 = results0 ,G = G, GI = GI)
  })
  #-----------------------------------
  #########################################
  #visNetwork作图
  #########################################
  output$task1_network <- renderVisNetwork({
    #===================================================
    #作图
    #====================================================
    library(visNetwork)
    visNetwork::visNetwork(task1_data()$G$nodes, task1_data()$G$relationships) %>%
      visInteraction(navigationButtons = TRUE) %>% #增加一些控件来对图进行移动啊，放大缩小啊
      visOptions(highlightNearest = list(enabled = TRUE, #highlightNearest:点击一个节点会只显示这个节点所有关系,
                                         hover = FALSE, #hover设定为TRUE,是当鼠标悬停在某个节点时，可以只显示跟这个节点所有关系
                                         algorithm = "hierarchical"), #这个算法， 只查看当前选择节点的关系
                 manipulation = TRUE) %>%  #manipulation编辑按钮，可以增加/删除节点和边  
      #visEdges(color = list( highlight = "red",hover = "red")) %>% #悬停到边的时候这条边变色为红色
      visIgraphLayout()  %>%  #Use igraph layout, use all available layouts in igraph and calculate coordinates
    # visLegend() #设置图例的
      visExport()
  })
  
  #----增加根据基因名筛选节点-------------------------------------------
  observe({
    visNetworkProxy("task1_network") %>%
      visOptions(nodesIdSelection = list(enabled = input$nodesIdSelection,  useLabels = TRUE, main = "Select by gene"),
                 highlightNearest = list(enabled = TRUE, hover = FALSE, algorithm = "hierarchical"), manipulation = TRUE)#根据基因名选择节点
  })
  #-----增加根据组别筛选节点 -------------------------------------------
  observe({
    if(input$selectedby){
      visNetworkProxy("task1_network") %>%
        visOptions(selectedBy = list(variable = 'group', multiple = T, main = "Select by times"),
                   highlightNearest = list(enabled = TRUE, hover = FALSE, algorithm = "hierarchical"), manipulation = TRUE)}#根据时间选择节点
  })
  
  #总过多少个节点在图中
  output$total_nodes_number <- renderText({
    paste0(nrow(task1_data()$G$nodes), " nodes are in this plot." )
  })
  #总共多少条边
  output$total_edges_number <- renderText({
    paste0(nrow(task1_data()$G$relationships), " edges are in this plot." )
  })
  
  #--------------------------------------------------------------------------
  #==========================================================================
  ##############################
  #输出节点/关系
  ##############################
  #-----输出节点--------
  output$Nodes_informations <- renderText({
    if(input$See_nodesTable){"Nodes informations"} 
  })
  
  output$nodeTable <- renderTable({
    if(input$See_nodesTable)  #这个按钮放在这，就会运行一次
    {task1_data()$results0$nodes[1:10,]}
  })
  #----输出边----------
  output$Edges_informations <- renderText({
    if(input$See_edgesTable){"Edges informations"} 
  })
  output$edgesTable <- renderTable({
    if(input$See_edgesTable) {task1_data()$results0$relationships[1:10,]}
  })
  #==========================================================================
  ############################
  #下载节点/边
  ############################
  #---下载节点信息----
  output$download_nodeTable <- downloadHandler(
    filename = function(){ paste(input$pathway_ID, "_nodes.csv", sep = "")},
    content = function(file){
      write.csv(task1_data()$results0$nodes, file, row.names = FALSE)}
    )
  #---下载边信息---
  output$download_edgeTable <- downloadHandler(
    filename = function(){ paste(input$pathway_ID, "_edges.csv", sep = "")},
    content = function(file){
      write.csv(task1_data()$results0$relationships, file, row.names = FALSE)}
  )
  ############################
  #igraph作图
  ############################
  #----igraph作图-------------------------------
   output$task1_igraph <- renderPlot({
     library(igraph)
     #转换成igraph格式
     ig <- igraph::graph_from_data_frame(
       d = task1_data()$GI$relationships, 
       directed = FALSE, 
       vertices = task1_data()$GI$nodes
     )
     #===========================
     #作图
     #===========================
     #布局
     if (input$Select_igraph_layput){
       if (input$igraph_layout == "circle" ){L <- layout_in_circle(ig)
       }else if (input$igraph_layout == "randomly"){L <- layout_randomly(ig)
       }else if (input$igraph_layout == "sphere" ){L <- layout_on_sphere(ig)
       }else if (input$igraph_layout == "Fruchterman-Reingold"){L <- layout_with_fr(ig)
       }else if (input$igraph_layout == "Kamada-Kawai"){L <- layout_with_kk(ig)
       }else if (input$igraph_layout == "Large_Graph"){L <- layout_with_lgl(ig)
       }else if (input$igraph_layout == "Merging_graph"){L <- layout_components(ig)
       }else if (input$igraph_layout == "appropriate_graph"){L <- layout_nicely(ig)
       }else if (input$igraph_layout == "Davidson-Harel"){L <- layout_with_dh(ig)
       }else if (input$igraph_layout == "DrL_graph"){L <- layout_with_drl(ig)
       }else if (input$igraph_layout == "multidimensional_scaling"){L <- layout_with_mds(ig)
       }else {L <- layout_with_graphopt(ig)}
       }
     else{
         L = layout_randomly(ig)}
     
     #coummunity 和作图
     if (input$Selcet_igraph_community){
       if (input$igraph_community == "edge_betweenness"){ 
         clu <- cluster_edge_betweenness(ig)
         plot(clu, ig, layout = L)}
       if (input$igraph_community == "propagating_labels"){
         clu <- cluster_label_prop(ig) 
         plot(clu, ig, layout = L)}
       if (input$igraph_community =="greedy_optimization_of_modularity"){
         clu <- cluster_fast_greedy(as.undirected(ig))
         plot(clu, as.undirected(ig),layout = L)}
     }
     else{
       plot(ig, layout = L)
     }
       
   }) 
}
#=======================================
#最后一步
#=======================================
shinyApp(ui = ui, server = server)
