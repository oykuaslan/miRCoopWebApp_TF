fluidPage(
  
  tags$head(tags$script(src="js/index.js")),
  
  tags$head( # must include css
    tags$style(HTML("
        .img-local {
        }
        .small-box .img-local {
        position: absolute;
        top: auto;
        bottom: 5px;
        right: 5px;
        z-index: 0;
        font-size: 70px;
        color: rgba(0, 0, 0, 0.15);
        }"
    ))
  ),
  

  tags$style(HTML('table.dataTable tr.selected td, table.dataTable td.selected {background-color: #179E93 !important;}')),
  tags$style(HTML(".shiny-notification {position:fixed;top: 30%;left: 30%;right: 30%;}")),
  

  navbarPage(title = div(class ="logo",img(src="logo.png")),
             id = "mirCoop",
             tabPanel('Home',
                      includeCSS("www/ui_styles.css"),

                      div(class = "container_circ",
                          div(id = "first_circ",
                              img(class = "icon_1",src = "icon_1.png"),
                              p(class = "intro_header1", 
                                a(style = "color: #1A1A1A;","Cancer Specific Synergy Modules", onclick="customHref('CancerSpecificSynergyModules')")
                                )
                              ),
                          div(id = "second_circ",
                              img(class = "icon_2",src = "icon_2.png"),
                              p(class = "intro_header2", 
                                a(style = "color: #1A1A1A;","Pan-cancer Synergy Modules", onclick="customHref('Pan-cancerSynergyModules')")
                                )
                              ),
                          div(id = "third_circ",
                              img(class = "icon_3",src = "icon_3.png",
                                  p(class = "intro_header3", 
                                    a(style = "color: #1A1A1A;","Pan-cancer miRNA Synergy Modules", onclick="customHref('Pan-cancermiRNASynergyModules')")
                                    )
                                  )
                              ),
                          div(id = "fourth_circ",
                              img(class = "icon_4",src = "icon_4.png",
                                  p(class = "intro_header4",
                                    a(style = "color: #1A1A1A;","Statistics", onclick="customHref('StatisticsSynergyModules')")
                                    )
                                  )
                              )
                      ),
                      div(class = "intro_exp_div",
                          p(class = "intr_exp","miRCoop uses kernel-based statistical interaction tests, together with miRNA and mRNA target information to identify synergistic miRNA pairs that have weak or no repression on the target mRNA individually, but when act together, induce strong repression. miRCoop web-based user interface allows users to examine the potential triplet interactions." ,
                            span(
                                 a(style ="color:#F1595A"," › Learn more", onclick="customHref('AboutSynergyModules')")
                                 )
                            )
                        ),
                      div(class = "footer_box_intro",
                          img(class = "footer_sabanci",
                              src = "sabanci.png"
                          ),
                          img(class = "footer_bilkent", src = "bilkent.png"),
                          p(class = "footer_text",
                              "miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University. This project was supported by the Health Institutes of Turkey [(TUSEB) to O.T - 4382]. For any enquiries or bug reports please send us an e-mail: oyku.aslan@sabanciuniv.edu"
                            )
                          )
             ),
             navbarMenu(title = "Synergy Modules",
               tabPanel("Cancer Specific Synergy Modules",value="CancerSpecificSynergyModules",fluid = TRUE,
                         sidebarLayout(
                           sidebarPanel(width = 3,
                                        selectInput(inputId = "SynergyModulesType", label = "Select Interaction Type",choices = c("Type 1","Type 2","Type 3"),selected = "Type 3",multiple = FALSE),
                                        conditionalPanel("input.SynergyModulesType == 'Type 3'",
                                                                          selectInput(inputId = "datasetTF",
                                                                                      label = p("Cancer Type ",infoBtn('workingPop') %>% 
                                                                                                  spsComps::bsTooltip(
                                                                                                    title = "TCGA Abbreviations of the cancer types",
                                                                                                    placement = "right",
                                                                                                    trigger = "hover"
                                                                                                    
                                                                                                  )),
                                                                                      choices = cancerNames,
                                                                                      selected = "50 Free",
                                                                                      width = "220px"
                                                                          ),
                                                                          hr(),
                                                                          selectizeInput("mirnaFilterTF_Type3", "miRNA Filter", choices=NULL, multiple=TRUE),
                                                                          hr(),
                                                                          br(),
                                                                          selectizeInput("tfFilterTF_Type3", "TF Filter", choices=NULL, multiple=TRUE),
                                                                          br(),
                                                                          selectizeInput("targetFilterTF_Type3", "Target mRNA Filter", choices=NULL, multiple=TRUE),
                                                                          br(),
                                                                          sliderInput("Lancaster_XY_Z_rangeTF",
                                                                                      label = p("Triplet pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                                                                                      value = c(0,0.01),
                                                                                      min = 0,
                                                                                      max = 0.01),
                                                                          hr(),
                                                                          checkboxInput("tfActivatorTF", "Show Activator TFs", value = TRUE, width = NULL),
                                                                          checkboxInput("tfRepressorTF", "Show Repressor TFs", value = TRUE, width = NULL),
                                                                          hr(),
                                                                          downloadButton("downloadDataTF", "Download")
                                                                          
                                        )
                        #                selectInput(inputId = "dataset",
                        #                            label = p("Cancer Type ",infoBtn('workingPop') %>% 
                        #                                        spsComps::bsTooltip(
                        #                                          title = "TCGA Abbreviations of the cancer types",
                        #                                          placement = "right",
                        #                                          trigger = "hover"
                        #                                          
                        #                                        )),
                        #                            choices = cancerNames,
                        #                            selected = "50 Free",
                        #                            width = "220px"
                        #                ),
                        #                uiOutput("dataset"),
                        #                
                        #                hr(),
                        #                selectizeInput("mrnaFilter", "mRNA Filter", choices=NULL, multiple=TRUE),
                        #                br(),
                        #                selectizeInput("mirnaFilter", "miRNA Filter", choices=NULL, multiple=TRUE),
                        #                hr(),
                        #                
                        #                
                        #                sliderInput("Lancaster_XY_Z_range",
                        #                            label = p("Triplet pvalue ",a(infoBtn('workingPop'), onclick="customHref('About')")),
                        #                            value = c(0,0.01),
                        #                            min = 0,
                        #                            max = 0.01),
                        #                
                        #                hr(),
                        #                
                        #                
                        #                hr(),
                        #                checkboxGroupInput(inputId = "is_mrna_tf",
                        #                                   label = "Filter out:",
                        #                                   choiceNames = c("mRNAs that are not TF"),
                        #                                   choiceValues = c("True"),
                        #                                   selected = NULL)
                        #                
                        #                
                        #                
                        #                
                        #                
                           ),
                           mainPanel(
                            tabsetPanel(type = "tabs",
                                        tabPanel("Table", br(),
                                                 DT::dataTableOutput("table", height = "800px"),
                                                 div(style = "margin-bottom:15px",
                                                   downloadButton("downloadData", "Download")
                                                   )),

                                        tabPanel("Network",
                                                 # conditionalPanel(condition = "input.dataset == 'ACC' || input.dataset == 'DLBC' ||
                                                 #                  input.dataset == 'LGG' || input.dataset == 'MESO' || input.dataset == 'OV' || input.dataset == 'TGCT'  ||
                                                 #                  input.dataset == 'UCS' || input.dataset == 'UVM'",
                                                 #                  selectInput("colorGroup1", "Color Nodes Based on :",
                                                 #                              c("miRNA Family", "miRNA Cluster"),selected = NULL)
                                                 #                  ),
                                                 # 
                                                 # conditionalPanel(condition = "input.dataset == 'BLCA' || input.dataset == 'BRCA' || input.dataset == 'CESC' ||
                                                 #                  input.dataset == 'CHOL' ||  input.dataset == 'COAD' ||  input.dataset == 'ESCA' || input.dataset == 'HNSC' ||
                                                 #                  input.dataset == 'KICH' || input.dataset == 'KIRC' || input.dataset == 'KIRP' || input.dataset == 'LIHC' ||
                                                 #                  input.dataset == 'LUAD' || input.dataset == 'LUSC' || input.dataset == 'PAAD' || input.dataset == 'PCPG' ||
                                                 #                  input.dataset == 'PRAD' || input.dataset == 'READ' || input.dataset == 'SARC' || input.dataset == 'SKCM' ||
                                                 #                  input.dataset == 'STAD' || input.dataset == 'THCA' || input.dataset == 'THYM' || input.dataset == 'UCEC'
                                                 #                  ",
                                                 #                  selectInput("colorGroup2", "Color Nodes Based on :",
                                                 #                              c("Differential Expression Analysis","miRNA Family", "miRNA Cluster"),selected = NULL)
                                                 # ),
                                                 # 
                                                 # shinycustomloader::withLoader(visNetworkOutput("vNetwork", height = "100vh"),type = "html",loader="loader3"),
                                                 # div(align = "right", downloadButton("downloadNodeTable", "Download Node Table"),
                                                 #     downloadButton("downloadEdgeTable", "Download Edge Table")),
                                                 br(),
                                                 br())
                            )
                            )
                          )
               ),
               
               
               tabPanel("Pan-cancer Synergy Modules", value="Pan-cancerSynergyModules",fluid = TRUE
                        # sidebarLayout(
                        #   sidebarPanel(
                        #     
                        #     shinyWidgets::pickerInput(inputId = "CommonTripletCancer", label = "Select Cancer Names",choices = cancerNames, selected=cancerNames, multiple = TRUE,options = pickerOptions(list(`actions-box` = TRUE))),
                        #     hr(),
                        #     selectizeInput("mrnaCommonTriplet", "mRNA Filter", choices=unique(mrnaFilterCommonTriplets$name), multiple=TRUE),
                        #     br(),
                        #     selectizeInput("mirnaCommonTriplet", "miRNA Filter", choices=unique(mirnaFilterCommonTriplets$name), multiple=TRUE),
                        #     hr(),
                        #     br(),
                        #     downloadButton("downloadCommonTripletsData", "Download")
                        #     
                        #   ),
                        #   
                        #   mainPanel(
                        #     useShinyalert(),
                        #     tabsetPanel(type = "tabs",
                        #                 tabPanel("Table", br(),
                        #                          DT::dataTableOutput("tableCommonTriplet")),
                        #                 tabPanel("Network", shinycustomloader::withLoader(visNetworkOutput("commonTripletNetwork", height = "100vh"),type = "html",loader="loader3"),
                        #                          br(),
                        #                          div(align = "right",downloadButton("downloadNodeTableCommonTriplet", "Download Node Table"),
                        #                              downloadButton("downloadEdgeTableCommonTriplet", "Download Edge Table")),
                        #                          br(),
                        #                          br()
                        #                 )
                        #     )
                        #   )
                        #   
                        # )
               ),
               
               tabPanel("Pan-cancer miRNA Synergy Modules", value="Pan-cancermiRNASynergyModules",fluid = TRUE
                        # sidebarLayout(
                        #   sidebarPanel(
                        #     
                        #     # checkboxGroupInput(inputId = "CommonCancer", 
                        #     #                    label = "Select Cancer Names",
                        #     #                    choices = cancerNames,
                        #     #                    selected=cancerNames),
                        #    
                        #     pickerInput(inputId = "CommonMirnaPairCancer", label = "Select Cancer Names",choices = cancerNames, selected=cancerNames, multiple = TRUE),
                        #     hr(),
                        #     selectizeInput("mirnaCommonMirnaPair", "miRNA Filter", choices=mirnaListCommonMirnaPairs, multiple=TRUE),
                        #     hr(),
                        #     selectInput(inputId = "CommonMirnaPairCancerCount", label = "Select with Count Above:",choices = c(1,2,3,4), selected=c(1), multiple = FALSE),
                        #     # checkboxGroupInput(inputId = "CommonMirnaPairCancerCount",
                        #     #                    label = "Filter with Count",
                        #     #                    choiceNames = c("Above 3","Above 4","Above 5","Above 6", "Above 7","Above 8"),
                        #     #                    choiceValues = c("Above3","Above4","Above5","Above6", "Above7","Above8"),
                        #     #                    selected = c("Above3","Above4","Above5","Above6", "Above7","Above8")),
                        #     #numericInput("CommonMirnaPairCancerCount2", "Select with Count Above:", 2, min = 2, max = 8, step = 1),
                        #     br(),
                        #     br(),
                        #     downloadButton("downloadCommonMirnaPairsData", "Download")
                        #     
                        #   ),
                        #   
                        #   mainPanel(
                        #     useShinyalert(),
                        #     tabsetPanel(type = "tabs",
                        #                 tabPanel("Common miRNA Pair Table", DT::dataTableOutput("tableCommonmiRNAPair")
                        #                          ),
                        #                 tabPanel("Network", selectInput("colorCommonMirna", "Color Nodes Based on :",
                        #                                                 c("miRNA Family", "miRNA Cluster"),selected = NULL),
                        #                          shinycustomloader::withLoader(visNetworkOutput("commonMirnaNetwork", height = "100vh"),type = "html",loader="loader3"),
                        #                          br(),
                        #                          div(align = "right", downloadButton("downloadNodeTableCommonMirnaPair", "Download Node Table"),
                        #                              downloadButton("downloadEdgeTableCommonMirnaPair", "Download Edge Table")),
                        #                          br(),
                        #                          br())
                        #                 
                        #                 
                        #     )
                        #   )
                        #   
                        # )
               )
               # tabPanel("Cancer Specific Synergy Modules", value = "CancerSpecificSynergyModules", fluid = TRUE,
               #          sidebarLayout(
               #            sidebarPanel(
               #              selectInput(inputId = "CancerSpecificSynergyModulesCancer",
               #                          label = p("Cancer Type ",infoBtn('workingPop') %>% 
               #                                      spsComps::bsTooltip(
               #                                        title = "TCGA Abbreviations of the cancer types",
               #                                        placement = "right",
               #                                        trigger = "hover"
               #                                        
               #                                      )),
               #                          choices = c("ACC","DLBC","KICH"),
               #                          selected = "50 Free",
               #                          width = "220px"
               #              ),
               #              hr(),
               #            ),
               #            
               #            mainPanel(
               #              useShinyalert(),
               #              tabsetPanel(type = "tabs",
               #                          tabPanel("Cancer Specific Synergy Modules", DT::dataTableOutput("CancerSpecificSynergyModulesType3")
               #                          )
               #                          
               #              )
               #            )
               #            
               #          )
               #          
               #   
               # )
             ),
             tabPanel("Statistics", value = "Statistics", fluid= TRUE
                      #   div(class = "total_counts_plot_exp_div",
                      #         div(class = "total_counts_plot_title", "Counts Across All Cancer Types"),
                      #         div(class = "total_counts_plot_exp", "Each bar represents statics for one cancer, from left to right: number of triplets found in the cancer, the number of unique miRNA pairs that participate in these triplets, number of miRNAs unique in the triplets, number of unique mRNAs.")
                      #       ),
                      #   div(class = "total_counts_plot",
                      #       shinycustomloader::withLoader(plotlyOutput("totalCountsPlot"),type = "html",loader="loader3")
                      #       ),
                      #   div(class = "mrna_heatmap_div",
                      #       div(class = "mrna_heatmap_title", "Most Frequent mRNAs Across All Cancers"),
                      #       div(class = "mrna_heatmap_exp","The heatmap shows the normalized number of triplets the mRNA participates in each cancer. The numbers are normalized by the number of triplets found in the cancer. mRNAs that have more than 20 total participation are shown.")
                      #       ),
                      # div(class = "mrna_heatmap_plot",
                      #     shinycustomloader::withLoader(plotlyOutput("commonMrnaHeatmap",height=687),type = "html",loader="loader3")
                      #     ),
                      # div(class = "mirna_heatmap_div",
                      #     div(class = "mirna_heatmap_title", "Most Frequent miRNAs Across All Cancers"),
                      #     div(class = "mirna_heatmap_exp","The heatmap shows the normalized number of triplets the miRNA participates in each cancer. The numbers are normalized by the number of triplets found in the cancer. miRNAs that have more than 50 total participation are shown.")
                      #     ),
                      # div(class = "mirna_heatmap_plot",
                      #     shinycustomloader::withLoader(plotlyOutput("commonMirnaHeatmap",height=627),type = "html",loader="loader3")
                      #     ),
                      # div(class = "scatter_exp_div",
                      #   div(class = "mrnaScatter_exp","The number of mRNAs the miRNA targets plotted against the number of triplets the mRNA is found in. The Pearson correlation is found 0.3429"),
                      #   div(class = "mirnaScatter_exp", "The number of miRNAs that target the mRNA plotted against the number of triplets the miRNA is found in. The Pearson correlation is found 0.63")
                      #   ),
                      # div(class = "scatter_div",
                      #     div(class="mrna_scatter",shinycustomloader::withLoader(plotlyOutput("MrnaScatterPlot"),type = "html",loader="loader3")),
                      #     div(class="mirna_scatter",shinycustomloader::withLoader(plotlyOutput("MirnaScatterPlot"),type = "html",loader="loader3"))
                      #     ),
                      # div(class = "footer_box_stats",
                      #     img(class = "footer_sabanci",
                      #         src = "sabanci.png"
                      #     ),
                      #     img(class = "footer_bilkent", src = "bilkent.png"),
                      #     div(class = "footer_text",
                      #         "miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University. This project was supported by the Health Institutes of Turkey [(TUSEB) to O.T - 4382]. For any enquiries or bug reports please send us an e-mail: oyku.aslan@sabanciuniv.edu")
                      #     )
             ),
             
             tabPanel("About",value="About",fluid = TRUE
                      # fluidRow(
                      #   column (12,
                      #           # h5(p(align = "justify;",style="font-size:16px;","miRCoop identifies synergistic miRNA pairs which have weak or no repression on the target mRNA, but when bound together induce strong repression of their target's. To achieve this, a three-step method was proposed. First, miRNA pairs targeting the common mRNA were identified. For this, experimentally validated databases(miRTarBase, TarBase v7.0, miRecords ) and a prediction algorithm (TargetScan) were resorted. A miRNA-mRNA target catalogue was generated by intersecting miRNA-mRNA pairs from 3 experimentally validated databases with those from TargetScan. The triplets where miRNA pairs have an overlapping binding site on the target mRNA were filtered. miRNA pairs targeting common mRNA composed a potential triplet. Secondly, potential triplets obtained from Step 1 were eliminated according to the expression profiles of miRNAs and mRNAs. The rationale for the exclusion of the potential triplets is based on this assumption: The mRNA expression level is expected to be lower when both miRNAs are upregulated compared to when both miRNAs are downregulated. In the thid step, statistical interaction tests were performed on miRNA and mRNA expression data for each potential triplet candidate that passes through Step2. We are interested in cases where miRNAs are pairwise independent with mRNAs but form a mutually dependent triplet. ")
                      #           # ),
                      #           div(class = "about_1",
                      #               p(class = "about_1_text",
                      #                 "miRCoop uses kernel-based statistical interaction tests, together with miRNA and mRNA target information to identify synergistic miRNA pairs that have weak or no repression on the target mRNA individually, but when act together, induce strong repression. We applied our approach to patient data of various The Cancer Genome Atlas Projects(TCGA) cancer types. miRCoop web-based user interface allows users to examine the potential triplet interactions.")
                      #           ),
                      #           div(class = "about_2",
                      #               p(class = "about_2_text",
                      #                 "We believe miRCoop can aid our understanding of the complex regulatory interactions in different health and disease states of the cell and can help in designing miRNA-based therapies."),
                      #               ),
                      #           div(
                      #             img(class = "steps_image",src ="miRCoopSteps.png")
                      #           ),
                      #           div(class = "about_3",
                      #               p(class = "about_3_text",
                      #                 "miRCoop identifies synergistic miRNA pairs which have weak or no repression on the target mRNA, but when bound together induce strong repression of their targets. To achieve this, a three-step method was proposed. First, miRNA pairs targeting the common mRNA were identified. For this, experimentally validated databases(miRTarBase, TarBase v7.0, miRecords ) and a prediction algorithm (TargetScan) were resorted. A miRNA-mRNA target catalogue was generated by intersecting miRNA-mRNA pairs from 3 experimentally validated databases with those from TargetScan. The triplets where miRNA pairs have an overlapping binding site on the target mRNA were filtered. miRNA pairs targeting common mRNA composed a potential triplet. Secondly, potential triplets obtained from Step 1 were eliminated according to the expression profiles of miRNAs and mRNAs. The rationale for the exclusion of the potential triplets is based on this assumption: The mRNA expression level is expected to be lower when both miRNAs are upregulated compared to when both miRNAs are downregulated. In the thid step, statistical interaction tests were performed on miRNA and mRNA expression data for each potential triplet candidate that passes through Step2. We are interested in cases where miRNAs are pairwise independent with mRNAs but form a mutually dependent triplet. ")
                      #           ),
                      #           div(
                      #               img(class = "web_image", src = "mircoop_web.png")
                      #           ),
                      #           div(class = "about_4",
                      #               p(class = "about_4_text",
                      #                 "The triplets detected for 31 different cancers can be examined in the ‘Cancer Specific Triplets’ screen. It takes pre-computed results as data source and presents themit to the user with a datatable in a structured manner. This section essentially builds on the identified triplets, miRNA pairs and their target mRNA, which is represented with both HGNC symbol and Entrez Gene ID, and triplet p-values. The data has been enriched with the following additional information:", br(),br(),"› Experimental data source of miRNA and mRNA relationships",br(),  "› Differential expression analysis results: Differential expression analysis was performed by comparing the expression of each miRNAs and mRNAs between Primary Tumor samples and Solid Tissue Normal samples. Positive logFC values indicate upregulation in the primary tumor samples compared to solid tissue normal samples. In contrast, a negative logFC value shows downregulation in the primary tumor samples compared to solid tissue normal. A p-value < 0.05 is selected as cut-off criteria to define statistically significant difference for the logFC",br(),"› Literature support of cancer-miRNA and cancer-mRNA relationship",br(), "› mRNA expressions of patients grouped by miRNA expression levels", br(),"› Transcription factor information")
                      #           ),
                      #           div(class = "about_cite_box",
                      #               div(class = "about_cite_inside",
                      #                 p(class = "about_cite_line1", "If you find miRCoop useful for your research, please cite the following papers:"),
                      #                 p(class = "about_cite_line2",
                      #                   a(href="https://ieeexplore.ieee.org/document/9311836","miRCoop: Identifying Cooperating miRNAs via Kernel Based Interaction Tests")
                      #                   ),
                      #                 p(class = "about_cite_line3",
                      #                   "G. Olgun and O. Tastan, IEEE/ACM Transactions on Computational Biology and Bioinformatics, doi: 10.1109/TCBB.2020.3047901."
                      #                   )
                      #               )
                      #               ),
                      #           div(class = "footer_box",
                      #               img(class = "footer_sabanci",
                      #                   src = "sabanci.png"
                      #                   ),
                      #               img(class = "footer_bilkent", src = "bilkent.png"),
                      #               div(class = "footer_text",
                      #                 "miRCoop is a product of the collaboration between the Department of Computer Science and Engineering from Sabanci University and Department of Molecular Biology and Genetics from Bilkent University. This project was supported by the Health Institutes of Turkey [(TUSEB) to O.T - 4382]. For any enquiries or bug reports please send us an e-mail: oyku.aslan@sabanciuniv.edu")
                      #               )
                      #           
                      #   ),
                      #   
                      # ),
             ),
             tabPanel("Glossary", value="Glossary",fluid = TRUE,
                      fluidRow(
                        column(6,
                               h4(p("Glossary")),
                               tags$div(style="font-size:16px;",tableOutput("Glossary"))
                        ),
                        column(6, 
                               h4(p("Abbreviations and Full Names of TCGA Projects")),
                               tags$div(style="font-size:16px;",tableOutput("TCGAAbbrv"))
                        )
                      )
                      
             )
             
             
  )
  
  
)


