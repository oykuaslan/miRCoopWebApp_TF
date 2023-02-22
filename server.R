function(input, output, session) {
  
  
  true_false_formatter <-
    formatter("span",
              style = x ~ style(
                font.weight = "bold",
                color = ifelse(x == 'true', "forestgreen", ifelse(x == 'false', "red", "black"))
              ))
  
  button <- function(tbl){
    function(i){
      sprintf(
        '<button id="button_%s_%d" type="button" style="background-color:#074487;border-radius:7px; border:1px solid #124d77; color:#ffffff; padding:4px 4px;font-family:Ubuntu;font-size:16px" onclick="%s">Box Plot</button>', tbl,
        i, "Shiny.setInputValue('button', this.id);")
    }
  }

  # datasetInput <- reactive({
  #   switch(input$dataset, 
  #          "ACC" = ACC,
  #          "BLCA"=BLCA,
  #          "BRCA"=BRCA,
  #          "CESC"=CESC,
  #          "CHOL"=CHOL,
  #          "COAD"=COAD,
  #          "DLBC"=DLBC,
  #          "ESCA"=ESCA,
  #          "HNSC"=HNSC,
  #          "KICH"=KICH,
  #          "KIRC"=KIRC,
  #          "KIRP"=KIRP,
  #          "LGG"=LGG,
  #          "LIHC"=LIHC,
  #          "LUAD"=LUAD,
  #          "LUSC"=LUSC,
  #          "MESO"=MESO,
  #          "OV"=OV,
  #          "PAAD"=PAAD,
  #          "PCPG"=PCPG,
  #          "PRAD"=PRAD,
  #          "READ"=READ,
  #          "SARC"=SARC,
  #          "SKCM"=SKCM,
  #          "STAD"=STAD,
  #          "TGCT"=TGCT,
  #          "THCA"=THCA,
  #          "THYM"=THYM,
  #          "UCEC"=UCEC,
  #          "UCS"=UCS,
  #          "UVM"=UVM
  #   )
  # })
  
  
  datasetInputTF <- reactive({
    if(input$SynergyModulesType =="Type 3"){
      switch(input$datasetTF, 
              "ACC" = ACC_Type3,
             # "BLCA"=BLCA,
              "BRCA"=BRCA_Type3,
              "CESC"=CESC_Type3,
            #  "CHOL"=CHOL_Type3,
              "COAD"=COAD_Type3,
              "DLBC"=DLBC_Type3,
              "ESCA"=ESCA_Type3,
             # "HNSC"=HNSC,
             "KICH"=KICH_Type3,
             "KIRC"=KIRC_Type3,
             "KIRP"=KIRP_Type3,
             # "LGG"=LGG,
             # "LIHC"=LIHC,
             "LUAD"=LUAD_Type3,
             "LUSC"=LUSC_Type3,
             "MESO"=MESO_Type3,
             "OV"=OV_Type3,
             "PAAD"=PAAD_Type3,
             "PCPG"=PCPG_Type3,
             "PRAD"=PRAD_Type3,
             "READ"=READ_Type3,
             # "SARC"=SARC,
              "SKCM"=SKCM_Type3,
             # "STAD"=STAD,
             # "TGCT"=TGCT,
              "THCA"=THCA_Type3,
             # "THYM"=THYM,
             # "UCEC"=UCEC,
              "UCS"=UCS_Type3,
              "UVM"=UVM_Type3
      )
    }
    
  })
  
  observeEvent(input$SynergyModulesType,{
    
      if(input$SynergyModulesType =="Type 3"){
        print("GİRDİ")
        observeEvent(input$datasetTF,{
          updateSelectizeInput(session,"tfFilterTF_Type3",choices = unique(datasetInputTF()$TF))
        })

        observeEvent(input$datasetTF,{
          updateSelectizeInput(session,"targetFilterTF_Type3",choices = unique(datasetInputTF()$Target))
        })

        observeEvent(input$datasetTF,{
          #mirnaList <- stringr::str_remove(stringr::str_replace(union_all(datasetInput()$mirna1,datasetInput()$mirna2),"mir","miR"),"hsa-")
          mirnaList <- stringr::str_replace(union_all(datasetInputTF()$miRNA1,datasetInputTF()$miRNA2),"mir","miR")
          updateSelectizeInput(session,"mirnaFilterTF_Type3",choices = unique(mirnaList))
        })
      }
  })
  
  
  datasetFinal <- reactive({
    
    
    mirnaFilter <- NULL
    mrnaFilter <- NULL
    TFFilter <- NULL
    targetFilter <- NULL
    
    dataset <- datasetInputTF()
    
    if(length(nrow(dataset)) >0 & !is.null(dataset)){
      if(length(input$mirnaFilterTF_Type3) == 0 & length(input$tfFilterTF_Type3) == 0 & length(input$targetFilterTF_Type3) == 0){
        concated <- datasetInputTF()
      }
      
      if(length(input$tfFilterTF_Type3) > 0){
        
        TFFilter <- filter(datasetInputTF(),TF %in% input$tfFilterTF_Type3)
      }
      
      if(length(input$targetFilterTF_Type3) > 0){
        
        targetFilter <- filter(datasetInputTF(),Target %in% input$targetFilterTF_Type3)
      }
      
      if(length(input$mirnaFilterTF_Type3) >0){
        mirnaFilter <- filter(datasetInputTF(), miRNA1 %in% tolower(input$mirnaFilterTF_Type3) | miRNA2 %in% tolower(input$mirnaFilterTF_Type3))
      }
      
      if(length(input$mirnaFilterTF_Type3) != 0 || length(input$tfFilterTF_Type3) != 0 ||  length(input$targetFilterTF_Type3) != 0){
        concated <- distinct(rbind(rbind(TFFilter, mirnaFilter),targetFilter))
      }
      
    }
    
    dataset1 <- concated
    
    # if(length(nrow(dataset1 >0)) & !is.null(dataset1)){
    #   
    #   if(length(tolower(input$is_mrna_tf)) !=0){
    #     dataset1 <- filter(concated,tolower(is_mrna_tf) %in% tolower(input$is_mrna_tf))
    #     if(nrow(dataset1) == 0){
    #       dataset1 <- NULL
    #       
    #     }
    #   }
    #   
    #   else{
    #     dataset1 <- concated
    #     
    #   }
    #   
    # }
    # else{
    #   dataset1 <- NULL
    # }
    
    if(length(nrow(dataset1))>0 & !is.null(dataset1)){
      filteredWithTests <-filter(dataset1,
                                 round(mirna1mirna2_target,4) >= round(input$Lancaster_XY_Z_rangeTF[1],4), round(mirna1mirna2_target,4) <=round(input$Lancaster_XY_Z_rangeTF[2],4))
      if(nrow(filteredWithTests) == 0){
        filteredWithTests <- NULL
      }
      
    }
    else{
      filteredWithTests <- NULL
    }
    
    # if(length(nrow(filteredWithTests))>0 & !is.null(filteredWithTests)){
    #   
    #   filteredWithBH_value <-dplyr::filter(filteredWithTests,
    #                                        round(BH_pvalues_adjusted,4) >= round(min_pval,4) & round(BH_pvalues_adjusted,4) <= round(max_pval,4))
    #   
    #   if(nrow(filteredWithBH_value) == 0){
    #     filteredWithBH_value <- NULL
    #     
    #   }
    #   
    # }
    # else{
    #   filteredWithBH_value <- NULL
    # }
    
    return (filteredWithTests)
    
  })

  
  DatasetRoundDigits <-reactive({

    #-> filtreler yapıldıktan sonra aç
    dataset <-datasetFinal()  
    #dataset <-datasetInputTF() 


    if(!is.null(dataset) & length(nrow(dataset)) >0){
      dataset$miRNA1 <- stringr::str_remove(dataset$miRNA1, "hsa-")
      dataset$miRNA2 <- stringr::str_remove(dataset$miRNA2, "hsa-")
      dataset$miRNA1 <- stringr::str_replace(dataset$miRNA1,"mir","miR")
      dataset$miRNA2 <- stringr::str_replace(dataset$miRNA2,"mir","miR")
      dataset <- dataset %>%
        dplyr::mutate(across(where(is.numeric),round,4))


    }
    else{
      dataset <- NULL
    }
    return (dataset)

  })
  
  
  # output$table <- DT::renderDataTable({
  #   data <-DatasetRoundDigits() 
  #   DT::datatable(data)
  # 
  #   })
  # 

  output$table <- DT::renderDataTable({

    if(!is.null(DatasetRoundDigits()) & length(nrow(DatasetRoundDigits())) >0){
      #DT1 <- DatasetRoundDigits()
      DT <- DatasetRoundDigits()
      
      # DT <- cbind(DT1,
      #             button = sapply(1:nrow(DT1), button("table")),
      #             stringsAsFactors = FALSE)
      # 
      # DT[is.na(DT)] <- " "

      # if("mirna1Literature" %in% colnames(DT1)){
      #   DT1$mirna1Literature <- stringr::str_replace(DT1$mirna1Literature,"NA"," ")
      # 
      # }
      # if("mirna2Literature" %in% colnames(DT1)){
      #   DT1$mirna2Literature <- stringr::str_replace(DT1$mirna2Literature,"NA"," ")
      # 
      # }
      # if("mrnaLiterature" %in% colnames(DT1)){
      #   DT1$mrnaLiterature <- stringr::str_replace(DT1$mrnaLiterature,"NA"," ")
      # 
      # }
      # 
      # 
      # hideList1 <- c(7,8,9,10,11,13,14,15,16)
      # hideList2 <- c(7,8,9,10,11,12,13,14,15,16,17,19,20,21,22)
      # hideList3 <- c(7,8,9,10,11,12,13,15,16,17,18)
      # hideList4 <- c(7,8,9,11,12,13,14)
      # hideList5 <-c(7,8,9,10,11,12,13,14,15,17,18,19,20)

      # ifelse(input$dataset=="ACC" || input$dataset=="DLBC" || input$dataset=="LGG" || input$dataset=="MESO" || input$dataset=="OV" || input$dataset=="UCS" || input$dataset=="UVM", columnHideList <-hideList1,
      #        ifelse(input$dataset=="BLCA" || input$dataset=="CESC" || input$dataset=="CHOL" || input$dataset=="ESCA" || input$dataset=="HNSC" || input$dataset=="KICH" || input$dataset=="KIRC" || input$dataset=="KIRP" || input$dataset=="LIHC" || input$dataset=="LUAD" || input$dataset=="LUSC" || input$dataset=="PAAD" || input$dataset=="PCPG" || input$dataset=="PRAD" || input$dataset=="SKCM" || input$dataset=="STAD" || input$dataset=="THCA" || input$dataset=="UCEC"|| input$dataset=="BRCA", columnHideList <-hideList2,
      #               ifelse(input$dataset=="COAD" || input$dataset=="READ" || input$dataset=="SARC", columnHideList <-hideList3,
      #                      ifelse(input$dataset=="TGCT", columnHideList <-hideList4,
      #                             ifelse(input$dataset=="THYM", columnHideList <-hideList5, columnHideList <-c())))))



      tripletvalue <- tags$span(
        "Triplet pvalue",
        a(infoBtn('question'), onclick="customHref('Glossary')")
      )%>% as.character()

      # mirna1Literature <- tags$span(
      #   "miRNA1 Literature",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # mirna2Literature <- tags$span(
      #   "miRNA2 Literature",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # mRNALiterature <- tags$span(
      #   "mRNA Literature",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA1mRNADatabase <- tags$span(
      #   "miRNA1-mRNA Database",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA2mRNADatabase <- tags$span(
      #   "miRNA2-mRNA Database",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA1pvalue <- tags$span(
      #   " miRNA1 pvalue",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA1LogFC <- tags$span(
      #   "miRNA1 LogFC",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA2pvalue <- tags$span(
      #   " miRNA2 pvalue",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA2LogFC <- tags$span(
      #   "miRNA2 LogFC",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # mRNApvalue <- tags$span(
      #   "mRNA pvalue",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # mRNALogFC <- tags$span(
      #   "mRNA LogFC",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # BH_pvalues <-  tags$span(
      #   "Corrected pvalue",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA1Family <-  tags$span(
      #   "miRNA1 Family",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA2Family <-  tags$span(
      #   "miRNA2 Family",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA1Cluster <-  tags$span(
      #   "miRNA1 Cluster",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()
      # 
      # miRNA2Cluster <-  tags$span(
      #   "miRNA2 Cluster",
      #   a(infoBtn('question'), onclick="customHref('Glossary')")
      # ) %>% as.character()

      nameList1 <- c("miRNA1", "miRNA2","TF Entrez ID", "Target Entrez ID", "Mode of Regulation", "Tripet pvalue")
      
      #Input tipleri için özelleştirmek gerekecek
      ifelse(input$datasetTF=="ACC" || input$datasetTF=="BRCA" || input$datasetTF=="CESC" || input$datasetTF=="COAD" ||  input$datasetTF=="DLBC" || input$datasetTF=="ESCA" 
             || input$datasetTF=="HNSC" || input$datasetTF=="KICH"|| input$datasetTF=="KIRC" || input$datasetTF=="KIRP" || input$datasetTF=="LUAD" || input$datasetTF=="LUSC" 
             || input$datasetTF=="MESO" || input$datasetTF=="OV" || input$datasetTF=="PAAD" || input$datasetTF=="PCPG" || input$datasetTF=="PRAD" || input$datasetTF=="READ"
             || input$datasetTF=="SKCM" || input$datasetTF=="THCA" || input$datasetTF=="UCS" || input$datasetTF=="UVM", columnNameList <-nameList1,
              columnNameList <-c())
      
      # ifelse(input$dataset=="ACC" || input$dataset=="DLBC" || input$dataset=="LGG" || input$dataset=="MESO" || input$dataset=="OV" || input$dataset=="UCS" || input$dataset=="UVM", columnNameList <-nameList1,
      #        ifelse(input$dataset=="BLCA" || input$dataset=="CESC" || input$dataset=="CHOL" || input$dataset=="ESCA" || input$dataset=="HNSC" || input$dataset=="KICH" || input$dataset=="KIRC" || input$dataset=="KIRP" || input$dataset=="LIHC" || input$dataset=="LUAD" || input$dataset=="LUSC" || input$dataset=="PAAD" || input$dataset=="PCPG" || input$dataset=="PRAD" || input$dataset=="SKCM" || input$dataset=="STAD" || input$dataset=="THCA" || input$dataset=="UCEC" || input$dataset=="BRCA", columnNameList <-nameList2,
      #               ifelse(input$dataset=="COAD" || input$dataset=="READ" || input$dataset=="SARC", columnNameList <-nameList3,
      #                      ifelse(input$dataset=="TGCT", columnNameList <-nameList4,
      #                             ifelse(input$dataset=="THYM", columnNameList <-nameList5, columnNameList <-c())))))


      icon_formatter <- function() {
        formatter("span",
                  style = x ~ formattable::style(color = ifelse(x, "#179E93", "red")), x ~ icontext(ifelse(x, "ok", "remove"), "")
        )
      }
      mor_formatter <- function(){
        formatter("span", 
                  style = x ~ formattable::style(font.weight = "bold", 
                                                 color = ifelse(x > 0, "#CC2936","1E4382")), 
                  x ~ icontext(ifelse(x > 0, "arrow-up", "arrow-down"), "")
        )
        
      }

      significant_bold <- formatter("span",
                                    style = x ~ formattable::style("font-weight" = ifelse(x <0.05, "bold", NA)))

      sign_formatter <- formatter("span",
                                  style = x ~ formattable::style(color = ifelse(x > 0, "red",
                                                                                ifelse(x < 0, "#074487", "black"))))


      as.datatable(formattable(DT, list(
        mirna1mirna2_target = color_tile("transparent", "lightpink"),
        ModeOfRegulation = mor_formatter()
        #BH_pvalues_adjusted = color_tile("transparent", "lightpink"),
        #is_mrna_tf = icon_formatter(),
        #miRNA1_logFC = sign_formatter,
        #miRNA2_logFC = sign_formatter,
        #miRNA1_pvalue = significant_bold,
        #miRNA2_pvalue = significant_bold

      )),escape = F, fillContainer = TRUE,
      colnames=columnNameList
      #extensions = 'Buttons'
      # options = list(dom = 'Bfrtip',
      #                buttons=list(list(extend = 'colvis', columns = c(5:ncol(DT)))),
      #                columnDefs = list(list(visible=FALSE, targets=columnHideList)),
      #                #headerCallback = JS(headerCallback),
      #                searching=FALSE, paging=TRUE)
      )

      # DT::datatable(DT,escape = F, fillContainer = TRUE,
      #               rownames=T,
      #               colnames=columnNameList
      #               # extensions = 'Buttons',
      #               # options = list(dom = 'Bfrtip',
      #               #                buttons=list(list(extend = 'colvis', columns = c(5:ncol(DT)))),
      #               #                columnDefs = list(list(visible=FALSE, targets=columnHideList))
      #               #                #headerCallback = JS(headerCallback)
      #               #
      #               #                )
      # )


    }
    else{
      #dataset <- datatable(data.frame(Nachricht = "Die ausgewählte Schnittstelle enthält hierfür keine Daten."))
      #DT::datatable(dataset,escape = F, fillContainer = TRUE)
      NULL
    }

  })
  
  
  # observeEvent(input$button, {
  #   splitID <- strsplit(input$button, "_")[[1]]
  #   DT <- DatasetRoundDigits()
  #   tbl <- splitID[2]
  #   row <- splitID[3]
  #   
  #   
  #   #############################################################################################################################################
  #   ACCwMedian1JustMedians <- as.double(ACCwMedian1[strtoi(row),15:ncol(ACCwMedian1)][grepl("[0-9]+",ACCwMedian1[strtoi(row),15:ncol(ACCwMedian1)])])
  #   ACCwMedian2JustMedians <- as.double(ACCwMedian2[strtoi(row),15:ncol(ACCwMedian2)][grepl("[0-9]+",ACCwMedian2[strtoi(row),15:ncol(ACCwMedian2)])])
  #   
  #   BLCAwMedian1JustMedians <- as.double(BLCAwMedian1[strtoi(row),21:ncol(BLCAwMedian1)][grepl("[0-9]+",BLCAwMedian1[strtoi(row),21:ncol(BLCAwMedian1)])])
  #   BLCAwMedian2JustMedians <- as.double(BLCAwMedian2[strtoi(row),21:ncol(BLCAwMedian2)][grepl("[0-9]+",BLCAwMedian2[strtoi(row),21:ncol(BLCAwMedian2)])])
  #   
  #   BRCAwMedian1JustMedians <- as.double(BRCAwMedian1[strtoi(row),18:ncol(BRCAwMedian1)][grepl("[0-9]+",BRCAwMedian1[strtoi(row),18:ncol(BRCAwMedian1)])])
  #   BRCAwMedian2JustMedians <- as.double(BRCAwMedian2[strtoi(row),18:ncol(BRCAwMedian2)][grepl("[0-9]+",BRCAwMedian2[strtoi(row),18:ncol(BRCAwMedian2)])])
  #   
  #   CESCwMedian1JustMedians <- as.double(CESCwMedian1[strtoi(row),21:ncol(CESCwMedian1)][grepl("[0-9]+",CESCwMedian1[strtoi(row),21:ncol(CESCwMedian1)])])
  #   CESCwMedian2JustMedians <- as.double(CESCwMedian2[strtoi(row),21:ncol(CESCwMedian2)][grepl("[0-9]+",CESCwMedian2[strtoi(row),21:ncol(CESCwMedian2)])])
  #   
  #   CHOLwMedian1JustMedians <- as.double(CHOLwMedian1[strtoi(row),21:ncol(CHOLwMedian1)][grepl("[0-9]+",CHOLwMedian1[strtoi(row),21:ncol(CHOLwMedian1)])])
  #   CHOLwMedian2JustMedians <- as.double(CHOLwMedian2[strtoi(row),21:ncol(CHOLwMedian2)][grepl("[0-9]+",CHOLwMedian2[strtoi(row),21:ncol(CHOLwMedian2)])])
  #   
  #   COADwMedian1JustMedians <- as.double(COADwMedian1[strtoi(row),17:ncol(COADwMedian1)][grepl("[0-9]+",COADwMedian1[strtoi(row),17:ncol(COADwMedian1)])])
  #   COADwMedian2JustMedians <- as.double(COADwMedian2[strtoi(row),17:ncol(COADwMedian2)][grepl("[0-9]+",COADwMedian2[strtoi(row),17:ncol(COADwMedian2)])])
  #   
  #   DLBCwMedian1JustMedians <- as.double(DLBCwMedian1[strtoi(row),15:ncol(DLBCwMedian1)][grepl("[0-9]+",DLBCwMedian1[strtoi(row),15:ncol(DLBCwMedian1)])])
  #   DLBCwMedian2JustMedians <- as.double(DLBCwMedian2[strtoi(row),15:ncol(DLBCwMedian2)][grepl("[0-9]+",DLBCwMedian2[strtoi(row),15:ncol(DLBCwMedian2)])])
  #   
  #   ESCAwMedian1JustMedians <- as.double(ESCAwMedian1[strtoi(row),21:ncol(ESCAwMedian1)][grepl("[0-9]+",ESCAwMedian1[strtoi(row),21:ncol(ESCAwMedian1)])])
  #   ESCAwMedian2JustMedians <- as.double(ESCAwMedian2[strtoi(row),21:ncol(ESCAwMedian2)][grepl("[0-9]+",ESCAwMedian2[strtoi(row),21:ncol(ESCAwMedian2)])])
  #   
  #   HNSCwMedian1JustMedians <- as.double(HNSCwMedian1[strtoi(row),21:ncol(HNSCwMedian1)][grepl("[0-9]+",HNSCwMedian1[strtoi(row),21:ncol(HNSCwMedian1)])])
  #   HNSCwMedian2JustMedians <- as.double(HNSCwMedian2[strtoi(row),21:ncol(HNSCwMedian2)][grepl("[0-9]+",HNSCwMedian2[strtoi(row),21:ncol(HNSCwMedian2)])])
  #   
  #   KICHwMedian1JustMedians <- as.double(KICHwMedian1[strtoi(row),21:ncol(KICHwMedian1)][grepl("[0-9]+",KICHwMedian1[strtoi(row),21:ncol(KICHwMedian1)])])
  #   KICHwMedian2JustMedians <- as.double(KICHwMedian2[strtoi(row),21:ncol(KICHwMedian2)][grepl("[0-9]+",KICHwMedian2[strtoi(row),21:ncol(KICHwMedian2)])])
  #   
  #   KIRCwMedian1JustMedians <- as.double(KIRCwMedian1[strtoi(row),21:ncol(KIRCwMedian1)][grepl("[0-9]+",KIRCwMedian1[strtoi(row),21:ncol(KIRCwMedian1)])])
  #   KIRCwMedian2JustMedians <- as.double(KIRCwMedian2[strtoi(row),21:ncol(KIRCwMedian2)][grepl("[0-9]+",KIRCwMedian2[strtoi(row),21:ncol(KIRCwMedian2)])])
  #   
  #   KIRPwMedian1JustMedians <- as.double(KIRPwMedian1[strtoi(row),21:ncol(KIRPwMedian1)][grepl("[0-9]+",KIRPwMedian1[strtoi(row),21:ncol(KIRPwMedian1)])])
  #   KIRPwMedian2JustMedians <- as.double(KIRPwMedian2[strtoi(row),21:ncol(KIRPwMedian2)][grepl("[0-9]+",KIRPwMedian2[strtoi(row),21:ncol(KIRPwMedian2)])])
  #   
  #   LGGwMedian1JustMedians <- as.double(LGGwMedian1[strtoi(row),15:ncol(LGGwMedian1)][grepl("[0-9]+",LGGwMedian1[strtoi(row),15:ncol(LGGwMedian1)])])
  #   LGGwMedian2JustMedians <- as.double(LGGwMedian2[strtoi(row),15:ncol(LGGwMedian2)][grepl("[0-9]+",LGGwMedian2[strtoi(row),15:ncol(LGGwMedian2)])])
  #   
  #   LIHCwMedian1JustMedians <- as.double(LIHCwMedian1[strtoi(row),21:ncol(LIHCwMedian1)][grepl("[0-9]+",LIHCwMedian1[strtoi(row),21:ncol(LIHCwMedian1)])])
  #   LIHCwMedian2JustMedians <- as.double(LIHCwMedian2[strtoi(row),21:ncol(LIHCwMedian2)][grepl("[0-9]+",LIHCwMedian2[strtoi(row),21:ncol(LIHCwMedian2)])])
  #   
  #   LUADwMedian1JustMedians <- as.double(LUADwMedian1[strtoi(row),21:ncol(LUADwMedian1)][grepl("[0-9]+",LUADwMedian1[strtoi(row),21:ncol(LUADwMedian1)])])
  #   LUADwMedian2JustMedians <- as.double(LUADwMedian2[strtoi(row),21:ncol(LUADwMedian2)][grepl("[0-9]+",LUADwMedian2[strtoi(row),21:ncol(LUADwMedian2)])])
  #   
  #   LUSCwMedian1JustMedians <- as.double(LUSCwMedian1[strtoi(row),21:ncol(LUSCwMedian1)][grepl("[0-9]+",LUSCwMedian1[strtoi(row),21:ncol(LUSCwMedian1)])])
  #   LUSCwMedian2JustMedians <- as.double(LUSCwMedian2[strtoi(row),21:ncol(LUSCwMedian2)][grepl("[0-9]+",LUSCwMedian2[strtoi(row),21:ncol(LUSCwMedian2)])])
  #   
  #   MESOwMedian1JustMedians <- as.double(MESOwMedian1[strtoi(row),15:ncol(MESOwMedian1)][grepl("[0-9]+",MESOwMedian1[strtoi(row),15:ncol(MESOwMedian1)])])
  #   MESOwMedian2JustMedians <- as.double(MESOwMedian2[strtoi(row),15:ncol(MESOwMedian2)][grepl("[0-9]+",MESOwMedian2[strtoi(row),15:ncol(MESOwMedian2)])])
  #   
  #   OVwMedian1JustMedians <- as.double(OVwMedian1[strtoi(row),15:ncol(OVwMedian1)][grepl("[0-9]+",OVwMedian1[strtoi(row),15:ncol(OVwMedian1)])])
  #   OVwMedian2JustMedians <- as.double(OVwMedian2[strtoi(row),15:ncol(OVwMedian2)][grepl("[0-9]+",OVwMedian2[strtoi(row),15:ncol(OVwMedian2)])])
  #   
  #   PAADwMedian1JustMedians <- as.double(PAADwMedian1[strtoi(row),21:ncol(PAADwMedian1)][grepl("[0-9]+",PAADwMedian1[strtoi(row),21:ncol(PAADwMedian1)])])
  #   PAADwMedian2JustMedians <- as.double(PAADwMedian2[strtoi(row),21:ncol(PAADwMedian2)][grepl("[0-9]+",PAADwMedian2[strtoi(row),21:ncol(PAADwMedian2)])])
  #   
  #   PCPGwMedian1JustMedians <- as.double(PCPGwMedian1[strtoi(row),21:ncol(PCPGwMedian1)][grepl("[0-9]+",PCPGwMedian1[strtoi(row),21:ncol(PCPGwMedian1)])])
  #   PCPGwMedian2JustMedians <- as.double(PCPGwMedian2[strtoi(row),21:ncol(PCPGwMedian2)][grepl("[0-9]+",PCPGwMedian2[strtoi(row),21:ncol(PCPGwMedian2)])])
  #   
  #   PRADwMedian1JustMedians <- as.double(PRADwMedian1[strtoi(row),21:ncol(PRADwMedian1)][grepl("[0-9]+",PRADwMedian1[strtoi(row),21:ncol(PRADwMedian1)])])
  #   PRADwMedian2JustMedians <- as.double(PRADwMedian2[strtoi(row),21:ncol(PRADwMedian2)][grepl("[0-9]+",PRADwMedian2[strtoi(row),21:ncol(PRADwMedian2)])])
  #   
  #   READwMedian1JustMedians <- as.double(READwMedian1[strtoi(row),17:ncol(READwMedian1)][grepl("[0-9]+",READwMedian1[strtoi(row),17:ncol(READwMedian1)])])
  #   READwMedian2JustMedians <- as.double(READwMedian2[strtoi(row),17:ncol(READwMedian2)][grepl("[0-9]+",READwMedian2[strtoi(row),17:ncol(READwMedian2)])])
  #   
  #   SARCwMedian1JustMedians <- as.double(SARCwMedian1[strtoi(row),17:ncol(SARCwMedian1)][grepl("[0-9]+",SARCwMedian1[strtoi(row),17:ncol(SARCwMedian1)])])
  #   SARCwMedian2JustMedians <- as.double(SARCwMedian2[strtoi(row),17:ncol(SARCwMedian2)][grepl("[0-9]+",SARCwMedian2[strtoi(row),17:ncol(SARCwMedian2)])])
  #   
  #   SKCMwMedian1JustMedians <- as.double(SKCMwMedian1[strtoi(row),21:ncol(SKCMwMedian1)][grepl("[0-9]+",SKCMwMedian1[strtoi(row),21:ncol(SKCMwMedian1)])])
  #   SKCMwMedian2JustMedians <- as.double(SKCMwMedian2[strtoi(row),21:ncol(SKCMwMedian2)][grepl("[0-9]+",SKCMwMedian2[strtoi(row),21:ncol(SKCMwMedian2)])])
  #   
  #   STADwMedian1JustMedians <- as.double(STADwMedian1[strtoi(row),21:ncol(STADwMedian1)][grepl("[0-9]+",STADwMedian1[strtoi(row),21:ncol(STADwMedian1)])])
  #   STADwMedian2JustMedians <- as.double(STADwMedian2[strtoi(row),21:ncol(STADwMedian2)][grepl("[0-9]+",STADwMedian2[strtoi(row),21:ncol(STADwMedian2)])])
  #   
  #   TGCTwMedian1JustMedians <- as.double(TGCTwMedian1[strtoi(row),13:ncol(TGCTwMedian1)][grepl("[0-9]+",TGCTwMedian1[strtoi(row),13:ncol(TGCTwMedian1)])])
  #   TGCTwMedian2JustMedians <- as.double(TGCTwMedian2[strtoi(row),13:ncol(TGCTwMedian2)][grepl("[0-9]+",TGCTwMedian2[strtoi(row),13:ncol(TGCTwMedian2)])])
  #   
  #   THCAwMedian1JustMedians <- as.double(THCAwMedian1[strtoi(row),21:ncol(THCAwMedian1)][grepl("[0-9]+",THCAwMedian1[strtoi(row),21:ncol(THCAwMedian1)])])
  #   THCAwMedian2JustMedians <- as.double(THCAwMedian2[strtoi(row),21:ncol(THCAwMedian2)][grepl("[0-9]+",THCAwMedian2[strtoi(row),21:ncol(THCAwMedian2)])])
  #   
  #   THYMwMedian1JustMedians <- as.double(THYMwMedian1[strtoi(row),19:ncol(THYMwMedian1)][grepl("[0-9]+",THYMwMedian1[strtoi(row),19:ncol(THYMwMedian1)])])
  #   THYMwMedian2JustMedians <- as.double(THYMwMedian2[strtoi(row),19:ncol(THYMwMedian2)][grepl("[0-9]+",THYMwMedian2[strtoi(row),19:ncol(THYMwMedian2)])])
  #   
  #   UCECwMedian1JustMedians <- as.double(UCECwMedian1[strtoi(row),21:ncol(UCECwMedian1)][grepl("[0-9]+",UCECwMedian1[strtoi(row),21:ncol(UCECwMedian1)])])
  #   UCECwMedian2JustMedians <- as.double(UCECwMedian2[strtoi(row),21:ncol(UCECwMedian2)][grepl("[0-9]+",UCECwMedian2[strtoi(row),21:ncol(UCECwMedian2)])])
  #   
  #   UCSwMedian1JustMedians <- as.double(UCSwMedian1[strtoi(row),15:ncol(UCSwMedian1)][grepl("[0-9]+",UCSwMedian1[strtoi(row),15:ncol(UCSwMedian1)])])
  #   UCSwMedian2JustMedians <- as.double(UCSwMedian2[strtoi(row),15:ncol(UCSwMedian2)][grepl("[0-9]+",UCSwMedian2[strtoi(row),15:ncol(UCSwMedian2)])])
  #   
  #   UVMwMedian1JustMedians <- as.double(UVMwMedian1[strtoi(row),15:ncol(UVMwMedian1)][grepl("[0-9]+",UVMwMedian1[strtoi(row),15:ncol(UVMwMedian1)])])
  #   UVMwMedian2JustMedians <- as.double(UVMwMedian2[strtoi(row),15:ncol(UVMwMedian2)][grepl("[0-9]+",UVMwMedian2[strtoi(row),15:ncol(UVMwMedian2)])])
  #   
  #   
  #   #############################################################################################################################################
  #   
  #   
  #   ifelse(input$dataset=="ACC", data <-data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(ACCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(ACCwMedian2JustMedians))),
  #     value=c(ACCwMedian1JustMedians,ACCwMedian2JustMedians)
  #   ),
  #   
  #   ifelse(input$dataset=="BLCA", data <-data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(BLCAwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(BLCAwMedian2JustMedians))),
  #     value=c(BLCAwMedian1JustMedians,BLCAwMedian2JustMedians)
  #   ),
  #   
  #   ifelse(input$dataset=="BRCA", data <-data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(BRCAwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(BRCAwMedian2JustMedians))),
  #     value=c(BRCAwMedian1JustMedians,BRCAwMedian2JustMedians)
  #   ),
  #   
  #   ifelse(input$dataset=="CESC", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(CESCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(CESCwMedian2JustMedians))),
  #     value=c(CESCwMedian1JustMedians,CESCwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="CHOL", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(CHOLwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(CHOLwMedian2JustMedians))),
  #     value=c(CHOLwMedian1JustMedians,CHOLwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="COAD", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(COADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(COADwMedian2JustMedians))),
  #     value=c(COADwMedian1JustMedians,COADwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="DLBC", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(DLBCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(DLBCwMedian2JustMedians))),
  #     value=c(DLBCwMedian1JustMedians,DLBCwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="ESCA", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(ESCAwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(ESCAwMedian2JustMedians))),
  #     value=c(ESCAwMedian1JustMedians,ESCAwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="HNSC", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(HNSCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(HNSCwMedian2JustMedians))),
  #     value=c(HNSCwMedian1JustMedians,HNSCwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="KICH", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(KICHwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(KICHwMedian2JustMedians))),
  #     value=c(KICHwMedian1JustMedians,KICHwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="KIRC", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(KIRCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(KIRCwMedian2JustMedians))),
  #     value=c(KIRCwMedian1JustMedians,KIRCwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="KIRP", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(KIRPwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(KIRPwMedian2JustMedians))),
  #     value=c(KIRPwMedian1JustMedians,KIRPwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="LGG", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(LGGwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(LGGwMedian2JustMedians))),
  #     value=c(LGGwMedian1JustMedians,LGGwMedian2JustMedians)
  #   ),
  #   
  #   ifelse(input$dataset=="LIHC", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(LIHCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(LIHCwMedian2JustMedians))),
  #     value=c(LIHCwMedian1JustMedians,LIHCwMedian2JustMedians)
  #   ),
  #   
  #   ifelse(input$dataset=="LUAD", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(LUADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(LUADwMedian2JustMedians))),
  #     value=c(LUADwMedian1JustMedians,LUADwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="LUSC", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(LUSCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(LUSCwMedian2JustMedians))),
  #     value=c(LUSCwMedian1JustMedians,LUSCwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="MESO", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(MESOwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(MESOwMedian2JustMedians))),
  #     value=c(MESOwMedian1JustMedians,MESOwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="OV", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(OVwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(OVwMedian2JustMedians))),
  #     value=c(OVwMedian1JustMedians,OVwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="PAAD", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(PAADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(PAADwMedian2JustMedians))),
  #     value=c(PAADwMedian1JustMedians,PAADwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="PCPG", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(PCPGwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(PCPGwMedian2JustMedians))),
  #     value=c(PCPGwMedian1JustMedians,PCPGwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="PRAD", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(PRADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(PRADwMedian2JustMedians))),
  #     value=c(PRADwMedian1JustMedians,PRADwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="READ", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(READwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(READwMedian2JustMedians))),
  #     value=c(READwMedian1JustMedians,READwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="SARC", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(SARCwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(SARCwMedian2JustMedians))),
  #     value=c(SARCwMedian1JustMedians,SARCwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="SKCM", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(SKCMwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(SKCMwMedian2JustMedians))),
  #     value=c(SKCMwMedian1JustMedians,SKCMwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="STAD", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(STADwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(STADwMedian2JustMedians))),
  #     value=c(STADwMedian1JustMedians,STADwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="TGCT", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(TGCTwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(TGCTwMedian2JustMedians))),
  #     value=c(TGCTwMedian1JustMedians,TGCTwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="THCA", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(THCAwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(THCAwMedian2JustMedians))),
  #     value=c(THCAwMedian1JustMedians,THCAwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="THYM", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(THYMwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(THYMwMedian2JustMedians))),
  #     value=c(THYMwMedian1JustMedians,THYMwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="UCEC", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(UCECwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(UCECwMedian2JustMedians))),
  #     value=c(UCECwMedian1JustMedians,UCECwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="UCS", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(UCSwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(UCSwMedian2JustMedians))),
  #     value=c(UCSwMedian1JustMedians,UCSwMedian2JustMedians)
  #   ),
  #   ifelse(input$dataset=="UVM", data <- data.frame(
  #     name=c(rep("Both miRNA's Downregulated",length(UVMwMedian1JustMedians)),rep("Both miRNA's Upregulated",length(UVMwMedian2JustMedians))),
  #     value=c(UVMwMedian1JustMedians,UVMwMedian2JustMedians)
  #   ),
  #   data <- data.frame(
  #     name=c(),
  #     value=c()
  #   )
  #   ))))))))))))))))))))))))))))))
  #   )
  #   
  #   
  #   #############################################################################################################################################
  #   
  #   
  #   showModal(modalDialog(
  #     renderPlot({
  #       data %>%
  #         ggplot( aes(x=name,y=value, fill=name))+
  #         geom_boxplot()+
  #         geom_jitter(color="black", size=0.4, alpha=0.9)+
  #         theme(
  #           legend.position = "none",
  #           axis.text.x = element_text(size = 14,color="black"),
  #           axis.title.y = element_text(size = 14) #, face="bold")
  #         )+
  #         scale_x_discrete(labels=c("Both miRNA's\nDownregulated", "Both miRNA's\nUpregulated"))+
  #         scale_fill_manual(values=c("#99ccff", "#ff6666"))+
  #         # theme(axis.line = element_line(colour = "black", 
  #         #                                 size = 0.4, linetype = "solid"))+
  #         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #               panel.background = element_blank(), axis.line = element_line(colour = "black",size = 0.4, linetype = "solid"),
  #               axis.text.y = element_text(color="black", 
  #                                          size=12))+
  #         xlab("")+
  #         ylab("mRNA Expression Level")
  #       
  #     }),
  #     footer = tagList(
  #       modalButton("Close")
  #     ),
  #     easyClose = TRUE,
  #   ))
  # })
  # 
  # 
  # ##################################################################      
  # 
  # sourceTargetInput <- reactive({
  #   switch (input$dataset,
  #           "ACC"=ACC_source_target,
  #           "BLCA" = BLCA_source_target,
  #           "BRCA" = BRCA_source_target,
  #           "CESC"=CESC_source_target,
  #           "CHOL"=CHOL_source_target,
  #           "COAD"=COAD_source_target,
  #           "DLBC"=DLBC_source_target,
  #           "ESCA"=ESCA_source_target,
  #           "HNSC"=HNSC_source_target,
  #           "KICH"=KICH_source_target,
  #           "KIRC"=KIRC_source_target,
  #           "KIRP"=KIRP_source_target,
  #           "LGG"=LGG_source_target,
  #           "LIHC"=LIHC_source_target,
  #           "LUAD"=LUAD_source_target,
  #           "LUSC"=LUSC_source_target,
  #           "MESO"=MESO_source_target,
  #           "OV"=OV_source_target,
  #           "PAAD"=PAAD_source_target,
  #           "PCPG"=PCPG_source_target,
  #           "PRAD"=PRAD_source_target,
  #           "READ"=READ_source_target,
  #           "SARC"=SARC_source_target,
  #           "SKCM"=SKCM_source_target,
  #           "STAD"=STAD_source_target,
  #           "TGCT"=TGCT_source_target,
  #           "THCA"=THCA_source_target,
  #           "THYM"=THYM_source_target,
  #           "UCEC"=UCEC_source_target,
  #           "UCS"=UCS_source_target,
  #           "UVM"=UVM_source_target
  #           
  #   )
  # })
  # 
  # nodeAttributeInput <- reactive({
  #   switch (input$dataset,
  #           "ACC" = ACC_node_attr,
  #           "BLCA" = BLCA_node_attr,
  #           "BRCA" = BRCA_node_attr,
  #           "CESC"=CESC_node_attr,
  #           "CHOL"=CHOL_node_attr,
  #           "COAD"=COAD_node_attr,
  #           "DLBC"=DLBC_node_attr,
  #           "ESCA"=ESCA_node_attr,
  #           "HNSC"=HNSC_node_attr,
  #           "KICH"=KICH_node_attr,
  #           "KIRC"=KIRC_node_attr,
  #           "KIRP"=KIRP_node_attr,
  #           "LGG"=LGG_node_attr,
  #           "LIHC"=LIHC_node_attr,
  #           "LUAD"=LUAD_node_attr,
  #           "LUSC"=LUSC_node_attr,
  #           "MESO"=MESO_node_attr,
  #           "OV"=OV_node_attr,
  #           "PAAD"=PAAD_node_attr,
  #           "PCPG"=PCPG_node_attr,
  #           "PRAD"=PRAD_node_attr,
  #           "READ"=READ_node_attr,
  #           "SARC"=SARC_node_attr,
  #           "SKCM"=SKCM_node_attr,
  #           "STAD"=STAD_node_attr,
  #           "TGCT"=TGCT_node_attr,
  #           "THCA"=THCA_node_attr,
  #           "THYM"=THYM_node_attr,
  #           "UCEC"=UCEC_node_attr,
  #           "UCS"=UCS_node_attr,
  #           "UVM"=UVM_node_attr
  #   )
  #   
  # })
  # 
  # cancerSpecificNetworkNodeEdge <- reactive({
  #   
  #   if(!is.null(DatasetRoundDigits())){
  #     combmi1mi2mrna <- unique(c(gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna1)),gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna2)),DatasetRoundDigits()$hgnc_symbol))
  #     orListForNetworkFiltering <- rep("|",length(combmi1mi2mrna))
  #     networkFilteringList <- paste(c(rbind(orListForNetworkFiltering, matrix(combmi1mi2mrna,ncol = length(orListForNetworkFiltering)))[-1]),collapse = '')
  #     sourceTargetFiltering <- paste(gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna1)),gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna2)),DatasetRoundDigits()$hgnc_symbol)
  #     
  #     splittedSourceTargetFiltering <- strsplit(sourceTargetFiltering,split = " ")
  #     splittedSourceTargetFilteringMRNA <- sapply(splittedSourceTargetFiltering,'[',3)
  #     splittedSourceTargetFilteringMIRNA1 <- sapply(splittedSourceTargetFiltering,'[',1)
  #     splittedSourceTargetFilteringMIRNA2 <- sapply(splittedSourceTargetFiltering,'[',2)
  #     splittedSourceTargetFilteringDUMMY1_2 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA1,"/",splittedSourceTargetFilteringMIRNA2))
  #     splittedSourceTargetFilteringDUMMY2_1 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA2,"/",splittedSourceTargetFilteringMIRNA1))
  #     
  #     
  #     for (i in 1:nrow(DatasetRoundDigits())){
  #       concated <- rbind(concated, filter(sourceTargetInput(), ((tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringMIRNA1[i]) & (tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY1_2[i]) |tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY2_1[i]) ))|
  #                                                                  (tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringMIRNA2[i]) & (tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY1_2[i]) | tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY2_1[i])))|
  #                                                                  ((tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringDUMMY1_2[i])| tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringDUMMY2_1[i])) & tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringMRNA[i]))
  #       )))
  #       
  #     }
  #     
  #     concatedUnique <- unique(concated[,c("source","target")])
  #     forNodeSharedName <- unique(c(concated$source,concated$target))
  #     forNodeName <- forNodeSharedName
  #     forNodeName[grepl("/",forNodeName)] <- " "
  #     intersectionSharedName <- intersect(filter(nodeAttributeInput(),stringr::str_detect(nodeAttributeInput()$shared.name,networkFilteringList))$shared.name,forNodeSharedName)
  #     
  #     nodes <- data.frame(id=intersectionSharedName, label=filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$name)
  #     edges <- data.frame(from= concatedUnique$source , to=concatedUnique$target)
  #     
  #     node_edge_list <- list("nodes" = nodes, "edges" = edges, "intersectionSharedName"  = intersectionSharedName)
  #     
  #     
  #   }
  #   return(node_edge_list)
  #   
  # })
  # 
  # 
  # output$vNetwork <- renderVisNetwork({
  #   
  #   if(!is.null(DatasetRoundDigits())){
  #     
  #     # combmi1mi2mrna <- unique(c(gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna1)),gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna2)),DatasetRoundDigits()$hgnc_symbol))
  #     # orListForNetworkFiltering <- rep("|",length(combmi1mi2mrna))
  #     # networkFilteringList <- paste(c(rbind(orListForNetworkFiltering, matrix(combmi1mi2mrna,ncol = length(orListForNetworkFiltering)))[-1]),collapse = '')
  #     # sourceTargetFiltering <- paste(gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna1)),gsub("hsa-", "",tolower(DatasetRoundDigits()$mirna2)),DatasetRoundDigits()$hgnc_symbol)
  #     # 
  #     # splittedSourceTargetFiltering <- strsplit(sourceTargetFiltering,split = " ")
  #     # splittedSourceTargetFilteringMRNA <- sapply(splittedSourceTargetFiltering,'[',3)
  #     # splittedSourceTargetFilteringMIRNA1 <- sapply(splittedSourceTargetFiltering,'[',1)
  #     # splittedSourceTargetFilteringMIRNA2 <- sapply(splittedSourceTargetFiltering,'[',2)
  #     # splittedSourceTargetFilteringDUMMY1_2 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA1,"/",splittedSourceTargetFilteringMIRNA2))
  #     # splittedSourceTargetFilteringDUMMY2_1 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA2,"/",splittedSourceTargetFilteringMIRNA1))
  #     # 
  #     # for (i in 1:nrow(DatasetRoundDigits())){
  #     #   concated <- rbind(concated, filter(sourceTargetInput(), ((tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringMIRNA1[i]) & (tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY1_2[i]) |tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY2_1[i]) ))|
  #     #                                                              (tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringMIRNA2[i]) & (tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY1_2[i]) | tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringDUMMY2_1[i])))|
  #     #                                                              ((tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringDUMMY1_2[i])| tolower(sourceTargetInput()$source)==tolower(splittedSourceTargetFilteringDUMMY2_1[i])) & tolower(sourceTargetInput()$target) == tolower(splittedSourceTargetFilteringMRNA[i]))
  #     #   )))
  #     #   
  #     # }
  #     # 
  #     # concatedUnique <- unique(concated[,c("source","target")])
  #     # forNodeSharedName <- unique(c(concated$source,concated$target))
  #     # forNodeName <- forNodeSharedName
  #     # forNodeName[grepl("/",forNodeName)] <- " "
  #     # intersectionSharedName <- intersect(filter(nodeAttributeInput(),stringr::str_detect(nodeAttributeInput()$shared.name,networkFilteringList))$shared.name,forNodeSharedName)
  #     # 
  #     
  #     #nodes <- data.frame(id=intersectionSharedName, label=filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$name)
  #     #edges <- data.frame(from= concatedUnique$source , to=concatedUnique$target)
  #     
  #     nodes <- cancerSpecificNetworkNodeEdge()$nodes
  #     edges <- cancerSpecificNetworkNodeEdge()$edges
  #     intersectionSharedName <- cancerSpecificNetworkNodeEdge()$intersectionSharedName
  #     
  #     
  #     nodes$size <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mrna",25,ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="dummy",3,20))
  #     edges$color <- "rgb(153,153,153)"
  #     edges$length <- 3
  #     
  #     
  #     if(input$dataset =='ACC' || input$dataset =='DLBC' || input$dataset =='LGG' || input$dataset =='MESO' || input$dataset =='OV' ||
  #        input$dataset =='TGCT' || input$dataset =='UCS' || input$dataset =='UVM'){
  #       coloring <- input$colorGroup1
  #     }
  #     
  #     if(input$dataset =='BLCA' || input$dataset =='BRCA' || input$dataset =='CESC' || input$dataset =='CHOL' ||
  #        input$dataset =='COAD' || input$dataset =='ESCA' || input$dataset =='HNSC' || input$dataset =='KICH' || input$dataset =='KIRC' ||
  #        input$dataset =='KIRP' || input$dataset =='LIHC' || input$dataset =='LUAD' || input$dataset =='LUSC' || input$dataset =='PAAD' ||
  #        input$dataset =='PCPG' || input$dataset =='PRAD' || input$dataset =='READ' || input$dataset =='SARC' || input$dataset =='SKCM' ||
  #        input$dataset =='STAD' || input$dataset =='THCA' || input$dataset =='THYM' || input$dataset =='UCEC'){
  #       
  #       coloring <- input$colorGroup2
  #     }
  #     
  #     print(coloring)
  # 
  #     if(coloring =="Differential Expression Analysis"){
  #       
  #       if(length(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown)) > 0){
  #         #if("miRNA1_pvalue" %in% colnames(DatasetRoundDigits())){
  #           nodes$color.background <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="up", "rgb(255,102,102)",
  #                                            ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="down","rgb(153,204,255)",
  #                                                   "rgb(153,153,153)"))
  #           nodes$color.border <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="up", "rgb(255,102,102)",
  #                                        ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown) =="down","rgb(153,204,255)",
  #                                               "rgb(153,153,153)"))
  #         
  #       }
  #       else{
  #         nodes$color.background <-  "rgb(153,153,153)"
  #         nodes$color.border <- "rgb(153,153,153)"
  #         
  #       }
  #       
  #     }
  #     
  #     if(coloring =="miRNA Family"){
  #       
  #       uniqueMirnaFamily <- unique(c(DatasetRoundDigits()$miRNA1Family, DatasetRoundDigits()$miRNA2Family, NA))
  #       palette <- distinctColorPalette(length(uniqueMirnaFamily))
  #       familyWithPalette <- paste(uniqueMirnaFamily, palette)
  #       familyWithPaletteDf <- as.data.frame(familyWithPalette)%>%separate(familyWithPalette, c("family", "FamilyColor"), " ")
  #       
  #       nodeAttribute <- nodeAttributeInput()
  #       merged <- sqldf::sqldf("SELECT nodeAttribute.*, FamilyColor from nodeAttribute left join familyWithPaletteDf on nodeAttribute.miRfamily = familyWithPaletteDf.family")
  #       #merged <- merge(nodeAttributeInput(),familyWithPaletteDf, by.x = "miRfamily", by.y = "family", all.x = TRUE, all.y = FALSE, sort = FALSE)
  #       merged["FamilyColor"][is.na(merged["FamilyColor"])] <- "rgb(153,153,153)"
  #       print(merged)
  #       
  #       nodes$color.background <-filter(merged,merged$shared.name %in% intersectionSharedName)$FamilyColor
  #       nodes$color.border <- filter(merged,merged$shared.name %in% intersectionSharedName)$FamilyColor
  # 
  #       
  #       nodes$title <- ifelse(filter(merged,merged$shared.name %in% intersectionSharedName)$info == "mirna",  paste("<p><b>Family: </b></p>", filter(merged,merged$shared.name %in% intersectionSharedName)$miRfamily)," ")
  # 
  #       
  #     }
  #     
  #     if(coloring =="miRNA Cluster"){
  #       
  #       uniqueMirnaCluster <- unique(c(DatasetRoundDigits()$miRNA1Cluster, DatasetRoundDigits()$miRNA2Cluster, NA))
  #       palette <- distinctColorPalette(length(uniqueMirnaCluster))
  #       clusterWithPalette <- paste(uniqueMirnaCluster, palette)
  #       clusterWithPaletteDf <- as.data.frame(clusterWithPalette)%>%separate(clusterWithPalette, c("cluster", "ClusterColor"), " ")
  #       
  #       nodeAttribute <- nodeAttributeInput()
  #       merged <- sqldf::sqldf("SELECT nodeAttribute.*, ClusterColor from nodeAttribute left join clusterWithPaletteDf on nodeAttribute.mirnaCluster = clusterWithPaletteDf.cluster")
  #       #merged <- merge(nodeAttributeInput(),familyWithPaletteDf, by.x = "miRfamily", by.y = "family", all.x = TRUE, all.y = FALSE, sort = FALSE)
  #       merged["ClusterColor"][is.na(merged["ClusterColor"])] <- "rgb(153,153,153)"
  #       
  #       nodes$color.background <-filter(merged,merged$shared.name %in% intersectionSharedName)$ClusterColor
  #       nodes$color.border <- filter(merged,merged$shared.name %in% intersectionSharedName)$ClusterColor
  #       
  #       nodes$title <- ifelse(filter(merged,merged$shared.name %in% intersectionSharedName)$info == "mirna",  paste("<p><b>Cluster: </b></p>", filter(merged,merged$shared.name %in% intersectionSharedName)$mirnaCluster)," ")
  #       
  #       
  # 
  #     }
  #     
  #     
  #     
  #     if(length(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$is_mrna_tf)) > 0){
  #       nodes$shape <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$is_mrna_tf)=="true","square",
  #                             ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mrna","diamond",
  #                                    ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mirna","dot",
  #                                           ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="dummy","dot","dot"))))
  #       
  #       
  #       nodes$size <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$is_mrna_tf)=="true",20,
  #                            ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mrna",25,
  #                                   ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mirna",20,
  #                                          ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="dummy",3,3))))
  #       
  #     }
  #     
  #     else {
  #       nodes$shape <- ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mrna","diamond",
  #                             ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="mirna","dot",
  #                                    ifelse(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$info)=="dummy","dot","dot")))
  #     }
  #     
  #     
  #     if(length(tolower(filter(nodeAttributeInput(),nodeAttributeInput()$shared.name %in% intersectionSharedName)$updown)) > 0){ 
  #       lnodes <- data.frame(label=c("Legend","mRNA","mRNA is TF","miRNA","Up Regulated","Down Regulated","Significant"),
  #                            shape=c("text","diamond","square","dot","box","box","dot"),
  #                            size =c(25,25,20,25,25,15,25),
  #                            font.size=c(50,25,25,25,25,25,25),
  #                            font.face=c("Ubuntu","Ubuntu","Ubuntu","Ubuntu","Ubuntu","Ubuntu","Ubuntu"),
  #                            color.background=c("rgb(153,153,153)","rgb(153,153,153)",  "rgb(153,153,153)", "rgb(153,153,153)","rgb(255,102,102)","rgb(153,204,255)","white"),
  #                            borderWidth=c(1,1,1,1,1,1,2),
  #                            color.border=c("rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)","rgb(255,102,102)","rgb(153,204,255)","black")
  #       )
  #       
  #       
  #       
  #       visNetwork(nodes, edges) %>%
  #         visLegend(addNodes = lnodes,width = 0.1, position = "right",zoom=F,stepY = 180,useGroups = F)%>%
  #         visNodes(font = list(color="black", size=40, face="Ubuntu"))%>%
  #         visIgraphLayout()%>% 
  #         visInteraction(navigationButtons = TRUE)
  #     }
  #     else{
  #       lnodes <- data.frame(label=c("Legend","mRNA","mRNA is TF","miRNA"),
  #                            shape=c("text","diamond","square","dot"),
  #                            size =c(25,30,30,25),
  #                            font.size=c(50,25,25,25),
  #                            font.face=c("Roboto","Roboto","Roboto","Roboto"),
  #                            color.background=c("rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)"),
  #                            color.border=c("rgb(153,153,153)", "rgb(153,153,153)","rgb(153,153,153)","rgb(153,153,153)")
  #       )
  #       visNetwork(nodes, edges) %>%
  #         visLegend(addNodes = lnodes,width = 0.1, position = "right",zoom=F,stepY = 120,useGroups = F)%>%
  #         visIgraphLayout()%>%
  #         visNodes(font = list(color="black", size=40, face="Roboto"))%>% 
  #         visInteraction(navigationButtons = TRUE)
  #     }
  #     
  #   }
  #   
  #   
  #   
  # })
  # 
  # 
  # ##################################################################################################    
  # 
  # commonMirnaNetworkNodeEdge <- reactive({
  #   if(!is.null(commonMirnaPairFilter()) || nrow(commonMirnaPairFilter()) >0){
  #     networkFilteringSplit <- unique(unlist(strsplit(commonMirnaPairFilter()$miRNAPair, split = "/")))
  #     orListForNetworkFilteringSplit<- rep("|",length(networkFilteringSplit))
  #     networkFilteringList <- paste(c(rbind(orListForNetworkFilteringSplit, matrix(networkFilteringSplit,ncol = length(orListForNetworkFilteringSplit)))[-1]),collapse = '')
  #     
  #     
  #     splittedMrnaFilteredSource <- strsplit(commonMirnaPairFilter()$miRNAPair, split = "/")
  #     splittedSourceTargetFilteringMIRNA1 <- sapply(splittedMrnaFilteredSource,'[',1)
  #     splittedSourceTargetFilteringMIRNA2 <- sapply(splittedMrnaFilteredSource,'[',2)
  #     
  #     
  #     for (i in 1:nrow(commonMirnaPairFilter())){
  #       concated <- rbind(concated, filter(commonMirnaPair_source_target, ((commonMirnaPair_source_target$source==splittedSourceTargetFilteringMIRNA1[i] & commonMirnaPair_source_target$target==splittedSourceTargetFilteringMIRNA2[i] )|
  #                                                                            (commonMirnaPair_source_target$source==splittedSourceTargetFilteringMIRNA2[i] & commonMirnaPair_source_target$target==splittedSourceTargetFilteringMIRNA1[i] )
  #                                                                          
  #       )))
  #       
  #     }
  #     
  #     concatedUnique <- unique(concated[,c("source","target","CancerTypes")])
  #     forNodeSharedName <- unique(c(concated$source,concated$target))
  #     intersectionSharedName <- intersect(filter(commonMirnaPairs_node_attr,stringr::str_detect(commonMirnaPairs_node_attr$shared_name,networkFilteringList))$shared_name,forNodeSharedName)
  #     
  #     nodes <- data.frame(id=forNodeSharedName, label=forNodeSharedName)
  #     
  #     edges <- data.frame(from = concatedUnique$source , to = concatedUnique$target, label = concatedUnique$CancerTypes)
  #     
  #     node_edge_list <- list("nodes" = nodes, "edges" = edges)
  #     
  #   }
  #   return(node_edge_list)
  #   
  # })
  # 
  # commonTripletNetworkNodeEdge <- reactive({
  #   
  #   if(!is.null(commonTripletFilter()) || nrow(commonTripletFilter()) >0){
  #     networkFilteringSplit <- unique(unlist(strsplit(commonTripletFilter()$Triplet, split = "/")))
  #     orListForNetworkFilteringSplit<- rep("|",length(networkFilteringSplit))
  #     networkFilteringList <- paste(c(rbind(orListForNetworkFilteringSplit, matrix(networkFilteringSplit,ncol = length(orListForNetworkFilteringSplit)))[-1]),collapse = '')
  #     
  #     
  #     splittedMrnaFilteredSource <- strsplit(commonTripletFilter()$Triplet, split = "/")
  #     splittedSourceTargetFilteringMRNA <- sapply(splittedMrnaFilteredSource,'[',3)
  #     splittedSourceTargetFilteringMIRNA1 <- sapply(splittedMrnaFilteredSource,'[',1)
  #     splittedSourceTargetFilteringMIRNA2 <- sapply(splittedMrnaFilteredSource,'[',2)
  #     splittedSourceTargetFilteringDUMMY1_2 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA1,"/",splittedSourceTargetFilteringMIRNA2))
  #     splittedSourceTargetFilteringDUMMY2_1 <- gsub(" ","",paste(splittedSourceTargetFilteringMIRNA2,"/",splittedSourceTargetFilteringMIRNA1))
  #     
  #     
  #     for (i in 1:nrow(commonTripletFilter())){
  #       concated <- rbind(concated, filter(commonTriplet_source_target, ((commonTriplet_source_target$source==splittedSourceTargetFilteringMIRNA1[i] & (commonTriplet_source_target$target == splittedSourceTargetFilteringDUMMY1_2[i] |commonTriplet_source_target$target == splittedSourceTargetFilteringDUMMY2_1[i] ))|
  #                                                                          (commonTriplet_source_target$source==splittedSourceTargetFilteringMIRNA2[i] & (commonTriplet_source_target$target == splittedSourceTargetFilteringDUMMY1_2[i] | commonTriplet_source_target$target == splittedSourceTargetFilteringDUMMY2_1[i]))|
  #                                                                          ((commonTriplet_source_target$source==splittedSourceTargetFilteringDUMMY1_2[i]| commonTriplet_source_target$source==splittedSourceTargetFilteringDUMMY2_1[i]) & commonTriplet_source_target$target == splittedSourceTargetFilteringMRNA[i])
  #       )))
  #       
  #     }
  #     
  #     
  #     concatedUnique <- unique(concated[,c("source","target","whichcancer")])
  #     forNodeSharedName <- unique(c(concated$source,concated$target))
  #     forNodeName <- forNodeSharedName
  #     forNodeName[grepl("/",forNodeName)] <- " "
  #     intersectionSharedName <- intersect(filter(commonTriplets_node_attr,stringr::str_detect(commonTriplets_node_attr$shared.name,networkFilteringList))$shared.name,forNodeSharedName)
  #     
  #     
  #     nodes <- data.frame(id=intersectionSharedName, label=filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$visname)
  #     edges <- data.frame(from = concatedUnique$source , to = concatedUnique$target, label = concatedUnique$whichcancer )
  #     
  #     node_edge_list <- list("nodes" = nodes, "edges" = edges, "intersectionSharedName"  = intersectionSharedName)
  #   }
  #   return(node_edge_list) 
  # })
  # 
  # 
  # ##################################################################################################
  # 
  # output$commonTripletNetwork <- renderVisNetwork({
  #   
  #   nodes <- commonTripletNetworkNodeEdge()$nodes
  #   edges <- commonTripletNetworkNodeEdge()$edges
  #   intersectionSharedName <- commonTripletNetworkNodeEdge()$intersectionSharedName
  #   
  #   
  #   nodes$shape <- ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="mrna","diamond",
  #                         ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="mirna","dot",
  #                                ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="dummy","dot","dot")))
  #   
  #   nodes$size <- ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="mrna",40,
  #                        ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="mirna",30,
  #                               ifelse(tolower(filter(commonTriplets_node_attr,commonTriplets_node_attr$shared.name %in% intersectionSharedName)$info)=="dummy",8,3)))
  #   
  #   
  #   nodes$color.background <- "rgb(153,153,153)"
  #   nodes$color.border <- "rgb(153,153,153)"
  #   
  #   edges$color <- "rgb(153,153,153)"
  #   edges$length <- 15
  #   
  #   visNetwork(nodes, edges)%>%
  #     visIgraphLayout()%>% 
  #     visEdges(font = list(align="horizontal", color="black", size=20, face="Roboto"))%>%
  #     visNodes(font = list(color="black", size=30, face="Roboto"))%>% 
  #     visInteraction(navigationButtons = TRUE)
  # })
  # 
  # 
  # output$commonMirnaNetwork <- renderVisNetwork({
  #   
  #   nodes <- commonMirnaNetworkNodeEdge()$nodes
  #   edges <- commonMirnaNetworkNodeEdge()$edges
  #   nodes$shape <- "dot"
  #   nodes$size <- 30
  #   nodes$color.background <- "rgb(153,153,153)"
  #   nodes$color.border <- "rgb(153,153,153)"
  #   
  #   edges$color <- "rgb(153,153,153)"
  #   edges$length <- 3
  #   
  #   coloring <- input$colorCommonMirna
  #   if(coloring =="miRNA Family"){
  #     
  #     uniqueMirnaFamily <- unique(c(commonMirnaPairs_node_attr$miRNAFamily, NA))
  #     palette <- distinctColorPalette(length(uniqueMirnaFamily))
  #     familyWithPalette <- paste(uniqueMirnaFamily, palette)
  #     familyWithPaletteDf <- as.data.frame(familyWithPalette)%>%separate(familyWithPalette, c("family", "FamilyColor"), " ")
  #     
  #     nodeAttribute <- commonMirnaPairs_node_attr
  #     merged <- sqldf::sqldf("SELECT nodeAttribute.*, FamilyColor from nodeAttribute left join familyWithPaletteDf on nodeAttribute.miRNAFamily = familyWithPaletteDf.family")
  #     #merged <- merge(nodeAttributeInput(),familyWithPaletteDf, by.x = "miRfamily", by.y = "family", all.x = TRUE, all.y = FALSE, sort = FALSE)
  #     merged["FamilyColor"][is.na(merged["FamilyColor"])] <- "rgb(153,153,153)"
  # 
  #     #nodes$color.background <-filter(merged,merged$shared_name %in% intersectionSharedName)$FamilyColor
  #     #nodes$color.border <- filter(merged,merged$shared_name %in% intersectionSharedName)$FamilyColor
  #     nodes$color.background <-merged$FamilyColor
  #     nodes$color.border <- merged$FamilyColor
  #     
  #     nodes$title <- paste("<p><b>Family: </b></p>", merged$miRNAFamily)
  #     
  #     
  #   }
  #   
  #   if(coloring =="miRNA Cluster"){
  # 
  #     uniqueMirnaCluster <- unique(c(commonMirnaPairs_node_attr$clusterString, NA))
  #     palette <- distinctColorPalette(length(uniqueMirnaCluster))
  #     clusterWithPalette <- paste(uniqueMirnaCluster, palette)
  #     clusterWithPaletteDf <- as.data.frame(clusterWithPalette)%>%separate(clusterWithPalette, c("cluster", "ClusterColor"), " ")
  # 
  #     nodeAttribute <- commonMirnaPairs_node_attr
  #     merged <- sqldf::sqldf("SELECT nodeAttribute.*, ClusterColor from nodeAttribute left join clusterWithPaletteDf on nodeAttribute.clusterString = clusterWithPaletteDf.cluster")
  #     #merged <- merge(nodeAttributeInput(),familyWithPaletteDf, by.x = "miRfamily", by.y = "family", all.x = TRUE, all.y = FALSE, sort = FALSE)
  #     merged["ClusterColor"][is.na(merged["ClusterColor"])] <- "rgb(153,153,153)"
  # 
  #     nodes$color.background <-merged$ClusterColor
  #     nodes$color.border <- merged$ClusterColor
  # 
  #     nodes$title <- paste("<p><b>Cluster: </b></p>", merged$clusterString)
  #     
  #   }
  # 
  #   
  #   
  #   visNetwork(nodes, edges)%>%
  #     visIgraphLayout() %>% 
  #     visEdges(font = list(align="horizontal", color="black", size=25, face="Roboto"))%>%
  #     visNodes(font = list(color="black", size=35, face="Roboto"))%>% 
  #     visInteraction(navigationButtons = TRUE)
  #   
  #   
  # })
  # 
  # ##################################################################################################    
  # 
  # commonTripletFilter <- reactive({
  #   
  #   dataset <- TripletsInWhichCancerWCount
  #   filteredWithMrna <- NULL
  #   filteredWithMirna <- NULL
  #   concated <-  NULL
  #   
  #   if(length(input$mrnaCommonTriplet) == 0 && length(input$mirnaCommonTriplet) ==0 ){
  #     concated <-  dataset
  #   }
  #   if(length(input$mrnaCommonTriplet) >0){
  #     orListForMrna <- rep("|",length(input$mrnaCommonTriplet))
  #     mrnaList <- paste(c(rbind(orListForMrna, matrix(input$mrnaCommonTriplet,ncol = length(orListForMrna)))[-1]),collapse = '')
  #     filteredWithMrna <- dataset%>%
  #       filter(stringr::str_detect(Triplet,mrnaList))
  #     
  #   }
  #   if(length(input$mirnaCommonTriplet) >0){
  #     orListForMirna <- rep("|",length(input$mirnaCommonTriplet))
  #     mirnaList <- paste(c(rbind(orListForMirna, matrix(input$mirnaCommonTriplet,ncol = length(orListForMirna)))[-1]),collapse = '')
  #     filteredWithMirna <- dataset%>%
  #       filter(stringr::str_detect(Triplet,mirnaList))
  #     
  #   }
  #   if(length(input$mrnaCommonTriplet) != 0 || length(input$mirnaCommonTriplet) !=0 ){
  #     concated <- distinct(rbind(filteredWithMrna, filteredWithMirna))
  #   }
  #   
  #   if(length(input$CommonTripletCancer) >0 ){
  #     orListForCommonCancer <- rep("|",length(input$CommonTripletCancer))
  #     cancerListForCommonTripletAndPair <- paste(c(rbind(orListForCommonCancer, matrix(input$CommonTripletCancer,ncol = length(orListForCommonCancer)))[-1]),collapse = '')
  #     
  #     filteredWithCancer <- concated%>%
  #       filter(stringr::str_detect(CancerTypes,cancerListForCommonTripletAndPair))
  #     
  #   }
  #   else{
  #     filteredWithCancer <- NULL
  #     
  #   }
  #   return(filteredWithCancer)
  #   
  # })
  # 
  # 
  # commonMirnaPairFilter <- reactive({
  #   dataset <-MirnaPairsInWhichCancerWCount
  #   
  #   if(length(input$mirnaCommonMirnaPair) >0){
  #     orListForMirna <- rep("|",length(input$mirnaCommonMirnaPair))
  #     mirnaList <- paste(c(rbind(orListForMirna, matrix(input$mirnaCommonMirnaPair,ncol = length(orListForMirna)))[-1]),collapse = '')
  #     filteredWithMirna <- dataset%>%
  #       filter(stringr::str_detect(miRNAPair,mirnaList))
  #     
  #   }
  #   else{
  #     filteredWithMirna <- dataset
  #     
  #   }
  #   
  #   if(length(input$CommonMirnaPairCancer) >0 ){
  #     orListForCommonCancer <- rep("|",length(input$CommonMirnaPairCancer))
  #     cancerListForCommonTripletAndPair <- paste(c(rbind(orListForCommonCancer, matrix(input$CommonMirnaPairCancer,ncol = length(orListForCommonCancer)))[-1]),collapse = '')
  #     
  #     filteredWithCancer <- filteredWithMirna%>%
  #       filter(stringr::str_detect(CancerTypes,cancerListForCommonTripletAndPair))
  #     
  #   }
  #   else{
  #     filteredWithCancer <- NULL
  #     
  #   }
  #   
  #   if(nrow(filteredWithCancer) >0 & !is.null(filteredWithCancer)){
  #     
  #     #CommonMirnaPairCancerCountSub <- substring(input$CommonMirnaPairCancerCount,6,6)
  #     
  #     
  #     
  #     #orListForCommonMirnaPairCancerCount <- rep("|",length(CommonMirnaPairCancerCountSub))
  #     #countListForCommonTripletAndPair <- paste(c(rbind(orListForCommonMirnaPairCancerCount, matrix(CommonMirnaPairCancerCountSub,ncol = length(orListForCommonMirnaPairCancerCount)))[-1]),collapse = '')
  #     
  #     if(input$CommonMirnaPairCancerCount ==1){
  #       count <- c(2,3,4,5)
  #     }
  #     if(input$CommonMirnaPairCancerCount ==2){
  #       count <- c(3,4,5)
  #     }
  #     if(input$CommonMirnaPairCancerCount ==3){
  #       count <- c(4,5)
  #     }
  #     if(input$CommonMirnaPairCancerCount ==4){
  #       count <- c(5)
  #     }
  #     # if(input$CommonMirnaPairCancerCount ==5){
  #     #   count <- c(6,7,8)
  #     # }
  #     # if(input$CommonMirnaPairCancerCount ==6){
  #     #   count <- c(7,8)
  #     # }
  #     # if(input$CommonMirnaPairCancerCount ==7){
  #     #   count <- c(8)
  #     # }
  #     # 
  #     orListForCommonMirnaPairCancerCount <- rep("|",length(count))
  #     countListForCommonTripletAndPair <- paste(c(rbind(orListForCommonMirnaPairCancerCount, matrix(count,ncol = length(orListForCommonMirnaPairCancerCount)))[-1]),collapse = '')
  #     
  #     #orListForCommonMirnaPairCancerCount <- rep("|",length(input$CommonMirnaPairCancerCount))
  #     #countListForCommonTripletAndPair <- paste(c(rbind(orListForCommonMirnaPairCancerCount, matrix(input$CommonMirnaPairCancerCount,ncol = length(orListForCommonMirnaPairCancerCount)))[-1]),collapse = '')
  #     
  #     filteredWithCount <- filteredWithMirna%>%
  #       filter(stringr::str_detect(Count,countListForCommonTripletAndPair))
  #     
  #   }
  #   else{
  #     filteredWithCount <- NULL
  #   }
  #   
  # })
  # 
  # 
  # 
  # output$tableCommonTriplet <- DT::renderDataTable({
  #   
  #   
  #   DT::datatable(commonTripletFilter(),
  #                 options = list(
  #                   columnDefs = list(
  #                     list(className = "dt-center", targets = "_all")
  #                   )
  #                 ))
  #   
  #   
  #   
  #   
  # })
  # 
  # output$tableCommonmiRNAPair <- DT::renderDataTable({
  #   
  #   
  #   
  #   
  #   DT::datatable(commonMirnaPairFilter(),
  #                 options = list(
  #                   columnDefs = list(
  #                     list(className = "dt-center", targets = "_all")
  #                   )
  #                 ))
  # })
  # 
  # 
  # ##################################################################################################    
  # 
  # datasetInputTF <- reactive({
  #   switch(input$CancerSpecificSynergyModulesCancer, 
  #          "ACC" = ACC_Type3,
  #          # "BLCA"=BLCA,
  #          # "BRCA"=BRCA,
  #          # "CESC"=CESC,
  #          # "CHOL"=CHOL,
  #          # "COAD"=COAD,
  #          "DLBC"=DLBC_Type3,
  #          # "ESCA"=ESCA,
  #          # "HNSC"=HNSC,
  #           "KICH"=KICH_Type3
  #          # "KIRC"=KIRC,
  #          # "KIRP"=KIRP,
  #          # "LGG"=LGG,
  #          # "LIHC"=LIHC,
  #          # "LUAD"=LUAD,
  #          # "LUSC"=LUSC,
  #          # "MESO"=MESO,
  #          # "OV"=OV,
  #          # "PAAD"=PAAD,
  #          # "PCPG"=PCPG,
  #          # "PRAD"=PRAD,
  #          # "READ"=READ,
  #          # "SARC"=SARC,
  #          # "SKCM"=SKCM,
  #          # "STAD"=STAD,
  #          # "TGCT"=TGCT,
  #          # "THCA"=THCA,
  #          # "THYM"=THYM,
  #          # "UCEC"=UCEC,
  #          # "UCS"=UCS,
  #          # "UVM"=UVM
  #   )
  # })
  # 
  # output$CancerSpecificSynergyModulesType3 <- DT::renderDataTable({
  #   
  #   
  #   
  #   
  #   DT::datatable(datasetInputTF(),
  #                 options = list(
  #                   columnDefs = list(
  #                     list(className = "dt-center", targets = "_all")
  #                   )
  #                 ))
  # })
  # ##################################################################################################    
  # 
  # output$TCGAAbbrv <- renderTable({
  #   TCGA_abbreviations
  # }, sanitize.text.function = function(x) x)
  # 
  # output$Glossary <- renderTable({
  #   Glossary
  # }, sanitize.text.function = function(x) x)
  # 
  # ##################################################################################################    
  # 
  # 
  # output$totalCountsPlot <- renderPlotly({
  #   
  #   t <- list(
  #     family = "Roboto Slab",
  #     size = 14)
  #   
  #   fig <- plot_ly(miRCoopTotalCounts, x = ~CancerType, y = ~N.Triplets, type = 'bar', name = '# Triplets', marker = list(color = 'rgb(140,69,130)'))
  #   fig <- fig %>% add_trace(y = ~N.miRNAPairs, name = '# miRNA Pairs', marker = list(color = 'rgb(224,161,72)'))
  #   fig <- fig %>% add_trace(y = ~N.miRNAs, name = '# miRNAs',marker = list(color = 'rgb(207,94,90)'))
  #   fig <- fig %>% add_trace(y = ~N.mRNAs, name = '# mRNAs', marker = list(color = 'rgb(41,62,109)'))
  #   fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'group', xaxis = list(title = 'Cancer Type'),font=t)
  #   
  # })
  # 
  # output$MrnaScatterPlot <- renderPlotly({
  #   
  #   fig <- plot_ly(
  #     mRNACountsScatter,
  #     y = ~mRNAinTriplets,
  #     x = ~TargetInteractionsofthemRNA, 
  #     marker = list(color="black",size=5),
  #     text = ~paste('mRNA:', mRNA)
  #   ) %>% layout(yaxis = list(title = '# mRNA in Triplets'), 
  #                xaxis = list(title = '# Target Interactions of the mRNA'),font=t1)
  #   
  # })
  # t1 <- list(
  #   family = "Roboto Slab",
  #   size = 14)
  # output$MirnaScatterPlot <- renderPlotly({
  #   t1 <- list(
  #     family = "Roboto Slab",
  #     size = 14)
  #   
  #   fig <- plot_ly(
  #     miRNACountsScatter, 
  #     y = ~miRNAinTriplets,
  #     x = ~miRNATargets,
  #     marker = list(color="black",size=5),
  #     text = ~paste('miRNA:', miRNA)
  #   ) %>% layout(yaxis = list(title = '# miRNA in Triplets'), 
  #                xaxis = list(title = '# miRNA Targets'),font=t1)
  # })
  # 
  # output$commonMrnaHeatmap <- renderPlotly({
  #   t <- list(
  #     family = "Roboto Slab",
  #     size = 14)
  #   
  #   p <- heatmaply::heatmaply(as.matrix(as.data.table(commonMrnaAbove20),rownames = 1),
  #                             margins = c(0,0,50,0),
  #                             grid_color = "white",
  #                             grid_width = 0.0001,
  #                             fontsize_row = 8, fontsize_col = 8,
  #                             branches_lwd = 0.08,
  #                             xlab = "Cancer Type", ylab = "mRNA",
  #                             #color= colorRampPalette(brewer.pal(3, "Greys"))(256),
  #                             scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
  #                               low = "white",
  #                               high = "black"
  #                             )) %>% layout(font = t)
  #                             #%>% layout(title = list(text=' Most frequent mRNAs across all cancers'))
  # })
  # 
  # output$commonMirnaHeatmap <- renderPlotly({
  #   
  #   t <- list(
  #     family = "Roboto Slab",
  #     size = 14)
  #   
  #   p <- heatmaply::heatmaply(as.matrix(as.data.table(commonMirnaAbove50),rownames = 1),
  #                             margins = c(5,5,50,0),
  #                             grid_color = "white",
  #                             grid_width = 0.0001,
  #                             #plot_method= "plotly",
  #                             fontsize_row = 8, fontsize_col = 8,
  #                             branches_lwd = 0.08,
  #                             xlab = "Cancer Type", ylab = "miRNA",
  #                             #color= colorRampPalette(brewer.pal(3, "Greys"))(256)
  #                             scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
  #                               low = "white",
  #                               high = "black"
  #                             ))%>% layout(font = t)
  #                               #layout(title = ' Most frequent miRNAs across all cancers')
  #   
  #   
  #   
  # })
  # 
  # ##################################################################################################    
  # 
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste(input$dataset, ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(DatasetRoundDigits(), file, row.names = FALSE)
  #   })
  # 
  # 
  # output$downloadCommonTripletsData <- downloadHandler(
  #   filename = function() {
  #     paste("pan-cancer-triplet-data", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(commonTripletFilter(), file, row.names = FALSE)
  #   })
  # 
  # output$downloadCommonMirnaPairsData <- downloadHandler(
  #   filename = function() {
  #     paste("pan-cancer-mirna-pair-data", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(commonMirnaPairFilter(), file, row.names = FALSE)
  #   })
  # 
  # 
  # output$downloadNodeTable <- downloadHandler(
  #   filename = function() {
  #     paste("cancer-specific-triplet-node-data", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(cancerSpecificNetworkNodeEdge()$nodes, file, row.names = FALSE)
  #   })
  # 
  # output$downloadEdgeTable <- downloadHandler(
  #   filename = function() {
  #     paste("cancer-specific-triplet-edge-data", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(cancerSpecificNetworkNodeEdge()$edges, file, row.names = FALSE)
  #   })
  # 
  # 
  # output$downloadNodeTableCommonMirnaPair <- downloadHandler(
  #   filename = function() {
  #     paste("pan-cancer-mirna-pair-node-data", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(commonMirnaNetworkNodeEdge()$nodes, file, row.names = FALSE)
  #   })
  # 
  # output$downloadEdgeTableCommonMirnaPair <- downloadHandler(
  #   filename = function() {
  #     paste("pan-cancer-mirna-pair-edge-data", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(commonMirnaNetworkNodeEdge()$edges, file, row.names = FALSE)
  #   })
  # 
  # output$downloadNodeTableCommonTriplet <- downloadHandler(
  #   filename = function() {
  #     paste("pan-cancer-triplet-node-data", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(commonTripletNetworkNodeEdge()$nodes, file, row.names = FALSE)
  #   })
  # 
  # output$downloadEdgeTableCommonTriplet <- downloadHandler(
  #   filename = function() {
  #     paste("pan-cancer-triplet-edge-data", ".csv", sep = "")
  #   },
  #   content = function(file) {
  #     write.csv(commonTripletNetworkNodeEdge()$edges, file, row.names = FALSE)
  #   })
  
  
}
