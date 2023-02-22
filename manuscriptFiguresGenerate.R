library(shiny)
library(DT)
library(dplyr)
library(shinythemes)
library(shinyWidgets)
library(igraph)
library(RColorBrewer)
library(networkD3)
library(htmlwidgets)
library(visNetwork)
library(shinyalert)
library(shinycssloaders)
library(shinycustomloader)
library(sqldf)
library(readr)
library(ggplot2)
library(formattable)
library(shinyBS)
library(purrr)
library(bslib)
library(spsComps)
library(bsplus)
library(reshape)
library(data.table)
library(readxl)
library(heatmaply)
library(plotly)

commonMrnaAbove20 <- read_excel("dataset/stats/totalMrnaCountCancerMatrixWithTotalMrnaCountInDatabasesAfterBHCorrectionAbove15.xlsx")
commonMrnaAbove20 <- as.data.frame(commonMrnaAbove20)

commonMirnaAbove50 <- read_excel("dataset/stats/totalMirnaCountCancerMatrixWithTotalMirnaCountInDatabasesAfterBHCorrectionAbove30.xlsx")
commonMirnaAbove50 <- as.data.frame(commonMirnaAbove50)

mRNACountsScatter <- read_delim("dataset/stats/mRNACountsScatterAfterBHCorrection.csv", 
                                delim = ",", escape_double = FALSE, trim_ws = TRUE)

miRNACountsScatter <- read_delim("dataset/stats/miRNACountsScatterAfterBHcorrection.csv", 
                                 delim = ",", escape_double = FALSE, trim_ws = TRUE)



t1 <- list(
  family = "Ubuntu",
  size = 20)

fig <- plot_ly(
  mRNACountsScatter,
  y = ~mRNAinTriplets,
  x = ~TargetInteractionsofthemRNA, 
  marker = list(color="black",size=6),
  text = ~paste('mRNA:', mRNA)
) %>% layout(yaxis = list(title = '<b># mRNA in Triplets</b>'), 
             xaxis = list(title = '<b># Target Interactions of the mRNA </b>'),font=t1)
fig

fig <- plot_ly(
  miRNACountsScatter, 
  y = ~miRNAinTriplets,
  x = ~miRNATargets,
  marker = list(color="black",size=6),
  text = ~paste('miRNA:', miRNA)
) %>% layout(yaxis = list(title = '<b># miRNA in Triplets</b>'), 
             xaxis = list(title = '<b># miRNA Targets</b>'),font=t1)
fig

p <- heatmaply::heatmaply(as.matrix(as.data.table(commonMrnaAbove20),rownames = 1),
                          margins = c(0,0,50,0),
                          grid_color = "white",
                          grid_width = 0.0001,
                          fontsize_row = 10, fontsize_col = 10,
                          branches_lwd = 0.08,
                          #xlab = "Cancer Type", ylab = "mRNA",
                          #color= colorRampPalette(brewer.pal(3, "Greys"))(256),
                          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                            low = "white",
                            high = "black"
                          ))%>% layout(xaxis=list(title=list(text="Cancer Type",font = list(size=20))),
                                       yaxis=list(title=list(text="mRNA",font = list(size=20),standoff=51)))

p


p <- heatmaply::heatmaply(as.matrix(as.data.table(commonMirnaAbove50),rownames = 1),
                          margins = c(5,5,50,0),
                          grid_color = "white",
                          grid_width = 0.0001,
                          #plot_method= "plotly",
                          fontsize_row = 10, fontsize_col = 10,
                          branches_lwd = 0.08,
                          #color= colorRampPalette(brewer.pal(3, "Greys"))(256)
                          scale_fill_gradient_fun = ggplot2::scale_fill_gradient2(
                            low = "white",
                            high = "black"
                          ))%>% layout(xaxis=list(title=list(text="Cancer Type",font = list(size=20))),
                                       yaxis=list(title=list(text="miRNA",font = list(size=20),standoff=51)))

p

print(commonMrnaAbove20$mRNA)