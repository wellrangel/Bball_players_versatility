library(reshape2)
library(ggplot2)
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
#library("corrplot")

#reading csv file with aggregate 9 seasons in nbb
dfstats <- read.csv("outputdata/aggregateData.csv", sep = "," ,stringsAsFactors=FALSE)

#------------ Correlation R2---------------#

correl <- cor(sub)^2
correl_df<-as.data.frame(correl)

melted_cormat <- melt(correl, na.rm = TRUE)

#plot heatmap
ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="R2") +
  theme_minimal()+ 
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1),
        axis.title.x=element_blank(),
        axis.title.y=element_blank()
        
  )+
  coord_fixed()


