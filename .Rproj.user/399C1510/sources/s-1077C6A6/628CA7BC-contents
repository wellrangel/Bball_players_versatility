
initH2o<-function()
{
  
  
  h2o.init()
  
  h2o.init(nthreads = -1)
  h2o.clusterInfo()
  
  options(scipen=20)
  return (h2o);
}
shutDownQuitH2o<-function(h2o)
{
  
  h2o.shutdown(prompt=FALSE)
  
}
readData<-function()
{
  
  #read csv
  totaldados <- read.csv("outputdata/aggregateData.csv", sep = "," ,stringsAsFactors=FALSE, encoding = "UTF-8")
  
  totaldados <-totaldados[totaldados$played_minutes > 100,]
  
  totaldados[is.na(totaldados)] <- 0
  totaldados <- totaldados[totaldados$id_jogador > 0,]
  totaldados <- totaldados[totaldados$posicao != "NOTFOUND",]
  totaldados <- totaldados[totaldados$posicao != "0",]
  totaldados <- totaldados[totaldados$posicao != "NOTFOUNDCONFIRMED",]
  totaldados <- totaldados[totaldados$posicao != "",]
  
  
  totaldados[,c("posicao")] <- as.factor(totaldados[,c("posicao")])
  
  
  
  
  return(totaldados);
  
}

getTreinoData<-function(totaldados)
{
  #separando dados de treino com todas as temporadas exceto as duas ultimas
  treino = totaldados[totaldados$Temporada != "NBB 8",]
  treino = treino[treino$Temporada != "NBB 9",]
  
  return (treino)
}

getValidacaoData<-function(totaldados)
{
  
  #separando dados de validacao com a penultima temporada
  validacao = totaldados[totaldados$Temporada == "NBB 8",]
  
  return (validacao)
}

getTesteData<-function(totaldados)
{
  
  #separando dados de teste com a ultima temporada
  teste = totaldados[totaldados$Temporada == "NBB 9",]
  
  
  return (teste)
}






normalizeMinMax <- function(x)
{
  #print(x)
  return(x- min(x) /(max(x)-min(x)))
  
}

normalizeMinutes <- function(minutes,x)
{
  return((40*x)/minutes)
}


normalizePercentual <- function(x)
{
  return( (x/sum(x))*100)
}






plot_clus_coord<-function (cluster_model, data, clusterNumber)
{
  ## Add mean for all rows.
  all_cluster_results=round(as.data.frame(rbind(cluster_model$centers, colMeans(data))),2)
  
  
  ######################################################################
  ##  Cluster profiling. Extracting main characteristics from each one.
  ######################################################################
  
  ## Scale data to plot all in only one graph
  maxs <- apply(all_cluster_results, 2, max)
  mins <- apply(all_cluster_results, 2, min)
  cl_scaled=as.data.frame(scale(all_cluster_results, center = mins, scale = maxs - mins))
  
  
  #browser()
  ## Assign cluster number (label)
  cl_scaled$cluster=c(paste("C",1:max(cluster_model$cluster), sep="_"),"All")
  cl_scaled <- cl_scaled[cl_scaled$cluster != "All",]
  ## This transform the data according to needed input of ggplot. The best way to understand this is to take a look at the data.
  melted_data=melt(cl_scaled, id.vars = "cluster")
  
  if (clusterNumber > 0)
  {
    
    melted_data = subset(melted_data, cluster == paste("C_", clusterNumber, sep=""))
    
  }  
  
  #color=cluster
  ## Coordinate plot
  coor_plot=ggplot(melted_data, aes(x=variable, y=value,  group=cluster, color=cluster),  environment = environment()) +  
    geom_path(alpha = 0.9) +
    geom_point() + 
    xlab("Variables") +
    ylab("Incidence percentage") + 
    ggtitle("Clusters") +
    theme(axis.text.x=element_text(angle = 90, vjust = 0.5, size=14), plot.title=element_text(size=20,face="bold"))  
  
  
  plot(coor_plot)
}





get_clus_coord<-function (cluster_model, data, clusterNumber)
{
  ## Add mean for all rows.
  all_cluster_results=round(as.data.frame(rbind(cluster_model$centers, colMeans(data))),2)
  
  
  ######################################################################
  ##  Cluster profiling. Extracting main characteristics from each one.
  ######################################################################
  
  ## Scale data to plot all in only one graph
  maxs <- apply(all_cluster_results, 2, max)
  mins <- apply(all_cluster_results, 2, min)
  cl_scaled=as.data.frame(scale(all_cluster_results, center = mins, scale = maxs - mins))
  
  
  #browser()
  ## Assign cluster number (label)
  cl_scaled$cluster=c(paste("C",1:max(cluster_model$cluster), sep="_"),"All")
  cl_scaled <- cl_scaled[cl_scaled$cluster != "All",]
  ## This transform the data according to needed input of ggplot. The best way to understand this is to take a look at the data.
  melted_data=melt(cl_scaled, id.vars = "cluster")
  
  if (clusterNumber > 0)
  {
    
    melted_data = subset(melted_data, cluster == paste("C_", clusterNumber, sep=""))
    
  }  
  
  
  return (cl_scaled)
}

criaMatrizMapa<-function (atletasFinalFiltrado)
{
  
  mat <- matrix(0, nrow = 10, ncol = 6)
  colnames(mat) <- c("0 Positions","1 Position","2 Positions","3 Positions","4 Positions","5 Positions")
  rownames(mat) <- c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
  
  inicio = 0
  #for(i in 1:1) {
  for(i in 1:nrow(atletasFinalFiltrado)) {
    for(j in 1:10) {
      totalEncontrado = 0;
      limite = ((j-1) *10) + 1
      if (limite == 1){
        #limite =0
      }
      if (atletasFinalFiltrado[i,]$Ala*100 >= limite){
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$AlaArmador*100 >= limite){
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$Armador*100 >= limite){
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$AlaPivo*100 >= limite){
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$Pivo*100 >= limite){
        totalEncontrado = totalEncontrado +1
      }
      
      mat[j, totalEncontrado+1] <- mat[j, totalEncontrado+1] + 1
      
    }
  }
  #browser()
  melted_cormat <- melt(mat)
  #head(melted_cormat)
  
  #melted_cormat$X1 <- as.factor(melted_cormat$X1)
  #melted_cormat$X2 <- as.factor(melted_cormat$X2)
  
  #melted_cormat$value <- as.factor(melted_cormat$value)
  
  
  
  return (melted_cormat)
  
  #melted_cormat2 <- melted_cormat[order(melted_cormat$X2, melted_cormat$value, decreasing = TRUE),] 
  
}


criaMatrizMapaV2<-function (atletasFinalFiltrado)
{
  
  # browser()
  mat <- matrix(0, nrow = 10, ncol = 3)
  colnames(mat) <- c( "1 Position","2 Positions","3 Positions")
  rownames(mat) <- c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
  
  inicio = 0
  #for(i in 1:1) {
  for(i in 1:nrow(atletasFinalFiltrado)) {
    
    col = round(atletasFinalFiltrado[i,]$range/10,0)
    row = as.numeric(substring( atletasFinalFiltrado[i,]$totalPos, 1, 1))
    
    #sbrowser()
    mat[col, row] <-  mat[col, row] + 1
    
  }
  
  melted_cormat <- melt(mat)
  
  
  return (melted_cormat)
  
  #melted_cormat2 <- melted_cormat[order(melted_cormat$X2, melted_cormat$value, decreasing = TRUE),] 
  
}

heatMapTime<-function (melted_cormat, nomeTime, cor)
{
  
  if (cor == 1){
    cores =  c("white", rev(heat.colors(8)))
  }else{
    
    cores = c("white", "antiquewhite", 
              "lightcyan", "lightblue", "olivedrab1", "darkorange",
              "cyan", "deepskyblue", "green","darkorange2",
              "darkcyan", "darkblue", "darkgreen","darkorange3")
  }
  titulo = paste("Distribution of player percentages of ", nomeTime)
  titulo = paste(titulo, " season 2016")
  
  ggplot(data = melted_cormat, aes(x=X2, y=X1)) + 
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = label ) ,col='black',cex=8) + 
  scale_x_discrete(limits=c("5 Positions","4 Positions","3 Positions","2 Positions","1 Position","0 Positions")) +
  #scale_fill_gradient2(low="red4", high="red", guide="colorbar")+
  #labs(title = titulo , fill="Number of players") +
  xlab("Positions") +
  guides(fill=FALSE) + 
  ylab("Probability Range (%)") + 
  theme(axis.text=element_text(size=25),
        axis.title=element_text(size=35,face="bold")) +
  scale_fill_manual(
    values = cores)
}

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

criaDetalheMapa<-function (atletasFinalFiltrado)
{
  
 
  DetalheMapa <- data.frame(faixa=character(),
                            PosX=integer(),
                            Posicao=character(), 
                            Jogador=character(),
                            Apelido=character(),
                            Equipe=character(),
                            Temporada=character(),
                            Ala=numeric(),
                            AlaArmador=numeric(),
                            Armador=numeric(),
                            AlaPivo=numeric(),
                            Pivo=numeric(),
                            stringsAsFactors=FALSE) 
  
  
  
  intervalos = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
  
  inicio = 0
  
  #for(i in 1:1) {
  for(i in 1:nrow(atletasFinalFiltrado)) {
    for(j in 1:10) {
      myRow <- DetalheMapa[1,]
      
      posicoes = ""
      totalEncontrado = 0;
      limite = ((j-1) *10) + 1
      if (limite == 1){
        #limite =0
      }
      if (atletasFinalFiltrado[i,]$Ala*100 >= limite){
        posicoes = paste(posicoes, "Ala", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$AlaArmador*100 >= limite){
        posicoes = paste(posicoes, "AlaArmador", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$Armador*100 >= limite){
        posicoes = paste(posicoes, "Armador", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$AlaPivo*100 >= limite){
        posicoes = paste(posicoes, "AlaPivo", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$Pivo*100 >= limite){
        posicoes = paste(posicoes, "Pivo", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      
      myRow$faixa =  intervalos[j]
      myRow$Posicao = substring(posicoes, 2) 
      myRow$Jogador =  atletasFinalFiltrado[i,]$jogador
      myRow$Apelido =  atletasFinalFiltrado[i,]$jogador_apelido
      myRow$Equipe =  atletasFinalFiltrado[i,]$equipe
      myRow$Temporada =  atletasFinalFiltrado[i,]$Temporada
      myRow$Ala = atletasFinalFiltrado[i,]$Ala
      myRow$AlaArmador = atletasFinalFiltrado[i,]$AlaArmador
      myRow$Armador = atletasFinalFiltrado[i,]$Armador
      myRow$AlaPivo = atletasFinalFiltrado[i,]$AlaPivo
      myRow$Pivo = atletasFinalFiltrado[i,]$Pivo
      
      if (totalEncontrado==1  ){
        myRow$PosX =  paste(totalEncontrado, "Position", sep=" ")  
      }else{
        myRow$PosX =  paste(totalEncontrado, "Positions", sep=" ")  
      }
      
      
      DetalheMapa <- rbind(DetalheMapa, myRow)   
      
      
    }
  }
  #browser();
  return (DetalheMapa)
}



criaDetalheMapaExclusivo<-function (atletasFinalFiltrado)
{
  
  
  DetalheMapa <- data.frame(faixa=character(),
                            PosX=integer(),
                            Posicao=character(), 
                            Jogador=character(),
                            Apelido=character(),
                            Equipe=character(),
                            Temporada=character(),
                            Ala=numeric(),
                            AlaArmador=numeric(),
                            Armador=numeric(),
                            AlaPivo=numeric(),
                            Pivo=numeric(),
                            stringsAsFactors=FALSE) 
  
  
  
  intervalos = c("1-10","11-20","21-30","31-40","41-50","51-60","61-70","71-80","81-90","91-100")
  
  inicio = 0
  limite_superior = 10
  #for(i in 1:1) {
  for(i in 1:nrow(atletasFinalFiltrado)) {
    for(j in 1:10) {
      myRow <- DetalheMapa[1,]
      
      posicoes = ""
      totalEncontrado = 0;
      
      limite = ((j-1) *10)
     
      if (limite == 1){
        #limite =0
        limite_superior = 10
      }
      limite_superior = limite + 9
      
      if (atletasFinalFiltrado[i,]$Ala*100 >= limite && atletasFinalFiltrado[i,]$Ala*100 <= limite_superior){
        posicoes = paste(posicoes, "Ala", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$AlaArmador*100 >= limite && atletasFinalFiltrado[i,]$AlaArmador*100 <= limite_superior){
      
        posicoes = paste(posicoes, "AlaArmador", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$Armador*100 >= limite && atletasFinalFiltrado[i,]$Armador*100 <= limite_superior){
      
        posicoes = paste(posicoes, "Armador", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$AlaPivo*100 >= limite && atletasFinalFiltrado[i,]$AlaPivo*100 <= limite_superior){
      
        posicoes = paste(posicoes, "AlaPivo", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      if (atletasFinalFiltrado[i,]$Pivo*100 >= limite && atletasFinalFiltrado[i,]$Pivo*100 <= limite_superior){
        posicoes = paste(posicoes, "Pivo", sep=",")
        totalEncontrado = totalEncontrado +1
      }
      
      myRow$faixa =  intervalos[j]
      myRow$Posicao = substring(posicoes, 2) 
      myRow$Jogador =  atletasFinalFiltrado[i,]$jogador
      myRow$Apelido =  atletasFinalFiltrado[i,]$jogador_apelido
      myRow$Equipe =  atletasFinalFiltrado[i,]$equipe
      myRow$Temporada =  atletasFinalFiltrado[i,]$Temporada
      
      myRow$Ala = atletasFinalFiltrado[i,]$Ala
      myRow$AlaArmador = atletasFinalFiltrado[i,]$AlaArmador
      myRow$Armador = atletasFinalFiltrado[i,]$Armador
      myRow$AlaPivo = atletasFinalFiltrado[i,]$AlaPivo
      myRow$Pivo = atletasFinalFiltrado[i,]$Pivo
      
      if (totalEncontrado==1  ){
        myRow$PosX =  paste(totalEncontrado, "Position", sep=" ")  
      }else{
        myRow$PosX =  paste(totalEncontrado, "Positions", sep=" ")  
      }
      
      
      DetalheMapa <- rbind(DetalheMapa, myRow)   
      
      
    }
  }
  #browser();
  return (DetalheMapa)
}

GetDetalheMapa<-function (DetalheMapa, intervalo, posx, valor, total )
{
  
  
  #browser()
  resultado = ""
  if (valor != "0"){
    
    
    #aggdata <- aggregate(Jogador~faixa+PosX+Posicao, data=DetalheMapa, FUN=NROW)
    
    
    #filtro <- aggdata[aggdata$faixa == intervalo & aggdata$PosX == posx,]
    
    
    #for(i in 1:nrow(filtro)) {
    #  resultado = paste(resultado, filtro[i,]$Posicao)
    #  resultado = paste(resultado, "=")
    #  resultado = paste(resultado, filtro[i,]$Jogador)
    #  resultado = paste(resultado, "", sep='\n')
      
    #}
    #browser()
    perc = (valor/total)*100
    resultado = paste(round(perc,2), "% (",  valor, ")", sep="")  
    
  }
  
  return (resultado)
}
