# function to plot cluster analysis

# maybe change colors to some nicer ones
# colorCl <- c( '1'=rgb(0.2,0.5,0.5,0.9) , 
#               '2'=rgb(0.8,0.2,0.5,0.9) , 
#               '3'=rgb(0.7,0.5,0.1,0.9) , 
#               '4'=rgb(0.9,1.0,0.1,0.9) , 
#               '5'=rgb(0.5,0.7,0.2,0.9) , 
#               '6'=rgb(0.9,0.1,0.0,0.9) , 
#               '7'=rgb(0.1,1.0,0.9,0.9) , 
#               'NA'='grey' ) # important that it is in the end
# colClIn <- c( '1'=rgb(0.2,0.5,0.5,0.4) , 
#               '2'=rgb(0.8,0.2,0.5,0.4) , 
#               '3'=rgb(0.7,0.5,0.1,0.4) , 
#               '4'=rgb(0.9,1.0,0.1,0.4) , 
#               '5'=rgb(0.5,0.7,0.2,0.4) , 
#               '6'=rgb(0.9,0.1,0.0,0.4) , 
#               '7'=rgb(0.1,1.0,0.9,0.4) )
colorCl <- c( '1'='red' ,
              '2'='green' ,
              '3'='blue' ,
              '4'='yellow' ,
              '5'='black' ,
              '6'='brown' ,
              '7'='pink' ,
              'NA'='grey' ) # important that it is in the end
colClIn <- c( '1'='red' ,
              '2'='green' ,
              '3'='blue' ,
              '4'='yellow' ,
              '5'='black' ,
              '6'='brown' ,
              '7'='pink')
# fill a map of the world with clusters
plotClusterMap <- function(cl, n) { 
  # cl is a named vector of clusters (the first argument of kmeans)
  # n number of clusters [not realy necessary]
  map.world <- map_data(map="world")
  #map.world$cluster <- sapply(map.world$region, function(x) cl[x])
  #View(map.world)
  map.world$cluster <- sapply(map.world$region, function(x) ifelse(is.na(cl[x]),'NA',as.character(cl[x])) )
  gg <- ggplot()  + 
    geom_map(data=map.world, map=map.world, aes(map_id=region, x=long, y=lat, fill=cluster)) + 
    scale_fill_manual(values = colorCl) + 
    coord_equal() +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank()) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank())
  return(gg)
}

# hierarchical ----
plotClusterHierarchical <- function(dc, showDist=T, showDend=T){
  nCnt <- length(row.names(dc))
  # compute the dissimilarity matrix of the data
  dc.e <- dist(dc, method='euclidean')
  dc.m <- dist(dc, method='manhattan')
  dc.c <- dist(dc, method='canberra')
  # hierarchical clustering
  dc.es <- hclust(dc.e, method='single')
  dc.ea <- hclust(dc.e, method='average')
  dc.ec <- hclust(dc.e, method='complete')
  dc.ew <- hclust(dc.e, method='ward.D2')
  if(showDist) {
    x11()
    par(mfrow=c(1,3))
    image(1:nCnt,1:nCnt,as.matrix(dc.e), main='metrics: Euclidean', asp=1, xlab='i', ylab='j' )
    image(1:nCnt,1:nCnt,as.matrix(dc.c), main='metrics: Canberra', asp=1, xlab='i', ylab='j' )
    image(1:nCnt,1:nCnt,as.matrix(dc.m), main='metrics: Manhattan', asp=1, xlab='i', ylab='j' )
  }
  if(showDend) {
    x11()
    par(mfrow=c(2,2))
    plot(dc.es, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
    plot(dc.ec, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
    plot(dc.ea, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
    plot(dc.ew, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
  }
}

# k-means ----
kmeansPlot     <- function(dc, cent=2, showSp=T, showMap=T, showMeans=T){
  res <- kmeans(dc,centers = cent,nstart = 100)
  if(showSp)  {x11(); plot(dc, col = res$cluster+1)}
  if(showMap) {x11(); print(plotClusterMap(res$cluster, cent))}
  if(showMeans) {
    dc$cluster <- sapply(row.names(dc), function(x) as.character(res$cluster[x]))
    meansCl <- dc %>% group_by(cluster) %>% summarize_each(funs(mean))
    #View(meansCl)
    #View(meansCl[,-'cluster'])
    #print(meansCl[,-cluster])
    x11()
    colors_border=colorCl
    colors_in=    colClIn
    radarchart(meansCl[,!names(meansCl) %in% c('cluster')] , axistype=0 , maxmin=F,
               #custom polygon
               pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
               #custom labels
               vlcex=0.8 )
    legend(x=0.7, y=1.2, legend = meansCl$cluster, bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)
  }
}

kmeansCompare <- function(dc, maxCl=10, n=1){
  b <- w <- NULL
  for(k in 1:maxCl){
    result.k <- kmeans(dc, k, nstart=n)
    w <- c(w, sum(result.k$wit))
    b <- c(b, result.k$bet)
  }
  x11()
  matplot(1:maxCl, b/(w+b), pch='', xlab='clusters', ylab='between/tot', main='Choice of k', ylim=c(0,1))
  lines(1:maxCl, b/(w+b), type='b', lwd=2)
  print('REM: k-means is sensible to the starting baricenters, if you run it 2 times it gives different results')
}
