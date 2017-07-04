# function to plot cluster analysis

simCnt <- function(dc,cnt,n){ # find the n most similar countries to cnt
  return(sort(as.matrix(dist(dc))[cnt,])[2:(n+1)])
}

getClu <- function(cnt,km) { # find the cluster of cnt
  return(km$cluster[cnt])
}

# # maybe change colors to some nicer ones
# colorCl <- c( '1'=rgb(0.9,0.4,0.0,0.9) , # orange
#               '2'=rgb(0.8,0.2,0.5,0.9) , # purple
#               '3'=rgb(0.4,0.0,1.0,0.9) , # blue
#               '4'=rgb(0.9,1.0,0.1,0.9) , # yellow
#               '5'=rgb(0.5,0.7,0.2,0.9) , # green
#               '6'=rgb(0.9,0.1,0.0,0.9) , # red
#               '7'=rgb(0.2,0.5,0.5,0.9) , # green water
#               '8'=rgb(0.1,1.0,0.9,0.9) , # light blue
#               '9'=rgb(0.7,0.5,0.1,0.9) , # light brown
#               'NA'='grey' ) # important that it is in the end
# colClIn <- c( '1'=rgb(0.9,0.4,0.0,0.4) ,
#               '2'=rgb(0.8,0.2,0.5,0.4) ,
#               '3'=rgb(0.4,0.0,1.0,0.4) ,
#               '4'=rgb(0.9,1.0,0.1,0.4) ,
#               '5'=rgb(0.5,0.7,0.2,0.4) ,
#               '6'=rgb(0.9,0.1,0.0,0.4) ,
#               '7'=rgb(0.2,0.5,0.5,0.4) ,
#               '8'=rgb(0.1,1.0,0.9,0.4) ,
#               '9'=rgb(0.7,0.5,0.1,0.4) )
colorClMac <- c( '1'='red' ,
              '2'='green' ,
              '3'='blue' ,
              '4'='yellow' ,
              '5'='black' ,
              '6'='brown' ,
              '7'='pink' ,
              'NA'='grey' ) # important that it is in the end
colorCl <- c( '1'=rgb(0.2,0.3,0.6,0.9) , # blue
              '2'=rgb(0.8,1.0,1.0,0.9) , # cream 0.8/1/1 
              '3'=rgb(1.0,0.3,0.4,0.9) , # purple
              '4'=rgb(1.0,1.0,0.2,0.9) , # yellow
              '5'=rgb(1.0,0.6,0.1,0.9) , # orange
              '6'=rgb(0.5,0.7,0.2,0.9) , # green
              '7'=rgb(0.1,1.0,0.9,0.9) , # light blue
              'NA'='grey' ) # important that it is in the end
colClIn <- c( '1'=rgb(0.2,0.3,0.6,0.4) , # blue
              '2'=rgb(0.8,0.7,0.5,0.4) ,   #'2'=rgb(1.0,1.0,0.7,0.4) , # cream -> darker
              '3'=rgb(1.0,0.3,0.4,0.4) , # purple
              '4'=rgb(1.0,1.0,0.2,0.4) , # yellow
              '5'=rgb(1.0,0.6,0.1,0.4) , # orange
              '6'=rgb(0.5,0.7,0.2,0.4) , # green
              '7'=rgb(0.1,1.0,0.9,0.4) ) # light blue
# fill a map of the world with clusters
plotClusterMap <- function(cl, n, mac=F) {
  if(mac) colorCl=colorClMac
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
          axis.ticks.y=element_blank()) + 
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(nrow = 1, label.position = "bottom",label.hjust = 0.5))
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
kmeansPlot     <- function(dc, cent=2, showSp=T, showMap=T, showMeans=T, mac=F){
  if(mac) {colClIn=NULL; colorCl=colorClMac}
  res <- kmeans(dc,centers = cent,nstart = 100)
  if(showSp)  {x11(); plot(dc, col = res$cluster+1)}
  if(showMap) {x11(); print(plotClusterMap(res$cluster, cent, mac))}
  if(showMeans) {
    dc$cluster <- sapply(row.names(dc), function(x) as.character(res$cluster[x]))
    meansCl <- dc %>% group_by(cluster) %>% summarize_each(funs(mean))
    x11()
    colors_border=colorCl
    colors_in=    colClIn
    radarchart(meansCl[,!names(meansCl) %in% c('cluster')] , axistype=0 , maxmin=F,
               #custom polygon
               pcol=colors_border , pfcol=colClIn , plwd=4 , plty=1,
               #custom the grid
               cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
               #custom labels
               vlcex=0.8 )
    legend(x=0.7, y=1.2, legend = meansCl$cluster, bty = "n", pch=20 , col=colors_border , text.col = "grey", cex=1.2, pt.cex=3)
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

radarTopic <- function(dc,km,cntRad=NULL,mac=F){
  if(mac) {colClIn=NULL; colorCl=colorClMac}
  dc$cluster <- sapply(row.names(dc), function(x) as.character(km$cluster[x]))
  meansCl <- dc %>% group_by(cluster) %>% summarize_each(funs(mean))
  if(!is.null(cntRad)){ # put a country in cntRad to plot it on the radarplot
    meansCl <- rbind(meansCl,dc[cntRad,])
    meansCl[cntRad,'cluster'] <- cntRad 
  }
  radarchart(meansCl[,!names(meansCl) %in% c('cluster')] , axistype=0 , maxmin=F,
             #custom polygon
             pcol=colorCl , pfcol=colClIn , plwd=4 , plty=1,
             #custom the grid
             cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
             #custom labels
             vlcex=0.8 )
  #legend(x=0, y=-1.3, xjust = 0.5, horiz=T, legend = meansCl$cluster, bty = "n", pch=20 , col=colorCl , text.col = "grey", cex=1.2, pt.cex=3)
  return(recordPlot())
}

