# PCA on natural resources rents

setwd("C:/Users/Leonardo/Desktop/POLIMI/ATTUALI/Stat App/Progetto/StatApp_test_loc")
load("ReadData/data.RData")

library(reshape2)                       # dcast
library(dplyr)                          # %>%
library(ggplot2)                        # ggplot

source('Filters/functionsFullMatrix.R') # extract2DmatrixWithFullestIndicators, fullness
source('function_extract.R')            # getIndicators, getCntInd, getIndYear, uniCnt, get3D
source("PCA/PCA_function.R")            # PC
source("function_name.R")               # name2codeCnt, ...

# http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/
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

# http://stackoverflow.com/questions/6578355/plotting-pca-biplot-with-ggplot2
PCbiplot <- function(PC, x="PC1", y="PC2") {
  # PC being a prcomp object
  data <- data.frame(obsnames=row.names(PC$x), PC$x)
  plot <- ggplot(data, aes_string(x=x, y=y)) + geom_text(alpha=.4, size=3, aes(label=obsnames))
  plot <- plot #+ geom_hline(yintercept=aes(0), size=.2) + geom_vline(xintercept=aes(0), size=.2)
  datapc <- data.frame(varnames=rownames(PC$rotation), PC$rotation)
  mult <- min(
    (max(data[,y]) - min(data[,y])/(max(datapc[,y])-min(datapc[,y]))),
    (max(data[,x]) - min(data[,x])/(max(datapc[,x])-min(datapc[,x])))
  )
  datapc <- transform(datapc,
                      v1 = .7 * mult * (get(x)),
                      v2 = .7 * mult * (get(y))
  )
  plot <- plot + coord_equal() + geom_text(data=datapc, aes(x=v1, y=v2, label=varnames), size = 5, vjust=1, color="red")
  plot <- plot + geom_segment(data=datapc, aes(x=0, y=0, xend=v1, yend=v2), arrow=arrow(length=unit(0.2,"cm")), alpha=0.75, color="red")
  plot
}

##################################### .
##### extraction of the dataset ##### .
##################################### .
# 01 filter out some countries from a previous analysis
# fixing the year (fixYear) select the best M indicators after filtering out the countries 
# with a number of indicators below Tind 
df <- extract2DmatrixWithFullestIndicators(Indicators, M=400, fixYear=2010, viewFlag=F, Tind=400)
# keep only the countries with at least Tind indicators in 2010
myCnt <- df$CountryName %>% unique() 

# 02 indicators concerning natural resources rents 
myInd <- c('Mineral rents (% of GDP)',
           'Oil rents (% of GDP)',
           'Coal rents (% of GDP)',
           'Natural gas rents (% of GDP)',
           'Forest rents (% of GDP)')
# sum of the previous indicators: 'Total natural resources rents (% of GDP)'

# 03 years of interest
myYear <- c(2000:2012)

# 04 filter Indicators s.t. the corr 3D matrix is full
indFull <- getIndicators(myYear = myYear, myCnt = myCnt, myInd = myInd) %>%
  unifCnt(showInd = F)
# Out some interesting countries:
# Afghanistan, Iraq, ... 
# [still 166 cnt in the dataset]

##################################### . let's do it really good for 1 year
#####       PCA for 2010        ##### . then replicate it for the others (maybe not entirely)
##################################### . indFull is full, try fullness(indFull)==1
# 00 get the 2D matrix
natRes <- getCntInd(indFull, 2010, dropNA = T, showCnt = T)

# 01 Scatterplot - snooping the data
# http://stackoverflow.com/questions/21716567/use-ggpairs-to-create-this-plot
library(GGally) # ggpairs does NOT accept a dataframe whose column have a space
colnames(natRes) <- gsub("\\(.*$", "", colnames(natRes))
colnames(natRes) <- gsub(" ", ".", colnames(natRes)) # we could do a better work with that....
sp <- ggpairs(natRes, 
              upper = list(continuous = wrap("cor", size = 10)),
              lower = list(continuous = "smooth")) # 
x11(); sp
# COMMENT: a lot of the data are more or less 0 (a lot are == 0)
#          and some others seems outliers (but they are interesting for our analysis)
#          apply a transofrmation?
#          if you don't want data == 0 add a noise(?)

# Boxplot - Standardize the data? 
natRes_s <- data.frame(scale(natRes))

bp   <- ggplot(stack(natRes),   aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of raw data')
bp_s <- ggplot(stack(natRes_s), aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of standardize data')
x11(); multiplot(bp,bp_s,cols = 2)
# COMMENT: same remarks as few lines up
#          the standardization of the data is necessary, the columns have different scale

# perform PCA
# prcomp or princomp? prcomp is preferred to princomp
# https://stats.stackexchange.com/questions/20101/what-is-the-difference-between-r-functions-prcomp-and-princomp

#                                prcomp             princomp
# ------------------------------------------------------------------------------------------------ .
# std dev                        $sdev              $sd
# eigenvalues                     -                 $values
# eigenvectors (loadings)        $rotation          $loadings
# scores                         $x                 $scores

# method                         SVD                eigen          SVD more accurate
# work with PCbiplot             yes                no             this is the real reason

nrPCA <- prcomp(natRes_s)
summary(nrPCA)
# a way to plot the cumulated variance with ggplot (could be improved)
cumVar <- data.frame(cumulated.variance=cumsum(nrPCA$sdev^2)/sum(nrPCA$sdev^2),
                     num.PC=c(1:length(nrPCA$sdev)))
vp <- ggplot(cumVar, aes(x = num.PC, y = cumulated.variance)) + geom_line() + geom_point() + ylim(0,1) + ggtitle('Variance explained')
x11(); vp
# COMMENT: the first 2 PC explain 55% of var
#          the first 3 PC explain 74% of var
#          not really satisfying

# loadings
x11(); par(mar = c(1,4,0,2), mfrow = c(5,1)); for(i in 1:5) barplot(nrPCA$rotation[,i], ylim = c(-1, 1), las=1)
# COMMENT: not very accurate....
#          PC.1 : gas and oil
#          PC.2 : coal and minerals
#          PC.3 : forests
#          PC.4 : minerals vs coal and forests
#          PC.5 : oil vs gas

# Boxplot of the PC
bp_pc <- ggplot(stack(data.frame(nrPCA$x)), aes(x = ind, y = values)) + geom_boxplot() + ggtitle('Boxplot of principal components')
x11(); multiplot(bp_s,bp_pc,cols = 1)
# COMMENT: don't know what to say...

# biplot
bip <- PCbiplot(nrPCA) + ggtitle('Biplot')
x11(); bip
# COMMENT: remember that we are dealing with % of GDP
#          1) on the left we will find countries with high gas and oil, low forests
#          2) on the right high forests
#                 so PC.1 is a contrast gas and oil vs forests?
#          3) down high coals and minerals
#
#          IDEA: perform analysis with only the countries with the sum of those rents above 
#                a certain threshold? to avoid that black spot around (0,0)

# another way for the biplot
# https://cran.r-project.org/web/packages/ggfortify/vignettes/plot_pca.html
library(ggfortify)
bip2 <- autoplot(nrPCA, shape=F, label=T, loadings = TRUE, loadings.colour = 'blue', loadings.label = TRUE)

##################################### . what would you change? 
#####          recap            ##### . would you perform the analysis differently?
##################################### . is it ok a structure like that? would you suggest a better one?

# write below, or tell me directly on the wa group