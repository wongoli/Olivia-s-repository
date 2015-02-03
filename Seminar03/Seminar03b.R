#The full photoRec dataset has 39 samples and 29,949 probesets. 
#Choose 2 . or 20 . or 200 random probesets/genes and look for gene expression differences between the two genotypes, wild type versus knockout. 
#Make use of the graphing techniques discussed this week such as scatter plots, box plot, etc. 
library(ggplot2)
kDat <- readRDS("GSE4051_MINI (1).rds")
#can generate a quick and dirty scatter plot with the artificially named genes crabHammer and eggBomb
qplot(crabHammer, eggBomb, data = kDat)
#now will generate scatter plot with ggplot() in layers
p <- ggplot(kDat, aes(x = crabHammer, y = eggBomb))
str(p)
# using the + sign to add layers, the geom_point plotted the normal scatter plot
(p <- p + geom_point())
# stat_smooth will plot a linear regression model
(p <- p + stat_smooth())
#this set of commands will switch the grey backdrop to white and put on titles (both plot and axies)
(p <- p + theme_bw() + xlab("Expression of crabHammer") + ylab("Expression of eggBomb") + ggtitle("Scatterplot for expression levels"))
#rearrangement of data to compare crabHammer to the other two (eggBomb and poisonFang) probes
nDat <- with(kDat, data.frame(sidChar, sidNum, devStage, gType, crabHammer, probeset = factor(rep(c("eggBomb", "poisonFang"), each = nrow(kDat))), geneExp = c(eggBomb, poisonFang)))
str(nDat)
#with the aes() function to plot the scatter BUT TAKE NOTE that the color determination must be in the aes command
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) +  geom_point())
#the following command will also generate the same plot
(p <- ggplot(nDat, aes(crabHammer, geneExp)) + geom_point(aes(color = probeset))))
#stat_smooth(se = F) will turn off standard error ribbon
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) +  geom_point() + stat_smooth(se = F))
#can also make the linear regression for all of the groups of probesets by setting it in the stat_smooth() with aes(group = 1)
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = probeset)) +  geom_point() +  stat_smooth(se = F, aes(group = 1)))
#plot in separate panels with facet_wrap
(p <- ggplot(nDat, aes(crabHammer, geneExp)) +  geom_point() + facet_wrap(~ probeset))
# what about facet_grid?  -> gave me the same outcome
(p <- ggplot(nDat, aes(crabHammer, geneExp)) + geom_point() + facet_grid(~ probeset))
# differentiate group (WT/ KO) by color
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = gType)) +  geom_point() +  facet_wrap(~ probeset))
# color in development stage
(p <- ggplot(nDat, aes(crabHammer, geneExp, color = devStage)) +   geom_point() +  facet_wrap(~ probeset))

#STRIPPLOTS!
#must first redefine the data set to make the geneexp as a variable of the factors 
oDat <-with(kDat, data.frame(sidChar, sidNum, devStage, gType, probeset = factor(rep(c("crabHammer", "eggBomb", "poisonFang"), each = nrow(kDat))), geneExp = c(crabHammer, eggBomb, poisonFang)))
str(oDat)
#this plots gene exp with the probset in a stripplot
(p <- ggplot(oDat, aes(geneExp, probeset)) +  geom_point())
#adding jitter(?) allow to see the closely related points?
(p <- ggplot(oDat, aes(geneExp, probeset)) +  geom_point(position = position_jitter(height = 0.1)))
#now looking at devstage in x-axis and geneexp in y-axis
(p <- ggplot(oDat, aes(devStage, geneExp)) + geom_point())
#different panels for each probeset
(p <- p + facet_wrap(~ probeset))
#adding the genotype information
(p <- p + aes(color = gType))
# adding the mean for each genotype
(p <- p + stat_summary(fun.y = mean, geom = "point", shape = 4, size = 4))


# DENSITY PLOTS!!!
# define the data as defined in the stripplot
#plotting the basic density plot
(p <- ggplot(oDat, aes(geneExp)) +  geom_density())
#removing the base lines
(p <- ggplot(oDat, aes(geneExp)) +  stat_density(geom = "line", position = "identity"))
# like the one generated in lattice, the points can be plotted below the density curve by adding the last commands
(p <- ggplot(oDat, aes(geneExp)) +  stat_density(geom = "line", position = "identity") +  geom_point(aes(y = 0.05), position = position_jitter(height = 0.005)))
#changing the bandwidth with the adjust statment in stat_density
(p <- ggplot(oDat, aes(geneExp)) +  stat_density(geom = "line", position = "identity", adjust = 0.5) + geom_point(aes(y = 0.05), position = position_jitter(height = 0.005)))
# separating into different panels
(p <- p + facet_wrap(~ gType))
# different colors for different gType
(p <- ggplot(oDat, aes(geneExp, color = gType)) +  stat_density(geom = "line", position = "identity") +  geom_point(aes(y = 0.05), position = position_jitter(height = 0.005)))


#BOX PLOTS!!!
