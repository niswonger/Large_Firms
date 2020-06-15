##############################################################################
# Set up Libraries 
##############################################################################
library(readr)
library(data.table)
library(ggplot2)
library(xtable)
library(foreign)
library(usmap)
library(multiwayvcov)
library(AER)
library(tidyr)
library(broom)
setwd('~/Documents/Harvard/Research/Large_Firms')
# Bring in data
dt.gsic <- data.table(fread('Data/COMPUSTAT/GSICS.csv'))[fyear == 2018 & indfmt == 'INDL'][,list(gvkey,emp,sale)]
dt.seg <- data.table(fread('Data/COMPUSTAT/segments.csv'))[stype == 'BUSSEG',list(gvkey,SICS1,SICS2)]
# write.csv(dt.gsic, 'Data/COMPUSTAT/compustat_main.csv')
# write.csv(dt.seg, 'Data/COMPUSTAT/compustat_segments.csv')


# Get count of industries or business segments
dt.ind <- melt(dt.seg[,list(gvkey, SICS1,SICS2)], id.vars = 'gvkey')
dt.seg_b <- dt.seg[, list(num_seg = .N), by = list(gvkey)]
dt.seg_i <- dt.ind[!is.na(value), list(industries = length(unique(value))), by = list(gvkey)]

dt.gsic[emp > 0 & sale >0,productivity := 1000*sale/emp]
dt.gsic[emp > 0 & sale >0,l_productivity := log(productivity)]

dt.comp <- merge(dt.gsic, dt.seg_b , by = c('gvkey'))
dt.comp2 <- merge(dt.comp, dt.seg_i , by = c('gvkey'))
# write.csv(dt.comp2, 'Data/COMPUSTAT/compustat_combined.csv')
# dt.comp2 <- data.table(read.csv('Data/COMPUSTAT/compustat_combined.csv'))
# ggplot(dt.comp2, aes(num_seg, industries, weight = emp)) + geom_point() + geom_smooth(method = 'lm')
# ggplot(dt.comp2, aes(productivity, industries )) + geom_point() 
# ggplot(dt.comp2, aes(l_productivity, industries )) + geom_point() 
# ggplot(dt.comp2, aes(l_productivity, num_seg )) + geom_point() 
# ggplot(dt.comp2, aes(l_productivity, emp )) + geom_point() 
# ggplot(dt.comp2, aes(emp, industries )) + geom_point() + geom_smooth(method = 'lm')
# 
# summary(lm(industries ~ emp, dt.comp2))
# 
# 
# ggplot(dt.comp2, aes(industries )) + geom_histogram() 
# ggplot(dt.comp2, aes(productivity )) + geom_histogram() 
# ggplot(dt.comp2, aes(l_productivity )) + geom_histogram() 
# ggplot(dt.comp2, aes(sale )) + geom_histogram() 


# Regress on quadratic including employment
dt.comp2[,l_productivity_2 := l_productivity^2]
dt.comp2[,productivity_2 := productivity^2]
model <- lm(industries ~ l_productivity + l_productivity_2 + emp, dt.comp2[!is.na(l_productivity)])
summary(model)
p.industries <- predict(model)

# Plot predicted value
dt.pred <- data.table(dt.comp2[!is.na(l_productivity)], p.industries)
ggplot(dt.pred, aes(l_productivity, industries, color = 'data' )) + geom_point() + 
  geom_point(aes(l_productivity,p.industries, color = 'fit') )
##############################################################################
# Regress on quadratic of log productivity only and plot fit. weighting by employment
##############################################################################
model <- lm(industries ~ l_productivity + l_productivity_2, dt.comp2[!is.na(l_productivity)], weight = emp)
summary(model)

p.industries <- predict(model)
dt.pred1 <- data.table(dt.comp2[!is.na(l_productivity)], p.industries)
ggplot(dt.pred1, aes(l_productivity, industries )) + geom_point(colour = "gray50") + 
  geom_line(aes(l_productivity,p.industries),size = 1.5 ) + 
  scale_x_continuous('Log Labor Productivity') +
  scale_y_continuous('Number of Listed Industries') + 
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20))


# Repeat but unweighted
# model <- lm(industries ~ l_productivity + l_productivity_2, dt.comp2[l_productivity >0])
# summary(model)
# p.industries <- predict(model)
# dt.pred <- data.table(dt.comp2[l_productivity >0], p.industries)
# ggplot(dt.pred, aes(l_productivity, industries, color = 'data' )) + geom_point() + 
#   geom_point(aes(l_productivity,p.industries, color = 'fit') )
##############################################################################
################# Look at number of segments #################################
##############################################################################
model <- lm(num_seg ~ l_productivity + l_productivity_2, dt.comp2[!is.na(l_productivity)], weight = emp)
summary(model)
p.segs <- predict(model)
dt.pred2 <- data.table(dt.comp2[!is.na(l_productivity)], p.segs)
ggplot(dt.pred2, aes(l_productivity, num_seg )) + geom_point(colour = "gray50") + 
  geom_line(aes(l_productivity,p.segs),size = 1.5 ) + 
  scale_x_continuous('Log Labor Productivity') +
  scale_y_continuous('Number of Listed Business Segments') + 
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20))
##############################################################################
# Make a table of productivity against segments and industries
##############################################################################
prod_dec <- quantile(dt.pred$l_productivity, p = (0:10/10))
ind_dec <- dt.pred1[order(l_productivity)][round(quantile(1:length(p.industries), p = (0:10/10)))]$p.industries
seg_dec <- dt.pred2[order(l_productivity)][round(quantile(1:length(p.industries), p = (0:10/10)))]$p.segs
dt.table <- data.table(prod_dec,ind_dec,seg_dec)
xtable(dt.table)

for (i in 2:length(prod_dec)){
  ind_dec[i-1] <- dt.pred1[l_productivity <=prod_dec[i] & l_productivity>= prod_dec[i-1], list(mean(industries))]
  seg_dec[i-1] <- dt.pred2[l_productivity <=prod_dec[i] & l_productivity>= prod_dec[i-1], list(mean(num_seg))]
  prod_dec[i-1] <- dt.pred2[l_productivity <=prod_dec[i] & l_productivity>= prod_dec[i-1], list(mean(l_productivity))]
  
}
ind_dec[11] <- NULL
seg_dec[11] <- NULL
prod_dec[11] <- NULL
dt.table <- data.table(prod_dec,ind_dec,seg_dec)
xtable(dt.table)
dt.table <- cbind(dt.table,dec = as.numeric(1:10))
dt.table <- melt(dt.table[,list(dec, ind_dec, seg_dec)],id.vars = 'dec')
ggplot(dt.table, aes(as.factor(dec), as.numeric(value), fill = variable )) + 
  geom_bar(stat = 'identity', position = 'dodge') + scale_x_discrete('Decile of Log Labor Productivity') +
  scale_y_continuous('Mean Observed Count') + 
  scale_fill_manual(values=c('black','gray'),labels = c("Inudstries", "Segments"), name = NULL) + 
  theme(panel.background = element_blank(),axis.line = element_line(colour = "black"),
        axis.text=element_text(size=15),
        axis.title=element_text(size=20),
        legend.text = element_text(size=12))

# emp weighted
for (i in 2:length(prod_dec)){
  ind_dec[i-1] <- dt.pred1[l_productivity <=prod_dec[i] & l_productivity>= prod_dec[i-1], list(sum(industries*emp)/sum(emp))]
  seg_dec[i-1] <- dt.pred2[l_productivity <=prod_dec[i] & l_productivity>= prod_dec[i-1], list(sum(num_seg*emp)/sum(emp))]
  prod_dec[i-1] <- dt.pred2[l_productivity <=prod_dec[i] & l_productivity>= prod_dec[i-1], list(sum(l_productivity*emp)/sum(emp))]
  
}
dt.table <- data.table(prod_dec,ind_dec,seg_dec)
xtable(dt.table)

##############################################################################
# Repeat unweighted
model <- lm(num_seg ~ l_productivity + l_productivity_2, dt.comp2[l_productivity >0])
summary(model)
p.segs <- predict(model)
dt.pred <- data.table(dt.comp2[l_productivity >0], p.segs)
ggplot(dt.pred, aes(l_productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(l_productivity,p.segs, color = 'fit') )


# Now look at it without logs
model <- lm(num_seg ~ productivity + productivity_2, dt.comp2[productivity > 0])
summary(model)
p.industries <- predict(model)
dt.pred <- data.table(dt.comp2[productivity > 0], p.industries)
ggplot(dt.pred, aes(productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(productivity,p.industries, color = 'fit') )
ggplot(dt.pred, aes(l_productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(l_productivity,p.industries, color = 'fit') )
# Get higher order polynomial
dt.comp2[,productivity_3 := productivity^3]
dt.comp2[,productivity_4 := productivity^4]
model <- lm(num_seg ~ productivity + productivity_2 + productivity_3 + productivity_4, 
            dt.comp2[productivity > 0])
summary(model)
p.industries <- predict(model)
dt.pred <- data.table(dt.comp2[productivity > 0], p.industries)
ggplot(dt.pred, aes(productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(productivity,p.industries, color = 'fit') )
ggplot(dt.pred, aes(l_productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(l_productivity,p.industries, color = 'fit') )
# Try again while weighting
model <- lm(num_seg ~ productivity + productivity_2 + productivity_3 + productivity_4, 
            dt.comp2[productivity > 0], weight = emp)
summary(model)
p.industries <- predict(model)
dt.pred <- data.table(dt.comp2[productivity > 0], p.industries)
ggplot(dt.pred, aes(productivity, num_seg, color = 'data')) + geom_point() + 
  geom_point(aes(productivity,p.industries, color = 'fit') )
ggplot(dt.pred, aes(l_productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(l_productivity,p.industries, color = 'fit') )
# higher order polynomials in logs
dt.comp2[,l_productivity_3 := l_productivity^3]
dt.comp2[,l_productivity_4 := l_productivity^4]
model <- lm(num_seg ~ l_productivity + l_productivity_2 + l_productivity_3 + l_productivity_4, 
            dt.comp2[productivity > 0])
summary(model)
p.industries <- predict(model)
dt.pred <- data.table(dt.comp2[productivity > 0], p.industries)
ggplot(dt.pred, aes(l_productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(l_productivity,p.industries, color = 'fit') )
# With weights
model <- lm(num_seg ~ l_productivity + l_productivity_2 + l_productivity_3 + l_productivity_4, 
            dt.comp2[productivity > 0], weight = emp)
summary(model)
p.industries <- predict(model)
dt.pred <- data.table(dt.comp2[productivity > 0], p.industries)
ggplot(dt.pred, aes(l_productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(l_productivity,p.industries, color = 'fit') )
# Higher order in logs with weights
dt.comp2[,l_productivity_5 := l_productivity^5]
dt.comp2[,l_productivity_6 := l_productivity^6]
dt.comp2[,l_productivity_7 := l_productivity^7]
dt.comp2[,l_productivity_8 := l_productivity^8]
cutoff <- 0
model <- lm(num_seg ~ l_productivity + l_productivity_2 + l_productivity_3 + l_productivity_4, 
            dt.comp2[productivity > 0 & emp > cutoff], weight = emp)
summary(model)
p.industries <- predict(model)
dt.pred <- data.table(dt.comp2[productivity > 0 & emp > cutoff], p.industries)
ggplot(dt.pred[emp > cutoff], aes(l_productivity, num_seg, color = 'data' )) + geom_point() + 
  geom_point(aes(l_productivity,p.industries, color = 'fit') )

+ scale_x_continuous(limits = c(-1,15))+
  scale_y_continuous(limits = c(-5,100))


