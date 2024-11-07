##covid19-GDP

library(ggplot2)


##############GDPLOSS-fig2

library(FD)
library(RColorBrewer)

setwd('~/final')
group <- read.csv("data.csv", fileEncoding = "GBK")	
gdpgroup <- subset(group,gdploss20>-25&gdploss20<25)	


#Extraction of correlation coefficient factors

fac=gdpgroup[,c("Country","agriculture","~")]

eco=gdpgroup[c("Country","gdploss20","~")]

# Merge two data boxes and keep the Country column
combined_data <- merge(fac, eco, by = "Country")

# Create an empty data box to store the results
results <- data.frame(from = character(),
                      to = character(),
                      coef = numeric(),
                      p_value = numeric(),
                      stringsAsFactors = FALSE)

# Calculate correlation coefficient and p-value
for (fac_col in colnames(fac)[-1]) {  
  for (eco_col in colnames(eco)[-1]) { 
    cor_test <- cor.test(combined_data[[fac_col]], combined_data[[eco_col]], method = "pearson",use = "pairwise.complete.obs")

    results <- rbind(results, data.frame(from = fac_col,
                                         to = eco_col,
                                         coef = cor_test$estimate,
                                         p_value = cor_test$p.value))
  }
}

###coef,p_value classification
results1<-results%>%
  mutate(
    coef1 = cut(
      coef,  
      breaks = c(-Inf, -0.4,-0.2, 0, 0.2, 0.4,0.6, Inf),
      labels = c("< -0.4","-0.4 - -0.2","-0.2 - 0", "0 - 0.2", "0.2 - 0.4","0.4 - 0.6","> 0.6")
    ),
    p_value1 = cut(
      p_value,  
      breaks = c(-Inf, 0.01, 0.05, 0.1, Inf),
      labels = c("< 0.01", "< 0.05", "< 0.1", ">= 0.1")
    )
  )


##制图


g<-qcorrplot(correlate(fac[, -which(names(fac) == "Country")], 
                       use = "pairwise.complete.obs", method = "pearson"), type = "lower", diag = FALSE) +	
  geom_tile() +	
  geom_couple(aes(colour = coef1, size = p_value1, from = to, to = from),	
              data = results1, curvature = 0.1) +	
  scale_fill_gradientn(colours = rev(brewer.pal(n = 9, name = "RdBu")))+	
  geom_mark(	
            sep = '\n',	
            size = 5.5,	
            sig_level = c(0.1,0.05, 0.01),
            sig_thres = 0.1 	
           )+	
  labs(x=" ",y = " ")+
  scale_size_manual(values = c(3.2, 1.6,0.8,0.2)) +	
  scale_colour_manual(values = mycol) +	
  theme(legend.position="none",	
        axis.text = element_text(size=18))

setwd ("~/figure")
tiff("~.tif",width=5000,height=5000,res=600,compression = "lzw")
g
dev.off()






###########GDPLOSS-fig4  Correlations between resistance and recovery

setwd ("~/final") 

gdps<-read.csv("data.csv")

gdps1<-subset(gdps,gdploss20<25&gdploss20>-25)

rc<-ggplot(data=gdps1,aes(x = gdploss20, y = gdploss21)) +
  
  geom_vline(xintercept=0,
             colour = "#A9A9A9",linetype="longdash",size=0.8)+
  geom_hline(yintercept=0,
             colour = "#A9A9A9",linetype="longdash",size=0.8)+
  geom_point(aes(color=region,size=gdpreal2020/1000000000),alpha = .8,shape=21,stroke=0.8)+
  scale_size_continuous(breaks = c(1,10,100,1000,10000,100000),range= c(1,10),name="GDP / billion",
                        label=c("1","10","100","1000","10000","100000"))+
  geom_smooth(method = 'lm',formula = y ~ x, se = T,alpha=.3,color="#505050",fill="#A9A9A9",size=0.8)+
  stat_cor(gdps1, mapping = aes(x = gdploss20, y =gdploss21),method = "pearson",
           label.y.npc = 1,size=4)+
  labs(x="Decline in GDP growth rate in 2020(%)",y = "Recovery in GDP growth rate in 2021(%)")+
  theme_classic()+
  theme(axis.title= element_text(size=10,color="black"),
        axis.text= element_text(size=9,color="black"),
        legend.title = element_text(size=12),
        legend.text= element_text(size=12),
        legend.position = "right"
        )

                                                 
                                                                
setwd ("~/figure")

tiff("~.tif",width=2100,height=2100,res=600,compression = "lzw")
rc
dev.off()                                                                
