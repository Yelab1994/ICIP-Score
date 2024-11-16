library(data.table)
library(tibble)
library(qs)
library(ComplexHeatmap)
library(circlize)
library(ggpubr)
library(survival)
library(dplyr)
library(ggplot2)
###Read the output risk scores.
rt <- readRDS("riskscore.rds")
rt <- rbind(rt$trainset_7,rt$trainset_3,rt$testset)
group $Risk <- ifelse(group$riskscore>0.996,"High","Low")
rt <- cbind(Risk=group$Risk,rt)
rt <- rt[rt$Risk=="High",]
###Identify key genes associated with poor prognosis in high-risk groups using univariate Cox regression
outTab=data.frame()
sigGenes=c("futime","fustat")
for(i in colnames(rt[,4:ncol(rt)])){
  dat <- rt[,c("futime","fustat","Type",i)]
  colnames(dat)[4] <- "gene"
  cox <- coxph(Surv(futime, fustat) ~., data = dat)
  coxSummary = summary(cox)
  coxP=coxSummary$coefficients["gene","Pr(>|z|)"]
  outTab=rbind(outTab,
               cbind(id=i,
                     HR=coxSummary$conf.int["gene","exp(coef)"],
                     HR.95L=coxSummary$conf.int["gene","lower .95"],
                     HR.95H=coxSummary$conf.int["gene","upper .95"],
                     pvalue=coxSummary$coefficients["gene","Pr(>|z|)"])
  )
}
outTab$pvalue <- as.numeric(outTab$pvalue)
outTab <- outTab[outTab$pvalue<0.001,]
##Read the rank of genes in CRISPR data.
rank <- readRDS("rnak.rds")
rank$id <- rownames(rank)
rank <- inner_join(rank,outTab,by="id")
rank <- rank[-1,]
###Draw the Rank plot
rank <- rank[1:20,]
rank$id <- factor(rank$id,levels = rank$id)
ggplot(rank, aes(x = id, y = order, group = 1)) +
  geom_line(color = "#1C4E80", size = 1) + 
  geom_point(color = "#E41A1C", size = 2.5) + 
  theme_bw() + 
  theme(
    legend.position = "none", 
    axis.text = element_text(face = "bold.italic", colour = "#441718", size = 12, hjust = 0.5),
    axis.line = element_blank(),
    plot.title = element_text(face = "bold.italic", colour = "#441718", size = 16, hjust = 0.5),
    title = element_text(face = "bold.italic", colour = "#441718", size = 13),
    panel.border = element_rect(fill = NA, color = "black", size = 1.5, linetype = "solid"),
    panel.background = element_rect(fill = "#F1F6FC"),
    panel.grid.major = element_line(color = "#CFD3D6", size = .5, linetype = "dotdash")
  ) +
  xlab("") +
  ylab("Order") +
  coord_flip() 
