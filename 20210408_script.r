#I. Package loading and Setup ----------------------------------------------------------
rm(list=ls())

a <- installed.packages()
pkgs <- rownames(a)

#Packages used in today's examples:
if("dplyr" %in% pkgs == FALSE){
  install.packages("dplyr")
}
if("stringr" %in% pkgs == FALSE){
  install.packages("stringr")
}
if("ggplot2" %in% pkgs == FALSE){
  install.packages("ggplot2")
}
if("ggExtra" %in% pkgs == FALSE){
  install.packages("ggExtra")
}
if("directlabels" %in% pkgs == FALSE){
  install.packages("directlabels")
}
if("data.table" %in% pkgs == FALSE){
  install.packages("data.table")
}
if("ROCR" %in% pkgs == FALSE){
  install.packages("ROCR")
}
if("forcats" %in% pkgs == FALSE){
  install.packages("forcats")
}
if("vip" %in% pkgs == FALSE) {
  install.packages("vip")
}
if("regclass" %in% pkgs == FALSE) {
  install.packages("regclass")
}
if("GGally" %in% pkgs == FALSE) {
	install.packages("GGally")
}
if("gridExtra" %in% pkgs == FALSE) {
	install.packages("gridExtra")
}
if("grid" %in% pkgs == FALSE) {
	install.packages("gridExtra")
}

rm(a,pkgs)

require(dplyr)
require(stringr)
require(ggplot2)
require(ggExtra)
require(directlabels)
require(data.table)
require(ROCR)
require(forcats)
require(vip)
require(regclass)
require(GGally)
require(grid)
require(gridExtra)

#Themes 	
#Set Graphing Options
out_theme <- theme_bw() + 
  theme(text=element_text(family="ArialMT"), 
        legend.position="bottom",
        plot.title = element_text(size = rel(2)), 
        axis.text.x = element_text(size= rel(1)), 
        axis.text.y = element_text(size= rel(1)))

color_scheme <- c("#778899", "#C90E17", "#001933", "#08519c", "#6495ED", "#B0C4DE", 
                      "#999999", "#000000", "#800000", "#B23232", 
                      "#691b14")    

#Set Options
options(scipen=999999)

#Set Working Directory
setwd("./")

#II. Part 1 - Plotting --------------------------------------------------
##A. Cars Plot ----------------------------------------------------------
###1. Data Loading and Processing ---------------------------------------
cars <- mtcars #System data
cars$cyl <- factor(cars$cyl, levels = sort(unique(cars$cyl)), ordered = TRUE)
cars$disp_size<- with(cars, ifelse(disp<=150, 'Small', ifelse(disp>120 & disp<=300, 'Medium', 'Large')))

###2. Cars Plot ---------------------------------------------------------
  p <- ggplot(data = cars, aes(x = wt, y = mpg))
  p <- p + geom_point(aes(colour = cyl))
  p <- p + scale_color_manual(values = color_scheme[c(1,2,5)])
  p <- p + scale_y_continuous(limits = c(0,40),
                                    breaks = seq(0,40,5))  #comment on what happens when breaks/labels are set (perhaps beyond limits) and limits not set
  p <- p + labs(title="Impact of Weight on MPG",
                      subtitle = "32 car selection",
                      x = "Weight",
                      y = "Miles Per Gallon",
                      color = "Cylinders",
                      caption = "Notes: The data used in this analysis is from the mtcars dataset.")
  p <- p + out_theme
  p <- p + theme(panel.grid.minor = element_blank(),
                       plot.caption = element_text(hjust=0),
                       plot.margin=unit(c(2,2,2,2),"cm")) #top, right, bottom, left, none
  p  
  
###3. Cars Plot with Facet ----------------------------------------------
  p1 <- ggplot(data = cars, aes(x = wt, y = mpg))
  p1 <- p1 + geom_point(aes(colour = cyl))
  p1 <- p1 + scale_color_manual(values = color_scheme[c(1,2,5)])
  p1 <- p1 + scale_y_continuous(limits = c(0,40),
                                    breaks = seq(0,40,5))  #comment on what happens when breaks/labels are set (perhaps beyond limits) and limits not set
  p1 <- p1 + labs(title="Impact of Weight on MPG",
                      subtitle = "32 car selection",
                      x = "Weight",
                      y = "Miles Per Gallon",
                      color = "Cylinders",
                      caption = "Notes: The data used in this analysis is from the mtcars dataset.")
  p1 <- p1 + out_theme
  p1 <- p1 + theme(panel.grid.minor = element_blank(),
                       plot.caption = element_text(hjust=0),
                       plot.margin=unit(c(2,2,2,2),"cm")) #top, right, bottom, left, none
  p1+facet_wrap(~disp_size) 
  p1+facet_grid(cyl~disp_size) 

##B. Miller D Plot -----------------------------------------------------
###1. Data Loading and Processing --------------------------------------
mill_d<-read.csv("https://raw.githubusercontent.com/jjghockey/DataFest2021/main/millerD.csv")
indexes<-read.csv("https://raw.githubusercontent.com/jjghockey/DataFest2021/main/indexes.csv")

  mill_d <- mill_d %>% 
    group_by(ticker) %>% 
    arrange(date) %>% 
    mutate(pct_change = px_last/first(px_last),
           ticker= "Miller-D") %>% #When we add new data, all x/y must match exactly
    ungroup()
  
  indexes <- indexes %>% 
    filter(ticker %in% c("NYSE","Nasdaq","SP_PREF")) %>%
    filter(date >= as.Date("2013-10-01") & date <= as.Date("2016-03-29")) %>% 
    group_by(ticker) %>% 
    arrange(date) %>% 
    mutate(pct_change = px_last/first(px_last)) %>% 
    ungroup()
	
  miller <- bind_rows(indexes,mill_d) %>% mutate(ticker = paste0(ticker, "   "))
  miller$ticker <- factor(miller$ticker, levels = sort(unique(miller$ticker)),ordered = TRUE)	
  miller$date<-as.Date(miller$date)
  mill_d$date<-as.Date(mill_d$date)

###2. Miller D Plot ------------------------------------------------------
  p2 <- ggplot(data = miller, aes(x=date, y = pct_change, colour = ticker))
  p2 <- p2+geom_line(size=1)
  p2 <- p2+scale_color_manual(values=color_scheme[c(2,1,3,4)])
  p2 <- p2+geom_dl(aes(label = ticker), method = list(dl.combine("last.points"), cex = 1,"last.bumpup"))
  p2 <- p2+scale_x_date(limits=as.Date(c("2013-01-01","2018-01-01")),
                        breaks = seq.Date(as.Date("2013-01-01"),as.Date("2017-01-01"), by = "1 year"),
                        date_labels = "%Y")
  p2 <- p2+scale_y_continuous(labels=scales::percent, 
                              breaks = seq(0,2,.25))
  p2 <- p2+out_theme
  p2 <- p2 + theme(panel.grid.minor = element_blank(),
                       plot.title = element_text(hjust=.5),
                       plot.margin=unit(c(2,2,2,2),"cm")) 
  p2 <- p2 + labs(title="Miller Series D Percent Change in Price\n Since 10/1/2013\n",
                  x="Date",
                  y="Percent Change",
                  color="Ticker:  ",
				  caption = "Red shading indicates period before disclosure.")
  p2 <- p2+geom_vline(xintercept = as.Date("2015-07-31")) 
  p2 <- p2+annotate("text",x = as.Date("2015-07-31"), y = .75, label = "  7/31/2015: Delisted from NYSE", hjust = 0, vjust=0, angle=0)
  p2 <- p2+theme(legend.position = "none",
                 panel.border = element_blank(),
                 axis.line = element_line(color = "black"))
  p2 <- p2+annotate("rect",xmin=as.Date("2013-10-01"),
                     xmax=as.Date("2014-09-22"),
                     ymin=-Inf,
                     ymax=Inf,
                     fill="#FF000033")
  p2

#III. Part 2 -------------------------------------------------------------
##A. Machine Learning Simple Exercise ------------------------------------
###1. Data Loading and Processing ----------------------------------------
cc<-fread("https://raw.githubusercontent.com/jjghockey/DataFest2021/main/UCI_Credit_Card.csv") %>% as.data.table()

cc[SEX==1, sex:="male"]
cc[SEX==2, sex:="female"]
cc[, SEX:=NULL]
cc[, SEX:=as.factor(sex)]
cc[, sex:=NULL]

cc[EDUCATION==1, EDUC:="Grad"]
cc[EDUCATION==2, EDUC:="University"]
cc[EDUCATION==3, EDUC:="High School"]
cc[EDUCATION>=4, EDUC:="Other"]
cc[EDUCATION==0, EDUC:="Other"]
cc[, EDUC:=as.factor(EDUC)]

cc[MARRIAGE==1, MAR:="Married"]
cc[MARRIAGE==2, MAR:="Single"]
cc[MARRIAGE==3, MAR:="Other"]
cc[is.na(MARRIAGE)==TRUE, MAR:="Other"]
cc[MARRIAGE==0, MAR:="Other"]
cc[, MAR:=as.factor(MAR)]

cc[default.payment.next.month==1, DEF:="Yes"]
cc[default.payment.next.month==0, DEF:="No"]
cc[, DEF:=as.factor(DEF)]

cc[, existing_balance:=PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6-(BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6)]

cc[, avg_payment:=(PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6)/6]

cc[, avg_bill:=(BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6)/6]

cc[, total_spent:=BILL_AMT1+BILL_AMT2+BILL_AMT3+BILL_AMT4+BILL_AMT5+BILL_AMT6]

cc[, total_paid:=PAY_AMT1+PAY_AMT2+PAY_AMT3+PAY_AMT4+PAY_AMT5+PAY_AMT6]

cc[, throughput:=abs(PAY_AMT1)+abs(PAY_AMT2)+abs(PAY_AMT3)+abs(PAY_AMT4)+abs(PAY_AMT5)+abs(PAY_AMT6)+abs(BILL_AMT1)+abs(BILL_AMT2)+abs(BILL_AMT3)+abs(BILL_AMT4)+abs(BILL_AMT5)+abs(BILL_AMT6)]

cc[, bill_ratio:=total_spent/LIMIT_BAL]
cc[total_spent!=0, paid_spent_ratio:=total_paid/total_spent]
cc[total_spent==0, paid_spent_ratio:=0]

cc[, avg_pay:=(PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6)/6]
cc[, sum_pay:=(PAY_0+PAY_2+PAY_3+PAY_4+PAY_5+PAY_6)]

cc[, flg1:=0]
cc[, flg2:=0]
cc[, flg3:=0]
cc[, flg4:=0]
cc[, flg5:=0]
cc[, flg6:=0]

cc[PAY_0<0, flg1:=1]
cc[PAY_2<0, flg2:=1]
cc[PAY_3<0, flg3:=1]
cc[PAY_4<0, flg4:=1]
cc[PAY_5<0, flg5:=1]
cc[PAY_6<0, flg6:=1]

cc[, num_timely:=flg1+flg2+flg3+flg4+flg5+flg6]

cc[, BAL:=	ifelse(LIMIT_BAL<=10000, "<=10000", 
			ifelse(LIMIT_BAL>10000 & LIMIT_BAL<=100000, "010001 to 100000",
			ifelse(LIMIT_BAL>100000 & LIMIT_BAL<=200000, "100001 to 200000",
			ifelse(LIMIT_BAL>200000 & LIMIT_BAL<=300000, "200001 to 300000",
			ifelse(LIMIT_BAL>300000 & LIMIT_BAL<=400000, "300001 to 400000",
			ifelse(LIMIT_BAL>400000 & LIMIT_BAL<=500000, "400001 to 500000",
			ifelse(LIMIT_BAL>500000 & LIMIT_BAL<=600000, "500001 to 600000",
			ifelse(LIMIT_BAL>600000 & LIMIT_BAL<=700000, "600001 to 700000",
			ifelse(LIMIT_BAL>700000 & LIMIT_BAL<=800000, "700001 to 800000",					
			ifelse(LIMIT_BAL>800000 & LIMIT_BAL<=900000, "800001 to 900000",					
			ifelse(LIMIT_BAL>900000 & LIMIT_BAL<=1000000, "900001 to 1000000",					
			ifelse(LIMIT_BAL>1000000, "1000001+",NA
			))))))))))))
	]
	
cc[, TP:=	ifelse(throughput<=10000, "<=10000", 
			ifelse(throughput>10000 & throughput<=100000, "010001 to 100000",
			ifelse(throughput>100000 & throughput<=200000, "100001 to 200000",
			ifelse(throughput>200000 & throughput<=300000, "200001 to 300000",
			ifelse(throughput>300000 & throughput<=400000, "300001 to 400000",
			ifelse(throughput>400000 & throughput<=500000, "400001 to 500000",
			ifelse(throughput>500000 & throughput<=600000, "500001 to 600000",
			ifelse(throughput>600000 & throughput<=700000, "600001 to 700000",
			ifelse(throughput>700000 & throughput<=800000, "700001 to 800000",					
			ifelse(throughput>800000 & throughput<=900000, "800001 to 900000",					
			ifelse(throughput>900000 & throughput<=1000000, "900001 to 1000000",					
			ifelse(throughput>1000000, "1000001+",NA
			))))))))))))
	]	

#B. Training/Test 
set.seed(1) #Makes the training/test set selection reproducible
tst<-dplyr::sample_n(cc, size=nrow(cc)*0.30) %>% as.data.table()  #30% Sample for testing
tst[, m:="tst"]
cc[, m:="cc"]
trn<-merge(cc, tst[, .(ID, m)], by=c("ID"), all=TRUE)
trn[, .N, by=list(m.x, m.y)]
   # m.x  m.y     N
# 1:  cc <NA> 24000
# 2:  cc  tst  6000
trn<-trn[is.na(m.y)==TRUE, ] #70% for Training

	#Age and Default
	tbl<-trn[, .N, by=list(AGE, DEF)]
	tbl<-tbl[, tot:=sum(N), by=AGE][, pct:=N/tot]
	setkey(tbl, AGE, DEF)
	plt<-ggplot(tbl, aes(AGE, pct, fill=DEF))+geom_bar(stat="identity", position="stack")
	plt<-plt+theme_bw()
	plt<-plt+scale_fill_manual(values=c('#6495ED','#800000'))
	plt<-plt+labs(colour = "Default", fill="Default", x="Age", y="%")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))
	graph1<-plt
		
	#Marriage and Default
	tbl<-trn[, .N, by=list(MAR, DEF)]
	tbl<-tbl[, tot:=sum(N), by=MAR][, pct:=N/tot]
	tbl<-tbl[, MAR:=fct_reorder(MAR, pct)]
	setkey(tbl, MAR, DEF)
	plt<-ggplot(tbl, aes(MAR, pct, fill=DEF))+geom_bar(stat="identity", position="stack")
	plt<-plt+theme_bw()
	plt<-plt+scale_fill_manual(values=c('#6495ED','#800000'))
	plt<-plt+labs(colour = "Default", fill="Default", x="Marriage", y="%")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))
	graph2<-plt
		
	#Education and Default
	tbl<-trn[, .N, by=list(EDUC, DEF)]
	tbl<-tbl[, tot:=sum(N), by=EDUC][, pct:=N/tot]
	tbl<-tbl[, EDUC:=fct_reorder(EDUC, pct)]
	setkey(tbl, EDUC, DEF)
	plt<-ggplot(tbl, aes(EDUC, pct, fill=DEF))+geom_bar(stat="identity", position="stack")
	plt<-plt+theme_bw()
	plt<-plt+scale_fill_manual(values=c('#6495ED','#800000'))
	plt<-plt+labs(colour = "Default", fill="Default", x="Education", y="%")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))
	graph3<-plt
	
	#Number of Timely Payments over 6 months
	tbl<-trn[, .N, by=list(num_timely, DEF)]
	tbl[, num_timely:=as.factor(num_timely)]
	tbl<-tbl[, tot:=sum(N), by=num_timely][, pct:=N/tot]
	tbl<-tbl[, num_timely:=fct_reorder(num_timely, pct)]
	setkey(tbl, num_timely, DEF)
	plt<-ggplot(tbl, aes(num_timely, pct, fill=DEF))+geom_bar(stat="identity", position="stack")
	plt<-plt+theme_bw()
	plt<-plt+scale_fill_manual(values=c('#6495ED','#800000'))
	plt<-plt+labs(colour = "Default", fill="Default", x="# of Timely Payments", y="%")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))
	graph4<-plt
		
	#Total Balance
	tbl<-trn[, .N, by=list(BAL, DEF)]
	tbl[, BAL:=as.factor(BAL)]
	tbl<-tbl[, tot:=sum(N), by=BAL][, pct:=N/tot]
	setkey(tbl, BAL, DEF)
	plt<-ggplot(tbl, aes(BAL, pct, fill=DEF))+geom_bar(stat="identity", position="stack")
	plt<-plt+theme_bw()
	plt<-plt+scale_fill_manual(values=c('#6495ED','#800000'))
	plt<-plt+labs(colour = "Default", fill="Default", x="Total Balance", y="%")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))
	graph5<-plt+coord_flip()		
		
	#Gender
	tbl<-trn[, .N, by=list(SEX, DEF)]
	tbl[, SEX:=as.factor(SEX)]
	tbl<-tbl[, tot:=sum(N), by=SEX][, pct:=N/tot]
	tbl<-tbl[, SEX:=fct_reorder(SEX, pct)]
	setkey(tbl, SEX, DEF)
	plt<-ggplot(tbl, aes(SEX, pct, fill=DEF))+geom_bar(stat="identity", position="stack")
	plt<-plt+theme_bw()
	plt<-plt+scale_fill_manual(values=c('#6495ED','#800000'))
	plt<-plt+labs(colour = "Default", fill="Default", x="Sex", y="%")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))
	graph6<-plt		
	
	#Average Payment Status
	tbl<-trn[, .N, by=list(avg_pay=round(avg_pay,0), DEF)]
	tbl[, avg_pay:=as.factor(avg_pay)]
	tbl<-tbl[, tot:=sum(N), by=avg_pay][, pct:=N/tot]
	tbl<-tbl[, avg_pay:=fct_reorder(avg_pay, pct)]
	setkey(tbl, avg_pay, DEF)	
	plt<-ggplot(tbl, aes(avg_pay, pct, fill=DEF))+geom_bar(stat="identity", position="stack")
	plt<-plt+theme_bw()
	plt<-plt+scale_fill_manual(values=c('#6495ED','#800000'))
	plt<-plt+labs(colour = "Default", fill="Default", x="Average Payment Category over 6 months", y="%")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))
	graph7<-plt		
	
	#Total throughput
	tbl<-trn[, .N, by=list(TP, DEF)]
	tbl[, TP:=as.factor(TP)]
	tbl<-tbl[, tot:=sum(N), by=TP][, pct:=N/tot]
	tbl<-tbl[, TP:=factor(TP, levels=c("<=10000", "010001 to 100000","100001 to 200000","200001 to 300000","300001 to 400000","400001 to 500000","500001 to 600000","600001 to 700000","700001 to 800000","800001 to 900000","900001 to 1000000","1000001+"))]
	setkey(tbl, TP, DEF)
	plt<-ggplot(tbl, aes(TP, pct, fill=DEF))+geom_bar(stat="identity", position="stack")
	plt<-plt+theme_bw()
	plt<-plt+scale_fill_manual(values=c('#6495ED','#800000'))
	plt<-plt+labs(colour = "Default", fill="Default", x="Throughput", y="%")
	plt<-plt+theme(legend.position="bottom")
	plt<-plt+theme(panel.grid.major = element_line(color="white"))
	graph8<-plt+coord_flip()	

#ggsave(file="./plot1.png", grid.arrange(graph1,graph2,graph3,graph6, ncol=2, top=textGrob("Demographics and Default", gp=gpar(fontsize=15))), height=8, width=11)
#ggsave(file="./plot2.png", grid.arrange(graph4,graph5,graph7,graph8, ncol=2, top=textGrob("Payments and Default", gp=gpar(fontsize=15))), height=8, width=11)

#E. Modeling
mod1<-glm(default.payment.next.month~LIMIT_BAL+num_timely+avg_pay+sum_pay+avg_payment+avg_bill+SEX+EDUC+MAR+AGE+throughput+bill_ratio+paid_spent_ratio, family="binomial", data=trn)

var_imp<-vip::vi_model(mod1) %>% as.data.table()
var_imp<-var_imp[order(-Importance)]
var_imp[Sign=="NEG", Importance:=Importance* -1]
var_imp[, Variable:=fct_reorder(Variable, Importance)]
var_imp

           # Variable Importance Sign
 # 1:         avg_pay 38.1165514  POS
 # 2:      num_timely 23.9596661  POS
 # 3:     avg_payment -6.3341372  NEG
 # 4:       LIMIT_BAL -6.2453113  NEG
 # 5:       EDUCOther -3.9360986  NEG
 # 6:       MARSingle -3.9358990  NEG
 # 7:      throughput  3.3524408  POS
 # 8:        avg_bill -3.1670122  NEG
 # 9:      bill_ratio  3.1033397  POS
# 10:         SEXmale  2.8733023  POS
# 11:             AGE  1.6006443  POS
# 12:        MAROther -1.0078689  NEG
# 13: EDUCHigh School -0.2590258  NEG
# 14:  EDUCUniversity -0.1681679  NEG

mod2<-glm(default.payment.next.month~LIMIT_BAL+num_timely+avg_pay+avg_payment, family="binomial", data=trn)
anova(mod2, mod1)

# Analysis of Deviance Table

# Model 1: default.payment.next.month ~ LIMIT_BAL + num_timely + avg_pay + 
    # avg_payment
# Model 2: default.payment.next.month ~ LIMIT_BAL + num_timely + avg_pay + 
    # sum_pay + avg_payment + avg_bill + SEX + EDUC + MAR + AGE + 
    # throughput + bill_ratio + paid_spent_ratio
  # Resid. Df Resid. Dev Df Deviance
# 1     20995      19590            
# 2     20984      19451 11   138.92

#F. Validation
pred1<-predict(mod2, newdata=trn, type="response")
pred<-prediction(pred1, labels=as.data.frame(trn$default.payment.next.month))
perf1<-performance(pred,"tpr", "fpr")
perf1<-data.table(fpr=perf1@x.values[[1]], tpr=perf1@y.values[[1]])
auc<-performance(pred, "auc")
auc<-round(auc@y.values[[1]],2)

mod1_plt1<-ggplot(perf1, aes(x=fpr, y=tpr))+geom_line()+out_theme
mod1_plt1<-mod1_plt1+labs(x="False Positive Rate", y="True Positive Rate", title="ROCR - Training Set", caption="Red diagonal represents a random guess")
mod1_plt1<-mod1_plt1+geom_abline(intercept=0, color="red")
mod1_plt1<-mod1_plt1+annotate("text", x=0.90, y=0.10, label=paste("AUC: ", auc*100, "%"))
conf1<-confusion_matrix(mod2, DATA=trn)

plt<-ggplot(var_imp, aes(x=Variable, y=Importance, fill=Sign))+geom_bar(stat="identity", position="stack")
plt<-plt+theme_bw()
plt<-plt+scale_fill_manual(values=c('#800000', '#6495ED'))
plt<-plt+labs(colour = "Variable", fill="Default", y="Variable Importance", x="Importance")
plt<-plt+theme(legend.position="bottom")
plt<-plt+theme(panel.grid.major = element_line(color="white"))
var_plot<-plt+coord_flip()	

write.csv(file="./var_imp.csv", var_imp)
write.csv(file="./conf1.csv", conf1)
ggsave(file="./rocr1.png", mod1_plt1, height=8, width=11)
ggsave(file="./var_imp1.png", var_plot, height=8, width=11)

#G. Prediction
pred1<-predict(mod2, newdata=tst, type="response")
pred<-prediction(pred1, labels=as.data.frame(tst$default.payment.next.month))
perf1<-performance(pred,"tpr", "fpr")
perf1<-data.table(fpr=perf1@x.values[[1]], tpr=perf1@y.values[[1]])
auc<-performance(pred, "auc")
auc<-round(auc@y.values[[1]],2)

mod1_plt2<-ggplot(perf1, aes(x=fpr, y=tpr))+geom_line()+out_theme
mod1_plt2<-mod1_plt2+labs(x="False Positive Rate", y="True Positive Rate", title="ROCR - Testing Set", caption="Red diagonal represents a random guess")
mod1_plt2<-mod1_plt2+geom_abline(intercept=0, color="red")
mod1_plt2<-mod1_plt2+annotate("text", x=0.90, y=0.10, label=paste("AUC: ", auc*100, "%"))
conf2<-confusion_matrix(mod2, DATA=tst)

write.csv(file="./conf2.csv", conf2)
ggsave(file="./rocr2.png", mod1_plt2, height=8, width=11)


