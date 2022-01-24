library(ggplot2)
library(Metrics)
library(ggpubr)
library(ggpmisc)
library(lemon)


## The data for collecting the RMSE, nRMSE, bias, and R2
mdr = model_root_compiled_data

## The data for plotting the model and scan comparisons
mdrg = model_root_compiled_data_ggplot

## The data for plotting the scan bit depth comparisons
mds = scans_compiled_data_ggplot


##### Code for producing the figures 


## Defining the formula used for the fit plots 
formula <- y ~ x

## Making a list of the facet headers using the data headers
facet_labs = c(avg_diam = "Avg~Diameter~(mm)",l_length="Length >1 mm",m_length="Length 0.5-1.0 mm",s_length="Length 0.0-0.5 mm",total_length="Total Length (mm)",sa ="Surface Area (mm2)", vol = "Volume mm3")

## Making the facet label list usable within the facet_wrap() function

my_labeller <- as_labeller(c(avg_diam = "Avg~Diameter~(mm)",
                 l_length="Length >= 1~mm",
                 m_length="Length~0.5-1.0~mm",
                 s_length="Length~0.0-0.5~mm",
                 total_length="Total~Length~(mm)",
                 sa ="Surface~Area~(mm^2)", 
                 vol = "Volume~(mm^3)"),
                 default = label_parsed)

## Creating the figures 

modelscan.comp <- ggplot(data = mdrg, aes(x=true_value, y=est_value,color=platform,shape=platform))+
    geom_point(size = 3)+
    geom_smooth(method=lm, formula=formula, se = FALSE)+
    geom_abline(intercept=0,slope=1,size=1)+
    facet_wrap(nrow=4,ncol=2,~trait, scales="free",labeller = my_labeller)+
    labs(y="Estimated Value",x="True Value", color="platform",shape="platform")+
    theme_classic2()+
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste(..rr.label.., sep = "*`,`~")), 
                 parse = TRUE,
                 rr.digits = 2,
                 label.x.npc = "right", label.y.npc = "bottom",
                 vstep = 0.1)+
    theme(aspect.ratio=1,text= element_text(size = 13), legend.title=element_text(size=16),legend.text=element_text(size=13),axis.text.x= element_text(size=8),axis.text.y=element_text(size=8))

bitdepth.comp <- ggplot(data = mds, aes(x=true_value, y=est_value,color=platform,shape=platform))+
    geom_point(size = 3)+
    geom_smooth(method=lm, formula=formula, se = FALSE)+
    geom_abline(intercept=0,slope=1,size=1)+
    facet_wrap(nrow=4,ncol=2,~trait, scales="free",labeller = my_labeller)+
    labs(y="Estimated Value",x="True Value", color="Bit depth",shape="Bit depth")+
    theme_classic2()+
    stat_poly_eq(formula = y ~ x, 
                 aes(label = paste(..rr.label.., sep = "*`,`~")), 
                 parse = TRUE,
                 rr.digits = 2,
                 label.x.npc = "right", label.y.npc = "bottom",
                 vstep = 0.1)+
    theme(aspect.ratio=1,text= element_text(size = 13), legend.title=element_text(size=16),legend.text=element_text(size=13),axis.text.x= element_text(size=8),axis.text.y=element_text(size=8))

## Checking where the empty facet is

gtable_show_names(modelscan.comp)
gtable_show_names(bitdepth.comp)

## Repositioning the legend into the empty space

reposition_legend(modelscan.comp, 'center', panel='panel-2-4')
reposition_legend(bitdepth.comp, 'center', panel='panel-2-4')
    
    
##### RMSE, nRMSE, Bias, and R2 for total length

# Calculating the RMSE and nRMSE for the scanner and table

rmse(mdr$true_total_length,mdr$scan_total_length)
rmse(mdr$true_total_length,mdr$scan_total_length)/mean(mdr$scan_total_length)

rmse(mdr$true_total_length,mdr$table_total_length)
rmse(mdr$true_total_length,mdr$table_total_length)/mean(mdr$table_total_length)

# Bias for the scanner and table

bias(mdr$true_total_length,mdr$scan_total_length)
bias(mdr$true_total_length,mdr$table_total_length)

## R2 for the fit of the scanner and table image lengths to the true length

scan.length.lm <- lm(true_total_length ~ scan_total_length, mdr)
summary(scan.length.lm)

table.length.lm <- lm(true_total_length ~ table_total_length, mdr)
summary(table.length.lm)


## Average diameter
# Calculations are done in the same way and order as for total length

rmse(mdr$true_avg_diam,mdr$scan_avg_diam)
rmse(mdr$true_avg_diam,mdr$scan_avg_diam)/mean(mdr$scan_avg_diam)

rmse(mdr$true_avg_diam,mdr$table_avg_diam)
rmse(mdr$true_avg_diam,mdr$table_avg_diam)/mean(mdr$table_avg_diam)

bias(mdr$true_avg_diam,mdr$scan_avg_diam)
bias(mdr$true_avg_diam,mdr$table_avg_diam)

scan.diam.lm <- lm(true_avg_diam ~ scan_avg_diam, mdr)
summary(scan.diam.lm)

table.diam.lm <- lm(true_avg_diam ~ table_avg_diam, mdr)
summary(table.diam.lm)

## 0.3 diameter length

rmse(mdr$true_0.3_length,mdr$scan_0.3_length)
rmse(mdr$true_0.3_length,mdr$scan_0.3_length)/mean(mdr$scan_0.3_length)

rmse(mdr$true_0.3_length,mdr$table_0.3_length)
rmse(mdr$true_0.3_length,mdr$table_0.3_length)/mean(mdr$table_0.3_length)

bias(mdr$true_0.3_length,mdr$scan_0.3_length)
bias(mdr$true_0.3_length,mdr$table_0.3_length)

scan.s.lm <- lm(true_0.3_length ~ scan_0.3_length, mdr)
summary(scan.s.lm)

table.s.lm <- lm(true_s_length ~ table_s_length, mdr)
summary(table.s.lm)

## 0.7 diameter length

rmse(mdr$true_0.7_length,mdr$scan_0.7_length)
rmse(mdr$true_0.7_length,mdr$scan_0.7_length)/mean(mdr$scan_0.7_length)

rmse(mdr$true_0.7_length,mdr$table_0.7_length)
rmse(mdr$true_0.7_length,mdr$table_0.7_length)/mean(mdr$table_0.7_length)

bias(mdr$true_0.7_length,mdr$scan_0.7_length)
bias(mdr$true_0.7_length,mdr$table_0.7_length)

scan.m.lm <- lm(true_0.7_length ~ scan_0.7_length, mdr)
summary(scan.m.lm)

table.m.lm <- lm(true_0.7_length ~ table_0.7_length, mdr)
summary(table.m.lm)

## Same stuff for 1.2

rmse(mdr$true_1.2_length,mdr$scan_1.2_length)
rmse(mdr$true_1.2_length,mdr$scan_1.2_length)/mean(mdr$scan_1.2_length)

rmse(mdr$true_1.2_length,mdr$table_1.2_length)
rmse(mdr$true_1.2_length,mdr$table_1.2_length)/mean(mdr$table_1.2_length)

bias(mdr$true_1.2_length,mdr$scan_1.2_length)
bias(mdr$true_1.2_length,mdr$table_1.2_length)

scan.l.lm <- lm(true_1.2_length ~ scan_1.2_length, mdr)
summary(scan.l.lm)

table.l.lm <- lm(true_1.2_length ~ table_1.2_length, mdr)
summary(table.l.lm)


## Surface Area

rmse(mdr$true_total_sa,mdr$scan_total_sa)
rmse(mdr$true_total_sa,mdr$scan_total_sa)/mean(mdr$scan_total_sa)

rmse(mdr$true_total_sa,mdr$table_total_sa)
rmse(mdr$true_total_sa,mdr$table_total_sa)/mean(mdr$table_total_sa)

bias(mdr$true_total_sa,mdr$scan_total_sa)
bias(mdr$true_total_sa,mdr$table_total_sa)

scan.sa.lm <- lm(true_total_sa ~ scan_total_sa, mdr)
summary(scan.sa.lm)

table.sa.lm <- lm(true_total_sa ~ table_total_sa, mdr)
summary(table.sa.lm)


## Volume

rmse(mdr$true_total_vol,mdr$scan_total_vol)
rmse(mdr$true_total_vol,mdr$scan_total_vol)/mean(mdr$scan_total_vol)

rmse(mdr$true_total_vol,mdr$table_total_vol)
rmse(mdr$true_total_vol,mdr$table_total_vol)/mean(mdr$table_total_vol)

bias(mdr$true_total_vol,mdr$scan_total_vol)
bias(mdr$true_total_vol,mdr$table_total_vol)

scan.vol.lm <- lm(true_total_vol ~ scan_total_vol, mdr)
summary(scan.vol.lm)

table.vol.lm <- lm(true_total_vol ~ table_total_vol, mdr)
summary(table.vol.lm)
