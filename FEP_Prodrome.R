install.packages("tidyverse")
install.packages("effectsize")
install.packages("readxl")
install.packages("MatchIt")
install.packages("rstatix")
install.packages("coin")

library(tidyverse)
library(effectsize)
library(readxl)
library(MatchIt)
library(rstatix)
library(coin)
library(gtsummary)

# Read in data
df <- read_csv("B:/BRC_CRIS/Dominic Oliver/MSc_2023/Maisie/First_Presentation_250624.csv")

# Make sure factors are treated as factors
df[,c(4:8,75)] <- lapply(df[,c(4:8,75)], factor)
df <- df %>% filter(!is.na(age_index))
df <- df %>% filter(oasis_only_accepted_ever_to_endofwindow==1 | fep_only_accepted_ever_to_endofwindow==1)
df$days <- difftime(df$index_new_date, as.Date.numeric(as.numeric(df$first_fep_only_accepted_date_ever_to_endofwindow),origin="1900-01-01"), units=c("days"))

# Propensity score match on the basis of age, gender, ethnicity, psychosis type and medication
match_df <- matchit(oasis_only_accepted_ever_to_endofwindow ~ age_index + Gender_ID + 
                      ethnicitycleaned + psychosis_type + ad_new_index + ms_new_index + 
                      anx_new_index + asy_new_index,
                    data=df, method="nearest", distance="glm",
                    ratio=1,
                    replace=FALSE)
summary(match_df)
matched_data <- match.data(match_df)
matched_data_tbl <- matched_data[,-1]

tbl1 <- tbl_summary(df, include=c(age_index, Gender_ID, ethnicitycleaned, psychosis_type,
                     ad_new_index, ms_new_index, asy_new_index),
              by=oasis_only_accepted_ever_to_endofwindow,
              type = list(age_index ~ "continuous2"),
              statistic = list(all_continuous() ~ c("{mean}", "{sd}")),
              digits = list(all_continuous() ~ c(1,1),
                            all_categorical() ~ c(0,1)))
tbl1 %>% as_gt() %>% gt::gtsave("B:/BRC_CRIS/Dominic Oliver/MSc_2023/table1.docx")

ggplot(data=matched_data, aes(x=oasis_only_accepted_ever_to_endofwindow, y=prodromal_period, group=oasis_only_accepted_ever_to_endofwindow)) +
  geom_violin(scale="width",trim=FALSE, aes(x=oasis_only_accepted_ever_to_endofwindow, colour=factor(oasis_only_accepted_ever_to_endofwindow), fill=factor(oasis_only_accepted_ever_to_endofwindow)), alpha=0.6) +
  geom_boxplot(aes(x=oasis_only_accepted_ever_to_endofwindow, group=factor(oasis_only_accepted_ever_to_endofwindow)),fill="white", colour="black", width=0.15, outlier.size=1, position=position_dodge(0.9)) + 
  stat_summary(aes(group=factor(oasis_only_accepted_ever_to_endofwindow)), fun=mean, geom="point", shape=1, size=4, color="black", position=position_dodge(0.9)) +
  scale_x_discrete(name="Group",labels=c("No OASIS", "OASIS"))+
  scale_y_continuous(name="Prodromal period (months)",breaks=seq(0, 108, 12)) +
  scale_fill_discrete(name="Group",labels=c("No OASIS", "OASIS"),type=c("skyblue","orange")) +
  scale_color_discrete(name="Group",labels=c("No OASIS", "OASIS"),type=c("skyblue","orange")) +
  theme_classic() +
  theme(text=element_text(size=20))
ggsave("B:/BRC_CRIS/Dominic Oliver/MSc_2023/Prodromal_period_270624.png", width=9, height=12)

df <- df %>% mutate(match = case_when(brcid_a %in% matched_data$brcid_a ~ 1,
                                      TRUE ~ 0))

# Create empty data frame for results for prodromal period
Output_prodromal_period <- data.frame(Variable = "prodromal_period",
                                      Mean_Non_OASIS = NA,
                                      Mean_OASIS = NA,
                                      SD_Non_OASIS = NA,
                                      SD_OASIS = NA,
                                      Median_Non_OASIS = NA,
                                      Median_OASIS = NA,
                                      IQR_Non_OASIS = NA,
                                      IQR_OASIS = NA,
                                      Test = NA,
                                      statistic = NA,
                                      Pval = NA,
                                      ES_Measure = NA,
                                      ES = NA,
                                      ES_CI = NA)

# Generate mean, standard deviation, median and interquartile range for prodrome duration and save into results data frame
temp_mean <- data.frame(aggregate(matched_data$prodromal_period ~ matched_data$oasis_only_accepted_ever_to_endofwindow, FUN=mean))
temp_sd <- data.frame(aggregate(matched_data$prodromal_period ~ matched_data$oasis_only_accepted_ever_to_endofwindow, FUN=sd))
temp_median <- data.frame(aggregate(matched_data$prodromal_period ~ matched_data$oasis_only_accepted_ever_to_endofwindow, FUN=median))
temp_IQR <- data.frame(aggregate(matched_data$prodromal_period ~ matched_data$oasis_only_accepted_ever_to_endofwindow, FUN=IQR))
Output_prodromal_period[1,2:3] <- temp_mean[1:2,2]
Output_prodromal_period[1,4:5] <- temp_sd[1:2,2]
Output_prodromal_period[1,6:7] <- temp_median[1:2,2]
Output_prodromal_period[1,8:9] <- temp_IQR[1:2,2]

# Check assumptions for t-test. Shapiro for normality and Levene's test for homogeneity of variance
shapiro <- as.data.frame(matched_data %>% group_by(oasis_only_accepted_ever_to_endofwindow) %>% shapiro_test(prodromal_period))
levene <- as.data.frame(matched_data %>% levene_test(prodromal_period ~ factor(oasis_only_accepted_ever_to_endofwindow)))

# If assumptions are met, run t-test and Cohen's d
if(shapiro$p[1]>=0.05 && shapiro$p[2]>=0.05 && levene$p>=0.05){
  t <- t.test(prodromal_period ~ oasis_only_accepted_ever_to_endofwindow, data=matched_data)
  Output_prodromal_period[1,10] <- "t-test"
  Output_prodromal_period[1,11] <- t$statistic
  Output_prodromal_period[1,12] <- t$p.value
  d <- (effectsize::cohens_d(prodromal_period ~ oasis_only_accepted_ever_to_endofwindow, data=matched_data))
  Output_prodromal_period[1,13] <- "Cohen's d"
  Output_prodromal_period[1,14] <- d$Cohens_d
  Output_prodromal_period[1,15] <- paste(d$CI_low,"-",d$CI_high)
  
  # If they are not, run Mann Whitney U test and Wilcoxon effect size (non-parametric equivalents)
} else {
  t <- wilcox.test(prodromal_period ~ oasis_only_accepted_ever_to_endofwindow, data=matched_data)
  Output_prodromal_period[1,10] <- "Mann Whitney U test test"
  Output_prodromal_period[1,11] <- t$statistic
  Output_prodromal_period[1,12] <- t$p.value
  d <- wilcox_effsize(prodromal_period ~ oasis_only_accepted_ever_to_endofwindow, data=matched_data, ci=TRUE)
  Output_prodromal_period[1,13] <- "Wilcoxon effect size (r)"
  Output_prodromal_period[1,14] <- d$effsize
  Output_prodromal_period[1,15] <- paste(d$conf.low,"-",d$conf.high)
}

# Create empty data frame for other results
Output <- data.frame(Variable = rep(NA,60),
                     Proportion_Non_OASIS = rep(NA,60),
                     Proportion_OASIS = rep(NA,60),
                     OR = rep(NA,60),
                     OR_CI = rep(NA,60),
                     Pval = rep(NA,60),
                     logOR = rep(NA,60),
                     SE = rep(NA,60))

# Run logistic regression and generate Odds ratios and 95% CIs
for(i in 14:(ncol(matched_data)-9)){
  temp <- matched_data[,c(75,i)]
  temp$temp <- unlist(temp[,2])
  n <- i-13
  Output$Variable[n] <- names(temp[,2])
  temp_mean <- data.frame(aggregate(temp$temp ~ temp$oasis_only_accepted_ever_to_endofwindow, FUN=mean))
  temp_prop <- temp_mean$temp.temp
  Output[n,2:3] <- round(temp_prop[1:2],2)
  if(temp_prop[1]>0 && temp_prop[2]>0){
    model <- glm(temp ~ oasis_only_accepted_ever_to_endofwindow, data=temp, family='binomial')
    Output[n,4] <- round(exp(coef(model))[2],2)
    Output[n,5] <- paste(round(exp(confint(model))[2,1],2),"-",round(exp(confint(model))[2,2]),2)
    Output[n,6] <- round(coef(summary(model))[2,4],2)
    Output[n,7] <- round((coef(model))[2],2)
    Output[n,8] <- round(summary(model)$coefficients[2,2],2)
  }
}
Output$Pval_corrected <- round(p.adjust(Output[,6],method="BH"),2) # Benjamini Hochberg correction for multiple comparisons
# Save out results
write.csv(Output_prodromal_period,"B:/BRC_CRIS/Dominic Oliver/MSc_2023/Maisie/Results_prodromal_period_270624.csv")
write.csv(Output,"B:/BRC_CRIS/Dominic Oliver/MSc_2023/Maisie/Results_280624.csv")

Output <- read_csv("B:/BRC_CRIS/Dominic Oliver/MSc_2023/Maisie/Results_240624.csv")

cluster <- Output %>% filter(Variable=='first_Positive_symptom_score'|Variable=='first_Negative_symptom_score'|Variable=='first_Depressive_symptom_score'|Variable=="first_Disorganised_symptom_score"|Variable=="first_Manic_symptom_score"|Variable=="first_Catatonic_symptom_score"|Variable=="first_Other_symptoms"|Variable=="first_Substance_score")
cluster <- cluster %>% mutate(name = case_when(Variable=='first_Positive_symptom_score' ~ "Positive",
                                               Variable=='first_Negative_symptom_score' ~ "Negative",
                                               Variable=='first_Depressive_symptom_score' ~ "Depressive",
                                               Variable=="first_Disorganised_symptom_score" ~ "Disorganised",
                                               Variable=="first_Manic_symptom_score" ~ "Manic",
                                               Variable=="first_Catatonic_symptom_score" ~ "Catatonic",
                                               Variable=="first_Other_symptoms" ~ "Other",
                                               Variable=="first_Substance_score"~ "Substance use"))
cluster$name <- factor(cluster$name,levels=c("Catatonic","Depressive","Disorganised","Manic","Negative","Positive","Substance use","Other" ))  
cluster <- cluster[order(cluster$name),]
cluster$name <- factor(cluster$name, levels=rev(levels(cluster$name)))

cluster %>% mutate(name = fct_reorder(name, Proportion_OASIS)) %>%
  ggplot()+
  geom_segment(aes(x=Proportion_Non_OASIS, xend=Proportion_OASIS,y = name, yend=name),colour="#aeb6bf", size=4.5, alpha=.5)+
  geom_point(aes(x=Proportion_Non_OASIS, y=name), color="skyblue", size=4)+
  geom_point(aes(x=Proportion_OASIS, y=name), color="orange", size=4)+
  scale_x_continuous(name="Proportion")+
  scale_y_discrete(name="Cluster") +
  theme_classic() +
  theme(text=element_text(size=20))

ggsave("B:/BRC_CRIS/Dominic Oliver/MSc_2023/Maisie/Proportions_Clusters_270624.png", width=9, height=12)

devtools::install_github("NightingaleHealth/ggforestplot")
library(ggforestplot)
ggforestplot::forestplot(
  df=cluster,
  name=name,
  estimate=logOR,
  se=SE,
  pvalue=Pval_corrected,
  psignif=0.05,
  logodds=TRUE,
  xlab = "Odds Ratio (OR)") 

ggsave("B:/BRC_CRIS/Dominic Oliver/MSc_2023/Maisie/Forest_OR_Clusters_270624.png", width=13, height=3)


feature <- Output %>% filter(Variable!='first_Positive_symptom_score'& Variable!='first_Negative_symptom_score'&Variable!='first_Depressive_symptom_score'& Variable!="first_Disorganised_symptom_score"&Variable!="first_Manic_symptom_score"&Variable!="first_Catatonic_symptom_score"&Variable!="first_Other_symptoms"&Variable!="first_Substance_score")
feature$Variable <- gsub("first_","",feature$Variable)
feature$Variable <- gsub("_"," ",feature$Variable)
feature$Variable <- gsub("_"," ",feature$Variable)
library(stringr)
feature$Variable <- str_to_sentence(feature$Variable)
feature <- feature %>% mutate(Variable = case_when(Variable=='Auditory hallucination' ~ "Hallucination (Auditory)",
                                                   Variable=='Visual hallucination' ~ "Hallucination (Visual)",
                                                   Variable=='Hallucination otg' ~ "Hallucination (OTG)",
                                                   Variable=="Weightloss" ~ "Weight loss",
                                                   Variable=="Diurnal" ~ "Diurnal mood",
                                                   Variable=="Delusion" ~ "Delusions",
                                                   Variable=="Persecutory" ~ "Persecutory delusions",
                                                   Variable=="Hopeless" ~ "Feeling hopeless",
                                                   Variable=="Helpless"~ "Feeling helpless",
                                                   Variable=="Worthless" ~ "Feeling worthless",
                                                   Variable=="Lonely"~ "Feeling lonely",
                                                   Variable=="Insight" ~ "Poor insight",
                                                   Variable=="Negative symptom"~ "Negative symptoms",
                                                   Variable=="Mdma" ~ "MDMA",
                                                   Variable=="Thought insert"~ "Thought insertion",
                                                   Variable=="Appetite"~ "Poor appetite",
                                                   Variable=="Emotional withdrawn"~ "Emotionally withdrawn",
                                                   Variable=="Early morning"~ "Early morning wakening",
                                                   Variable=="Nightmare"~ "Nightmares",
                                                   TRUE ~ Variable))

feature$Variable <- factor(feature$Variable)
feature$Variable <- factor(feature$Variable, levels=rev(levels(feature$Variable)))
feature$Variable <- fct_reorder(feature$Variable, feature$OR)
feature <- arrange(feature,-xtfrm(Variable))
feature %>% mutate(Variable = fct_reorder(Variable, Proportion_OASIS)) %>%
  ggplot()+
  geom_segment(aes(x=Proportion_Non_OASIS, xend=Proportion_OASIS,y = Variable, yend=Variable),colour="#aeb6bf", size=4.5, alpha=.5)+
  geom_point(aes(x=Proportion_Non_OASIS, y=Variable), color="skyblue", size=4)+
  geom_point(aes(x=Proportion_OASIS, y=Variable), color="orange", size=4)+
  scale_x_continuous(name="Proportion")+
  scale_y_discrete(name="Feature") +
  theme_classic() +
  theme(text=element_text(size=20))

ggsave("B:/BRC_CRIS/Dominic Oliver/MSc_2023/Maisie/Proportions_Features_270624.png", width=9, height=12)

library(ggforestplot)
ggforestplot::forestplot(
  df=feature,
  name=Variable,
  estimate=logOR,
  se=SE,
  pvalue=Pval_corrected,
  psignif=0.05,
  logodds=TRUE,
  xlim=c(0.1,15),
  xlab = "Odds Ratio (OR)") 

ggsave("B:/BRC_CRIS/Dominic Oliver/MSc_2023/Maisie/Forest_OR_Features_270624.png", width=9, height=12)
