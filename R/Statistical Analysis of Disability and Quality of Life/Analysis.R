library(readstata13)
library(tidyverse)
library(lme4)
install.packages("readstate13")



Rdata <- read.dta13('disability-data.dta', nonint.factors = F)

# define colors 

colors = c('Depression'='#008000',
           'Dementia' = '#0000ff', 
           'Psychosis' = '#ff7f00',
           'Epilepsy' = '#ff0000',
           'Suicide' = '#800080',
           'Other complaints' = '#000000')


subset(data,diagnosis1 == 'Suicide')$id

# Disability models

m.dep <- lmer(Disability ~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id), 
     data = subset(data,diagnosis1 == 'Depression'), 
     REML = F)

m.psy <- lmer(Disability ~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id), 
           data = subset(data,diagnosis1 == 'Psychosis'), 
           REML = F)

m.ep <- lmer(Disability ~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id), 
           data = subset(data,diagnosis1 == 'Epilepsy'), 
           REML = F)

m.suic <- lmer(Disability ~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id), 
           data = subset(data,diagnosis1 == 'Suicide'), 
           REML = F)

m.oth <- lmer(Disability ~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id), 
           data = subset(data,diagnosis1 == "Other mental health complaints"), 
           REML = F)

m.dem <- lmer(Disability ~ Time + age  + Marital_status + Education + occupation2  + (1|id), 
           data = subset(data,diagnosis1 == 'Dementia'), 
           REML = F)



#extract CIs

m.dem.ci <- data.frame(confint(m.dem)[c('Time', 'age'),])
m.dep.ci <- data.frame(confint(m.dep)[c('Time', 'age', 'SexMale'),])
m.ep.ci <- data.frame(confint(m.ep)[c('Time', 'age', 'SexMale'),])
m.suic.ci <- data.frame(confint(m.suic)[c('Time', 'age', 'SexMale'),])
m.psy.ci <- data.frame(confint(m.psy)[c('Time', 'age', 'SexMale'),])
m.oth.ci <- data.frame(confint(m.oth)[c('Time', 'age', 'SexMale'),])


# Create diagnosis variable
m.dem.ci$Diagnosis <- 'Dementia'
m.dep.ci$Diagnosis <- 'Depression'
m.psy.ci$Diagnosis <- 'Psychosis'
m.ep.ci$Diagnosis <- 'Epilepsy'
m.suic.ci$Diagnosis <- 'Suicide'
m.oth.ci$Diagnosis <- 'Other complaints'


# extract estimats
m.dem.ci$Estim <- fixef(m.dem)[c('Time', 'age')]
m.dep.ci$Estim <- fixef(m.dep)[c('Time', 'age', 'SexMale')]
m.psy.ci$Estim <- fixef(m.psy)[c('Time', 'age', 'SexMale')]
m.ep.ci$Estim <- fixef(m.ep)[c('Time', 'age', 'SexMale')]
m.suic.ci$Estim <- fixef(m.suic)[c('Time', 'age', 'SexMale')]
m.oth.ci$Estim <- fixef(m.oth)[c('Time', 'age', 'SexMale')]

m.dem.ci$variab <- rownames(m.dem.ci)
m.dep.ci$variab <- rownames(m.dep.ci)
m.ep.ci$variab <- rownames(m.ep.ci)
m.psy.ci$variab <- rownames(m.psy.ci)
m.oth.ci$variab <- rownames(m.oth.ci)
m.suic.ci$variab <- rownames(m.suic.ci)




df <- bind_rows(list(m.dem.ci, m.dep.ci, m.psy.ci, m.suic.ci, m.oth.ci, m.ep.ci))



df[df$variab == 'age',]$variab <- 'Age'
df[df$variab == 'SexMale',]$variab <- 'Male'
df$variab <- factor(df$variab, levels = c("Time", "Age", 'Male'))



p1 <- df %>% filter(Diagnosis != 'Suicide') %>%
  ggplot(aes(x=Estim, y = Diagnosis, color = Diagnosis))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_point(shape=18,
             size = 3)+
  facet_wrap(.~ variab, scales = 'free_x')+
  geom_errorbarh(aes(xmin = X2.5.., xmax = X97.5..),
                 height = 0, 
                 )+
  theme_classic()+
  scale_color_manual(values = colors)+
  theme(legend.position = 'top',
        axis.text.y = element_blank(), 
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  labs(y = NULL, x = 'Coefficient estimate')+
  guides(color=guide_legend(ncol=2))+
   ggtitle('Changes in disability scores, n=261') 


ggsave(p1, filename = 'disability.png', height = 5, width = 6)




# Quality of life models
# The same thing as before but in a loop


df2 <- data.frame()

for (varib in c("Physical","Psychological" , "Social" ,"Environment")){

  m.dep <- lmer(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id)')), 
                data = subset(data,diagnosis1 == 'Depression'), 
                REML = F)
  
  m.psy <- lmer(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id)')), 
                data = subset(data,diagnosis1 == 'Psychosis'), 
                REML = F)
  
  m.ep <- lmer(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id)')), 
               data = subset(data,diagnosis1 == 'Epilepsy'), 
               REML = F)
  
  m.suic <- lmer(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id)')), 
                 data = subset(data,diagnosis1 == 'Suicide'), 
                 REML = F)
  
  m.oth <- lmer(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id)')), 
                data = subset(data,diagnosis1 == "Other mental health complaints"), 
                REML = F)
  
  m.dem <- lmer(formula(paste(varib, '~ Time + age  + Marital_status + Education + occupation2  + (1|id)')), 
                data = subset(data,diagnosis1 == 'Dementia'), 
                REML = F)
  
  
  m.dem.ci <- data.frame(confint(m.dem)[c('Time', 'age'),])
  m.dep.ci <- data.frame(confint(m.dep)[c('Time', 'age', 'SexMale'),])
  m.ep.ci <- data.frame(confint(m.ep)[c('Time', 'age', 'SexMale'),])
  m.suic.ci <- data.frame(confint(m.suic)[c('Time', 'age', 'SexMale'),])
  m.psy.ci <- data.frame(confint(m.psy)[c('Time', 'age', 'SexMale'),])
  m.oth.ci <- data.frame(confint(m.oth)[c('Time', 'age', 'SexMale'),])
  
  m.dem.ci$Diagnosis <- 'Dementia'
  m.dep.ci$Diagnosis <- 'Depression'
  m.psy.ci$Diagnosis <- 'Psychosis'
  m.ep.ci$Diagnosis <- 'Epilepsy'
  m.suic.ci$Diagnosis <- 'Suicide'
  m.oth.ci$Diagnosis <- 'Other complaints'
  
  
  m.dem.ci$Estim <- fixef(m.dem)[c('Time', 'age')]
  m.dep.ci$Estim <- fixef(m.dep)[c('Time', 'age', 'SexMale')]
  m.psy.ci$Estim <- fixef(m.psy)[c('Time', 'age', 'SexMale')]
  m.ep.ci$Estim <- fixef(m.ep)[c('Time', 'age', 'SexMale')]
  m.suic.ci$Estim <- fixef(m.suic)[c('Time', 'age', 'SexMale')]
  m.oth.ci$Estim <- fixef(m.oth)[c('Time', 'age', 'SexMale')]
  
  
  
  m.dem.ci$variab <- rownames(m.dem.ci)
  m.dep.ci$variab <- rownames(m.dep.ci)
  m.ep.ci$variab <- rownames(m.ep.ci)
  m.psy.ci$variab <- rownames(m.psy.ci)
  m.oth.ci$variab <- rownames(m.oth.ci)
  m.suic.ci$variab <- rownames(m.suic.ci)
  
  df_t <- bind_rows(list(m.dem.ci, m.dep.ci, m.psy.ci, m.suic.ci, m.oth.ci, m.ep.ci))
  
  df_t$dv <- varib
  
  df2 <- rbind(df2, df_t)

}


df2[df2$variab == 'age',]$variab <- 'Age'
df2[df2$variab == 'SexMale',]$variab <- 'Male'
df2$variab <- factor(df2$variab, levels = c("Time", "Age", 'Male'))



p2 <- df2 %>% filter(Diagnosis != 'Suicide') %>%
  ggplot(aes(x=Estim, y = Diagnosis, color = Diagnosis))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  geom_point(#position = position_dodge(width = 0.8), 
    shape=18,
    size = 3)+
  facet_grid(dv~variab, scales = 'free_x')+
  geom_errorbarh(aes(xmin = X2.5.., xmax = X97.5..),
                 height = 0, 
                 # position = position_dodge(width = 0.8)
  )+
  theme_classic()+
  scale_color_manual(values = colors)+
  theme(legend.position = 'top', 
        axis.text.y = element_blank(), 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5))+
  labs(y = NULL, x = 'Coefficient estimate')+
  guides(color=guide_legend(ncol=2)) +
  ggtitle('Changes in domains of quality of life, n=261')



ggsave(p2, filename = 'quality.png', height = 5.5, width = 7)














library(ordinal) #library wirh ordinal mixed effects logit



df3 <- data.frame()

for (varib in c("cgi1","cgi2" ,"efficacy_index")){
  
  m.dep <- clmm(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id) + (1|steps)')), 
                data = subset(data,diagnosis1 == 'Depression')) 
  
  m.psy <- clmm(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id) + (1|steps)')), 
                data = subset(data,diagnosis1 == 'Psychosis'))
  
  
  
  if (varib != 'efficacy_index'){
    
    m.ep <- clmm(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id) + (1|steps)')), 
                 data = subset(data,diagnosis1 == 'Epilepsy'), maxIter = 100 )
    
    
    
  } else {
    
    d <- table(subset(data,diagnosis1 == 'Epilepsy')$efficacy_index)[table(subset(data,diagnosis1 == 'Epilepsy')$efficacy_index) > 10]
    
    m.ep <- clmm(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id) + (1|steps)')), 
                 data = subset(data,diagnosis1 == 'Epilepsy' & efficacy_index %in% names(d)),
                 method = "ucminf", maxIter = 1000 , maxLineIter=1000)
    
  }
  
  
  m.oth <- clmm(formula(paste(varib, '~ Time + age + Sex + Marital_status + Education + occupation2  + (1|id) + (1|steps)')), 
                data = subset(data,diagnosis1 == "Other mental health complaints"))
  

  m.dep.ci <- data.frame(confint(m.dep)[c('Time', 'age', 'SexMale'),])
  m.ep.ci <- data.frame(confint(m.ep)[c('Time', 'age', 'SexMale'),])
  m.psy.ci <- data.frame(confint(m.psy)[c('Time', 'age', 'SexMale'),])
  m.oth.ci <- data.frame(confint(m.oth)[c('Time', 'age', 'SexMale'),])
  
  m.dep.ci$Diagnosis <- 'Depression'
  m.psy.ci$Diagnosis <- 'Psychosis'
  m.ep.ci$Diagnosis <- 'Epilepsy'
  m.oth.ci$Diagnosis <- 'Other complaints'
  
  
  m.dep.ci$Estim <- coef(m.dep)[c('Time', 'age', 'SexMale')]
  m.psy.ci$Estim <- coef(m.psy)[c('Time', 'age', 'SexMale')]
  m.ep.ci$Estim <- coef(m.ep)[c('Time', 'age', 'SexMale')]
  m.oth.ci$Estim <- coef(m.oth)[c('Time', 'age', 'SexMale')]
  
  
  
  m.dep.ci$variab <- rownames(m.dep.ci)
  m.ep.ci$variab <- rownames(m.ep.ci)
  m.psy.ci$variab <- rownames(m.psy.ci)
  m.oth.ci$variab <- rownames(m.oth.ci)

  df_t <- bind_rows(list(m.dep.ci, m.psy.ci, m.oth.ci, m.ep.ci))
  
  df_t$dv <- varib
  
  df3 <- rbind(df3, df_t)
  
}



df3[df3$variab == 'age',]$variab <- 'Age'
df3[df3$variab == 'SexMale',]$variab <- 'Male'


df3[df3$dv == 'cgi1',]$dv <- 'Severity'
df3[df3$dv == 'cgi2',]$dv <- 'Improvement'
df3[df3$dv == 'efficacy_index',]$dv <- 'Efficacy'

df3$variab <- factor(df3$variab, levels = rev(c("Time", "Age", 'Male')))



df3_odds <- df3

df3_odds$Estim <- exp(df3_odds$Estim)
df3_odds$X2.5.. <- exp(df3_odds$X2.5..)
df3_odds$X97.5.. <- exp(df3_odds$X97.5..)



colors_ill = c('Severity'='#008000',
           'Improvement' = '#0000ff', 
           'Efficacy' = '#ff7f00')





p3 <- df3_odds %>% 
  ggplot(aes(x=Estim, y = variab, color = dv))+
  geom_vline(xintercept = 1, linetype = 'dashed')+
  facet_wrap(.~Diagnosis, scales = 'free_x', nrow = 2)+
  geom_point(position = position_dodge(width = 0.8), 
    shape=18,
    size = 3)+
  geom_errorbarh(aes(xmin = X2.5.., xmax = X97.5..),
                 height = 0, 
                  position = position_dodge(width = 0.8)
  )+
  theme_classic()+
  scale_color_manual(values = colors_ill)+
  theme(legend.position = 'top', 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5))+
  labs(y = NULL, x = 'Odds ratio')+
  guides(color=guide_legend(ncol=3)) +
  ggtitle('Changes in severity of illness, n=261')

ggsave(p3, filename = 'severity_odds.png', height = 5.2, width = 5)





p3 <- df3  %>% 
  ggplot(aes(x=Estim, y = variab, color = dv))+
  geom_vline(xintercept = 0, linetype = 'dashed')+
  facet_wrap(.~Diagnosis, scales = 'free_x', nrow = 2)+
  geom_point(position = position_dodge(width = 0.8), 
             shape=18,
             size = 3)+
  geom_errorbarh(aes(xmin = X2.5.., xmax = X97.5..),
                 height = 0, 
                 position = position_dodge(width = 0.8)
  )+
  theme_classic()+
  scale_color_manual(values = colors_ill)+
  theme(legend.position = 'top', 
        legend.title = element_blank(), 
        plot.title = element_text(hjust = 0.5))+
  labs(y = NULL, x = 'Coefficient estimate')+
  guides(color=guide_legend(ncol=3)) +
  ggtitle('Changes in severity of illness, n=261')


ggsave(p3, filename = 'severity_coefs.png', height = 5.2, width = 5)




