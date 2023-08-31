rm(list=ls())

setwd("/media/antonioproietti/7AB7-FF3D/Dati per empirical economics/Dataset")
getwd()

# loading packages
library(tidyverse)
library(sandwich)
library(lmtest)
library(modelsummary)
library(lspline)
library(mfx)


data <- read.table("linear_wage.csv", sep=";", header=T)
View(data)

# all the variables: dropped "occasionali" and "co.co.co" due to singularity
# also dropped richiesta_laurea_generica_dummy.si.
# dropped the sector due to correlation with "privato".

lm1 = lm( log(reddito_tot_mens)  ~
            zona_ateneo_nord + zona_ateneo_centro + laurea_triennale + 
            laurea_professionale + laurea_scientifica + laure_scientificaXinge + 
            sesso_dummy + cittadinanza + X.22 + X23.24 + X25...29 +
            liceo + tecnico + voto_diploma + erasmus_dummy +
            in_corso_dummy + voto_laurea + master_1_dummy +
            master_2_dummy + interazione_master + nuova_laurea_dummy +
            dottorato_dummy + spec_post_dummy + master_extra_dummy +
            dirigente +
            quadro + impiegato_high + impiegato_low + apprendista +
            privato + indeterminato + full.time +
            richiesta_laurea_specifica_dummy.si. +
            richiesta_vot_min_dummy.si. + 
            p_medie + p_superiori + p_laurea + p_post.laurea + m_medie + 
            m_superiori + m_laurea + m_post.laurea + Interaction_pm,
            data=data)
coeftest(lm1, vcov = vcovHC(lpm1, type = "HC0"))
summary(lm1)

#repeat the regression considering only full time employee. That's because part-time
#is considered only as a binary variable, but it can be a wide range.

lm1 = lm( log(reddito_tot_mens)  ~
             zona_ateneo_nord + zona_ateneo_centro + laurea_triennale + 
             laurea_professionale + laurea_scientifica + laure_scientificaXinge + 
             sesso_dummy + cittadinanza + X.22 + X23.24 + X25...29 +
             liceo + tecnico + voto_diploma + erasmus_dummy +
             in_corso_dummy + voto_laurea + master_1_dummy +
             master_2_dummy + interazione_master + nuova_laurea_dummy +
             dottorato_dummy + spec_post_dummy + master_extra_dummy +
             dirigente + apprendista +
             quadro + impiegato_high + impiegato_low + operaio +
             privato + indeterminato + richiesta_laurea_specifica_dummy.si. +
             richiesta_vot_min_dummy.si. + 
             p_medie + p_superiori + p_laurea + p_post.laurea + m_medie + 
             m_superiori + m_laurea + m_post.laurea + Interaction_pm,
           data=filter(data,full.time==1))
summary(lm1)

#checks voto diploma

lm_votodip = lm(reddito_tot_mens ~ voto_diploma, data = data)
summary(lm_votodip)

lm_votolaurea = lm(reddito_tot_mens ~ voto_laurea + voto_diploma, data = data)
summary(lm_votolaurea)

#both these variables don't explain much of the variation in the dependent variable

#quantile regression

attach(data)
Y <- cbind(reddito_tot_mens)
X <- cbind(zona_ateneo_nord, zona_ateneo_centro, laurea_triennale, 
             laurea_professionale, laurea_scientifica, laure_scientificaXinge, 
             sesso_dummy, cittadinanza, X.22, X23.24, X25...29,
             liceo, tecnico, voto_diploma, erasmus_dummy,
             in_corso_dummy, voto_laurea, master_1_dummy,
             master_2_dummy, interazione_master, nuova_laurea_dummy,
             dottorato_dummy, spec_post_dummy, master_extra_dummy,
             dirigente, apprendista,
             quadro, impiegato_high, impiegato_low,
             privato, indeterminato, richiesta_laurea_specifica_dummy.si.,
             richiesta_vot_min_dummy.si., 
             p_medie, p_superiori, p_laurea, p_post.laurea, m_medie, 
             m_superiori, m_laurea, m_post.laurea, Interaction_pm)

qr <- rq(log(Y) ~ X, data=filter(data,full.time==1),tau = c(0.1,0.25,0.5,0.75,0.9))

summary(qr)



