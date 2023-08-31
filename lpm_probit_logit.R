# BINARY DEPENDENT VARIABLES: MODELLING PROBABILITIES

rm(list=ls())

setwd("C:/Users/UTENTE/Desktop/Dati per empirical economics")
getwd()

# loading packages
library(tidyverse)
library(sandwich)
library(lmtest)
library(modelsummary)
library(lspline)
library(mfx)

##################################################################
# Modelling probabilities: y is either 0 or 1
# p(y = 1 | x)

# 1) LPM
# 2) Logit model
# 3) Probit model

##################################################################

data = read_csv("logit.csv")
data.df <- read.table("logit.csv", sep=";", header=T)
View(data.df)

summary(data.df$voto_diploma)
summary(data.df$voto_laurea)

### Linear probability model

# simple

lpm1 = lm(lavoro_dummy ~ 
            cittadinanza + zona_ateneo_nord + zona_ateneo_centro +
          laurea_triennale + laurea_professionale + laurea_scientifica +
            laure_scientificaXinge + sesso_dummy + X23.24 + X25...29 +
            X..30 + liceo + tecnico + voto_diploma + erasmus_dummy +
            in_corso_dummy + voto_laurea + master_1_dummy + master_2_dummy +
            interazione_master + nuova_laurea_dummy + dottorato_dummy +
            spec_post_dummy + master_extra_dummy + p_medie + p_superiori +
            p_laurea + p_post.laurea + m_medie + m_superiori + m_laurea +
            m_post.laurea + Interaction_pm, 
          data=data.df)
coeftest(lpm1, vcov = vcovHC(lpm1, type = "HC0"))
summary(lpm1)
data.df$pred1 = predict(lpm1)

table(data.df$pred1, data.df$lavoro_dummy)
data.df$pred1_transformed <- round(data.df$pred1) 

#ratio of correctness
for(i in c(1:54880)) {
  if (data.df$lavoro_dummy[i] == data.df$pred1_transformed[i]){
    data.df$check[i] = 1
  } else {
    data.df$check[i] = 0
  }
}

ratiolm <- sum(data.df$check)/54880

# predicted probabilities

summary(lpm1$fitted.values) # problem of fitted values <0 and >1
# or: 
data.df$pred_lpm1 = predict( lpm1 )
datasummary( pred_lpm1 ~ min + max + mean + median + sd , 
             data = data.df, fmt="%.3f"  )

# Show the predicted probabilities' distribution
ggplot(data.df , aes( x = pred_lpm1 ) ) +
  geom_histogram( fill = 'navyblue' , color = 'grey90') +
labs(x = "Predicted probability of having a job (LPM)",y = "Count")

### LOGIT AND PROBIT MODELS

logit = glm(lavoro_dummy ~ 
              cittadinanza + zona_ateneo_nord + zona_ateneo_centro +
              laurea_triennale + laurea_professionale + laurea_scientifica +
              laure_scientificaXinge + sesso_dummy + X23.24 + X25...29 +
              X..30 + liceo + tecnico +voto_diploma + erasmus_dummy +
              in_corso_dummy + voto_laurea + master_1_dummy + master_2_dummy +
              interazione_master + nuova_laurea_dummy + dottorato_dummy +
              spec_post_dummy + master_extra_dummy + p_medie + p_superiori +
              p_laurea + p_post.laurea + m_medie + m_superiori + m_laurea +
              m_post.laurea + Interaction_pm, 
              data=data.df,
              family=binomial(link="logit"))
summary(logit)
data.df$pred2 = predict(logit)
# Predictions
betalogit = logit$coefficients; betalogit
attach(data.df)
# 1) Linear Predictors
X = cbind(1, cittadinanza, zona_ateneo_nord, zona_ateneo_centro,
            laurea_triennale, laurea_professionale, laurea_scientifica,
            laure_scientificaXinge, sesso_dummy, X23.24, X25...29,
            X..30, liceo, tecnico, voto_diploma, erasmus_dummy,
            in_corso_dummy, voto_laurea, master_1_dummy, master_2_dummy,
            interazione_master, nuova_laurea_dummy, dottorato_dummy,
            spec_post_dummy, master_extra_dummy, p_medie, p_superiori,
            p_laurea, p_post.laurea, m_medie, m_superiori, m_laurea,
            m_post.laurea, Interaction_pm)

head(X)

xb = X %*% betalogit

head(xb) # this is our linear combination ("z")

# 2) Predicted Probabilities

phat = exp(xb)/(1 + exp(xb))

head(cbind(xb,phat))

summary(phat) # predicted probabilities are in the range [0,1]

plot(xb,phat) # s-shaped

# we can do the same in this way:
plot(logit$linear.predictors, logit$fitted.values)

# GOODNESS OF FIT: Pseudo R^2 (McFadden)

#we have to derive the likelihoods of two models
lk_null = -0.5*(logit$null.deviance); lk_null

lki = data.df$lavoro_dummy*log(phat) +
      (1-data.df$lavoro_dummy)*log(1-phat)
lk_full = sum(lki); lk_full
# or: 
lk_full = -0.5*(logit$deviance); lk_full

mf_r2 = 1 - lk_full/lk_null ; mf_r2 # McFadden Pseudo R^2

# ratio
data.df$pred2 <- phat
data.df$pred2_transformed <- round(data.df$pred2)
for(i in c(1:54880)) {
  if (data.df$lavoro_dummy[i] == data.df$pred2_transformed[i]){
    data.df$check2[i] = 1
  } else {
    data.df$check2[i] = 0
  }
}

ratiolm2 <- sum(data.df$check2)/54880

# we can do the same analysis with probit
probit = glm(lavoro_dummy ~ 
              cittadinanza + zona_ateneo_nord + zona_ateneo_centro +
              laurea_triennale + laurea_professionale + laurea_scientifica +
              laure_scientificaXinge + sesso_dummy + X23.24 + X25...29 +
              X..30 + liceo + tecnico +voto_diploma + erasmus_dummy +
              in_corso_dummy + voto_laurea + master_1_dummy + master_2_dummy +
              interazione_master + nuova_laurea_dummy + dottorato_dummy +
              spec_post_dummy + master_extra_dummy + p_medie + p_superiori +
              p_laurea + p_post.laurea + m_medie + m_superiori + m_laurea +
              m_post.laurea + Interaction_pm, 
              data=data.df,
              family=binomial(link="probit"))
summary(probit)


# predictions
betaprobit = probit$coefficients; betaprobit

xb2 = X %*% betaprobit

phat2 = pnorm(xb2)

summary(phat2) # predicted probabilities are in the range [0,1]

plot(xb2,phat2) # s-shaped

# probit and logit marginal effects
probit_marg = probitmfx(formula = lavoro_dummy ~ 
                          cittadinanza + zona_ateneo_nord + zona_ateneo_centro +
                          laurea_triennale + laurea_professionale + laurea_scientifica +
                          laure_scientificaXinge + sesso_dummy + X23.24 + X25...29 +
                          X..30 + liceo + tecnico +voto_diploma + erasmus_dummy +
                          in_corso_dummy + voto_laurea + master_1_dummy + master_2_dummy +
                          interazione_master + nuova_laurea_dummy + dottorato_dummy +
                          spec_post_dummy + master_extra_dummy + p_medie + p_superiori +
                          p_laurea + p_post.laurea + m_medie + m_superiori + m_laurea +
                          m_post.laurea + Interaction_pm ,
                          data=data, atmean=FALSE)
print(probit_marg)

logit_marg = logitmfx(formula = lavoro_dummy ~ 
                        cittadinanza + zona_ateneo_nord + zona_ateneo_centro +
                        laurea_triennale + laurea_professionale + laurea_scientifica +
                        laure_scientificaXinge + sesso_dummy + X23.24 + X25...29 +
                        X..30 + liceo + tecnico +voto_diploma + erasmus_dummy +
                        in_corso_dummy + voto_laurea + master_1_dummy + master_2_dummy +
                        interazione_master + nuova_laurea_dummy + dottorato_dummy +
                        spec_post_dummy + master_extra_dummy + p_medie + p_superiori +
                        p_laurea + p_post.laurea + m_medie + m_superiori + m_laurea +
                        m_post.laurea + Interaction_pm ,
                        data=data.df)
# atmean: if false calculates the average partial effects
# if true (default), it gives partial effects for the average observation

print(logit_marg)
data.df$pred3 <- phat2
data.df$pred3_transformed <- round(data.df$pred3)
for(i in c(1:54880)) {
  if (data.df$lavoro_dummy[i] == data.df$pred3_transformed[i]){
    data.df$check3[i] = 1
  } else {
    data.df$check3[i] = 0
  }
}

ratiolm3 <- sum(data.df$check3)/54880


