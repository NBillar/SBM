# Perform pre-processing

df <- read.csv("begin2018.csv")
df$privacy_policy <- as.logical(df$privacy_policy)
df$min_downloads <- as.factor(df$min_downloads)
df$categ_app <- as.factor(df$categ_app)
df$in_app <- as.logical(df$in_app)
df$has_ads <- as.logical(df$has_ads)
df$operating_system <- as.factor(df$operating_system)
df$software_version <- as.factor(df$software_version)
df$email <- as.logical(df$email)
df$visit_website <- as.logical(df$visit_website)
df$family_library <- as.logical(df$family_library)
df$email <- as.logical(df$email)
df$pegi_category <- as.factor(df$pegi_category)
df$hedonic <- as.logical(ifelse(df$hedonic == "Hedonic", T, F))
df$label <- as.factor(df$label)
df$freemium <- as.factor(df$freemium)
df$premium <- as.factor(df$premium)
df$free <- as.factor(df$free)
df$business_model <- df$label

## Install packages

install.packages("pscl")
install.packages("MASS")
install.packages("car")
install.packages("brant")
#install.packages("truncnorm)
install.packages('truncreg')

## Libraries

library(pscl)
library(MASS)
library(car)
library(brant)
library(truncnorm)
library(truncreg)


## Hypothesis 1 

olr <-polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots+nb_words+has_ads,
            data = df,  Hess = T)

summary(olr)
vif(olr)
(ctable <- coef(summary(olr)))
exp(coef(olr))

## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))

# Brant test is slow and does not work with all the variables (numerical)
# thus include the limited test 
rand_df <- df[sample(nrow(df), size=10000), ]
olrs <- polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots+nb_words+has_ads, data = rand_df,  Hess = T)
brant(olrs)
exp(coef(olrs))

library(dplyr)
df %>% dplyr::group_by(min_downloads) %>% summarise(means = mean(nb_screenshots))
df %>% dplyr::group_by(min_downloads) %>% summarise(means = mean(free==1))
df %>% dplyr::group_by(min_downloads) %>% summarise(means = mean(freemium==1))
df %>% dplyr::group_by(min_downloads) %>% summarise(means = mean(hedonic))
df %>% dplyr::group_by(min_downloads) %>% summarise(means = mean(nb_similar_apps))
df %>% dplyr::group_by(min_downloads) %>% summarise(means = mean(nb_words))
df %>% dplyr::group_by(min_downloads) %>% summarise(means = mean(has_ads))


## Hypothesis 2

df$rating <- pmax(0, pmin(5, df$rating_app))


trunc_model <- truncreg(rating_app ~ business_model+ hedonic+ hedonic*business_model,data = df, Hess =T)
summary(trunc_model)
df$yhat <- fitted(trunc_model)
plot(trunc_model)

(r <- with(df, cor(rating_app, yhat)))
r^2


texreg::screenreg(list(polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model,data = rand_df,  Hess = T),                                                  #0
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category,data = rand_df,  Hess = T),                                    #1
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_screenshots,data = rand_df,  Hess = T),                                   #2
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_words,data = rand_df,  Hess = T),                                         #3
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+has_ads,data = rand_df,  Hess = T),                                          #6
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_screenshots,data = rand_df,  Hess = T),                     #12 < 55
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_words,data = rand_df,  Hess = T),                           #13 < 70
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+has_ads,data = rand_df,  Hess = T),                            #16 < 60
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_screenshots+nb_words,data = rand_df,  Hess = T),                          #23 < 140
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_screenshots+has_ads,data = rand_df,  Hess = T),                           #26 < 273
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_words+has_ads,data = rand_df,  Hess = T),                                 #36 < 
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_screenshots+nb_words,data = rand_df,  Hess = T)))            #123
                       texreg::screenreg(list(polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_screenshots+has_ads,data = rand_df,  Hess = T),

polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_words+has_ads,data = rand_df,  Hess = T),                   #136 < 70
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_screenshots+nb_words+has_ads,data = rand_df,  Hess = T),                  #236 < 140
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_screenshots+nb_words+has_ads,data = rand_df,  Hess = T),    #1236 
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps,data = rand_df,  Hess = T),                                    #1#0                                         #6
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots,data = rand_df,  Hess = T),                     #12 < 55
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_words,data = rand_df,  Hess = T),                           #13 < 70
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+has_ads,data = rand_df,  Hess = T),                            #16 < 60                             #36 < 
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots+nb_words,data = rand_df,  Hess = T),            #123
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots+has_ads,data = rand_df,  Hess = T),             #126
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_words+has_ads,data = rand_df,  Hess = T),                   #136 < 70                 #236 < 140
                       polr(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots+nb_words+has_ads,data = rand_df,  Hess = T)    #1236
))
                       texreg::screenreg(list(truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model,data = rand_df,  Hess = T),                                                  #0
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category,data = rand_df,  Hess = T),                                    #1
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_screenshots,data = rand_df,  Hess = T),                                   #2
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_words,data = rand_df,  Hess = T),                                         #3
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+has_ads,data = rand_df,  Hess = T),                                          #6
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_screenshots,data = rand_df,  Hess = T),                     #12 < 55
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_words,data = rand_df,  Hess = T),                           #13 < 70
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+has_ads,data = rand_df,  Hess = T),                            #16 < 60
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_screenshots+nb_words,data = rand_df,  Hess = T),                          #23 < 140
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_screenshots+has_ads,data = rand_df,  Hess = T),                           #26 < 273
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_words+has_ads,data = rand_df,  Hess = T),                                 #36 < 
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_screenshots+nb_words,data = rand_df,  Hess = T)))            #123
                       texreg::screenreg(list(truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_screenshots+has_ads,data = rand_df,  Hess = T),
                                              
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_words+has_ads,data = rand_df,  Hess = T),                   #136 < 70
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_screenshots+nb_words+has_ads,data = rand_df,  Hess = T),                  #236 < 140
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+pegi_category+nb_screenshots+nb_words+has_ads,data = rand_df,  Hess = T),    #1236 
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps,data = rand_df,  Hess = T),                                    #1#0                                         #6
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots,data = rand_df,  Hess = T),                     #12 < 55
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_words,data = rand_df,  Hess = T),                           #13 < 70
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+has_ads,data = rand_df,  Hess = T),                            #16 < 60                             #36 < 
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots+nb_words,data = rand_df,  Hess = T),            #123
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots+has_ads,data = rand_df,  Hess = T),             #126
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_words+has_ads,data = rand_df,  Hess = T),                   #136 < 70                 #236 < 140
                                              truncreg(min_downloads ~ business_model+ hedonic+ hedonic*business_model+nb_similar_apps+nb_screenshots+nb_words+has_ads,data = rand_df,  Hess = T)))    #1236















