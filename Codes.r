#Settings
library(brms)
library(bayesplot)
library(rstan)
rstan_options(auto_write=TRUE)
options(mc.cores=parallel::detectCores())

#Load Data
Data=read.delim(file.choose(), row.name=NULL)

#Compile the model by brms
glm_brms=brm(agent~A_animacy+wordcount+P_animacy+affectedness+(A_animacy||verb)
     ,family=bernoulli(),data=Data,seed=1, prior=c(set_prior("",class="Intercept")), 
     control = list(adapt_delta = 0.95, max_treedepth=15),iter = 10000, warmup = 8000, thin=4)

#Output Figure 4
stanplot(glm_brms,type='intervals',pars='^b_',prob=0.8,prob_outer=0.95)

#Output Figure 5 and 6
brms::conditional_effects(glm_brms, categorical=FALSE)

#Output Figure 7
eff=conditional_effects(glm_brms, effects="A_animacy:affectedness")
plot(eff, points=FALSE)

#Output Figure 8
conditions=data.frame(verb=c('terpengaruh','terinspirasi','terlihat','terganggu','tertutup','tertangkap',
    'terbunuh','terikat','terdengar','tersentuh','terbentuk','terancam','terjangkau','terdorong','tertarik',
    'terinfeksi', 'terbawa','terasa', 'terdesak','terkesan','terhambat','terpisah', 'terbatas', 'terbakar',
    'tertekan','terjebak','terhubung','tergantung','terluka','terpilih','tersusun','tercemar','terbukti', 
    'terbuka'))
eff_2=conditional_effects(glm_brms, re_formula=NULL, effects='A_animacy:affectedness', conditions=conditions, 
    categorical = FALSE)
plot(eff_2,points=FALSE)


