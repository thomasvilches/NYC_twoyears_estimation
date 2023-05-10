setwd("/data/thomas-covid/NYC_booster_revision/")
library(dplyr)

library(readxl)
library(tidyverse)     ## data wrangling + ggplot2
library(colorspace)    ## adjust colors
library(rcartocolor)   ## Carto palettes

# Parameters -------------------------------------------------------------------
set.seed(1432)
#Total cost of vaccine clinic setup 
cost_setup = 4344813

#Expenditure on advertisement and awareness campaigns
cost_advertisement = 243390768.20 #242,986,305.11
#Total cost of vaccine storage and transport 
cost_storage_and_transport = 7205179.89
#Cost of vaccine administration (all other costs) 
cost_administration = 1872058070.42 #1,752,152,923.18
#Other vaccination expenses (mobile and homebound vaccinations) 
#Expenses that will benefit all vaccinations. ie. CIR, DOITT developed applications
expenses_benefits = 30900000
# Total cost of vaccines
vaccines_cost = 282663374.80 #282,663,374.8


#Indirect costs
pcpi_nyc = 74472 ## Per-capita personal income NYC
perc_vac_emp = 0.7176 ## proportion of vaccinated people that is employed (18-64 yo)
wdl_vac = 0.5 #work days lost due to visit for vaccination
pm_adverse_1 = 0.517 #proportion of adverse reaction First dose Moderna
pm_adverse_2 = 0.748 #proportion of adverse reaction First dose Moderna
pp_adverse_1 = 0.48 #proportion of adverse reaction First dose Moderna
pp_adverse_2 = 0.642 #proportion of adverse reaction First dose Moderna
pjj_adverse = 0.76 #proportion of adverse reaction First dose Moderna
wdl_adverse_1 = 1.66 #(sd = 1.48)working days lost due to adverse reactions first dose
wdl_adverse_2 = 1.39 #(sd = 0.82)working days lost due to adverse reactions

#Direct costs
cost_outpatient_appointment = 1146 #outpatient appointment (symptomatic cases)
n_outpatient_visits = 0.5 #(total number of mild cases / 2) per mild case - ASSUMED
cost_transp_outpatients = 50 #for each visit
cost_hosp_nICU = 44381 #cost of hospital non-ICU admission
cost_hosp_ICU = 127247 #cost of ICU admission
n_ED_visits = 1 #for each severe non-hospitalized case- ASSUMED
cost_ED_care = 3713 #cost ED care
n_EMS_calls = 2.5 #per hospitalized case
cost_transp_EMS = 1011
r = 0.03 #discount rate
cost_lifelost = 100000 #441325 ##240676 #455484 #per year of life lost #average of statistical life in US is
# between US$ 9-10 mi with life expectancy of 79 years - REVISE
max_cost_life = 10300000
# Cost of Illness

symp_isolation = 10 #days out of work
# hospitalization - take it from JAMA paper and add another 4 days
duration_hosp_niCU = c(6,6,6,6,6,3) #data for each strain for non-ICU
duration_hosp_ICU = c(15,15,15,15,15,7)  #data for each strain for ICU
days_beforeafter = 3.5+4


basedate = as.Date("2020-09-01")
basedate_vac = as.Date("2020-12-14")
enddate = as.Date("2022-01-31")
population = 8336817


idx_1 = 0
idx_2 = 4
#  Reading file function -----------------------------------------------------------------

# Let's create a function to read the incidence file

read_file_incidence <- function(index,type, folder = "./",strain = c(1,2,3,4,5,6),st2 = "newyorkcity",beta = "121",ag="all"){
  
  data.cases1 = read.table(paste0(folder, "results_prob_0_",beta,"_",index,"_",st2,"/simlevel_",type,"_inc_",ag,".dat"),',',h = T) 
  data.cases1 = data.cases1[,-1]
  
  data.cases2 = read.table(paste0(folder, "results_prob_0_",beta,"_",index,"_",st2,"/simlevel_",type,"2_inc_",ag,".dat"),',',h = T) 
  data.cases2 = data.cases2[,-1]
  
  data.cases3 = read.table(paste0(folder, "results_prob_0_",beta,"_",index,"_",st2,"/simlevel_",type,"3_inc_",ag,".dat"),',',h = T) 
  data.cases3 = data.cases3[,-1]
  
  data.cases4 = read.table(paste0(folder, "results_prob_0_",beta,"_",index,"_",st2,"/simlevel_",type,"4_inc_",ag,".dat"),',',h = T) 
  data.cases4 = data.cases4[,-1]
  
  data.cases5 = read.table(paste0(folder, "results_prob_0_",beta,"_",index,"_",st2,"/simlevel_",type,"5_inc_",ag,".dat"),',',h = T) 
  data.cases5 = data.cases5[,-1]
  
  data.cases6 = read.table(paste0(folder, "results_prob_0_",beta,"_",index,"_",st2,"/simlevel_",type,"6_inc_",ag,".dat"),',',h = T) 
  data.cases6 = data.cases6[,-1]
  
  l = list(data.cases1,data.cases2,data.cases3,data.cases4,data.cases5,data.cases6)
  
  return(l[strain])
}

# And a function to bootstrap 

fc <- function(d, i){
  return(mean(d[i],na.rm=T))
}

# discount formula for YLL
formula = function(x) min((cost_lifelost/r) - (1/((1+r)^x))*(cost_lifelost/r),max_cost_life)
#formula = function(x) (cost_lifelost/r) - (1/((1+r)^x))*(cost_lifelost/r)

# Illness and Hospitalization (direct) ---------------------------------------------------------


ffolder = "fmild_1.0/"
# we want to see the hospitalization scaling factor

#total hospitalization per 100,000 population from the beginning of vaccination
total_hosp = data.cases %>% filter(date_of_interest >= basedate_vac,date_of_interest <= enddate) %>% pull(inc_hosp) %>% sum()/population*100000
total_hosp

#Let's see this number in the simulation

hos_sim = read_file_incidence(idx_1,"hos", folder = ffolder)
icu_sim = read_file_incidence(idx_1,"icu", folder = ffolder)

hos = Reduce('+', hos_sim) # adding the strains
icu = Reduce('+', icu_sim)# adding the strains
nn = nrow(hos)

v_date = basedate+seq(0,nn-1) #creating a vector with the dates of simulation


sum.sim.hos = sum(hos[v_date >= basedate_vac & v_date <= enddate,])/ncol(hos)
sum.sim.icu = sum(icu[v_date >= basedate_vac & v_date <= enddate,])/ncol(icu)

# factor_hos = total_hosp/(sum.sim.hos+sum.sim.icu)
# factor_hos
factor_hos = 1.1888

asymp = Reduce('+',read_file_incidence(idx_1,"asymp", ffolder)) # adding the strains
inf = Reduce('+',read_file_incidence(idx_1,"inf", ffolder)) # adding the strains
mild = Reduce('+',read_file_incidence(idx_1,"mild", ffolder)) # adding the strains

# Let's set inf to be severe non-hospitalized
inf_nh = inf - hos - icu
# number of extra hospitalization after scaling
n_extra = (sum.sim.hos+sum.sim.icu)*factor_hos - (sum.sim.hos+sum.sim.icu)

#total number
sum.sim.asymp = sum(asymp[v_date >= basedate_vac & v_date <= enddate,])/ncol(asymp)
sum.sim.mild = sum(mild[v_date >= basedate_vac & v_date <= enddate,])/ncol(mild)
sum.sim.sev = sum(inf_nh[v_date >= basedate_vac & v_date <= enddate,])/ncol(inf_nh)

factor_non_hos = (sum(c(sum.sim.asymp,sum.sim.mild,sum.sim.sev))-n_extra)/sum(c(sum.sim.asymp,sum.sim.mild,sum.sim.sev))

# Now we want to bootstrap the mean of those matrices

sum.sim.mild = colSums(mild)
#sum.sim.mild = boot::boot(sum.sim,fc,500)$t[,1]

sum.sim.inf = colSums(inf_nh)
#sum.sim.inf = boot::boot(sum.sim,fc,500)$t[,1]

sum.sim.hos = colSums(hos)
#sum.sim.hos = boot::boot(sum.sim,fc,500)$t[,1]

sum.sim.icu = colSums(icu)
#sum.sim.icu = boot::boot(sum.sim,fc,500)$t[,1]

# we increased the hospitalizations by some amount, therefore,
# we decrease the other infections proportionately

sum.sim.mild = sum.sim.mild*factor_non_hos
sum.sim.inf = sum.sim.inf*factor_non_hos

sum.sim.hos = sum.sim.hos*factor_hos
sum.sim.icu = sum.sim.icu*factor_hos


#cost mild infection of hospital
cost_symp = (sum.sim.mild)*
  n_outpatient_visits*(cost_outpatient_appointment+cost_transp_outpatients)
#cost severe non-hospitalized infection
cost_inf = (sum.sim.inf)*cost_ED_care
#cost for hospitalizations
cost_hos = (sum.sim.hos)*(cost_hosp_nICU+n_EMS_calls*cost_transp_EMS)
cost_icu = (sum.sim.icu)*(cost_hosp_ICU+n_EMS_calls*cost_transp_EMS)

cost_hospital = (cost_symp+cost_inf+cost_hos+cost_icu)*population/100000
#cost_hospital

###
# For the SCENARIO without vaccination
###

hos_sim = read_file_incidence(idx_2,"hos", ffolder)
icu_sim = read_file_incidence(idx_2,"icu", ffolder)

hos = Reduce('+', hos_sim) # adding the strains
icu = Reduce('+', icu_sim)# adding the strains

nn = nrow(hos)
v_date = basedate+seq(0,nn-1) #creating a vector with the dates of simulation

sum.sim.hos2 = sum(hos[v_date >= basedate_vac & v_date <= enddate,])/ncol(hos)
sum.sim.icu2 = sum(icu[v_date >= basedate_vac & v_date <= enddate,])/ncol(icu)


asymp = Reduce('+',read_file_incidence(idx_2,"asymp", ffolder)) # adding the strains
inf = Reduce('+',read_file_incidence(idx_2,"inf", ffolder)) # adding the strains
mild = Reduce('+',read_file_incidence(idx_2,"mild", ffolder)) # adding the strains

# Let's set inf to be severe non-hospitalized
inf_nh = inf - hos - icu
# number of extra hospitalization after scaling
n_extra = (sum.sim.hos2+sum.sim.icu2)*factor_hos - (sum.sim.hos2+sum.sim.icu2)

#total number
sum.sim.asymp2 = sum(asymp[v_date >= basedate_vac & v_date <= enddate,])/ncol(asymp)
sum.sim.mild2 = sum(mild[v_date >= basedate_vac & v_date <= enddate,])/ncol(mild)
sum.sim.sev2 = sum(inf_nh[v_date >= basedate_vac & v_date <= enddate,])/ncol(inf_nh)

factor_non_hos_2 = (sum(c(sum.sim.asymp2,sum.sim.mild2,sum.sim.sev2))-n_extra)/sum(c(sum.sim.asymp2,sum.sim.mild2,sum.sim.sev2))


# Bootstraping

sum.sim.mild2 = colSums(mild)
#sum.sim.mild2 = boot::boot(sum.sim,fc,500)$t[,1]

sum.sim.inf2 = colSums(inf_nh)
#sum.sim.inf2 = boot::boot(sum.sim,fc,500)$t[,1]

sum.sim.hos2 = colSums(hos)
#sum.sim.hos2 = boot::boot(sum.sim,fc,500)$t[,1]

sum.sim.icu2 = colSums(icu)
#sum.sim.icu2 = boot::boot(sum.sim,fc,500)$t[,1]

# we increased the hospitalizations by some amount, therefore,
# we decrease the other infections proportionately

sum.sim.mild2 = sum.sim.mild2*factor_non_hos_2
sum.sim.inf2 = sum.sim.inf2*factor_non_hos_2

sum.sim.hos2 = sum.sim.hos2*factor_hos
sum.sim.icu2 = sum.sim.icu2*factor_hos

#cost mild infection of hospital
cost_symp = (sum.sim.mild2)*
  n_outpatient_visits*(cost_outpatient_appointment+cost_transp_outpatients)
#cost severe non-hospitalized infection
cost_inf = (sum.sim.inf2)*cost_ED_care
#cost for hospitalizations
cost_hos = (sum.sim.hos2)*(cost_hosp_nICU+n_EMS_calls*cost_transp_EMS)
cost_icu = (sum.sim.icu2)*(cost_hosp_ICU+n_EMS_calls*cost_transp_EMS)

cost_hospital2 = (cost_symp+cost_inf+cost_hos+cost_icu)*population/100000
#cost_hospital2


#-----

cost_outpatient <- (sum.sim.mild-sum.sim.mild2)*population/100000*cost_outpatient_appointment
cost_inf = (sum.sim.inf-sum.sim.inf2)*cost_ED_care*population/100000
cost_ems = (sum.sim.hos-sum.sim.hos2)*(n_EMS_calls*cost_transp_EMS)+(sum.sim.icu-sum.sim.icu2)*(n_EMS_calls*cost_transp_EMS)*population/100000
cost_inp = (sum.sim.hos-sum.sim.hos2)*(cost_hosp_nICU)+(sum.sim.icu-sum.sim.icu2)*(cost_hosp_ICU)*population/100000

cb_out <- boot::boot(as.vector(cost_outpatient),fc, 500)
boot::boot.ci(cb_out, 0.95, "all")
mean(cb_out$t)


cb_inf <- boot::boot(as.vector(cost_inf),fc, 500)
boot::boot.ci(cb_inf, 0.95, "all")
mean(cb_inf$t)

cb_ems <- boot::boot(as.vector(cost_ems),fc, 500)
boot::boot.ci(cb_ems, 0.95, "all")
mean(cb_ems$t)


cb_inp <- boot::boot(as.vector(cost_inp),fc, 500)
boot::boot.ci(cb_inp, 0.95, "all")
mean(cb_inp$t)


total <- cost_outpatient+cost_inf+cost_ems+cost_inp

cb_total <- boot::boot(as.vector(total),fc, 500)
boot::boot.ci(cb_total, 0.95, "all")
mean(cb_total$t)


# Boost vac ---------------------------------------------------------------

rb <- read.table("./fmild_1.0/results_prob_0_121_4_newyorkcity/n_booster_doses.dat")
mean(rb$V1)

