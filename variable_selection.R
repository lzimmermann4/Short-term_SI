library(PGEE)
library(groupdata2)

# This section is used to prepare stratified blocked k-fold CV.
# Participant IDs serve as blocks, with all observations from a given ID kept within the same fold, 
# and stratifies to keep proportion of high/low risk participants consistent between folds.
# This is performed by calculating proportion of days of SI ideation per ID and dichotomizing to 1 (high)/0 (low) based on median value
# then use "cat_col=" option within fold() function in groupdata2 package to ensure that each fold has congruent number of ID's with 1's and 0's.

IDs_prop <- analytic_dat%>%
  dplyr::group_by(ID)%>%
  dplyr::summarise(SI_prop = sum(as.numeric(SI_any_nextday))/n())%>%
  dplyr::ungroup()

median_df <- IDs_prop%>%
  dplyr::summarise(median_SI_prop = median(SI_prop))

median_val <- median_df$median_SI_prop

IDs_medsplit <- IDs_prop%>%
  dplyr::mutate(SI_binary = case_when(SI_prop >= median_val ~ 1,
                                      SI_prop < median_val ~ 0))

dat_median<-merge(x=analytic_dat,y=IDs_medsplit,by=c("ID"))

set.seed(seed_num)
dat <- groupdata2::fold(dat_median, k = 5, 
                               cat_col = 'SI_binary', id_col = 'ID')

dat <- dat%>% 
  dplyr::arrange(.folds)%>%
  dplyr::select(-c(SI_prop, SI_binary)) # drop interim variables

# initialize output
rhs_vars_PGEE <- data.frame()
lambda.vec <- seq(0.01,0.2,0.01)

sink(paste0(outdir,"/log archive/","log_",srce,".txt"))

for (fold in 1:5){
  system.time({
  set.seed(seed_num)
  
  print("Fold number:")
  print(fold)
  
  training_data <- dat[dat$.folds != fold,]%>%
    dplyr::ungroup()%>%
    dplyr::mutate(ID = as.numeric(ID)
    )%>%
    dplyr::select(-c(.folds))
  training_data<-as.data.frame(training_data)

  testing_data <- dat[dat$.folds == fold,]%>%
    dplyr::ungroup()%>%
    dplyr::mutate(ID = as.numeric(ID)
    )%>%
    dplyr::select(-c(.folds))
  testing_data<-as.data.frame(testing_data)
  
  cv <- CVfit(formula=formula_PGEE,
              id=ID, data=training_data, family = binomial(link="logit"), 
              scale.fix = T, scale.value = 1, fold = 5, lambda.vec = lambda.vec, pindex = c(1,2), eps = 10^-6,
              maxiter = 30, tol = 10^-6)
  print(cv)
  opt_lambda<-cv$lam.opt
  
  res_PGEE <- PGEE(formula_PGEE,
                   id=ID, data=training_data, family = binomial(link="logit"), corstr = "AR-1",
                   scale.value = 1, lambda=opt_lambda, pindex = NULL, eps = 10^-6, maxiter = 30, tol = 10^-3,
                   silent = T)
  print(res_PGEE)
  head(coef(summary(res_PGEE)),7)
  
  # variables which have non-zero coefficients
  index1 <- which(abs(coef(summary(res_PGEE))[,"Estimate"]) > 10^-3)
  coef(summary(res_PGEE))[index1,]
  
  # store selected variables
  rhs.vars.PGEE<-names(abs(coef(summary(res_PGEE))[index1,"Estimate"]))
  rhs.vars.PGEE
  
  if (grepl(rhs.vars.PGEE[1],"(Intercept)",fixed=TRUE)==T) {
    rhs.vars.PGEE <- rhs.vars.PGEE[-c(1)]
  }
  
  rhs.vars.PGEE <- rhs.vars.PGEE[!rhs.vars.PGEE %in% c("Day","DayOfWeek","Missingness","Adherence")] # variables treated as compulsory
  rhs.vars.PGEE <- c(rhs.day,missing_var,rhs.vars.PGEE)
  
  print("variables selected using penalized GEE:")
  print(rhs.vars.PGEE)
  
  rhs_vars_PGEE_temp <- as.data.frame(rhs.vars.PGEE)%>%
    dplyr::mutate(Source = source,
                  Fold = fold)
  
  rhs_vars_PGEE <- bind_rows(rhs_vars_PGEE,rhs_vars_PGEE_temp)
  })
}

sink()

# export to dated CSV 
write.csv(rhs_vars_PGEE,file=paste0(outdir,"/PGEE_selectedvars_",srce,"_",Sys.Date(),".csv"), row.names = FALSE)
