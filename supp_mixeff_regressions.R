require(lme4)

# center by grand mean
rhs.vars.logistic.me <- rhs.vars.PGEEstandard[!rhs.vars.PGEEstandard %in% c(id,resp.vars,rhs.day,missing_var)]
analytic_dat_c <- analytic_dat %>%
  dplyr::mutate(across(all_of(rhs.vars.logistic.me),cntr_fn,.names = "{col}"))

# initialize output
reg_ouput <- data.frame()

# perform mixed effects logistic regressions
for (var_name in rhs.vars.logistic.me) {
  print("Covariate:")
  print(var_name)
  
  glmm_formula <- paste(resp.vars," ~ ",var_name," + Day + DayOfWeek + ",missing_var," + (1 | ID)") # random intercept and adjusting for day, day of week and missingness indicator    
  res_glmm <- glmer(formula = glmm_formula, data = analytic_dat_c, family = binomial(link = "logit"),
                    control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))
  sum_res <- summary(res_glmm)

  reg_temp <- as.data.frame(sum_res$coefficients)
  reg_temp$Covariate <- rownames(reg_temp)
  rownames(reg_temp) <- c()
  reg_temp$RHS_variable <- var_name
  
  reg_output <- bind_rows(reg_ouput,reg_temp)
}

reg_results <- reg_output%>%
  rename(SE=`Std. Error`,`P-value`=`Pr(>|z|)`) %>%
  dplyr::mutate(log_odds_lower = Estimate - (1.96 * SE),
                log_odds_upper = Estimate + (1.96 * SE),
                OR = exp(Estimate),
                OR_lower = exp(log_odds_lower),
                OR_upper = exp(log_odds_upper))

write.csv(reg_results,file=paste0(outdir,"/csv/TableS1_",Sys.Date(),".csv"), row.names = F)