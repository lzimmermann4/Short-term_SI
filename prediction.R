require(glmertree)
require(groupdata2)
require(vip)
require(pROC)

IDs_prop <- analytic_dat%>%
  group_by(ID)%>%
  dplyr::summarise(SI_prop = sum(SI_any_nextday)/n())%>%
  ungroup()

median_df <- IDs_prop%>%
  dplyr::summarise(median_SI_prop = median(SI_prop))

median_val <- median_df$median_SI_prop

IDs_medsplit <- IDs_prop%>%
  dplyr::mutate(SI_binary = case_when(SI_prop >= median_val ~ 1,
                                      SI_prop < median_val ~ 0))

dat_median <- merge(x=analytic_dat,y=IDs_medsplit,by=c(id))

# initialize output
perf_output <- data.frame()
vimp_output <- data.frame()

seeds <- as.list(sample(1:80000, 10, replace=F))
for (s in 1:10) {
  iteration_s <- seeds[[s]]
  set.seed(iteration_s)
  
  dat <- groupdata2::fold(dat_median, k = 5, 
                                 cat_col = 'SI_binary', 
                                 id_col = 'ID')
  
  dat <- dat %>% arrange(.folds)%>%
    mutate(SI_any_nextday = as.factor(SI_any_nextday))
  
  for (fold in 1:5){
    system.time({
      set.seed(iteration_s)
      print("Iteration:")
      print(s)
      print("Fold number:")
      print(fold)
      
      training_data <- dat[dat$.folds != fold,]%>%
        dplyr::ungroup()%>%
        dplyr::mutate(ID = as.numeric(ID)
        )%>%
        dplyr::select(-c(.folds))
      training_data <- as.data.frame(training_data)
      
      testing_data <- dat[dat$.folds == fold,]%>%
        dplyr::ungroup()%>%
        dplyr::mutate(ID = as.numeric(ID)
        )%>%
        dplyr::select(-c(.folds))
      testing_data <- as.data.frame(testing_data)
      
      res_glmertree <- glmertree(formula_glmertree,
                                 data = training_data, family = "binomial")
      pred_glmertree <- predict(res_glmertree, newdata= testing_data, re.form = NA)

      glmertree_roc <- pROC::roc(testing_data[, resp.vars], pred_glmertree)
      auc <- as.numeric(pROC::auc(glmertree_roc))
      
      perf_temp <- as.data.frame(auc)%>%
        dplyr::mutate( Source = source,
                       Iteration = s,
                       Fold = fold,
                       With_SI = with_SI
                       )%>%
        dplyr::relocate(c(With_SI,Source,Iteration,Fold),.before=auc)
      perf_temp$auc_sd <- as.numeric(sqrt(var(glmertree_roc)))
      
      # use threshold value minimizing the distance between ROC and top left corner (0,1) to obtain optimal threshold
      perf_coords <- as.data.frame(pROC::coords(glmertree_roc, "best", ret=c("sensitivity","specificity","npv","ppv","accuracy","tp","tn","threshold"), 
                                              transpose = F, best.method="closest.topleft"))%>%
        dplyr::mutate(
          sensitivity_sd = stdev_prop(sensitivity,tp),
          specificity_sd = stdev_prop(specificity,tn)
        )
      rownames(perf_coords) <- c()
      perf_temp <- bind_cols(perf_temp,perf_coords)
      
      perf_output <- bind_rows(perf_output,perf_temp)
      
      vi_scores <- vip::vi(res_glmertree, feature_names=rhs.vars.PGEEstandard, method = "firm")%>%
        dplyr::mutate( Source = source,
                       Iteration = s,
                       Fold = fold,
                       With_SI = with_SI
                      )%>%
        dplyr::relocate(c(With_SI,Source,Iteration,Fold),.before=Variable)
      
      vimp_output <- bind_rows(vimp_output,vi_scores)
      
    })
  }  
}
tbl3_metrics <- c("auc","auc_sd","sensitivity","sensitivity_sd","specificity","specificity_sd","accuracy","ppv","npv")

# average across folds in k-fold CV
perf_output_avg <- perf_output%>%
  dplyr::group_by(Source,With_SI)%>%
  dplyr::summarise(
    across(all_of(tbl3_metrics), 
           mean_fn,
           .names = "{col}")
  )%>%
  dplyr::ungroup()

vimp_output_avg <- vimp_output%>%
  dplyr::group_by(Source,With_SI,Variable)%>%
  dplyr::summarise(Importance = mean(Importance))%>%
  dplyr::arrange(Source,With_SI,Importance)%>%
  dplyr::ungroup()

# save to dated CSV file
write.csv(perf_output_avg,file=paste0(outdir,"/csv/Table3_",srce,"_",Sys.Date(),".csv"), row.names = FALSE)
write.csv(vimp_output_avg,file=paste0(outdir,"/csv/VarImp_",srce,"_",Sys.Date(),".csv"), row.names = FALSE)

