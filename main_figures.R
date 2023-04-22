require(glmertree)

analytic_dat<-analytic_dat%>%
  dplyr::mutate(SI_any_nextday = factor(SI_any_nextday, levels = 0:1, labels = c("no", "yes")))

set.seed(seed_num)
res_glmertree <- glmertree(formula_glmertree, data = analytic_dat, 
                                      family = "binomial")
summary(res_glmertree)
plot(res_glmertree, which = "tree", type="simple")

## diagrams
require(data.tree)

background_color  = "#e8e8e8"
color1            = "#FAF0DC"
color2            = "#e4dae4"
color3            = "#f0cfc3"
color4            = "#CEC1B6"
leaf_color        = "#c9dfd5"


tree_wSI <- Node$new("ED Discharge")

#LHS of tree
SI2_freq_L <- tree_wSI$AddChild("SI Frequency = 0")
SI2_freq_L_Pred <- SI2_freq_L$AddChild("Next-Day SI=No")

#RHS of tree
SI2_freq_U <- tree_wSI$AddChild("SI Frequency > 0")
SI2_freq_CM_3_L <- SI2_freq_U$AddChild("CM SI Frequency \u2264 1.14")
SI2_freq_CM_3_U <- SI2_freq_U$AddChild("CM SI Frequency > 1.14")
SE_CM_L <- SI2_freq_CM_3_L$AddChild("CM Self-Efficacy \u2264 8.74")
SE_CM_R <- SI2_freq_CM_3_L$AddChild("CM Self-Efficacy > 8.74")
SI2_freq_CM_4_L <- SE_CM_L$AddChild("CM SI Frequency \u2264 0.20")
SI2_freq_CM_4_U <- SE_CM_L$AddChild("CM SI Frequency > 0.20")
SI2_freq_CM_4_L_Pred <- SI2_freq_CM_4_L$AddChild("Next-Day SI=No")
SI2_freq_CM_4_U_Pred <- SI2_freq_CM_4_U$AddChild("Next-Day SI=Yes")
SI2_freq_CH_L <- SE_CM_R$AddChild("CH SI Frequency \u2264 1.42")
SI2_freq_CH_U <- SE_CM_R$AddChild("CH SI Frequency > 1.42")
SI2_freq_CH_L_Pred <- SI2_freq_CH_L$AddChild("Next-Day SI=No")
SI2_freq_CH_U_Pred <- SI2_freq_CH_U$AddChild("Next-Day SI=Yes")
SI1_dur_mean_L <- SI2_freq_CM_3_U$AddChild("Death Thoughts Duration \u2264 0.33")
SI1_dur_mean_U <- SI2_freq_CM_3_U$AddChild("Death Thoughts Duration > 0.33")
SI1_dur_mean_L_Pred <- SI1_dur_mean_L$AddChild("Next-Day SI=Yes")
SI1_dur_mean_U_Pred <- SI1_dur_mean_U$AddChild("Next-Day SI=Yes")

type1_var1 <- Traverse(tree_wSI, filterFun = function(x) substr(x$name,0,7) == 'SI Freq')
type1_var2 <- Traverse(tree_wSI, filterFun = function(x) substr(x$name,0,10) == 'Death Thou')
type2_var1 <- Traverse(tree_wSI, filterFun = function(x) substr(x$name,0,7) == 'CM SI F')
type2_var2 <- Traverse(tree_wSI, filterFun = function(x) substr(x$name,0,7) == 'CM Self')
type3_var1 <- Traverse(tree_wSI, filterFun = function(x) substr(x$name,0,7) == 'CH SI F')

SetGraphStyle(tree_wSI, rankdir = "LR")
SetEdgeStyle(tree_wSI, arrowhead = "vee", arrowsize=0.8, color = "grey35", penwidth = 1)
SetNodeStyle(tree_wSI, style = "filled,rounded", shape = "box", fillcolor = background_color, 
             fontname = "helvetica", fontcolor = "Black", tooltip = GetDefaultTooltip)
Do(type1_var1, SetNodeStyle, fillcolor = color1,inherit = F)
Do(type1_var2, SetNodeStyle, fillcolor = color1,style = "dashed,filled,rounded",inherit = F)
Do(type2_var1, SetNodeStyle, fillcolor = color2,inherit = F)
Do(type2_var2, SetNodeStyle, fillcolor = color2,style = "dashed,filled,rounded",inherit = F)
Do(type3_var1, SetNodeStyle, fillcolor = color3,inherit = F)
Do(tree_wSI$leaves, function(node) SetNodeStyle(node, fillcolor = leaf_color,penwidth = "0px" ))
figure_1 <- plot(tree_wSI)


tree_noSI <- Node$new("ED Discharge")

#LHS of tree
SI1_dur_mean_L <- tree_noSI$AddChild("Death Thoughts Duration \u2264 0.33")
SE_CM_L <- SI1_dur_mean_L$AddChild("CM Self-Efficacy \u2264 8.25")
SE_CM_U <- SI1_dur_mean_L$AddChild("CM Self-Efficacy > 8.25")
Worry_CM_L <- SE_CM_L$AddChild("CM Worry \u2264 1.5")
Worry_CM_U <- SE_CM_L$AddChild("CM Worry > 1.5")
Worry_CM_L_Pred <- Worry_CM_L$AddChild("Next-Day SI=No")
SE_CH_2_L <- Worry_CM_U$AddChild("CH Self-Efficacy \u2264 0.32")
SE_CH_2_U <- Worry_CM_U$AddChild("CH Self-Efficacy > 0.32")
SE_CH_2_L_Pred <- SE_CH_2_L$AddChild("Next-Day SI=Yes")
SE_CH_2_U_Pred <- SE_CH_2_U$AddChild("Next-Day SI=No")
SE_CH_L <- SE_CM_U$AddChild("CH Self-Efficacy \u2264 0.01")
SE_CH_U <- SE_CM_U$AddChild("CH Self-Efficacy > 0.01")
SE_CH_L_Pred <- SE_CH_L$AddChild("Next-Day SI=No")
Burden_max_L <- SE_CH_U$AddChild("Max. Burden \u2264 2")
Burden_max_U <- SE_CH_U$AddChild("Max. Burden > 2")
Burden_max_L_Pred <- Burden_max_L$AddChild("Next-Day SI=No")
Cope_total_CM_L <- Burden_max_U$AddChild("CM Coping \u2264 4.24")
Cope_total_CM_U <- Burden_max_U$AddChild("CM Coping > 4.24")
Cope_total_CM_L_Pred <- Cope_total_CM_L$AddChild("Next-Day SI=No")
Cope_total_CM_U_Pred <- Cope_total_CM_U$AddChild("Next-Day SI=Yes")

#RHS of tree
SI1_dur_mean_U <- tree_noSI$AddChild("Death Thoughts Duration > 0.33")
SE_CM_2_L <- SI1_dur_mean_U$AddChild("CM Self-Efficacy \u2264 7.41")
SE_CM_2_U <- SI1_dur_mean_U$AddChild("CM Self-Efficacy > 7.41")
SE_CM_2_L_Pred <- SE_CM_2_L$AddChild("Next-Day SI=Yes")
Burden_mean_2_L <- SE_CM_2_U$AddChild("Burden \u2264 3.25")
Burden_mean_2_U <- SE_CM_2_U$AddChild("Burden > 3.25")
Burden_mean_2_L_Pred <- Burden_mean_2_L$AddChild("Next-Day SI=No")
SE_CH_3_L <- Burden_mean_2_U$AddChild("CH Self-Efficacy \u2264 0.25")
SE_CH_3_U <- Burden_mean_2_U$AddChild("CH Self-Efficacy > 0.25")
SE_CH_3_L_Pred <- SE_CH_3_L$AddChild("Next-Day SI=Yes")
SE_CH_3_U_Pred <- SE_CH_3_U$AddChild("Next-Day SI=No")

type1_var1 <- Traverse(tree_noSI, filterFun = function(x) substr(x$name,0,7) == 'Death T')
type1_var2 <- Traverse(tree_noSI, filterFun = function(x) substr(x$name,0,6) == 'Burden')
type2_var1 <- Traverse(tree_noSI, filterFun = function(x) substr(x$name,0,7) == 'CM Worr')
type2_var2 <- Traverse(tree_noSI, filterFun = function(x) substr(x$name,0,7) == 'CM Copi')
type2_var3 <- Traverse(tree_noSI, filterFun = function(x) substr(x$name,0,7) == 'CM Self')
type3_var1 <- Traverse(tree_noSI, filterFun = function(x) substr(x$name,0,7) == 'CH Self')
type4_var1 <- Traverse(tree_noSI, filterFun = function(x) substr(x$name,0,7) == 'Max. Bu')

SetGraphStyle(tree_noSI, rankdir = "LR")
SetEdgeStyle(tree_noSI, arrowhead = "vee", arrowsize=0.8, color = "grey35", penwidth = 1)
SetNodeStyle(tree_noSI, style = "filled,rounded", shape = "box", fillcolor = background_color, 
             fontname = "helvetica", fontcolor = "Black", tooltip = GetDefaultTooltip)
Do(type1_var1, SetNodeStyle, fillcolor = color1,style = "dashed,filled,rounded",inherit = F)
Do(type1_var2, SetNodeStyle, fillcolor = color1,style = "dashed,filled,rounded",inherit = F)
Do(type2_var1, SetNodeStyle, fillcolor = color2,style = "dashed,filled,rounded",inherit = F)
Do(type2_var2, SetNodeStyle, fillcolor = color2,inherit = F)
Do(type2_var3, SetNodeStyle, fillcolor = color2,style = "dashed,filled,rounded",inherit = F)
Do(type3_var1, SetNodeStyle, fillcolor = color3,style = "dashed,filled,rounded",inherit = F)
Do(type4_var1, SetNodeStyle, fillcolor = color4,style = "dashed,filled,rounded",inherit = F)
Do(tree_noSI$leaves, function(node) SetNodeStyle(node, fillcolor = leaf_color,penwidth = "0px" ))
figure_2 <- plot(tree_noSI)
