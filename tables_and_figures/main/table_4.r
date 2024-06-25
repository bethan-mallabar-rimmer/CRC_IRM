#install.packages('pROC')
library('pROC')
library('dplyr')

basemod <- glm('case~abdo_pain+sym_age+grs+rectal_bloodloss+change_bowel_habit+sex', p_v_train$p_40_ve, family='binomial')
#each participant's probability of being a case:
predicted_probs <- predict(basemod, newdata = p_v_test$p_40_ve, type = "response")

# this function takes a level and outputs the spec, sens, ppv, npv, youdens
formatepi=function(level,frame,prob){
  predicted.classes <- prob > level
  atab=sum(predicted.classes==TRUE & frame$case==TRUE)
  btab=sum(predicted.classes==TRUE & frame$case==FALSE)
  ctab=sum(predicted.classes==FALSE & frame$case==TRUE)
  dtab=sum(predicted.classes==FALSE & frame$case==FALSE)
  data <- as.table(matrix(c(atab,btab,ctab,dtab), nrow = 2, byrow = TRUE))
  rval <- epi.tests(data, conf.level = 0.95)
  df=data.frame(rval$detail)
  out=data.frame(threshold=level*100,
                 specificity=df[df$statistic=='sp',2],
                 sensitivity=df[df$statistic=='se',2],
                 PPV=df[df$statistic=='pv.pos',2],
                 NPV=df[df$statistic=='pv.neg',2],
                 Youdens=df[df$statistic=='youden',2])
  return(out%>%round(3))
}


epistats=rbind(
  formatepi(0.005896726,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.01,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.02,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.03,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.04,p_v_test$p_40_ve,predicted_probs),
  formatepi(0.05,p_v_test$p_40_ve,predicted_probs))

formatepi(0.01,p_v_test$p_40_ve,predicted_probs)
