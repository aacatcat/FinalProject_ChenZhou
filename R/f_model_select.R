##' @title R package demo for Forward Stepwise Selection
##' @author Chen Zhou
##' @export


f_model_select<-function(data){

  rm(list=ls())

  if(!requireNamespace("broom",quietly=TRUE))
    install.packages("broom")

  library(broom)

  ####Defined function.
  #d is the data set of all independent variables, and v is the set of column NO of independent variables confirmed to be included in the final equation.
  #Calculate RSS and adjustes R2 for the regression equation with the dependent variable as y and the set of independent variables as v plus a new independent variable ss.
  f1<-function(ss,v,d_train,d_test,t_train,t_test){
    #Calculate the dependent variable as y and the set of independent variables as v plus a new independent variable ss as RSS and adjustes R2 as the set of independent variable column Numbers used in this model-building loop.
    vtemp<-append(v,ss)

    #The set of independent variables used in the modeling step.
    xtemp_train<-as.data.frame(d_train[,vtemp])
    names(xtemp_train)<-names(d_train[vtemp])

    xtemp_test<-as.data.frame(d_test[,vtemp])
    names(xtemp_test)<-names(d_test[vtemp])

    #Establish the regression equation.
    lm.mod<-lm(t_train~.,data=xtemp_train)

    #Calculate the RSS.
    rss<- sum((t_test-predict(lm.mod,xtemp_test))^2)
    #Calculate the adjustes R2.
    adj_r2<-summary(lm.mod)$adj.r.squared

    #Return the rss and the adjustes R2.
    return(c(rss=rss,adj_r2=adj_r2))
  }


  #d is the data set of all independent variables, and v is the set of column NO of independent variables confirmed to be included in the final equation.
  #Find a variable from the set of remaining independent variables so that the RSS of the new equation is minimized.
  f2<-function(v,d_train,d_test,t_train,t_test){
    #index_all is the set of all independent variable column Numbers.
    index_all<-1:p

    #s is the set of remaining independent variables.
    s<-unlist(setdiff(index_all,v))
    e_r<-data.frame(ss=character(),rss=numeric(),adj_r2=numeric())

    #RSS and adjustes R2 for each equation in this step.
    for(ss in s){
      e_r<-rbind(e_r,data.frame(ss,t(f1(ss,v,d_train,d_test,t_train,t_test))))
    }

    #Returns the column number of the new argument that minimizes the equation RSS.
    k<-which.min(e_r[,2])
    return(e_r[k,])
  }

  #Get the confidence intervals
  # m is the model,alpha is the confidence coefficient.
  f3<-function(m,alpha=0.05){
    l<-summary(m)$coefficients
    df<-m$df.residual
    left<-l[,1]-l[,2]*qt(1-alpha/2,df)
    rigth<-l[,1]+l[,2]*qt(1-alpha/2,df)
    pvalue<-l[,4]
    rowname<-dimnames(l)[[1]]
    colname<-c("Estimate","Left","Right","p-value")
    vp<-matrix(c(l[,1],left,rigth,pvalue),ncol=4,dimnames=list(rowname,colname))
    return(vp)
  }


  ########################################################################################################
  ########################################################################################################

  ####Simulate the experimental data.
  #Simulate the data.

  #The number of independent variables.
  p<-length(data)-1

  #Establish training and test sets for cross-validation.
  #The training set contains 70% samples, and the test set contains the remaining 30% samples.
  index<-sample(x=(1:nrow(data)),size=floor(nrow(data)*0.7),replace=FALSE)

  #Training set.
  data_train<-data[index,]
  #Test set.
  data_test<-data[-index,]

  #The dependent variables of training set and test set were extracted respectively.
  #"y" is the name of the dependent variable.
  y_train<-data_train[,which(names(data_train)=="y")]
  y_test<-data_test[,which(names(data_test)=="y")]

  #All independent variable data sets of training set and test set were extracted respectively.
  all_train<-data_train[,-which(names(data_train)=="y")]
  all_test<-data_test[,-which(names(data_test)=="y")]

  #Create a data frame that displays the results of each step.
  result<-data.frame(n=numeric(),ss=character(),rss=numeric(),adj_r2=numeric(),vv=character())
  result$ss<-as.character(result$ss)
  result$vv<-as.character(result$vv)

  ####Calculate.
  #buildlistofIndependentVariablesone-at-a-time,startingwithempty.
  v<<-as.numeric()

  #Circulation.
  for(i in 1:p){
    result<-rbind(result,data.frame(n=i,f2(v,all_train,all_test,y_train,y_test),vv=""))
    result$vv<-as.character(result$vv)
    result$vv[i]<- ifelse(i==1,result$ss[i],paste0(result$vv[i-1],",",result$ss[i]))
    v<-append(v,as.numeric(result$ss[i]))
  }

  row.names(result)<-1:p
  result1<-as.data.frame(result)

  ####Find the optimal equation.
  #The optimal equation appears at which step.
  opt_index<-which.max(result$adj_r2)

  #The column number of the independent variable contained in the optimal equation.
  opt_v<-as.numeric(unlist(strsplit(result$vv[opt_index],split=",")))

  #Fit the optimal equation using the training set.
  opt_train<-all_train[,opt_v]
  opt_lm<-lm(y_train~.,data=opt_train)

  #Browse the optimal equation.
  result2<-summary(opt_lm)
  #We can see that the p-value of this equation < 2.2e-16,so,reject the null hypothesis.

  #The p-values and the confidence intervals in the optimal equation
  result3<-f3(opt_lm)
  #The coefficients of all the independent variables in this equation are valid.

  print("Step result: ")
  print(result1)
  print("/n")
  print("The summary of optimal model: ")
  print(result2)
  print("/n")
  print("The estimation of optimal model: ")
  print(result3)

  a1<-glance(result2)$p.value
  a2<-ifelse(a1<0.05,"valid","not valid")
  return(a2)
}

