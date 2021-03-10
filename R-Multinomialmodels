########################################################################################################################
#Create dummies, model matrix
########################################################################################################################

fn_create_dummy <- function(df, var_list) {
  

  fmla_01 <- paste0("~ ",paste(paste(var_list,collapse = " + "), "1", sep =" - ")) #set formula to be passed to create dummy
  df1 <- df[,c(var_list), with = FALSE]  #create a datable with just variables needed
  var_NA <- intersect(which(sapply(df1, is.factor)), which(sapply(df1, anyNA))) #get variables need to force NA
  if (!(is_empty(var_list)))  df1[, (var_NA) := lapply(.SD, addNA), .SDcols = var_NA] #explicit NA level to create a dummy for incidence
  #create a dummyfor each factor
  df2 <- df1 %>% model.matrix(as.formula(fmla_01), ., 
                              contrasts.arg = lapply(.[,..var_list], contrasts, contrasts = FALSE)) %>% data.table(.)
  
  #for variables with NA, a dummy for NA is created
  varname_NA <- colnames(df2)[grep("NA",colnames(df2))]
  if (!(is_empty(varname_NA))) {
  df3 <- df2[, lapply(.SD, function(x) ifelse(x == 1, 1, 0)), .SDcols = varname_NA]
  colnames(df3) <- (paste0(substr(varname_NA, 1, nchar(varname_NA)-2), "Inc")) 
  
  df4 <- cbind(df2[,!(varname_NA), with = FALSE ], df3)
  }
  else {
  df4 <- df2
  }
  
  df5 <- cbind(df[, !(var_list), with = FALSE], df4)
  
  
  return(df5)
}


########################################################################################################################
#Get Model String
########################################################################################################################

get_model_string<-function(lst_items){
  for (var_items in 1:length(lst_items)){
    
    x <- unlist(lst_items[var_items])
    
    for (elem_list in 1:length(x)){
      
      if(var_items==1){ Str_out <- paste( x[elem_list],"~",sep=" ")}
      if(var_items>1 & var_items<length(lst_items)){Str_out <- paste( Str_out,x[elem_list],"+",sep=" ")}
      if(var_items == length(lst_items) & elem_list < length(x)){Str_out <- paste( Str_out,x[elem_list],"+",sep=" ")}
      if(var_items == length(lst_items) & elem_list == length(x)){Str_out <- paste( Str_out,x[elem_list],sep=" ")}
      
      #  var_items<length(x)){Str_out <- paste( Str_out,x[elem_list],"+",sep=" ")}
      #}
    }
    
  }
  # print(Str_out)
  # regmodel <- multinom(Str_out, data= df_indata, family = binomial)
  return(Str_out)
}

########################################################################################################################
#Multinomial Models
########################################################################################################################
library(data.table)
library(reshape2)
library(nnet)
library(DescTools)

get_model_tables <- function(model_summary_stat,name){

  t1 <- model_summary_stat
  #t2 <- matrix(data = name ,nrow=length(rownames(t1)), ncol=1)
  t2 <- data.table(matrix(data = rownames(t1), nrow=length(rownames(t1)), ncol = 1))
  colnames(t2) <- c("Dependent_Var")
  t3 <- cbind(t2,data.table(t1))
  t4 <- melt(t3, id = c("Dependent_Var"))
  names(t4)[names(t4) == 'value'] <- name
  return(t4)
}


getCoeffMNL <- function(df_Data, lst_Model,outfilepath,filename,mdl_desc,pvalue=0.20){
  time1 = Sys.time()
  tx1 <- as.formula(gsub(","," +",get_model_string(lst_Model)))
  dput(tx1)
  df_model <<- df_Data #assign variable globally as PesudoR2 evaluates the model and need to eval the dataset to give scores
  regmodel <- multinom(tx1, data= df_model, family = binomial, model = TRUE) #multinomial models

  cf <- coef(regmodel) #coefficients
  se <- summary(regmodel)$standard.errors #standard erros
  z<- cf/se #z score

  p <- (1 - pnorm(abs(z), 0, 1)) * 2 #P-value, 2-tailed wald z test significance of coefficients
  oddsratio <- exp(coef(regmodel)) #odds ratio

  
  model.fit <- PseudoR2(regmodel, which = "all") #Get Model Fit Metrics

  #Convert the model values to a table and combine them
  ex <- get_model_tables(cf,"Coefficients")
  ex1 <- get_model_tables(se,"SE")
  ex2 <- get_model_tables(z,"Z")
  ex3 <- get_model_tables(p,"p-value")
  ex4 <- get_model_tables(oddsratio, "Odds Ratio")
  ex5 <- merge(ex,ex1) %>% merge(ex2) %>% merge(ex3) %>% merge(ex4)
  ex5$Model <- gsub(",","+",get_model_string(lst_Model))
  ex5 <- data.frame(ex5)
  ex5 <-ex5[,c(length(colnames(ex5)), 1:(length(colnames(ex5))-1))]
  #ex5$AIC <- summary(regmodel)$AIC
  ex5$Deviance <- summary(regmodel)$deviance
  ex5 <- cbind(ex5,data.frame(t(model.fit)))
  ex5$Desc <- paste0(mdl_desc)
  ex5$Sortvar <- paste0(ex5$variable,ex5$Dependent_Var)
  dput(paste0("AIC = ", summary(regmodel)$AIC))
  dput(paste0("Deviance = ", summary(regmodel)$deviance))
  dput("")
  
  rm(list=ls(envir=globalenv())[grep(glob2rx("df_model"), ls(envir=globalenv()))],envir=globalenv()) #remove the global assignment
  
  # varslct_ex6 <- c("Desc", "Model", "Dependent_Var", "variable", "Coefficients","p.value","Odds.Ratio","Sortvar")
  # pval <- 0.05
  # ex6 <- as.data.table(ex5)[, ..varslct_ex6][,Coefficients := round(ifelse(p.value > pval,0,Coefficients),2)][order(Sortvar)]
  
  fwrite(ex5, paste0(outfilepath,filename), append = TRUE )
  #fwrite(ex6, paste0(outfilepath,substr(filename,1,nchar(filename)-4),"_Simplified.csv"), append = TRUE )
  time2 = Sys.time()
  print(time2 - time1)
  output_list <- list(ex5, regmodel)
  names(output_list) <- c(as.name(paste("ModelDescr",mdl_desc,sep="_")), as.name(paste("Model",mdl_desc,sep="_")))
  return(output_list)
 
}


get_all_models <- function(df, items, outfilepath,filename,modelpvalue=0.20) {

  
  vars <- items
  vars1 <- vars[2:length(vars)]
  vars2 <- vars[1]
  
  all_model_terms <- unlist(lapply(1:(length(vars)-1), function(x) combn( vars1,x,simplify=FALSE)), recursive = FALSE)
  
  model_output <- lapply(1:length(all_model_terms), function(x) getCoeffMNL(df, list(vars2,all_model_terms[[x]]),outfilepath,filename,mdl_desc = paste("Model",x,sep=" "), pvalue=0.20))
  output_file <- do.call(c,model_output)
  return(output_file)
  
}


########################################################################################################################
#Model Plots
########################################################################################################################
library(ggplot2)
library(stringr)

mdl_coef_plot <- function(df, xvar, yvar, fillvar, ttl,mdlfitvar,pvalvar, pval=0.05){
 
  df1 <- data.table(df)[eval(parse(text = xvar)) != "(Intercept)"  & eval(parse(text = pvalvar)) <= pval]
  break_vals <- pretty(df1[[yvar]])
  
  c1 <- df1%>% 
    ggplot(aes(x = eval(parse(text = xvar)), 
               y = eval(parse(text = yvar)), 
               fill = eval(parse(text = fillvar))))+
    geom_bar(stat = "identity",position = "dodge", width = 0.5) +
    geom_text(aes(x = eval(parse(text = xvar)), 
                  y = eval(parse(text = yvar)),
                  group = eval(parse(text = fillvar)),
                  label = paste0(round(eval(parse(text = yvar)),2)),
                  hjust = ifelse(eval(parse(text = yvar)) >= 0, - 0.1 ,1.1)),
              position = position_dodge(width = 0.5),
              fontface = 4,
              #size = 4,
              inherit.aes = TRUE)+
    coord_flip()+
    scale_y_continuous(expand = c(.1,.1),
                       breaks = break_vals,
                       labels = break_vals)+
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15))+
    theme_minimal()+
    scale_fill_manual(values=c("#FA8072", "#98FB98"))+
    labs(title = paste(paste(strwrap(unique(df[, ttl,with =FALSE])),collapse="\n"), 
                       unique(paste(mdlfitvar ,unique(round(df[, mdlfitvar ,with =FALSE],3)), sep = " = " )), sep = "\n"),
         x = paste(""), y = paste(toupper(yvar)),
         fill = paste(f1))+
    theme(plot.title = element_text(color = "black", size = 16, face = "bold.italic", hjust = 0.5),
          axis.title.x = element_text(color="black", size=10, face="bold"),
          axis.title.y = element_text(color="black", size=10, face="bold"),
          axis.text.x =element_text(face="bold", size = 10),
          axis.text.y = element_text(face="bold", size = 12),
          #text = element_text(face = "bold", size = 10),
          legend.position = "bottom",
          legend.background = element_rect(fill="lightgray"),
          axis.line = element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
 return(c1)
}

########################################################################################################################
#Clean Output
########################################################################################################################


library(data.table)
library(openxlsx)

filepath <- "path"

nm_neutral4 <-  "Model_Coefficients.csv"
df_neutral4 <- fread(paste0(filepath, nm_neutral4))

model_frmt <-function(df, ptrn, fmla, valvar){
  df1 <- dcast(subset(df, grepl(ptrn, variable)), as.formula(fmla), value.var = valvar) %>%
    as.data.table(.) %>%
    .[, Var := substr(variable,1,nchar(variable)-3)]
    
  return(df1)
}



df_neu4_neg_coef <- model_frmt(df_neutral4, glob2rx("*neg"), "Desc + Model + variable ~ Dependent_Var", 'Coefficients')
colnames(df_neu4_neg_coef) <- c("Desc", "Model", "variable", "Negmodel_Coef_neg", "Negmodel_Coef_pos", "Var")
df_neu4_neg_coef1 <- df_neu4_neg_coef[,  c("Desc", "Model", "Var", "Negmodel_Coef_neg", "Negmodel_Coef_pos")]

df_neu4_neg_pval <- model_frmt(df_neutral4, glob2rx("*neg"), "Desc + Model + variable ~ Dependent_Var", 'p.value')
colnames(df_neu4_neg_pval) <- c("Desc", "Model", "variable", "Negmodel_pval_neg", "Negmodel_pval_pos", "Var")
df_neu4_neg_pval1 <- df_neu4_neg_pval[,  c("Desc", "Model", "Var", "Negmodel_pval_neg", "Negmodel_pval_pos")]


df_neu4_pos_coef <- model_frmt(df_neutral4, glob2rx("*pos"), "Desc + Model + variable ~ Dependent_Var", 'Coefficients')
colnames(df_neu4_pos_coef) <- c("Desc", "Model", "variable", "Posmodel_Coef_neg", "Posmodel_Coef_pos", "Var")
df_neu4_pos_coef1 <- df_neu4_pos_coef[,  c("Desc", "Model", "Var",  "Posmodel_Coef_neg", "Posmodel_Coef_pos")]

df_neu4_pos_pval <- model_frmt(df_neutral4, glob2rx("*pos"), "Desc + Model + variable ~ Dependent_Var", 'p.value')
colnames(df_neu4_pos_pval) <- c("Desc", "Model", "variable", "Posmodel_pval_neg", "Posmodel_pval_pos", "Var")
df_neu4_pos_pval1 <- df_neu4_pos_pval[,  c("Desc", "Model", "Var","Posmodel_pval_neg", "Posmodel_pval_pos")]

df_mdl_output <-Reduce(
  function(x, y, ...) merge(x, y, ...), 
  list(df_neu4_neg_coef1,df_neu4_pos_coef1,df_neu4_neg_pval1,df_neu4_pos_pval1)
)

df_factor_names <- data.table(Var = c('Q6','Q12','Q11','Q3','Q8','Q9','Q7','Q15','Q14','Q4','Q5','Q16','Q10','Q13','FA','FB','FC','FD','FE', 'FF'),
                        Factor = c('A*','A','A','B','B','B','B','C','C','D','D','E','E','F*', 'A','B', 'C', 'D', 'E','F' ))

df_mdl_output <- merge(df_mdl_output, df_factor_names, by = "Var", all.x = TRUE)
var_df_mdl_output <- c( "Desc", "Model","Factor","Var",
                       "Negmodel_Coef_neg", "Negmodel_Coef_pos", "Posmodel_Coef_neg", "Posmodel_Coef_pos",
                       "Negmodel_pval_neg", "Negmodel_pval_pos", "Posmodel_pval_neg", "Posmodel_pval_pos")
df_mdl_output <- setDT(df_mdl_output)[,var_df_mdl_output, with = FALSE]
 

outname <- "Model_Coefficients.xlsx"
wb1 <- createWorkbook()
sheetname = "Models"
addWorksheet(wb1,sheetname )
writeData(wb1, sheet=sheetname,  x = df_mdl_output)
saveWorkbook(wb1, paste0(filepath,outname), overwrite = TRUE)

########################################################################################################################
#Get Dummy Together
########################################################################################################################
df_mdl <- df_data4
var_cat_01 <- c("Q1", "FA", "FB", "FC", "FD", "FE", "FF")

#get all the dummy variables together & set names in one function
v_01 <- lapply(var_cat_01, function(x) {
  ptrn <- varnames(glob2rx(paste0(x,"*")),df_mdl)
  ptrn_not <-  varnames(glob2rx(paste0(x,"neu","*")),df_mdl)
  rtrn_ptrn <- paste0(ptrn[ifelse(length(which(ptrn %in% ptrn_not)) == 0, 1:length(ptrn), -which(ptrn %in% ptrn_not))],
                      collapse=",")
  return(rtrn_ptrn)})

names(v_01)<- var_cat_01
for (i in 1:length(v_01)){
  assign(var_cat_01[i],c(v_01[[i]]))
  print(paste(var_cat_01[i], c(v_01[[i]]),sep = " = "))}


########################################################################################################################
#Run Models together
########################################################################################################################
t1 <- Sys.time()
t1
mdl_descr <- c( "MDL_A", "MDL_B")
model_items <- list (  MDL_A = c(Q, A),
                      MDL_B = c(Q, B))

 m1 <- lapply(1:length(model_items),
              function(x) getCoeffMNL(df_mdl,unlist(model_items[[x]]),filepath,outname,mdl_descr[x]))
 m1 <- do.call(c,m1)

#m1 <- getCoeffMNL(df_mdl, model_items,filepath, outname,mdl_descr)
t2 <- Sys.time()
t2
t2 - t1



########################################################################################################################
#Run Plots
########################################################################################################################
m1 <- fread(paste0(filepath, outname))
df <- m1 [variable != "(Intercept)"]

#Plots of all models
df$variable <- as.character(df$variable)
filter_var <- "Model"
title_var <- "Model"
x1 <- "variable"
y1 <- "Coefficients"
f1 <- "Dependent_Var"
fitvar <- "McFaddenAdj"
pvalvar <-"p.value" 
#title_var <- unique(dt$Model)


All_Mdl_Plots <- lapply(unique(df[[filter_var]]), function(x) mdl_coef_plot(df[eval(parse(text = filter_var)) == x,],
                                                                            x1, 
                                                                            y1, 
                                                                            f1,
                                                                            title_var,
                                                                            fitvar,
                                                                            pvalvar,
                                                                            pval = 0.05))

All_Mdl_Plots

#Plots of all Dependent Variables
df <- df[, NModels := uniqueN(Model), by = "variable"]
df$variable <- as.character(df$variable)
filter_var <- "variable"
title_var <- "variable"
x1 <- "Desc"
y1 <- "Coefficients"
f1 <- "Dependent_Var"
fitvar <- "NModels" #this needs to be something that is empty or something that is constant and is in the dataset as we are comparing this across modesl
pvalvar <-"p.value"


Var_Plots_AllMdl <- lapply(unique(df[[filter_var]]), function(x) mdl_coef_plot(df[eval(parse(text = filter_var)) == x,],
                                                                               x1, 
                                                                               y1, 
                                                                               f1, 
                                                                               title_var,
                                                                               fitvar,
                                                                               pvalvar,
                                                                               pval = 0.05))

Var_Plots_AllMdl
#Table of Models
var_to_keep_01 <- c("Desc","Model","McFaddenAdj")
df_tbl <- df[,..var_to_keep_01]%>% unique()
print(df_tbl)


#Print it to a pdf
filepath <- "/Analysis/"
plot_name <- "neutral.pdf"

pdf(paste0(filepath,plot_name),onefile = TRUE)
All_Mdl_Plots
Var_Plots_AllMdl
dev.off()



library(xtable)
filepath <-  "Analysis/"
latex_name <-"list.tex"

addtorow          <- list()
addtorow$pos      <- list()
addtorow$pos[[1]] <- c(0)
addtorow$command  <- c(paste(
  "\\hline \n",
  "\\endhead \n",
  "\\hline \n",
  "{\\footnotesize Continued on next page} \n",
  "\\endfoot \n",
  "\\endlastfoot \n",
  sep=""))

latex <- print.xtable(xtable(df_tbl), 
                      print.results = FALSE,
                      tabular.environment="longtable",
                      floating=FALSE,
                      include.rownames = FALSE,  # because addtorow will substitute the default row names 
                      add.to.row = addtorow,     # this is where you actually make the substitution
                      hline.after=c(-1))         # because addtorow will substitute the default hline for the first row
#latex
writeLines(
  c(
    "\\documentclass[12pt]{article}",
    "\\usepackage{longtable}",
    "\\usepackage[landscape]{geometry}",
    "\\usepackage{booktabs}",
    "\\usepackage{tabularx, rotating, makecell, cellspace, caption}",
    "\\usepackage{blindtext}",
    "\\begin{document}",
    "\\thispagestyle{empty}",
    latex,
    "\\end{document}"
  ),
  paste0(filepath,latex_name)
)



tools::texi2pdf(paste0(filepath,latex_name), clean = TRUE)
