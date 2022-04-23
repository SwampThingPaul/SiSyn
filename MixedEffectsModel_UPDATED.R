#linear model
#install.packages("lme4")
#install.packages("MuMIn")
#install.packages("MASS")
#install.packages("cAIC4")
require(lme4)
require(MuMIn)
require(MASS)
require(cAIC4)
require(QuantPsyc)
require(ggalt)


setwd("/Users/keirajohnson/Box Sync/Keira_Johnson/SiSyn")

#read in data, rename columns, prepare for merge
trends<-read.csv("Trends_for_regression_UPDATED.csv")
trends<-trends[complete.cases(trends),]

site_chars<-read.csv("SiteChars_042322.csv")
site_chars<-site_chars[,-1]

#merge
trends_tot<-merge(trends, site_chars, by=c("LTER", "site"))

#convert all NA to 0
trends_tot[is.na(trends_tot)]<-0

#keep columns important for models
lm_file<-trends_tot[,c(1,3,4,9,13:15,18,19,22:32)]

#log drainage area - too large of a range for model
lm_file$drainSqKm<-log(lm_file$drainSqKm)

#write.csv(lm_file, "FinalMixedEffectsVariables.csv")

#scale data
final_center<-lm_file
final_center[c(4:20)]<-data.frame(sapply(final_center[c(4:20)], scale))

#examine ranges of centered and scaled data
ranges = vapply(final_center[,c(4:20)], FUN=function(col) c(min(col, na.rm = TRUE), max(col, na.rm = TRUE)), numeric(2))
rownames(ranges) = c("vmin", "vmax") 
rframe = as.data.frame(t(ranges))  # make ymin/ymax the columns
rframe$varName = rownames(rframe)

#plot ranges
ggplot()+geom_dumbbell(data=rframe, aes(y=varName, x=vmin, xend=vmax))+
  xlab("Centered/Scaled Value")+ylab("Variable")

#look at distribution
melt_final_center<-melt(final_center, id=c("LTER", "Conc_trend", "Yield_Trend"))
ggplot()+geom_histogram(data = melt_final_center, aes(x=value), binwidth = 0.5)+facet_wrap(~variable)+theme_bw()

#rename centered file back to main file
lm_file<-final_center

#create groups of variables
#n = nutrients
#g = geology
#l = landcover
#c = climate
#d = drainage area
n<-c("DSi+P+NOx")
g<-c("volcanic+sedimentary+carbonate_evaporite+metamorphic+plutonic")
l<-c("forest+imacted+shrubgrass+snow_ice+open_water+wetland")
c<-c("MAT+MAP")
d<-c("drainSqKm")

#create combination touples
#each letter is now associated with the groups created above
letters<-c("n", "g", "l", "c", "d")
group_touple<-list(list("n", n), list("g", g), list("l", l), list("c", c), list("d", d))

#create function that takes letters and creates unique combinations of letters in 
#range 1 through max length (1-5 letters long here)
generate_combinations<-function(items,max_length) {
  
  #open list to append combinations to
  combination_list<-list()
  
  #create combinations of length of 1:max length using "items" variable
  #order does not matter in these combinations
  #append each set of combinations to combination list
  for (i in 1:max_length) {
    
    combination_list[[i]] <- combn(items, i, simplify = FALSE)
    
  }
  
  return(combination_list)
  
}

#this function takes the list of letter combinations above and
#turns them into character strings and puts them into a dataframe
get_strings <- function(list) {
  
  #open list to append character strings into
  char_vectors<-list()
  
  #open for loop that will cycle through the list created in the 
  #"generate combinations" function
  #letters from each list element are appended together and added to a new, single layer list
  for (i in 1:length(list)) {
    
    characters<-as.data.frame(do.call(rbind, list[[i]]))
    
    char_vectors[[i]]<-unite(characters, col = "vectors", sep = "")
  }
  
  #rbind all strings in char_vectors list into dataframe
  char_df<-do.call(rbind, char_vectors)
  
  return(char_df)
    
}



#function will use letters in combination list to reference column names
variable_combinations<-function(comb_df) {
  
  #open list to append variable strings into
  var_list<-list()
  
  #open list to cycle through dataframe created in get_strings function
  for (i in 1:nrow(comb_df)) {
    
    #select first row of the dataframe
    comb<-comb_df[i,]
    
    #select first row of dataframe and create new dataframe
    comb_df_one<-as.data.frame(comb_df[i,])
    
    #now loop through each letter in each row of the dataframe   
    for (k in 1:length(letters)) {
      
      #create columns for each letter in "letters" variable
      #each column will contain the variable associated with each letter
      #when the letter is present in comb_df[,i], then the list of variables from the
      #tuple will paste into that column
      #if it is not present then NA will be pasted into it
      comb_df_one[,paste0(letters[k], "_val")]<-ifelse(grepl(group_touple[[k]][[1]], comb), 
      group_touple[[k]][[2]], NA)
      
    }    
    
    #remove the first column of the comb_df_one data frame - this column just contains "letter"_val
    comb_df_one<-comb_df_one[,-1]
    
    #combine all the columns (minus the one removed above), separated by a "+" and removing
    #the NA values
    #this will create one long string of variables for input into a linear model
    var_list[[i]]<-unite(comb_df_one, col = "variables", sep = "+", na.rm = TRUE)
  }
  
  #rbind the variable list
  var_list<-do.call(rbind, var_list)
  
  return(var_list)
  
}

###FROM FUNCTIONS ABOVE###

#generate lists from functions above
#list of combinations
combination_list<-generate_combinations(letters, 5)
#list to dataframe
char_vectors<-get_strings(combination_list)
#convert list of characters to associtaed varables
variable_strings<-variable_combinations(char_vectors)

#turn into string - use this string in model input below
Vars<-(variable_strings$variables)

#take absolute value of concentration and yield trends
#this way models will predict absolute change instead of most negative being smallest
#and more positive being larger
lm_file$Conc_trend<-abs(lm_file$Conc_trend)
lm_file$Yield_Trend<-abs(lm_file$Yield_Trend)

#get all models - change for either Conc or Yield
#use the vars list of variables generated above, add "+(1|LTER)" for mixed effects of LTER
#this will return list of formulas ready to be put into lmer models
allModelsList <- lapply(paste("Conc_trend ~", Vars, "+(1|LTER)"), as.formula)

#run models
#use output from above
allModelsResults <- lapply(allModelsList, function(x) lmer(x, data = lm_file, REML = FALSE))  

#get summaries of models
allModelsSummaries <- lapply(allModelsResults, summary)

#get r2 of models
allModelsR2 <- lapply(allModelsList, function(x) r.squaredGLMM(lmer(x, data = lm_file, REML = FALSE)))

#get AIC of models
allModelsAIC <- lapply(allModelsList, function(x) AIC(lmer(x, data = lm_file, REML = FALSE)))

#bind into dataframe
r2_df<-do.call(rbind, allModelsR2)
AIC_df<-do.call(rbind, allModelsAIC)
master_df<-cbind(char_vectors, r2_df, AIC_df)

write.csv(master_df, "Conc_LM_Models.csv")



#####FOR STEPWISE AIC####
#create overall model for step AIC - uses normal linear regression not mixed effects
###CHANGE FIRST VARIABLE FOR YIELD VS CONCENTRATION
lm_tot<-lm(Yield_Trend~DSi+NOx+P+sedimentary+carbonate_evaporite+metamorphic+plutonic+
              forest+imacted+shrubgrass+snow_ice+open_water+wetland+MAP+MAT+drainSqKm, lm_file)

#stepwise AIC
stepAIC(object = lm_tot, direction = "both", trace = TRUE)

#create best model from stepwise AIC - MAKE SURE THIS WAS CHANGED ABOVE
#for yield
lm_best<-lm(Yield_Trend~forest+shrubgrass+MAT+drainSqKm, data = lm_file)

#for concentration
lm_best<-lm(Conc_trend~DSi+sedimentary+carbonate_evaporite+metamorphic+
                  forest+shrubgrass+MAT, data = lm_file)

#assign summary of best model to dataframe to extract coefficients
sum<-summary(lm_best)

#get beta coefficients and make df
beta_df<-as.data.frame(lm.beta(lm_best))
beta_df$name<-rownames(beta_df)

#get vif values and make df
vif_df<-as.data.frame(vif(lm_best))
vif_df$name<-rownames(vif_df)

#make final df
lm_df<-as.data.frame(sum$coefficients)
lm_df$name<-rownames(lm_df)
lm_df<-merge(lm_df, beta_df, by="name", all.x = TRUE)
lm_df<-merge(lm_df, vif_df, by="name", all.x = TRUE)

#WRITE TO CSV - CHANGE NAME FOR CONCENTRATION VS YIELD
write.csv(lm_df, "Yield_AIC_LM.csv")
