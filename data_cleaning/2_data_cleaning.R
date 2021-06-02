#format date as year
lbdata <- livebirths %>%
  mutate(DTNASC = format(DTNASC, format="%Y")) %>%
  as.factor(lbdata$DTNASC) # transform DTNASC in factor

#adjust variable education
lbdata$ESCMAE <- as.character(lbdata$ESCMAE)
lbdata$educ[(lbdata$ESCMAE == "8 a 11 anos")|(lbdata$ESCMAE == "9 a 11 anos")] <- "8-11 years"
lbdata$educ[lbdata$ESCMAE == "12 anos ou mais"] <- "12 years or more"
lbdata$educ[(lbdata$ESCMAE == "0")|(lbdata$ESCMAE =="Nenhum")|(lbdata$ESCMAE == "1 a 3 anos") |(lbdata$ESCMAE == "4 a 7 anos")] <- "0-7 years"
lbdata$ESCMAE <- as.factor(lbdata$ESCMAE)
lbdata$educ <- as.factor(lbdata$educ) #transform educ in factor 

#reorder levels of educ
lbdata$educ <- factor(lbdata$educ, levels = c("0-7 years", "8-11 years", "12 years or more"))
levels(lbdata$educ)

#adjust variable ethnicity 
lbdata <- lbdata %>% replace_with_na(replace = list(RACACOR = 9))
lbdata$RACACOR <- fct_drop(lbdata$RACACOR, only = "9")

lbdata <- ldata %>%
  mutate(RACACOR = recode(RACACOR, Parda = "Parda",
                          Preta = "Black",
                          Branca = "White", 
                          Yellow = "Asian",
                          Indígena = "Indigenous"))
lbdata$RACACOR <- as.factor(lbdata$RACACOR) #transfor RACACOR in factor

#reorder levels of ethnicity 
lbdata$RACACOR <- factor(lbdata$RACACOR, levels = c("White", "Black", "Asian", "Parda", "Indigenous"))
save(lbdata, file = "lbsample.Rda")


#adjust marital status 
lbdata$ESTCIVMAE <- as.character(lbdata$ESTCIVMAE)
lbdata$ESTCIVMAE[lbdata$ESTCIVMAE == "Separada ou Viúva"] <- "Solteira"
lbdata$ESTCIVMAE <- as.factor(lbdata$ESTCIVMAE) #transform ESTCIV in factor


#create dummy order of birth 
lbdata$order_dummy <- 0
lbdata$order_dummy[lbdata$QTDFILVIVO == 0] <- 1 #first birth
lbdata$order_dummy[lbdata$QTDFILVIVO > 0] <- 0 #high order birth
lbdata$order_dummy <- as.factor(lbdata$order_dummy) #transform order_dummy in factor 

#create regions based on municipality 
regions.list <- list(North = c("AC", "AP", "AM", "PA", "RO", "RR", "TO"), 
                     Northeast = c ("AL", "BA", "CE", "MA", "PB", "PE", "PI", "RN", "SE"),
                     Central-West = c("GO", "MT", "MS", "DF"),
                     South = c("PR", "SC", "RS"), 
                     Southeast = c("ES", "MG", "RJ", "SP"))
lbdata$Region <- sapply(lbsample$MUN,
                        function(x) names(regions.l)[grep(x,regions.l)])
lbdata$Region <- as.factor(lbsample$Region) #transform Region in factor

#reorder levels of Region 
lbdata$Region <- factor(lbdata$Region, levels = c("North", "Northeast", "Central-West", "South", "Southeast"))
levels(lbdata$Region)

#create dummy dependent variable
lbdata$age_dummy <- 0
lbdata$age_dummy[lbdata$IDADEMAE<=19] <- 1 #teenage pregnancy
lbdata$age_dummy[lbdata$IDADAME>19] <- 0 #not teenage pregnancy 
lbdata$age_dummy <- as.factor(lbsample$age_dummy) #transfor age_dummy in factor

#remove all NAs
lbdata <- na.omit(lbdata)
save(lbdata, file = "lbdata.Rda")