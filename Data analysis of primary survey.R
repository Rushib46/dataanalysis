
# Dependencies ------------------------------------------------------------

library(tidyverse) 
library(readxl) 
library(googlesheets4)
library(lubridate) 

sheet =
    "<https://docs.google.com/spreadsheets/d/1HhDGJIx--P7KJ6WsjwaSFjm9dj4-caQHX_ux4gAMYEk/edit#gid=0>"

all_data <- 
  "C:/Users/91982/Documents/Three Fold/Three Fold Monitoring/R script/Agriculture_livelihood_HH_Survey (1).xlsx"

# Import data set ----------------------------------------------------------
read_xlsx("C:/Users/91982/Documents/Three Fold/Three Fold Monitoring/R script/Agriculture_livelihood_HH_Survey (1).xlsx")

data <- read_xlsx (path =  
        "C:/Users/91982/Documents/Three Fold/Three Fold Monitoring/R script/Agriculture_livelihood_HH_Survey (1).xlsx", 
        sheet = "data")

HH_group <- read_xlsx(path = all_data, sheet = "HH_group")

crop_repeat <- readxl::read_xlsx(path = all_data,sheet = "CROP_repeat")

crp_season <-   read_xlsx(path = all_data,sheet="crp_season")

QF2_3repeat <- read_xlsx(path = all_data, sheet = "QF2_3repeat")

QG3_3_repeat <-
  read_xlsx(path = all_data,sheet="QG3_3_repeat")

QG3_4_repeat <- read_xlsx(path = all_data, sheet = "QG3_4_repeat")

QG3_5_repeat <- read_xlsx(path = all_data, sheet = "QG3_5_repeat")

QG4_3_repeat <- read_xlsx(path = all_data, sheet = "QG4_3_repeat")

QG4_4_repeat <- read_xlsx (path = all_data, sheet = "QG4_4_repeat")

QG5_2_repeat <- read_xlsx(path = all_data, sheet = "QG5_2_repeat")

QG5_3_repeat <- read_xlsx(path = all_data, sheet = "QG5_3_repeat")

QG5_12_repeat <- read_xlsx(path = all_data, sheet = "QG5_12_repeat")

QP1_repeat <- read_xlsx(path = all_data, sheet = "QP1_repeat")

QH1_repeat <- read_xlsx(path = all_data, sheet = "QH1_repeat")

QI1_7repeat <- read_xlsx(path = all_data, sheet = "QI1_7repeat")

QI2_repeat <- read_xlsx(path = all_data, sheet = "QI2_repeat")

QJ1_repeat <- read_xlsx(path = all_data, sheet = "QJ1_repeat")

NOL <- read_xlsx(path = all_data, sheet = "NOL")

QM2_3_repeat <- read_xlsx(path = all_data, sheet = "QM2_3_repeat")

# Merge data set as per sections ------------------------------------------

crop_details <- # All crop details and groups in this object \#
crop_repeat %>% 
  rename("MAIN_KEY" = "KEY") %>% 
full_join((crp_season) %>% 
            # rename(c("SEASON_KEY" ="KEY" ,
    ("SEASON_PARENT_KEY" = "PARENT_KEY"), by=c("MAIN_KEY"="SEASON_PARENT_KEY")) %>% 
full_join((QF2_3repeat) %>% 
         "QF2_3repeat_PARENT_KEY" = "PARENT_KEY" , by=c("MAIN_KEY" = "KEY")) %>% 
crop_repeat %>% full_join(QG3_3_repeat,by=c("KEY"="PARENT_KEY")) %>% 
full_join(QG3_4_repeat,by=c("KEY"="PARENT_KEY")) %>% 
full_join((QG3_5_repeat %>% mutate(across(.cols = PARENT_KEY,as.character))),by=c("KEY"="PARENT_KEY")) %>% 
full_join((QG4_3_repeat %>% mutate(across(.cols = PARENT_KEY,as.character))),by=c("KEY"="PARENT_KEY")) %>% 
full_join((QG4_4_repeat %>% mutate(across(.cols = PARENT_KEY,as.character))),by=c("KEY"="PARENT_KEY")) %>% 
full_join(QG5_2_repeat,by=c("KEY"="PARENT_KEY")) %>% 
full_join(QG5_3_repeat,by=c("KEY"="PARENT_KEY")) %>%
full_join((QG5_12_repeat %>% mutate(across(.cols = PARENT_KEY,as.character))),by=c("KEY"="PARENT_KEY")) %>%
full_join(QP1_repeat,by=c("KEY"="PARENT_KEY"))

master_crop <- data %>%  
  full_join(crop_repeat, by = c("KEY" = "PARENT_KEY"))

master_data <- data %>%
  full_join( HH_group, by = c("KEY" = "PARENT_KEY")) %>% 
  full_join(NOL, by = c("KEY" = "PARENT_KEY")) %>% 
  full_join(QM2_3_repeat, by= c("KEY" = "PARENT_KEY"))

CSA_data <-data %>% 
    full_join(QP1_repeat, by = c("KEY" = "PARENT_KEY"))

livestock_data <- data %>% 
  rename("MAIN_d_KEY" = "KEY") %>% 
  full_join((QH1_repeat %>% rename("L_KEY" = "KEY", "L_PARENT_KEY" = "PARENT_KEY")), by = c("MAIN_d_KEY" = "L_PARENT_KEY"))

hh_assets <- data %>% 
  full_join(QI1_7repeat, by = c("KEY" = "PARENT_KEY"))

prod_capital <- data %>%
  full_join(QI2_repeat, BY = C("KEY" = "PARENT_KEY"))

schemes_data <- data %>% full_join( QJ1_repeat, by = c("KEY" = "PARENT_KEY"))

# Summary - Basic Details --------------------------------------------

# Average household size

avg_hh <- data %>% group_by(fpo) %>%
  summarise(n=n_distinct(uid), avg_hh = mean(hhidsize))

range_write(ss = sheet, data = avg_hh, range = "summary!A2")

#Intervention group details

int_group <- data %>% 
  group_by(int_group_calc) %>% 
  summarise(count = n())

range_write(ss = sheet, data = int_group, range = "summary!A10")

#Intervention group and fpo wise data collection status.

int_fpo <- data %>% select(uid,fpo,int_group_calc) %>% 
  group_by(fpo, int_group_calc) %>% summarise(n = n_distinct(uid)) %>%
  pivot_wider(names_from = int_group_calc, values_from = n)

range_write(data = int_fpo, ss= sheet, range = "summary!H4")

#Land conversion to acres 
land <- data %>%  
  select(c(uid,fpo,QC1,QC1_1)) %>% drop_na() %>% 
  mutate(new_QC1=
  case_when(QC1_1==1~QC1/100, 
            QC1_1==2~QC1, 
            QC1_1==3~QC1/60,
            QC1_1==4~QC1/3))

avg_land <- land %>% 
  group_by(fpo) %>% 
  summarise(n = n_distinct(uid), avg_land_acres = mean(QC1_1))

range_write(data = avg_land, ss = sheet, range = "summary!A18")

# Income -----------------------------------------------------------

#Average income of the household - half annual

income_int <-  data %>% 
  group_by(int_group_calc)  %>% 
  summarise(n=n_distinct(uid), avg_income = mean(QB4_1), MX = max(QB4_1))

range_write(ss= sheet, data = income_int, range = "income!A3")

#Expenditure details of hh - half annual

exp <- data %>% 
  group_by(int_group_calc) %>% 
  summarise(n = n_distinct(uid), Avg_expd = mean(QB4_2), MX = max(QB4_2), MN = min(QB4_2))

range_write(data = exp, ss = sheet, range = "income!A12")

#Income from livestock

livestock_incm <- livestock_data %>% drop_na() %>%  
  group_by(int_group_calc) %>% 
  summarise(n = n_distinct(uid), Avg_income = mean(QH1_5)) 

view(livestock_data)

# crop ---------------------------------------------------------------------

#Crop details 

cropdata %>% filter(is.na(QD1)) %>% 
  count() %\% view()

cropwise <- data %>% 
  inner_join(crop_repeat, by =c ("KEY" = "PARENT_KEY")) %>% 
  # filter(!is.na(crp_id1)) %\>% select(uid, crp_id1QD1) %>% group_by(QD1) %>% 
  summarise(n = n_distinct(uid)) %>%  
  pivot_wider(names_from = crp_id1, values_from = n)

range_write(ss = sheet, data = cropwise, range = "crop!A5")

#Total harvested quantity in tons

harv <- crop_repeat %>% 
  select(c(crp_id1,QE3, QE3_1)) %>% drop_na() %>% 
  mutate(new_QE3= 
           case_when(QE3_1==1~QE3/2000, 
                     QE3_1==2~QE3/1000,
                     QE3_1==3~QE3/100, 
                     QE3_1==4~QE3, 
                     QE3_1==99~QE3)) %>%
group_by(crp_id1) %>% summarise(n = n(), avg = mean(new_QE3))
#pivot_wider(values_from = n, names_from = new_QE3) %\>% view()

range_write(data = harv, ss = sheet, range = "crop!A20")

#Total cost for seeds

seed_cost <- master_crop %>% 
  mutate(crop_name = 
           case_when(crp_id1 == 1 ~ "gram", 
                     crp_id1 == 2 ~ "Groundnut", 
                     crp_id1 == 3 ~ "Paddy", 
                     crp_id1 == 4 ~ "Maize", 
                     is.na(crp_id1) ~ "NA", 
                      TRUE ~ as.character(crp_id1))) %>% 
  group_by(crop_name) %>% drop_na(QG1_Total_Seed_Cost) %>%
  summarise(n = n_distinct(uid), avg_seed_cost = mean(QG1_Total_Seed_Cost))

range_write(ss= sheet, data = as.data.frame(seed_cost), range = "crop!H10")

#Total cost for seed sowing seed_sowing <- master_crop %>%
mutate(crop_name = 
         case_when(crp_id1 == 1 ~ "gram", 
                   crp_id1== 2 ~ "Groundnut", 
                   crp_id1 == 3 ~ "Paddy", 
                   crp_id1 == 4 ~ "Maize",
                    is.na(crp_id1) ~ "NA", 
                    TRUE ~ as.character(crp_id1))) %>%
    group_by(crop_name) %>% drop_na(QG1_16) %>% 
  summarise(n = n_distinct(uid), avg_seed_cost = mean(QG1_16), Max = max(QG1_16), MIN = min(QG1_16))

range_write(ss = sheet, data = as.data.frame(seed_sowing), range = "crop!N10")

#Cost for irrigation

Irrigation_cost <- master_crop %>% 
  group_by(crp_id1) %>% drop_na(QG5_7) %>% 
  summarise(n = n_distinct(uid), avg_irr_cost = mean(QG5_7), MIN = min(QG5_7), Max = max(QG5_7))

#Total cost for packaging operation
packaging_cost <- master_crop %>%  
  group_by(int_group_calc) %>% drop_na() %>% 
  summarise(n)

#Total cost for local transportation


# Marketing Channel

Quantity_sold <- master_crop %>% 
  mutate(crop_name = case_when(crp_id1 == 1 ~ "gram", 
                               crp_id1 == 2 ~ "Groundnut", 
                               crp_id1 == 3 ~ "Paddy",
                                crp_id1 == 4 ~ "Maize", 
                                is.na(crp_id1) ~ "NA", 
                              TRUE ~ as.character(crp_id1))) %>% 
  mutate(quantity = case_when(QE3_1 == 1 ~ "kg" ,  
                              QE3_1 == 2 ~ "Litre",  
                              QE3_1 == 3 ~ "ML", 
                              QE3_1 == 4 ~ "NUMBERS", 
                              QE3_1 == 5 ~ "BOX",  
                              QE3_1 == 6 ~ "BAGS", 
            TRUE ~ as.character(QE3_1))) %>% 
  group_by(crop_name, quantity, int_group_calc) %>% drop_na(QE4_6) %>% 
  summarise( n = n_distinct(uid), avg_sold = mean(QE4_6), MX = max(QE4_6), MN = min(QE4_6))

#Price Received when selling crop

Price <- master_crop %>% 
  group_by(int_group_calc) %>% drop_na(QF2_Total_Sales_Value) %>% 
  summarise(n = n_distinct(uid), Maxi = max(QF2_Total_Sales_Value), Mini = min(QF2_Total_Sales_Value)) 

# Credit Details ----------------------------------------------------------

credit_data <- data %>% 
  full_join(NOL,c("KEY" = "PARENT_KEY"))

#Number of people who have taken loan

n_loan <- 
  credit_data %>% filter(QK1>0) %>% group_by(fpo) %>%
  summarise(n= n_distinct(uid))

range_write(ss= sheet, data = n_loan, range = "credit!A20")

#Loan amount greater than 10 lakh

credit <- credit_data %>% 
  select(uid, LN, QK2_3) %>% 
  filter(QK2_3 - > 1000000) %>% arrange(QK2_3)

loan <- credit_data %>% 
  drop_na(QK2_3) %>% 
  summarise(n = n_distinct(uid), avg_loan_SZ = mean(QK2_3), MX = max(QK2_3), MN = min(QK2_3)) 

range_write(ss = sheet, data = loan, range = "credit!A12")

#Loan taken by intervention group type 
loantaken <- crdt %>% 
  select(uid, QK1, int_group_calc) %>% filter(QK1 == 1) %>% 
  group_by(int_group_calc) %>% 
  summarise(n = n_distinct(uid)

range_write(ss= sheet, data = loantaken, range = "credit!H10")

#Total number of household who have taken/not taken loan

yn_loan <- crdt %>% group_by(QK1) %>% 
  summarise(n = n_distinct(uid)) %>% view()

range_write(data = yn_loan, ss = sheet, range = "credit!A4")

#Average Loan size 
loan_avg <- crdt %>% 
  select(uid, QK2_3) %>% drop_na() %>% 
  summarise(n = n_distinct(uid), avg = mean(QK2_3), MX = max(QK2_3), MN = min(QK2_3))

range_write(data= loan_avg, ss = sheet, range = "credit!A10")

#Average savings of the household

savings <- data %>% 
  select(uid, QL4, int_group_calc) %>% group_by(int_group_calc) %>% drop_na() %>% 
  summarise(n = n_distinct(uid), avg_savings = mean(QL4), MX = max(QL4), MN = min(QL4))

total_savings <- data %>% 
  select(uid, QL4) %>% drop_na() %>%
  summarise(n = n_distinct(uid), t_avg_saving = mean(QL4), MX = max(QL4), MN = min(QL4))

range_write(ss = sheet, data = total_savings, range = "credit!I4")

#Outlier - savings greater than 10000 - show the values and farmer details  

data %>% group_by(fpo) %>% filter(QL4 >10000) %>%
  summarise(n = n_distinct(uid), QL4 )

# Livestock ---------------------------------------------------------------------

livestock <- data %>% 
    inner_join( QH1_repeat, by = c("KEY"= "PARENT_KEY") ) %>% 
    select(uid,int_group_calc, LiveStk_repeat1,LiveStk_id1) %>% 
    group_by(int_group_calc,LiveStk_id1) %>% 
    summarise(n=n_distinct(uid)) %>% 
    pivot_wider(values_from = n,names_from = int_group_calc)

range_write(data = livestock,ss = sheet, range = "livestock!A4")

# Training attended -------------------------------------------------------

#Technical support on agricultural operations

agri_support <- master_data %>% 
  filter(QM2== 1) %>% group_by(fpo,int_group_calc) %>% summarise(n = n_distinct(uid)) %>%
  pivot_wider(names_from = fpo, values_from = n) 

#Financial Literacy training attended by farmer

fit <- master_data %>% 
  filter(QL9 == 1 ) %>% group_by(fpo) %>% summarise(n = n_distinct(uid))

# FPO Facilitation --------------------------------------------------------

#Financial Literacy \# Number of people who have got training from their
FPO

FI_LIT <- master_data %>% 
  filter(QL10 == 1) %>% group_by(fpo,int_group_calc) %>% 
  summarise(n= n_distinct(uid))

#Agriculture operation support provided by FPO

Agri_training <- master_data %>% 
  select(uid, fpo, int_group_calc, QM2_3) %>% filter(QM2_3 >= 0) %>% 
  group_by (fpo, int_group_calc) %>%
  summarise(n = n_distinct(uid)) %>% 
  pivot_wider(names_from = fpo, values_from = n) 

#Number of people who have got facilitation from their FPO/BAU/Activator

ag_fpo <- master_data %>% filter(QM2_3 == 0) %>%
  group_by(int_group_calc) %>%
  summarise(n = n_distinct(uid)) 

# Women Empowerment -------------------------------------------------------

#Women involved in decision making

WE <- master_data %>% 
  filter(QO1 == 1) %>% group_by(int_group_calc) %>% 
  summarise(n = n_distinct(uid)) 

#Women monetary contribution to the household

Women_money_hh <- master_data %>% 
  group_by(int_group_calc) %>%drop_na(QO4) %>% 
  summarise(n = n_distinct(uid), avg = mean(QO4), MX = max(Q04), MN = min(QO4))


money_hh <- master_data %>% 
  select(uid, fpo, QO4, int_group_calc) %>% group_by(fpo) %>% drop_na(QO4) %>% 
  summarise(n = n_distinct(uid), MAX = max(QO4)) %>%
  view()


# cropwise ----------------------------------------------------------------

 
cropwise <-  master_crop %>% 
  select(uid, int_group_calc, crp_id1, fpo, QC1, QG1_16, QG2_8, QG3_6, QG4_5, QG5_7, QG6_22, QG6_28) %>% 
  drop_na() %>% 
  group_by(int_group_calc, crp_id1) %>% 
  summarise(n =n_distinct(uid), avg = mean(QG1_16 + QG2_8 + QG3_6  + QG6_28 + QG5_7+ QG6_22+ QG6_28)) %>%  view()

cropwise_avg <-  master_crop %>%  drop_na() %>% 
  mean(QG1_16 , QG2_8 , QG3_6 ,QG6_28T ,QG6_28) %>%  view()

cultivated_land <- master_crop %>% 
  select(c(uid,fpo,QC1,QC1_1, crp_id1, int_group_calc)) %>% drop_na() %>% 
  mutate(new_QC1=
           case_when(QC1_1==1~QC1/100, 
                     QC1_1==2~QC1, 
                     QC1_1==3~QC1/60,
                     QC1_1==4~QC1/3)) %>% 
  group_by(crp_id1) %>% 
  summarize(n = n_distinct(uid) , avg = mean(new_QC1))

range_write(ss = sheet, data = cultivated_land, range =  "crop!J4")


cultivated_land <- master_crop %>% 
  select(c(uid,fpo,QC1,QC1_1, crp_id1, int_group_calc)) %>% drop_na() %>% 
  mutate(new_QC1=
           case_when(QC1_1==1~QC1/100, 
                     QC1_1==2~QC1, 
                     QC1_1==3~QC1/60,
                     QC1_1==4~QC1/3)) %>% 
  group_by(crp_id1) %>% 
  summarize(n = n_distinct(uid) , avg = mean(new_QC1))



yield <- master_crop %>% 
  mutate(crop_name = case_when(crp_id1 == 1 ~ "gram", 
                               crp_id1 == 2 ~ "Groundnut", 
                               crp_id1 == 3 ~ "Paddy",
                               crp_id1 == 4 ~ "Maize", 
                               is.na(crp_id1) ~ "NA", 
                               TRUE ~ as.character(crp_id1))) %>% 
  mutate(new_QC1 =  case_when(QC1_1==1~QC1/100, 
                              QC1_1==2~QC1, 
                              QC1_1==3~QC1/60,
                              QC1_1==4~QC1/3))%>% 
  mutate(quantity = case_when(QE3_1 == 1 ~ "kg" ,  
                              QE3_1 == 2 ~ "Litre",  
                              QE3_1 == 3 ~ "ML", 
                              QE3_1 == 4 ~ "NUMBERS", 
                              QE3_1 == 5 ~ "BOX",  
                              QE3_1 == 6 ~ "BAGS", 
                              TRUE ~ as.character(QE3_1))) %>% 
  group_by(crop_name, quantity) %>% drop_na(QE4_6) %>% 
  summarise( n = n_distinct(uid), avg_sold = mean(QE4_6), MX = max(QE4_6), MN = min(QE4_6)) %>%  view()

range_write( ss = sheet, data = yield, range = "crop!J18")

crop_income <- master_crop %>%  
  group_by( crp_id1) %>% 
  summarise(n = n_distinct(uid), avg = mean(QF2_11), MX = max(QF2_11), MN = min(QF2_11))

range_write(ss = sheet, data = yield, range = "crop!J18")


cost <- master_crop %>% select(crp_id1, uid, fpo) %>% 
  mutate(total_cost = sum(QG1_Total_Seed_Cost, QG1_16, QG2_8, QG3_6, QG4_5, QG5_7 )) %>% 
  summarise(M = n_distinct(uid), averag = mean(total_cost)) %>%  view()

  sum(QG1_1, QG1_16)

seed_avg <- master_crop %>%  
  group_by( crp_id1) %>% drop_na(QG1_Total_Seed_Cost) %>% 
  summarise(n = n_distinct(uid), avg =  sum(QG1_Total_Seed_Cost, QG5_7)) %>%  view()

            