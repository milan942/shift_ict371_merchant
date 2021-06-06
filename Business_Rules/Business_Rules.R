#A. Code to edit column names
colnames(mth_data) <- c("Site", "Month", "IDMH", "Scheme",
                        "Interchange_Type", "Type", "ID",
                        "Sales_Returns_No",
                        "Sales_Returns_Amount", "Interchange_Rate",
                        "Interchange_Amount", "Acquiring_Charge_Rate")

#B. Code to edit data type in columns.
mth_data <- mth_data %>%
  mutate(Sales_Returns_Amount = as.numeric(str_replace(Sales_Returns_Amount, ",", "")),
         Sales_Returns_No = as.numeric(str_replace(Sales_Returns_No, ",", "")),
         Interchange_Amount = as.numeric(str_replace(Interchange_Amount, ",","")),
         Interchange_Rate = as.numeric(sub("%", "", Interchange_Rate))/100,
         Acquiring_Charge_Rate = as.numeric(sub("%", "", Acquiring_Charge_Rate))/100)

#code for detecting string patterns for international cards.
mth_data %>% select(Interchange_Type, Sales_Returns_Amount, Sales_Returns_No) %>%
  filter(str_detect(Interchange_Type, "ASIA") |
           str_detect(Interchange_Type, "INTERNATIONAL") |
           str_detect(Interchange_Type, "JAPAN"))

#Lesson learned today: DON'T USE GROUP_BY FOR TOTAL SUMS. Also !str_detect will
#not show the string pattern you have searched, handy if you have one column
#you want to exclude from dataset.


#1. Channel(Currency conversion)
Sale_Returns_No_Total <- mth_data %>%
  select(Interchange_Type, Sales_Returns_No) %>%
  filter(str_detect(Interchange_Type, "ASIA") |
           str_detect(Interchange_Type, "INTERNATIONAL") |
           str_detect(Interchange_Type, "INTERREG") |
           str_detect(Interchange_Type, "AP") |
           str_detect(Interchange_Type, "JAPAN")) %>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Sale_Returns_No_Total

channel_currency_conversion <- mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount) %>%
  filter(str_detect(Interchange_Type, "ASIA") |
           str_detect(Interchange_Type, "INTERNATIONAL") |
           str_detect(Interchange_Type, "INTERREG") |
           str_detect(Interchange_Type, "AP") |
           str_detect(Interchange_Type, "JAPAN")) %>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

channel_currency_conversion

#2. Scheme Debit COntactless (Domestic)
#HAVE NOT USED INTERCHANGE TYPE 

#Step1
scheme_debit_volume_All<- mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate ) %>%
  filter(Interchange_Rate == .000020) %>%
summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

scheme_debit_volume_All

#Step2
scheme_debit_volume_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate ==.000020,
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

scheme_debit_volume_International

#Step3

scheme_debit_volume_Total<- scheme_debit_volume_All - scheme_debit_volume_International


#Step1
scheme_debit_count_All<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No) %>%
  filter(Interchange_Rate == .000020) %>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

scheme_debit_count_All

#Step2
scheme_debit_count_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate ==.000020,
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

scheme_debit_count_International

#Step3
scheme_debit_count_Total<- scheme_debit_count_All-scheme_debit_count_International

#Step1
Interchange_Amount_All<- mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Interchange_Amount) %>%
  filter(Interchange_Rate == .000020) %>%
  summarise(Interchange_Amount_Total = sum(Interchange_Amount))

Interchange_Amount_All

#Step2
Interchange_Amount_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Interchange_Amount) %>%
  filter(Interchange_Rate ==.000020,
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Interchange_Amount_Total = sum(Interchange_Amount))

Interchange_Amount_International

#Step3
Interchange_Amount_Total<- Interchange_Amount_All-Interchange_Amount_International

#3. Credit Contactless (Increase IC) - Visa Standard
#HAVE NOT USED INTERCHANGE TYPE 

Visa_Standard_Increase_IC_Volume_All<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000065, str_detect(Scheme, "Visa" )) %>%
 summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Visa_Standard_Increase_IC_Volume_All

Visa_Standard_Increase_IC_Volume_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000065, str_detect(Scheme, "Visa" ),
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Visa_Standard_Increase_IC_Volume_International

Visa_Standard_Increase_IC_Volume_Total<- Visa_Standard_Increase_IC_Volume_All-scheme_debit_volume_International
Visa_Standard_Increase_IC_Volume_Total

#Step1
Visa_Standard_Increase_IC_Count_All<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000065, str_detect(Scheme, "Visa" )) %>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Visa_Standard_Increase_IC_Count_All

#Step2
Visa_Standard_Increase_IC_Count_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000065, str_detect(Scheme, "Visa" ),
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Visa_Standard_Increase_IC_Count_International

#Step3
Visa_Standard_Increase_IC_Count_Total<- Visa_Standard_Increase_IC_Count_All-Visa_Standard_Increase_IC_Count_International
Visa_Standard_Increase_IC_Count_Total



#4. Credit Contactless (Decrease IC) - Visa Standard
#HAVE NOT USED INTERCHANGE TYPE

Visa_Standard_Decrease_IC_Volume_All<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000085, str_detect(Scheme, "Visa" )) %>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Visa_Standard_Decrease_IC_Volume_All

Visa_Standard_Decrease_IC_Volume_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000085, str_detect(Scheme, "Visa" ),
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Visa_Standard_Decrease_IC_Volume_International

Visa_Standard_Decrease_IC_Volume_Total<-Visa_Standard_Decrease_IC_Volume_All-Visa_Standard_Decrease_IC_Volume_International
Visa_Standard_Decrease_IC_Volume_Total


Visa_Standard_Decrease_IC_Count_All<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000085, str_detect(Scheme, "Visa" )) %>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Visa_Standard_Decrease_IC_Count_All

Visa_Standard_Decrease_IC_Count_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000085, str_detect(Scheme, "Visa" ),
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Visa_Standard_Decrease_IC_Count_International

Visa_Standard_Decrease_IC_Count_Total<-Visa_Standard_Decrease_IC_Count_All-Visa_Standard_Decrease_IC_Count_International
Visa_Standard_Decrease_IC_Count_Total

#5. Credit Contactless (Increase IC) - Platinum
#DOes not work need to filter on NZ PLatinum contactless only???


#1st Step 
Visa_Platinum_Increase_IC_Volume_All<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000125, str_detect(Scheme, "Visa" ),
  str_detect(Interchange_Type, "CONTACTLESS"))%>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Visa_Platinum_Increase_IC_Volume_All

#2nd Step - Filter on international cards (Check if there are any)

Visa_Platinum_Increase_IC_Volume_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate ==.000125, str_detect(Scheme, "Visa" ),
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Visa_Platinum_Increase_IC_Volume_International

#3rd Step
Visa_Platinum_Increase_Total_Amount<-Visa_Platinum_Increase_IC_Volume_All-Visa_Platinum_Increase_IC_Volume_International
Visa_Platinum_Increase_Total_Amount

#1st Step
Visa_Platinum_Increase_IC_Count_All<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000125, str_detect(Scheme, "Visa" ),
         str_detect(Interchange_Type, "CONTACTLESS"))%>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Visa_Platinum_Increase_IC_Count_All

#2nd Step - Filter on international cards (Check if there are any)
Visa_Platinum_Increase_IC_Count_International<-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate ==.000125, str_detect(Scheme, "Visa" ),
         (str_detect(Interchange_Type, "CONTACTLESS")),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Visa_Platinum_Increase_IC_Count_International

#3rd Step
Visa_Platinum_Increase_Total_Count<-Visa_Platinum_Increase_IC_Count_All-Visa_Platinum_Increase_IC_Count_International
Visa_Platinum_Increase_Total_Count

#6. Credit Contactless (Decrease IC) - Platinum

#1st Step
Credit_Contactless_Decrease_IC_Platinum_All <-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000185, str_detect(Scheme, "Visa" ))%>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Credit_Contactless_Decrease_IC_Platinum_All

#2nd Step - Filter on international cards (Check if there are any)
Credit_Contactless_Decrease_IC_Platinum_International <- mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000185, str_detect(Scheme, "Visa" ),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Credit_Contactless_Decrease_IC_Platinum_International

#3rd Step
Credit_Contactless_Decrease_Total_Amount<- Credit_Contactless_Decrease_IC_Platinum_All-Credit_Contactless_Decrease_IC_Platinum_International
Credit_Contactless_Decrease_Total_Amount

#1st Step
Credit_Contactless_Decrease_IC_Platinum_Count_All <-mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000185, str_detect(Scheme, "Visa" ))%>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Credit_Contactless_Decrease_IC_Platinum_Count_All

Credit_Contactless_Decrease_IC_Platinum_International <- mth_data %>%
  select(Interchange_Type, Sales_Returns_Amount, Interchange_Rate, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .000185, str_detect(Scheme, "Visa" ),
         (str_detect(Interchange_Type, "ASIA")) |
           (str_detect(Interchange_Type, "INTERNATIONAL")) |
           (str_detect(Interchange_Type, "INTERREG")) |
           (str_detect(Interchange_Type, "AP")) |
           (str_detect(Interchange_Type, "JAPAN")))%>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Credit_Contactless_Decrease_IC_Platinum_International

Credit_Contactless_Decrease_IC_Platinum_Count_All<-Credit_Contactless_Decrease_IC_Platinum_International
Credit_Contactless_Decrease_IC_Platinum_Count_All

#7. Mastercard Increases

#This piece of code is to filter corporate and commercial card transactions
#that can be later subtracted from the Mastercard total card transactions (for .005)
#to get the total Mastercard increases.
Mastercard_subtractions <- mth_data %>%
  select(Interchange_Rate, Interchange_Type, Sales_Returns_Amount, Scheme) %>%
  filter(Interchange_Rate == .005 &
           str_detect(Interchange_Type, "CORPORATE") &
           str_detect(Interchange_Type, "COMMERCIAL") &
           Scheme == "Mastercard") %>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Mastercard_subtractions


#This code is to calculate the all the Mastercard (corporate and non corporate) transactions.
Mastercard_volume <- mth_data %>%
  select(Interchange_Rate, Interchange_Type, Sales_Returns_Amount, Scheme) %>%
  filter(Interchange_Rate == .005 &
           Scheme == "Mastercard") %>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount))

Mastercard_volume


#This is the formula to calculate business rule #7.
Mastercard_Increases <- ((Mastercard_volume - Mastercard_subtractions)*0.01)

Mastercard_Increases


#This is a filter of corporate and commercial cards needed to work out the total count
#of Mastercard transactions (0.05). This will be subtracted from the total.
Mastercard_Sales_No_Count_Subtractions <- mth_data %>%
  select(Interchange_Rate, Interchange_Type, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .005 &
           Scheme == "Mastercard" &
           str_detect(Interchange_Type, "CORPORATE") &
           str_detect(Interchange_Type, "COMMERCIAL")) %>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Mastercard_Sales_No_Count_Subtractions


#This is the formula for the total filtered transactions for the count of
#Mastercard transactions (0.05)
Mastercard_Sales_No_Count <- mth_data %>%
  select(Interchange_Rate, Interchange_Type, Sales_Returns_No, Scheme) %>%
  filter(Interchange_Rate == .005 &
           Scheme == "Mastercard") %>%
  summarise(Sales_Returns_No_Total = sum(Sales_Returns_No))

Mastercard_Sales_No_Count


#This is the formula to calculate the count in business rule #7.
Mastercard_Sales_No_Total <- Mastercard_Sales_No_Count - Mastercard_Sales_No_Count_Subtractions



#8.

#This is the formula filter for the card types required in business rule #8,
#multiplied by the .9 weighting.
mth_data %>%
  select(Interchange_Rate, Interchange_Type, Sales_Returns_Amount, Scheme) %>%
  filter(Interchange_Rate >= .005 & Scheme == "Mastercard" &
           str_detect(Interchange_Type, "NZ CONSUMER SUPER PREMIUM 1-CP-CRD-WZNL") |
           str_detect(Interchange_Type, "NZ CONSUMER PREMIUM - CP-CREDIT-WNZL") |
           str_detect(Interchange_Type, "NZ CONSUMER PREMIUM - CP - PREPAID") |
           str_detect(Interchange_Type, "NZ CONSUMER STANDARD - CP - CREDIT") |
           str_detect(Interchange_Type, "NZ CONSUMER STANDARD - CP-CREDIT-WNZL") |
           str_detect(Interchange_Type, "CONSUMER PREMIUM ELECTRONIC")) %>%
  summarise(Sales_Returns_Amount_total = sum(Sales_Returns_Amount)*.9)


#This is to create new columns to filter the totals for the adjusted Interchange
#rates for question #8.
mth_data <- mth_data %>%
  mutate(Prepaid_Adjusted = Interchange_Rate - 0.002,
         Credit_Adjusted = Interchange_Rate - 0.005)

#This is the formula to get the filtered data for the adjusted prepaid
#interchange rates.
mth_data %>%
  select(Prepaid_Adjusted, Interchange_Type, Sales_Returns_No, Scheme) %>%
  filter(Scheme == "Mastercard" &
           str_detect(Interchange_Type, "NZ CONSUMER PREMIUM - CP - PREPAID")) %>%
  summarise(Sales_Returns_No_total = sum(Sales_Returns_No)*9)

#This is the formula to get the filtered data for the adjusted credit
#rates for question #8.
mth_data %>%
  select(Credit_Adjusted, Interchange_Type, Sales_Returns_No, Scheme) %>%
  filter(Scheme == "Mastercard" &
           str_detect(Interchange_Type, "NZ CONSUMER SUPER PREMIUM 1-CP-CRD-WZNL") |
           str_detect(Interchange_Type, "NZ CONSUMER PREMIUM - CP-CREDIT-WNZL") |
           str_detect(Interchange_Type, "NZ CONSUMER STANDARD - CP - CREDIT") |
           str_detect(Interchange_Type, "NZ CONSUMER STANDARD - CP-CREDIT-WNZL")) %>%
  summarise(Sales_Returns_No_total = sum(Sales_Returns_No)*.9)

