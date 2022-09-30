

library(MCOE)
library(tidyverse)
library(odbc)
library(ggthemes)


con <- MCOE::mcoe_sql_con()

temp <- tbl(con, "ELAS") %>% 
    filter(
        AcademicYear == "2021-22",
 #       is.na(DistrictCode),
        AggLevel == "C",
 Gender == "ALL"
        ) 

local <- collect(temp)


EL.summation <- name <- function(df, grouper) {
   
    df %>%
    group_by({{grouper}}) %>%
    summarise(EEL = sum(Total_EE), 
              EL = sum(EL),
              Tot = sum(TotalEnrollment)
              ) %>%
    mutate(perc.EEL = EEL/Tot,
           perc.EL = EL/Tot
           ) %>%
    ungroup() %>%
    arrange(desc(perc.EEL))

    
}

summation <- EL.summation(local, CountyName)

### Districts ----


dist <- tbl(con, "ELAS") %>% 
    filter(
        AcademicYear == "2021-22",
        #       is.na(DistrictCode),
        AggLevel == "D",
        CountyCode == "27",
        Gender == "ALL"
    ) %>%
    collect()



dist.summ <- EL.summation(dist, DistrictName)


lollipop(dist.summ,  perc.EL*100, DistrictName, "gold") +
    labs(title = "Percent Current English Learner")

ggsave("Current EL.png", width = 8 , height = 6)

lollipop(dist.summ,  perc.EEL*100, DistrictName, "orange") +
    labs(title = "Percent Ever English Learner")


ggsave("Ever EL.png", width = 8 , height = 6)


