library(tabulizer)
library(dplyr)
library(purrr)


role_primary_lut <- extract_tables(
    "./misc/HSCOrgRefData_DataItemCatalogue.pdf",
    pages = c(32:36),
    area = list(
        c(292.50993,  50.24216, 532.76170, 796.78724),
        c(105.87366,  50.92083, 532.76170, 794.75121),
        c(109.26705,  52.27819, 521.90286, 795.42988),
        c(107.23102,  52.27819, 532.08302, 797.46592),
        c(108.58837,  51.59951, 254.50400, 797.46592)
    ),
    guess = FALSE, output = "data.frame"
)

map(.x = role_primary_lut, .f = names)

df1 <- role_primary_lut[[1]]
df2 <- role_primary_lut[[2]]
df2 <- rbind.data.frame(df2, names(df2))
df3 <- role_primary_lut[[3]]
df3 <- rbind.data.frame(df3, names(df3))
df4 <- role_primary_lut[[4]]
df4 <- rbind.data.frame(df4, names(df4))
df5 <- role_primary_lut[[5]]
df5 <- rbind.data.frame(df5, names(df5))
df5$notes <- NA_character_

some_names <- c("role_id", "role_name", "role_start_date",
                "role_end_date", "notes")

names(df1) <- some_names
names(df2) <- some_names
names(df3) <- some_names
names(df4) <- some_names
names(df5) <- some_names
rm(some_names)

role_primary_lut <- bind_rows(df1, df2, df3, df4, df5)
role_primary_lut <- role_primary_lut %>%
    filter(substr(role_id, 1, 2) == "RO") %>%
    mutate(role_end_date = ifelse(nchar(role_end_date) != 10, "", role_end_date),
           role_end_date = lubridate::dmy(role_end_date),
           role_start_date = lubridate::dmy(role_start_date))

role_primary_lut %>% filter(is.na(role_name) | role_name == "")

role_primary_lut$role_name[role_primary_lut$role_id == "RO216"] <- "DATA SERVICES FOR COMMISSIONERS REGIONAL OFFICE (DSCRO)"
role_primary_lut %>% filter(is.na(role_start_date))

role_primary_lut <- role_primary_lut %>%
    mutate(role_start_date = case_when(
        role_id == "RO104" & is.na(role_start_date) ~ lubridate::dmy("25/11/2008"),
        role_id == "RO144" & is.na(role_start_date) ~ lubridate::dmy("01/04/2004"),
        role_id == "RO180" & is.na(role_start_date) ~ lubridate::dmy("01/04/2000"),
        role_id == "RO228" & is.na(role_start_date) ~ lubridate::dmy("01/04/2003"),
        TRUE ~ role_start_date
    ),
    role_end_date = case_when(
        role_id == "RO144" & is.na(role_end_date) ~ lubridate::dmy("30/09/2009"),
    )) %>%
    mutate(osrt_id = as.numeric(substr(role_id, 3, nchar(role_id)))) %>%
    arrange(osrt_id) %>%
    select(-osrt_id)

View(role_primary_lut)

usethis::use_data(role_primary_lut)
