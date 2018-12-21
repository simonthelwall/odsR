library(tabulizer)
library(dplyr)
library(purrr)

locate_areas(file = "./misc/HSCOrgRefData_DataItemCatalogue.pdf",
             pages = c(37, 38, 39))

role_nonprimary_lut <- extract_tables(
    "./misc/HSCOrgRefData_DataItemCatalogue.pdf",
    pages = c(37:39),
    area = list(
        c(125.98535, 52.13131, 528.97694, 617.45020),
        c(110.64098, 52.13131, 532.20734, 620.68060),
        c(107.41059, 51.32371, 192.20842, 619.87300)
    ),
    guess = FALSE, output = "data.frame"
)


map(.x = role_nonprimary_lut, .f = names)

some_names <- c("code", "role_name", "role_start_date", "role_end_date")
df1 <- role_nonprimary_lut[[1]]
df2 <- role_nonprimary_lut[[2]]
df2 <- rbind.data.frame(df2, names(df2))
df3 <- role_nonprimary_lut[[3]]
df3 <- rbind.data.frame(df3, names(df3))
df3 <- df3 %>% mutate(role_end_date = "")
df2 <- df2 %>% mutate(role_end_date = "")

names(df1) <- some_names
names(df2) <- some_names
names(df3) <- some_names

role_nonprimary_lut <- bind_rows(df1, df2, df3)
role_nonprimary_lut <- role_nonprimary_lut %>%
    mutate(
        role_start_date = stringr::str_replace_all(role_start_date, "X", ""),
        role_start_date = stringr::str_replace_all(role_start_date, "\\.", "/"),
        role_start_date = lubridate::dmy(role_start_date),
        role_end_date = lubridate::dmy(role_end_date))
View(role_nonprimary_lut)
usethis::use_data(role_nonprimary_lut)
rm(df1, df2, df3, some_names)

# named relationships ####

named_relationships <- data.frame(
    id = c("RE1", "RE2", "RE3", "RE4", "RE5", "RE6"),
    meaning = c("HAS A LEGACY RELATIONSHIP TO",
                "IS A SUB-DIVISION OF",
                "IS DIRECTED BY",
                "IS COMMISSIONED BY",
                "IS LOCATED IN THE GEOGRAPHY OF",
                "IS OPERATED BY"),
    ex_id_from = c("", "RO210", "RO173", "RO177", "RO172", "RO101"),
    ex_role_from = c("", "NHS ENGLAND (REGION, LOCAL OFFICE)", "PATHOLOGY LAB",
                     "PRESCRIBING COST CENTRE",
                     "INDEPENDENT SECTOR HEALTHCARE PROVIDER", "CARE HOME SITE"),
    ex_id_to = c("", "IS A SUBDIVISION OF", "IS DIRECTED BY",
                 "IS COMMISSIONED BY", "IS LOCATED IN THE GEOGRAPHY OF",
                 "IS OPERATED BY"),
    ex_role_to = c("", "NHS ENGLAND (REGION)", "NHS TRUST",
                   "CLINICAL COMMISSIONING GROUP",
                   "NHS ENGLAND (REGION, LOCAL OFFICE", "CARE HOME HQ")
)
usethis::use_data(named_relationships)
