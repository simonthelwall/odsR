
# retrieve org IDs and API URLs for all Active GP Practices (only retrives first 1000)

# Work In Progress

activegp <- data.frame()

for (i in (1:30)) {

    thisgp <- getODS(Status="Active", PrimaryRoleId = "RO177", NonPrimaryRoleId = "RO76")

   activegp <- rbind(thisgp,activegp)
}


# testing

# get ODS code to input to getODSfull function (Woodseats medical Centre)
orgs  <- getODS(Name="Woodseats",Status="Active")[5,2]

# return full record for practice
full1 <- getODSfull(orgs)

# create data frame and select practice code and name and parent code
df1 <- data.frame(matrix(full1,nrow=1),stringsAsFactors=FALSE) %>%
    select(6,1,53)

# name columns
#names(df1) <- rownames(full1)[c(6,1,53)]
names(df1) <- c("GPPracticeCode","GPPracticeName","CCGCode")

# get parent organisation name
parent <- getODSfull(df1[3])[1]

df2 <- data.frame(df1,CCGName = parent)


