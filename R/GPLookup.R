
# retrieve org IDs and API URLs for all Active GP Practices (only retrives first 1000)

# Work In Progress


# test practice : getODSfull("A81008") # this practice clodes 31st jan 2018


# retrieve all GP practices - **********  NEED TO BIND PAGES *************
allgps <- getODS(PrimaryRoleId="RO177",NonPrimaryRoleId="RO76")

# create loop

activegp <- data.frame()

for (i in (1:nrow(allgps))) {

    thisgp    <- getODSfull(allgps[i,2])
    lkp        <- data.frame(matrix(thisgp,nrow=1),stringsAsFactors=FALSE) %>%
                 select(6,1,2,53)  # need to identify best end date field or combine with status and last change date?
    names(lkp) <- c("GPPracticeCode","GPPracticeName","CCGCode")  # data lake principles say keep names as source but can't as parent name doesn't have a 'name'
    ccgname    <- getODSfull(lkp[3])[1]

    activegp <- rbind(thisgp,activegp)
}




# development - code below creates lookup for single medical practice

# get ODS code to input to getODSfull function (Woodseats medical Centre)
#org1  <- getODS(Name="Woodseats",Status="Active")[5,2]

# return full record for practice
#full1 <- getODSfull(org1)

# create data frame and select practice code and name and parent code
#df1 <- data.frame(matrix(full1,nrow=1),stringsAsFactors=FALSE) %>%
#    select(6,1,53)

# name columns
#names(df1) <- rownames(full1)[c(6,1,53)]
#names(df1) <- c("GPPracticeCode","GPPracticeName","CCGCode")

# get parent organisation name
parent1 <- getODSfull(df1[3])[1]

lkup1 <- data.frame(df1,CCGName = parent1)


