
# retrieve org IDs and API URLs for all Active GP Practices (only retrives first 1000)

# Work In Progress - TO DO
# **********  NEED TO BIND PAGES so working with full GP list *************
# Need to account for records with operational and legal start and end dates
# Need to account for eecords with only one parent (colname = extension not extension 1)



# retrieve all GP practices - **********  NEED TO BIND PAGES *************
allgps <- getODS(PrimaryRoleId="RO177",NonPrimaryRoleId="RO76")

# Create empty Lookup dataframe
lkup <- setNames(data.frame(matrix(ncol = 4, nrow = 0)),
                 c("GPPRAC15CD","GPPRAC15NM","CCG15CD", "CCG15NM"))

# loop through each GP Practice record - error at record 38 - parent field just extension not extension1

for (i in (1:37)) {  #nrow(allgps))) {

    live <- 0

    thisgp <- t(getODSfull(allgps[i,2]))

    if("Organisation.Date.Start" %in% colnames(thisgp)) {

        if ("Organisation.Date.End" %in% colnames(thisgp)) {
           lkup1  <- data.frame(matrix(thisgp,nrow=1, dimnames=dimnames(thisgp)),stringsAsFactors=FALSE) %>%
                       select(Organisation.OrgId.extension,
                              Organisation.Name,
                              Organisation.Rels.Rel.Target.OrgId.extension1,
                              Organisation.Date.Start,
                              Organisation.Date.End)
           if (lkup1$Organisation.Date.Start <= "2015-12-31" &
               (is.na(lkup1$Organisation.Date.End) | lkup1$Organisation.Date.End > "2015-12-31")) {
               live <- 1
           }

        } else {
           lkup1  <- data.frame(matrix(thisgp,nrow=1, dimnames=dimnames(thisgp)),stringsAsFactors=FALSE) %>%
                        select(Organisation.OrgId.extension,
                          Organisation.Name,
                           Organisation.Rels.Rel.Target.OrgId.extension1,
                          Organisation.Date.Start)
           if (lkup1$Organisation.Date.Start <= "2015-12-31") {
               live <- 1
           }
        }
    }
    # append live organisation data to lkup dataframe
    if (live == 1) {
        ccgname    <- getODSfull(lkup1[3])[1]
        names(lkup1) <- c("GPPRAC15CD","GPPRAC15NM","CCG15CD", "CCG15NM")
        lkup1      <- data.frame(lkup1[1:3],CCG15Name = ccgname)
        lkup       <- rbind(lkup,lkup1)
    }
}
