





live <- 0


thisgp <- t(getODSfull(allgps[i,2]))

# get start end orgid, orgname, parentname

thisgp1 <- data.frame(matrix(thisgp,nrow=1, dimnames=dimnames(thisgp)),stringsAsFactors=FALSE)


if(!("Organisation.Date.Start" %in% colnames(thisgp1))) {
    thisgp1 <- mutate(thisgp1,Organisation.Date.Start = "None")
}
if(!("Organisation.Date.Start1" %in% colnames(thisgp1))) {
    thisgp1 <- mutate(thisgp1,Organisation.Date.Start1 = "None")
}
if(!("Organisation.Date.Start2" %in% colnames(thisgp1))) {
    thisgp1 <- mutate(thisgp1,Organisation.Date.Start2 = "None")
}

if(!("Organisation.Date.End" %in% colnames(thisgp1))) {
    thisgp1 <- mutate(thisgp1,Organisation.Date.End = "None")
}
if(!("Organisation.Date.End1" %in% colnames(thisgp1))) {
    thisgp1 <- mutate(thisgp1,Organisation.Date.End1 = "None")
}
if(!("Organisation.Date.End2" %in% colnames(thisgp1))) {
    thisgp1 <- mutate(thisgp1,Organisation.Date.End2 = "None")
}

thisgp3 <- thisgp1 %>%
    select(Organisation.Date.Start,Organisation.Date.Start1,Organisation.Date.Start2,
           Organisation.Date.End,Organisation.Date.End1,Organisation.Date.End2)







thisgp2 <- mutate(thisgp1, OrgStart  = if("Organisation.Date.Start"  %in% colnames(thisgp1)) {
    thisgp1$Organisation.Date.Start
} else {
    thisgp1$Organisation.Date.Start1
},
OrgEnd    = if("Organisation.Date.End"  %in% colnames(thisgp1)) {
    thisgp1$Organisation.Date.End
} else {
    thisgp1$Organisation.Date.End1
},
# need to find parent for same date
OrgParent = if("Organisation.Rels.Rel.Target.OrgId.extension"  %in% colnames(thisgp1)) {
    thisgp1$Organisation.Rels.Rel.Target.OrgId.extension
} else {
    thisgp1$Organisation.Rels.Rel.Target.OrgId.extension1
}) %>%
    select(Organisation.OrgId.extension, Organisation.Name, OrgStart, OrgEnd, OrgParent)

lkup <- rbind(lkup,thisgp2)


}



dplyr::bind_rows(test$Organisation$Rels$Rel$Date)








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


