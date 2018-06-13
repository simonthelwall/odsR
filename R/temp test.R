



#temp file for testing code

OrgLkp1 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(1:1000))
OrgLkp2 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(1001:2000))
OrgLkp3 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(2001:3000))
OrgLkp4 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(3001:4000))
OrgLkp5 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(4001:5000))
OrgLkp6 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(5001:6000))
OrgLkp7 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(6001:7000))
OrgLkp8 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(7001:8000))
OrgLkp9 <- OrgParentLkp(PrimaryRole,NonPrimaryRole,ParentPrimaryRoles,FromDate,Slicerows=as.vector(8001:9000))

OrgLkp <- bind_rows(OrgLkp1,OrgLkp2) %>%
    bind_rows(OrgLkp3) %>%
    bind_rows(OrgLkp4) %>%
    bind_rows(OrgLkp5) %>%
    bind_rows(OrgLkp6) %>%
    bind_rows(OrgLkp7) %>%
    bind_rows(OrgLkp8) %>%
    bind_rows(OrgLkp9)

