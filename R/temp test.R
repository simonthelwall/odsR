




getODSall <- function() {

    loops <- NA
    i <- 1

    for (i in (1:10)) {
        if(is.null(getODS(PrimaryRoleId="RO177",NonPrimaryRoleId="RO76", Offset=i))) {
            loops <- i-1
        } else {
            i <- i+1
        }
    }



}
