


count_alleles <- function(gv_actg)
    {
    labeled <- label_ref_snp(gv_actg)
    # labeled is a list with two components; first is labeled alleles; second is vector of length 2*length(gv_actg) where each entry is a single nucleotide
    f2<- matrix(ncol = 2, data=labeled[[2]], byrow=TRUE)
    o1  <- apply(FUN = function(x)sum(x==labeled[[1]][1]), X = f2, MARGIN=1)
    names(o1)<- names(gv_actg)
    return(o1)
    }



label_ref_snp<- function(gv_actg)
    {
    foo<- strsplit(gv_actg, "")
    # define two alleles, "other" & "ref"
    ufoo<- unlist(foo)
    ref <- ufoo[1] #arbitrarily set first entry to be 'ref' allele
    other <- ufoo[ufoo != ref][1] # choose (arbitrarily) the first entry in the vector of those characters that aren't ref.
    #* we should write a test to verify that we the input has only two alleles present.
    o2 <- c(ref, other)
    names(o2)<- c("reference", "other")
    gv_unlist <- ufoo
    return(list(o2, gv_unlist))
}


make_ref_table<- function(gv_actg)
{
    all_num <- count_alleles(gv_actg)
    snp_labels <- label_ref_snp(gv_actg)[[1]]
    data.frame(gv_actg, all_num, refallele = snp_labels[1], otherallele= snp_labels[2])
}

