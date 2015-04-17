


count_alleles <- function(gv_actg)
    {
    foo<- strsplit(gv_actg, "")
    #strsplit() returns a list, so foo is a list, where each element in the list is a length-two character vector, consisting of the two letters of the original genotype.
    #We convert foo to a character vector using unlist(), then put it into a matrix (with byrow=TRUE)
    # byrow=TRUE tells R to load the character vector unlist(foo) into the matrix by putting entries 1 & 2 from unlist(foo) into the first row, entries 3&4 into row 2, etc. Note that we tell R that the matrix should have 2 columns. thus, each element of the list foo becomes a row in the matrix f2.
    f2<- matrix(ncol = 2, data=unlist(foo), byrow=TRUE)
    o1  <- apply(FUN = function(x)sum(x==f2[1]), X = f2, MARGIN=1)
    names(o1)<- names(gv_actg)
    return(o1)
    }


label_ref_snp<- function(gv_actg)
    {
    foo<- strsplit(gv_actg, "")
    # define two alleles, "other" & "ref"
    ufoo<- unlist(foo)
    ref <- ufoo[1] #arbitrarily set first entry to be 'ref' allele
    ufoo<- ufoo[ufoo != ref][1] # choose (arbitrarily) the first entry in the vector of those characters that aren't ref.
    #* we should write a test to verify that we the input has only two alleles present.
    o2 <- c(ref, other)
    names(o2)<- c("reference", "other")
    return(o2)
}


make_ref_table<- function(gv_actg)
{
    genotype_num <- count_alleles(gv_actg)
    snp_labels <- label_ref_snp(gv_actg)
    data.frame(gv_actg, all_num, refallele = snp_labels[1], otherallele= snp_labels[2])

}

