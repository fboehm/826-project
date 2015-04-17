genoparse <- function(gv.actg, # gv.actg is a character vector with each entry a 2-letter genotype for a single SNP. Example: c("TT", "CT", "TC")
                      sid=names(gv.actg) # sid is the vector of subject IDs
)
{
    foo<- strsplit(gv.actg, "")
    #strsplit() returns a list, so foo is a list, where each element in the list is a length-two character vector, consisting of the two letters of the original genotype.
    #We convert foo to a character vector using unlist(), then put it into a matrix (with byrow=TRUE)
    # byrow=TRUE tells R to load the character vector unlist(foo) into the matrix by putting entries 1 & 2 from unlist(foo) into the first row, entries 3&4 into row 2, etc. Note that we tell R that the matrix should have 2 columns. thus, each element of the list foo becomes a row in the matrix f2.
    f2<- matrix(ncol = 2, data=unlist(foo), byrow=TRUE)
    o1  <- apply(FUN = function(x)sum(x==f2[1]), X = f2, MARGIN=1)
    # define two alleles, "other" & "ref"
    other<- f2[f2 != f2[1]][1]
    ref <- f2[1]
    o2 <- c(ref, other)
    names(o2)<- c("ref", "oth")
    names(o1)<- sid
    names(gv.actg)<- sid
    out1<- list(f2, o1, o2)
    names(out1)<- c("input_geno", "genovec", "allele_identities")
    o4 <- c(other, ref)
    names(o4)<- c("ref", "oth")
    o3 <- 2-o1
    out2 <- list(f2, o3, o4)
    names(out2)<- c("input_geno", "genovec", "allele_identities")
    out <- list(out1, out2)
    return(out)
}




count_alleles <- function(gv.actg)
    {
    foo<- strsplit(gv.actg, "")
    #strsplit() returns a list, so foo is a list, where each element in the list is a length-two character vector, consisting of the two letters of the original genotype.
    #We convert foo to a character vector using unlist(), then put it into a matrix (with byrow=TRUE)
    # byrow=TRUE tells R to load the character vector unlist(foo) into the matrix by putting entries 1 & 2 from unlist(foo) into the first row, entries 3&4 into row 2, etc. Note that we tell R that the matrix should have 2 columns. thus, each element of the list foo becomes a row in the matrix f2.
    f2<- matrix(ncol = 2, data=unlist(foo), byrow=TRUE)
    o1  <- apply(FUN = function(x)sum(x==f2[1]), X = f2, MARGIN=1)
    names(o1)<- names(gv.actg)
    return(o1)
    }


label_ref_snp<- function(gv.actg)
    {
    foo<- strsplit(gv.actg, "")
    # define two alleles, "other" & "ref"
    ufoo<- unlist(foo)
    ref <- ufoo[1]
    ufoo<- ufoo[ufoo != ref][1] # choose (arbitrarily) the first entry in the vector of those characters that aren't ref.
    #* we should write a test to verify that we the input has only two alleles present.
    o2 <- c(ref, other)
    names(o2)<- c("reference", "other")
    return(o2)
}



