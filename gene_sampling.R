# Sampling of all Drosophila genes to eventually use in generating networks
# and topological analysis

all_genes <- read.csv("all_Dro_genes.csv")
all_genes <- unlist(all_genes)     # to convert data into vector.

sample(all_genes, 20)  # randomly retrieve 20 genes from the data

sampling <- function(x, size, n, p = 1){
         set.seed(p)    
    # define an empty vector, nulls
nulls <- vector("character", n)
    for(i in 1:n){
       out <- sample(x, size)
       nulls[i] <- data.frame(out)
    }
    write.csv(nulls, "gene_samples.csv")

}

