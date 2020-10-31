

generate_random_signatures <- function(dataset, signature_length=100, n_signatures=0,n_cores=0) {
    if (length(dim(dataset)) != 2) {
        stop("The dataset dim length should be equal 2, stopping.")
    }
    if (signature_length <= 0) {
        stop("Signature length should be greater than zero, stopping.")
    }
    if (n_signatures == 0) {
        n_signatures = dim(dataset)[1]*10
    }
    if (n_cores == 0){
        n_cores <- detectCores() - 2
    }

    cl <- parallel::makeForkCluster(n_cores)
    doParallel::registerDoParallel(cl)

    random_gene_signatures <- foreach (i=1:n_signatures) %dopar% {
        sample(rownames(dataset), signature_length)
    }
    parallel::stopCluster(cl)
    return(random_gene_signatures)



}


