

GenerateRandomSignatures <- function(dataset, signatureLength=100, nSignatures=0, nCores=1) {
    if (length(dim(dataset)) != 2) {
        stop("The dataset dim length should be equal 2, stopping.")
    }
    if (signatureLength <= 0) {
        stop("Signature length should be greater than zero, stopping.")
    }
    if (signatureLength > dim(dataset)[1]) {
        stop ("signatureLength shoudld be less or equal to size of dataset, stopping.")
    }
    if (nSignatures == 0) {
        nSignatures = dim(dataset)[1]*10
    }

    
    if (nCores == 0){
        nCores = detectCores() - 1
    }

    cl <- parallel::makeForkCluster(nCores)
    doParallel::registerDoParallel(cl)

    randomGeneSignatures <- foreach (i=1:nSignatures) %dopar% {
        sample(rownames(dataset), signatureLength, replace=FALSE)
    }
    parallel::stopCluster(cl)
    return(randomGeneSignatures)



}



GenerateDistributionByPermutations <- function(distMatrix, annotation, nPermutations=0, nCores=1) {
    # 
    if (length(dim(distMatrix)) != 2) {
        stop("The distMatrix dim length should be equal 2, stopping.")
    }
    if (dim(distMatrix)[1] != dim(distMatrix)[2] ) {
        stop("distMatrix should be a square matrix, stopping")
    }
    if (length(signature) <= 0) {
        stop("Signature length should be greater than zero, stopping.")
    }

    if (length(annotation) <= 0) {
        stop("Annotation length should be greater than zero, stopping")
    }

    if (length(annotation)  != dim(distMatrix)[2]) {
        stop("Annotation length should be equal number of columns in dataset, stopping")
    }

    if (nPermutations == 0) {
        nPermutations = dim(distMatrix)[1]*5
    }
    if (nCores == 1) {
        scores <- list()
        for (i in 1:nPermutations) {
            permutedAnnotation <- sample(annotation, length(annotation), replace=FALSE)
            scores[[i]] <- Hobotnica(distMatrix, permutedAnnotation)
        }

    } else {
    if (nCores == 0) {
        nCores = detectCores() - 1
    }
    cl <- parallel::makeForkCluster(nCores)
    doParallel::registerDoParallel(cl)

    scores <- foreach (i = 1:nPermutations) %dopar% {

        permutedAnnotation <- sample(annotation, length(annotation), replace=FALSE)
        Hobotnica(distMatrix, permutedAnnotation)

    
    }
    

    }


    return (scores)


}





