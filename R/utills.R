

GenerateRandomSignatures <- function(dataset, signatureLength=100, nSignatures=0, nCores=0) {
    if (length(dim(dataset)) != 2) {
        stop("The dataset dim length should be equal 2, stopping.")
    }
    if (signatureLength <= 0) {
        stop("Signature length should be greater than zero, stopping.")
    }
    if (nSignatures == 0) {
        nSignatures = dim(dataset)[1]*10
    }
    if (nCores == 0){
        nCores <- detectCores() - 1
    }

    cl <- parallel::makeForkCluster(nCores)
    doParallel::registerDoParallel(cl)

    randomGeneSignatures <- foreach (i=1:nSignatures) %dopar% {
        sample(rownames(dataset), signatureLength)
    }
    parallel::stopCluster(cl)
    return(randomGeneSignatures)



}



GenerateDistributionByPermutations <- function(distMatrix, annotation, n_permutations=0, n_cores = 0) {
    # 
    if (length(dim(dataset)) != 2) {
        stop("The datasetHobot_stat dim length should be equal 2, stopping.")
    }
    if (length(signature) <= 0) {
        stop("Signature length should be greater than zero, stopping.")
    }

    if (length(annotation) <= 0) {
        stop("Annotation length should be greater than zero, stopping")
    }

    if (length(annotation)  != dim(dataset)[2]) {
        stop("Annotation length should be equal number of columns in dataset, stopping")
    }

    if (n_permutations == 0) {
        n_permutations = dim(dataset)[2]*5
    }

    if (n_cores == 0) {
        n_cores = detectCores() - 1
    }
    cl <- parallel::makeForkCluster(n_cores)
    doParallel::registerDoParallel(cl)

    scores <- foreach (i = 1:n_permutations) %dopar% {

        permutedAnnotation <- sample(annotation)
        Hobotnica(distMatrix, permutedAnnotation)

    return (scores)


    }








}





