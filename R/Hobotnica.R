Hobot_stat <- function(distance_matrix, anno){
    if (typeof(anno) == "list") {
        annotation <- as.vector(unlist(anno))
    } else {
        annotation <- as.vector(anno)
    }
    rank.m <- as.matrix(distance_matrix) # transform distance matrix to matrix object
    re_rank.m <- rank.m
    re_rank.m[lower.tri(rank.m)] <- rank(rank.m[lower.tri(rank.m)]) # transform distances to ranks
    re_rank.m[upper.tri(rank.m)] <- rank(rank.m[upper.tri(rank.m)]) #

    inclass_sum <- 0
    classes <- unique(annotation) # unique classes
    Ns <- vector()

    for (i  in 1:length(classes)){

        clas <- classes[i]
        class_samples <- which(annotation == clas)
        l_tmp <- length(class_samples)
        Ns[i] <- l_tmp
        tmp_sum_inclass <- sum(re_rank.m[class_samples,class_samples]) # sum of ranks, describing in-class distances
        inclass_sum <- inclass_sum + tmp_sum_inclass


    }
    Ns_sum <- sum(Ns)
    biggest_bossible_rank <-  Ns_sum * (Ns_sum - 1)/2
    number_of_unique_inclass_elements <-  sum(Ns * (Ns-1))/2
    maximal_value <- number_of_unique_inclass_elements * (2*biggest_bossible_rank - number_of_unique_inclass_elements + 1)
    minimal_value <- number_of_unique_inclass_elements* (1 + number_of_unique_inclass_elements)

    normalization_factor <- maximal_value - minimal_value
    #sum_clever <- 0 #(2*sum(Ns)*sum(Ns) - Ns_squared_sum +1)*Ns_squared_sum/2 - (1 + Ns_squared_sum)*Ns_squared_sum/2
    return (max(0, 1 - (inclass_sum - minimal_value)/normalization_factor ))

}



Hobot_distr <- function(N ,distance_matrix, annotation){

    hobots <- vector()
    for (i in 1:100000){
        sample_anno <- annotation
        sample_anno[,1] <- sample(annotation[,1])
        hobots <- c(hobots, Hobot_stat(sampleDists, sample_anno)$total)
    }

    return(hobots)
}


Hobot_pval <- function(Test_hobot ,Hobots){
    p_val <- mean(Hobots <= Test_hobot)
    return(p_val)

}
