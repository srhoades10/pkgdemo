# Basic function setup

#' Add 2 numbers!
#
#' @param x: Float/Int
#' @param y: Float/Int
#' @return Summed numbers
#' @export 
addNums <- function(x, y, ...){
    val <- x+y
    return(val)
}

#' Create random table!
#
#' @param nRow: Int
#' @param nCol: Int
#' @param addSample: Bool - Add a "Sample" column, numbered 1:nRow
#' @return Data.Frame
#' @export 
addNums <- function(nRow, nCol, addSample=TRUE, ...){
    if(nRow <= 0 | nCol <= 0){
        stop('Number of rows and columns must g.t.e 1')
    }
    df <- data.frame(matrix(rnorm(nRow*nCol), nrow=nRow, ncol=nCol))
    if(addSample==TRUE){
        df$Sample <- 1:nRow
    }
    return(df)
}

#' Fit PCA on data.frame
#'
#' Note: columns are removed which have 0 variance
#' 
#' @param data: Individual MZ data aggregated into NxM
#' @param nonDatCols: List - Non-data-containing columns
#' @param returnFit: List - Return only the fit object
#' @return PCA scores
#' @export 
fitPCA <- function(data, nonDatCols=c('Sample'), returnFit=FALSE){
    dataOnly <- data %>% select(-nonDatCols)
    dataOnly[is.na(dataOnly)] <- 0
    dataOnly <- dataOnly[ , apply(dataOnly, 2, var) != 0]
    pcaFit <- prcomp(dataOnly, center=TRUE, scale=TRUE)

    if(returnFit == TRUE){
        return(pcaFit)
    } else {
        pcaDF <- as.data.frame(pcaFit$x)
        projections <- cbind(data$Sample, pcaDF[, c('PC1' ,'PC2')])
        colnames(projections)[1] <- 'Sample'
        return(projections)
    }
}

#' Calculate the ellipse for a confidence interval on the PCA projections
#' 
#' @param pcaProjections: Scores from a prcomp() fit
#' @param confidence: Hotellings confidence (usually 95%)
#' @return 2D Ellipse boundaries for plotting the confidence interval
#' @export 
calcPCAEllipse <- function(pcaProjections, confidence=95, ...){
    theta <- c(seq(-pi, pi, length = 50), seq(pi, -pi, length = 50))
    circle <- as.matrix(cbind(cos(theta), sin(theta)))
    sigma <- var(cbind(pcaProjections$PC1, pcaProjections$PC2))
    mu <- c(mean(pcaProjections$PC1), mean(pcaProjections$PC2))
    ed <- sqrt(qchisq(confidence/100, df = 2))
    ell <- data.frame(sweep(circle %*% chol(sigma) * ed, 2,
                    mu, FUN = "+"), groups = 1)
    names(ell)[1:2] <- c("xvar", "yvar")
    return(ell)
}


#' Plot PCA for a data frame
#' 
#' @param data: Data.frame
#' @param confidence: Int/Float - Hotellings confidence (usually 95%)
#' @return ggplot object of PCA scores
#' @export
plotPCA <- function(data, confidence=95, nonDatCols=c('Sample'), ggTitle=NULL, ...){
    
    pcaProjections <- fitPCA(data, nonDatCols=nonDatCols)
    pcaEllipse <- calcPCAEllipse(pcaProjections=pcaProjections, confidence=confidence)

    #Re-run part of fitPCA just to get the variance explained
    dataOnly <- data %>% select(-nonDatCols)
    dataOnly[is.na(dataOnly)] <- 0
    dataOnly <- dataOnly[ , apply(dataOnly, 2, var) != 0]
    pcaFit <- prcomp(dataOnly, center = TRUE, scale = TRUE)
    varExp1 <- round(summary(pcaFit)$importance[2,1] * 100, 2) 
    varExp2 <- round(summary(pcaFit)$importance[2,2] * 100, 2)

    pcaPlot <- ggplot(pcaProjections, aes(x = PC1, y = PC2, colour = Sample))+
        geom_point(size = 5)+
        geom_path(data = pcaEllipse, aes(x = xvar, y = yvar), color = 'black')+
        theme_bw()+
        ylab(sprintf('PC2 (%s%s variance explained)', varExp2, '%')) + 
        xlab(sprintf('PC1 (%s%s variance explained)', varExp1, '%')) + 
        ggtitle(ggTitle)+
        geom_abline(aes(intercept = 0, slope = 0), color = 'black', size = 0.25)+
        geom_vline(aes(xintercept = 0), color = 'black', size = 0.25)+
        theme(axis.text = element_blank(), axis.title = element_text(size = 11, face = "bold"), 
                legend.title = element_text(size = 11), legend.text = element_text(size = 10), 
                title = element_text(size = 12, face = "bold"),
                plot.title = element_text(hjust = 0.5))

    return(pcaPlot)
}
