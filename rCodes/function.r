####################################################################
#  Decomposition
####################################################################



getEffect <- function(labdta) {
    # DecompHoriuchi
    #   rates1 <-  as.matrix(labdta[year==1981,.(tPop,pStr,wpx,whx)])
    #   rates2 <-  as.matrix(labdta[year==2016,.(tPop,pStr,wpx,whx)])
    getLabor <- function(x) {
        x <- as.data.table(x)
        res <- x[, sum(tpop * stx * whx * wpx)]
        return(as.numeric(res))
    }
    #   x <- rates1
    #   getLabor(x)
    #   getLabor(rates2) - getLabor(rates1)
    #   system.time(eff1 <- DecompContinuous(getLabor,rates1, rates2, N=N))
    #   sum(eff1)
    #   s <-"Immigrant"
    #   y <- 1981
    N <- 52
    years <- min(labdta$year):(max(labdta$year) - 1)
    etable <- data.table()
    counter <- 0
    cat("\n", "Processing...", "\n")
    nt <- length(years)
    for (y in years) {
        rates1 <- as.matrix(labdta[year == y, .(tpop, stx, wpx, whx)])
        rates2 <- as.matrix(labdta[year == y + 1, .(tpop, stx, wpx, whx)])
        # compute res for etable
        res <- DecompContinuous(func = getLabor, rates1, rates2, N = N)
        res <- data.table(labdta[year == y, .(year, age, nation)], res)
        etable <- rbind(etable, res)
        # increment counter and diplay only progress that are multiple of 5
        counter <- counter + 1
        progress <- round((100 * counter / nt) / 5) * 5
        if (progress %% 10 == 0) {
            cat("...", progress, "%", "\t")
        }
    }
    return(etable)
}
# getEffect(labdta)


####################################################################
#  DecompContinuous as adapted from https://github.com/timriffe/DecompHoriuchi/blob/master/DecompHoriuchi/R/DecompContinuous.R
####################################################################

DecompContinuous <-
    function(func, rates1, rates2, N, ...) {
        # number of interval jumps
        L <- nrow(rates1) # number of ages
        P <- ncol(rates1) # number of factors
        d <- (rates2 - rates1)
        deltascale <- .5:(N - .5) / N
        effectmat <- rates1 * 0
        for (k in 1:N) { # over intervals
            # this part implements the assumption that the other rates (all ages and factors)
            # are also changing linearly in sync
            ratesprop <- rates1 + d * deltascale[k]
            for (i in 1:P) { # across effects
                deltaiak <- 0
                deltaia <- rep(0, L)
                for (a in 1:L) { # down ages
                    # now, we select a single element, [a,i] and increment it forward by .5 delta
                    # and also bring it backward by .5 delta
                    ratesminus <- ratesplus <- ratesprop
                    ratesplus[a, i] <- ratesplus[a, i] + (d[a, i] / (2 * N))
                    ratesminus[a, i] <- ratesminus[a, i] - (d[a, i] / (2 * N))
                    # the difference between the funciton evaluated with these rates is the change in y
                    # due to this age (La), factor (Pi) and interval (Nk)
                    deltaiak <- func(ratesplus, ...) - func(ratesminus, ...)
                    # for this factor and interval, we define a vector of these mini-deltas over age
                    deltaia[a] <- deltaiak
                }
                # and when age is done looping we add it to the effects matrix in the appropriate column
                # the the effects matrix sums over all intervals (split according to N- bigger N = finer)
                effectmat[, i] <- effectmat[, i] + deltaia
            }
        }
        return(effectmat)
    }


###################################################################
# GGPlot settings
# used aluf and ueCanada
###################################################################
mygthemep <- theme_bw() +  theme(
    legend.position = "top", 
    legend.title = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid = element_line(linetype = "dotted"),
    panel.border = element_rect(colour = NA, fill = NA),
    strip.background = element_blank(),
    strip.text.x = element_text(hjust = 0),
    strip.text = element_text(size = 8)
    
  )
mygcolor <- c("#021250", "#8B0D11", "#FCB507")
mygshape <- c(15, 16, 17,18)

sepline <- ggplot()+ 
    geom_vline(xintercept = 1, linetype = "dashed",size = 0.1) + 
    geom_vline(xintercept = 2, linetype = "dashed",size = 0.1) +
    scale_x_continuous(limits = c(0, 3)) +
    theme_void()

sepline2 <- ggplot() +
    annotate("text", x = 1, y = 0.5, label = "+", size = 7, fontface = "bold", colour = "black") +
    annotate("text", x = 2, y = 0.5, label = "=", size = 7, fontface = "bold", colour = "black") +
    scale_x_continuous(limits = c(0, 3), expand = c(0, 0)) +
    scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
    theme_void()
