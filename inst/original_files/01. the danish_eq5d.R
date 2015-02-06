#### 2014-06-19 EQ-5D index Swedish value set


eqDanish <- function(Mobility, Selfcare, Usual, Pain, Anxiety) {
    
    mobility <- ifelse(Mobility == 1, 0, ifelse(Mobility == 2, -0.053, -0.411))
    selfcare <- ifelse(Selfcare == 1, 0, ifelse(Selfcare == 2, -0.063, -0.192))
    usual <- ifelse(Usual == 1, 0, ifelse(Usual == 2, -0.048, -0.144))
    pain <- ifelse(Pain == 1, 0, ifelse(Pain == 2, -0.062, -0.396))
    anxiety <- ifelse(Anxiety == 1, 0, ifelse(Anxiety == 2, -0.068, -0.367))
    N23 <- ifelse((Mobility %in% c(2, 3) | Selfcare %in% c(2, 3) | Usual %in% c(2, 3) | Pain %in% c(2, 3) | Anxiety %in% c(2, 3)), -0.114, 
        0)
    
    EQind <- 1 + mobility + selfcare + usual + pain + anxiety + N23
    
    return(EQind)
} 
