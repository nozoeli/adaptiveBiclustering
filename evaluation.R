#-----------Install Package ggplot2 to run the code----------
library('ggplot2')

#-----------Balanced Setup----------------------------
#-----------Data construction-------------------------
load('result.Rdata')
dataadap <- as.vector(t(errada))
datagss <- as.vector(t(errgss))
dataint <- c(dataadap, datagss)
tep_mean <- (0:29) * 0.1 + 1
ele_mean <- rep(rep(tep_mean, each = length(dataadap) / 30, ), 2)
perm_method <- factor(rep(c('Adaptive Hill-climbing', 'Golden Section Search'), each = length(dataadap)), c('Adaptive Hill-climbing', 'Golden Section Search'))
datapic <- data.frame('mean' = as.factor(ele_mean), 'method' = perm_method, 'data' = dataint)
# Constructed a dataset with variables as (elevated mean - permuting method - p-value) combo

#----------Graphing----------------------------------
p_balance <- ggplot(datapic, aes(x = mean, y = data, color = method))
p_balance <- p_balance + scale_colour_manual(values = c("#DD0000", "#00DD00"))
p_balance <- p_balance + geom_boxplot() + xlab(bquote(theta * " (multiplication of " * theta['crit'] * ')')) + ylab("Error") + ggtitle("(M,N,m,n) = (1000,1200,170,140)")
p_balance <- p_balance + theme(legend.position="top") + theme(legend.title=element_blank())
p_balance


#-----------Imbalanced Setup----------------------------
#-----------Data construction-------------------------
load('result2.Rdata')
dataadap <- as.vector(t(errada))
datagss <- as.vector(t(errgss))
dataint <- c(dataadap, datagss)
tep_mean <- (0:29) * 0.1 + 1
ele_mean <- rep(rep(tep_mean, each = length(dataadap) / 30, ), 2)
perm_method <- factor(rep(c('Adaptive Hill-climbing', 'Golden Section Search'), each = length(dataadap)), c('Adaptive Hill-climbing', 'Golden Section Search'))
datapic <- data.frame('mean' = as.factor(ele_mean), 'method' = perm_method, 'data' = dataint)
# Constructed a dataset with variables as (elevated mean - permuting method - p-value) combo

#----------Graphing----------------------------------
p_imbalance <- ggplot(datapic, aes(x = mean, y = data, color = method))
p_imbalance <- p_imbalance + scale_colour_manual(values = c("#DD0000", "#00DD00"))
p_imbalance <- p_imbalance + geom_boxplot() + xlab(bquote(theta * " (multiplication of " * theta['crit'] * ')')) + ylab("Error") + ggtitle("(M,N,m,n) = (4000,500,70,250)")
p_imbalance <- p_imbalance + theme(legend.position="top") + theme(legend.title=element_blank())
p_imbalance

