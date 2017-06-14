#-----------Install Package ggplot2 to run the code----------
library('ggplot2')

#-----------Balanced Setup----------------------------
#-----------Data construction-------------------------
load('result5.Rdata')
dataadap <- as.vector(t(errada))
datagss <- as.vector(t(errgss))
datalmg <- as.vector(t(errlmg))
dataspe <- as.vector(t(errspe))
dataint <- log(c(dataadap, datagss, datalmg, dataspe) + 1)
tep_mean <- (0:29) * 0.1 + 1
ele_mean <- rep(rep(tep_mean, each = length(dataadap) / 30 ), 4)
perm_method <- factor(rep(c('Adaptive Hill-climbing', 'Golden Section Search', 'Greatest Marginal Gap', 'Spectral Method'), each = length(dataadap)), c('Adaptive Hill-climbing', 'Golden Section Search', 'Greatest Marginal Gap', 'Spectral Method'))
datapic <- data.frame('mean' = as.factor(ele_mean), 'method' = perm_method, 'data' = dataint)
# Constructed a dataset with variables as (elevated mean - permuting method - p-value) combo

#----------Graphing----------------------------------
p_balance <- ggplot(datapic, aes(x = mean, y = data, color = method))
p_balance <- p_balance + scale_colour_manual(values = c("#FF0010", "#990000", "#2BCE48", "#0075DC"))
p_balance <- p_balance + geom_boxplot(outlier.shape = NA, aes(fill = method)) + xlab(bquote(theta * " (multiplication of " * theta['crit'] * ')')) + ylab("Error") + ggtitle(bquote("(M,N," * m^'*' * "," * n^'*' *  ") = (1000,1200,170,140), Rademacher")) + theme(plot.title = element_text(hjust = 0.5))
p_balance <- p_balance + theme(legend.position="top") + theme(legend.title=element_blank()) + scale_fill_manual(values = c("#FF0010", "#990000", "#2BCE48", "#0075DC"))
p_balance


#-----------Imbalanced Setup----------------------------
#-----------Data construction-------------------------
load('result6.Rdata')
dataadap <- as.vector(t(errada))
datagss <- as.vector(t(errgss))
datalmg <- as.vector(t(errlmg))
dataspe <- as.vector(t(errspe))
dataint <- log(c(dataadap, datagss, datalmg, dataspe) + 1)
tep_mean <- (0:29) * 0.1 + 1
ele_mean <- rep(rep(tep_mean, each = length(dataadap) / 30 ), 4)
perm_method <- factor(rep(c('Adaptive Hill-climbing', 'Golden Section Search', 'Greatest Marginal Gap', 'Spectral Method'), each = length(dataadap)), c('Adaptive Hill-climbing', 'Golden Section Search', 'Greatest Marginal Gap', 'Spectral Method'))
datapic <- data.frame('mean' = as.factor(ele_mean), 'method' = perm_method, 'data' = dataint)
# Constructed a dataset with variables as (elevated mean - permuting method - p-value) combo

#----------Graphing----------------------------------
p_imbalance <- ggplot(datapic, aes(x = mean, y = data, color = method))
p_imbalance <- p_imbalance + scale_colour_manual(values = c("#FF0010", "#990000", "#2BCE48", "#0075DC"))
p_imbalance <- p_imbalance + geom_boxplot(outlier.shape = NA, aes(fill = method)) + xlab(bquote(theta * " (multiplication of " * theta['crit'] * ')')) + ylab("Error") + ggtitle(bquote("(M,N," * m^'*' * "," * n^'*' *  ") = (4000,500,70,250), Rademacher")) + theme(plot.title = element_text(hjust = 0.5))
p_imbalance <- p_imbalance + theme(legend.position="top") + theme(legend.title=element_blank()) + scale_fill_manual(values = c("#FF0010", "#990000", "#2BCE48", "#0075DC"))
p_imbalance

