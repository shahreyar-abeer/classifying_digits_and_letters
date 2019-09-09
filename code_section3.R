library(ggplot2)
library(reshape2)
library(corrplot)
library(RColorBrewer)
library(kableExtra)
library(car)
library(Hmisc)
library(caret)
library(pander)

#setwd('./500$')
data = read.csv('./features.csv')

set.seed(1000)

# filtering the digits and letters
data$type = as.factor(c(rep('digit', 50), rep('letter', 50)))
digits = data[data$type == 'digit', ]
letters = data[data$type == 'letter', ]


# reduced data after omitting the correlated features
data_reduced = data[, -c(2, 4, 6, 10, 13, 14, 15, 18)]
digits_reduced = data_reduced[data_reduced$type == 'digit', ]
letters_reduced = data_reduced[data_reduced$type == 'letter', ]



## function that plots boxplots
## takes only one of two arguments: 'all' or 'by type'

boxer = function(method){
  d2 = melt(data[, -1], id.var = 'type')
  gg = NULL
  if( method == 'all'){
    gg = ggplot(d2, aes(x = variable, y = value)) + geom_boxplot() + 
      coord_flip()
  }
  else if( method == 'by type'){
    gg = ggplot(d2, aes(x = variable, y = value)) + 
      geom_boxplot(aes(fill = type)) + 
      coord_flip()
  }
  else
    return(gg)
}



#ab = letters[1:20, ]


# function to get the table of descriptive stats

descriptives = function(data){
  out_data = data.frame(matrix(nrow = 0, ncol = 3))
  colnames(out_data) = c('feature', 'mean', 'sd')
  for( i in 1: ncol(data)){
    if( colnames(data)[i] == 'label' | colnames(data)[i] == 'type')
      next
    avg = mean(data[, i])
    std = sd(data[, i])
    ds = data.frame('feature' = colnames(data)[i], 'mean' = avg, 'sd' = std)
    out_data = rbind(out_data, ds)
  }
  return(out_data)
}

# function that plots a corrplot
corrplotter = function(data){
  c = rcorr(as.matrix(data))
  corrplot(c$r, method = 'square', order = 'hclust',
           type = 'upper', tl.col = 'black',
           p.mat = c$P, insig = 'blank',
           sig.level = .05,
           col = brewer.pal(n = 11, name = 'BrBG'))
}





# function to get the significance of tests
get_signifs = function(col, data){
  f = as.formula(paste(col, '~', 'label'))
  kr = kruskal.test(f, data = data)
  return(kr[[3]])
}

# multiple comparison, kruskal-wallis
multiple_test = function(data){
  out_data = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(out_data) = c('feature', 'sig')
  for(i in 1:ncol(data)){
    if( colnames(data)[i] == 'label' | colnames(data)[i] == 'type')
      next
    s = get_signifs(colnames(data)[i], data)
    d = data.frame('feature' = colnames(data)[i], 'sig' = s[[1]])
    if(s[[1]] < .05)
      out_data = rbind(out_data, d)
  }
  return(out_data)
}


# two sample comparison, Mann-Whitney U test
two_sample = function(data1, data2){
  out_data = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(out_data) = c('feature', 'sig')
  for(i in 1:ncol(data1)){
    if( colnames(data1)[i] == 'label' | colnames(data1)[i] == 'type')
      next
    wil = wilcox.test(data1[, i], data2[, i])
    d = data.frame('feature' = colnames(data)[i], 'sig' = wil$p.value)
    if(wil$p.value < .05)
      out_data = rbind(out_data, d)
  }
  return(out_data)
}


# function that tests if a feature meets the anova assumptions
assumptions = function(data){
  out_data = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(out_data) = c('feature', 'test rejected')
  for(i in 1:ncol(data)){
    if( colnames(data)[i] == 'label' | colnames(data)[i] == 'type')
      next
    f = as.formula(paste(colnames(data)[i], '~', 'label'))
    fit = lm(f, data = data)
    shapiro = shapiro.test(fit$residuals)
    lev = leveneTest(f, data)
    rejected = 'None'
    if( shapiro$p.value < .05 & lev$`Pr(>F)` < .05)
      rejected = 'both'
    else if( shapiro$p.value < .05 & lev$`Pr(>F)` > .05)
      rejected = "Shapiro-Wilk's normality"
    else if( shapiro$p.value > .05 & lev$`Pr(>F)` < .05)
      rejected = 'Levene Test of homogeneity'
    d = data.frame('feature' = colnames(data)[i],
                   'test rejected' = rejected)
    out_data = rbind(out_data, d)
  }
  return(out_data)
}


# function to print a table using kable
print_table = function(data_frame, title){
  k = kable(data_frame,
        format = 'latex', booktabs = T,
        caption = title) %>%
    kable_styling(latex_options = c('striped', 'HOLD_position'))
  return(k)
}

# funtion that prints a pander table to word
number <<- 1
print_pander = function(object, title){
  title = paste0('Table ', number, ': ', title)
  number <<- number + 1
  p = pander(object, caption = title)
  return(p)
}


# function to to do multiple comparisons and return pairs of
# significant differences, post hoc test

multiple_comp = function(feature, data){
  out_data = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(out_data) = c('feature', 'pairs')
  test = pairwise.wilcox.test(data[[feature]],
                              data$label,
                              p.adjust.method = 'bonferroni')
  sig = test[[3]] < .05
  wh = which(sig == TRUE, arr.ind = T)
  cols = colnames(sig)
  if( nrow(wh) == 0)
    return(out_data)
  for( i in 1:nrow(wh)){
    out_data[i, 1] = feature
    out_data[i, 2] = paste('(', cols[wh[i, 2]], rownames(wh)[i], ')')
  }
  return(out_data)
}

# function that makes a data frame of the features and pairs
features_pairs = function(features, data){
  out_data = data.frame(matrix(nrow = 0, ncol = 2))
  colnames(out_data) = c('feature', 'pairs')
  for( i in 1: length(features)){
    out_data = rbind(out_data, multiple_comp(features[i], data))
  }
  return(out_data)
}

## function that makes table using features and pairs to print
print_features_pairs = function(data_frame, title){
  kable(data_frame, format = 'html',
        caption = title) %>%
    kable_styling(bootstrap_options = c('striped', "HOLD_position"),
                  full_width = F) %>%
    collapse_rows(1, latex_hline = 'major', valign = 'middle')
}










