library(bnlearn)
gradcafe <- read.csv('cs_level.csv')
df <- gradcafe[, c('degree', 'decision', 'decision_method', 'gpa_level', 'gre_level', 'uni_pub_level', 'status')]
hcdag <- hc(na.omit(df))
mb(hcdag, 'decision')
hcfit <- bn.fit(hcdag, na.omit(df))
