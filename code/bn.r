# Bayesian network
library(bnlearn)
gradcafe <- read.csv(url("https://i.cs.hku.hk/~jyzhang/misc/cs_raw.csv") , header = TRUE)
df <- gradcafe[, c('degree', 'decision', 'decision_method',
		   'gpa_level', 'gre_level', 'uni_pub_level', 'status')]
hcdag <- hc(na.omit(df))
mb(hcdag, 'decision')
hcfit <- bn.fit(hcdag, na.omit(df))
