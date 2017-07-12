## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, message = FALSE)

## ------------------------------------------------------------------------
library(randomForest)
library(randomForestExplainer)

## ------------------------------------------------------------------------
load("GlioblastomaWide.rda")
GlioblastomaWide <- GlioblastomaWide[, -1]
GlioblastomaWide$death1y <- as.factor(GlioblastomaWide$death1y)
remove <- colSums(is.na(GlioblastomaWide))
GlioblastomaWide <- GlioblastomaWide[, remove == 0]
rm(remove)

## ------------------------------------------------------------------------
# set.seed(2017)
# forest <- randomForest(death1y ~ ., data = GlioblastomaWide, ntree = 10000, localImp = TRUE)
# save(forest, file = "GlioblastomaWide_forest.rda")
load("GlioblastomaWide_forest.rda")

## ------------------------------------------------------------------------
plot(forest, main = "Learning curve of the forest")
legend("topright", c("error for 'dead'", "misclassification error", "error for 'alive'"), lty = c(1,1,1), col = c("green", "black", "red"))

## ------------------------------------------------------------------------
forest

## ------------------------------------------------------------------------
min_depth_frame <- min_depth_distribution(forest)
head(min_depth_frame, n = 10)

## ------------------------------------------------------------------------
plot_min_depth_distribution(min_depth_frame)

## ------------------------------------------------------------------------
plot_min_depth_distribution(min_depth_frame, min_no_of_trees = 60, mean_sample = "relevant_trees")

## ------------------------------------------------------------------------
# importance_frame <- measure_importance(forest)
# save(importance_frame, file = "GlioblastomaWide_importance_frame.rda")
load("GlioblastomaWide_importance_frame.rda")
head(importance_frame, n = 10)

## ------------------------------------------------------------------------
plot_multi_way_importance(importance_frame, size_measure = "no_of_nodes", min_no_of_trees = 30)

## ------------------------------------------------------------------------
plot_multi_way_importance(importance_frame, x_measure = "accuracy_decrease", y_measure = "gini_decrease", size_measure = "p_value")

## ------------------------------------------------------------------------
plot_importance_ggpairs(importance_frame)

## ------------------------------------------------------------------------
plot_importance_rankings(importance_frame)

## ------------------------------------------------------------------------
(vars <- important_variables(importance_frame, k = 20, measures = c("mean_min_depth", "no_of_trees")))

## ------------------------------------------------------------------------
# interactions_frame <- min_depth_interactions(forest, vars)
# save(interactions_frame, file = "GlioblastomaWide_interactions_frame.rda")
load("GlioblastomaWide_interactions_frame.rda")
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

## ------------------------------------------------------------------------
# set.seed(2017)
# forest_v2 <- randomForest(death1y ~ ., data = GlioblastomaWide, ntree = 10000, mtry = floor(ncol(GlioblastomaWide)/3), importance = TRUE, localImp = TRUE)
# save(forest_v2, file = "GlioblastomaWide_forest_v2.rda")
load("GlioblastomaWide_forest_v2.rda"); rm(forest)
importance_frame <- measure_importance(forest_v2)
vars <- important_variables(importance_frame, k = 20, measures = c("mean_min_depth", "no_of_trees"))
# interactions_frame <- min_depth_interactions(forest_v2, vars)
# save(interactions_frame, file = "GlioblastomaWide_interactions_frame_v2.rda")
load("GlioblastomaWide_interactions_frame_v2.rda")
head(interactions_frame[order(interactions_frame$occurrences, decreasing = TRUE), ])

## ------------------------------------------------------------------------
plot_min_depth_interactions(interactions_frame)

## ------------------------------------------------------------------------
interactions_frame <- min_depth_interactions(forest_v2, vars, mean_sample = "relevant_trees", uncond_mean_sample = "relevant_trees")
plot_min_depth_interactions(interactions_frame)

## ------------------------------------------------------------------------
plot_predict_interaction(forest_v2, GlioblastomaWide, "SLC17A9", "IFIT2", grid = 80)

## ---- eval = FALSE-------------------------------------------------------
#  explain_forest(forest, interactions = TRUE, GlioblastomaWide, pred_grid = 70)

## ---- eval = FALSE-------------------------------------------------------
#  explain_forest(forest_v2, interactions = TRUE, GlioblastomaWide, pred_grid = 80)

## ---- eval = FALSE-------------------------------------------------------
#  # source("https://bioconductor.org/biocLite.R")
#  # biocLite("DESeq")
#  # biocLite("limma")
#  # biocLite("TxDb.Hsapiens.UCSC.hg18.knownGene")
#  # biocLite("org.Hs.eg.db")
#  # biocLite("DESeq2")
#  # biocLite("edgeR")
#  # devtools::install_github("geneticsMiNIng/MLGenSig", subdir = "MetExpR")
#  
#  # brca <- MetExpR::BRCA_mRNAseq_chr17
#  # colnames(brca) <- make.names(colnames(brca))
#  # brca$SUBTYPE <- factor(brca$SUBTYPE)
#  
#  # save(brca, file = "BreastCancer.rda")
#  load("BreastCancer.rda")

## ---- eval = FALSE-------------------------------------------------------
#  # set.seed(2017)
#  # forest_brca <- randomForest(SUBTYPE ~ ., data = brca, ntree = 10000, localImp = TRUE)
#  # save(forest_brca, file = "BreastCancer_forest.rda")
#  load("BreastCancer_forest.rda")
#  explain_forest(forest_brca, interactions = TRUE, brca)

## ---- eval = FALSE-------------------------------------------------------
#  # devtools::install_github("pbiecek/PISA2012lite")
#  # library("PISA2012lite")
#  
#  # pisa <- na.omit(student2012[,c(1, 4, 12, 13, 18:20, 39, 61:62, 114, 488, 457, 501)])
#  # pisa <- pisa[pisa$CNT %in% c("Austria", "Belgium", "Czech Republic", "Germany", "Denmark", "Spain", "Estonia", "Finland", "France", "United Kingdom", "Greece", "Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", "Netherlands", "Poland", "Portugal", "Slovak Republic", "Slovenia", "Sweden", "Romania", "Croatia", "Bulgaria"),] # consider only EU countries to reduce the size
#  # save(pisa, file = "PISA.rda")
#  load("PISA.rda")
#  
#  # Further reduce the size
#  pisa <- pisa[pisa$CNT %in% c("Czech Republic", "Hungary", "Poland", "Slovak Republic"), ] # only the Visegrad group
#  pisa$CNT <- factor(pisa$CNT)

## ---- eval = FALSE-------------------------------------------------------
#  # set.seed(2017)
#  # forest_pisa <- randomForest(PV1MATH ~ ., data = pisa, localImp = TRUE)
#  # save(forest_pisa, file = "PISA_forest.rda")
#  load("PISA_forest.rda")
#  explain_forest(forest_pisa, interactions = TRUE, pisa)

## ---- eval = FALSE-------------------------------------------------------
#  data(Boston, package = "MASS")
#  Boston$chas <- as.logical(Boston$chas)
#  # set.seed(2017)
#  # forest_Boston <- randomForest(medv ~ ., data = Boston, ntree = 1000, localImp = TRUE)
#  # save(forest_Boston, file = "Boston_forest.rda")
#  load("Boston_forest.rda")
#  explain_forest(forest_Boston, interactions = TRUE, Boston)

