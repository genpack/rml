# translib.R
# A library of transformers

xgb     = CLS.SCIKIT.XGB(rfe.enabled = T)
xgb.lgt = CLS.SCIKIT.XGB(return_logit = T, rfe = T)

nr_lr.l1     = CLS.SCIKIT.LR(penalty = 'l1', transformers = NORMALIZER(), rfe = T)
nr_lr.l2     = CLS.SCIKIT.LR(penalty = 'l2', transformers = NORMALIZER())
nr_lr.l1.lgt = CLS.SCIKIT.LR(penalty = 'l1', transformers = NORMALIZER(), return_logit = T, rfe = T)
nr_lr.l2.lgt = CLS.SCIKIT.LR(penalty = 'l2', transformers = NORMALIZER(), return_logit = T)

nr_knn     = CLS.SCIKIT.KNN(transformers = NORMALIZER(), max_train = 5000)
nr_knn.lgt = CLS.SCIKIT.KNN(transformers = NORMALIZER(), return_logit = T, max_train = 5000)

nr_svm = CLS.SCIKIT.SVM(transformers = NORMALIZER(), max_train = 10000)
nr_svm = CLS.SCIKIT.SVM(transformers = NORMALIZER(), return_logit = T, max_train = 10000)

smb_dm = DUMMIFIER(transformers = SMBINNING())

prc      = PRCOMP()
prc.nc10 = PRCOMP(num_components = 10)
prc.nc20 = PRCOMP(num_components = 20)

prc.nc50_sfs    = SFS(transformers = PRCOMP(num_components = 50))
prc.nc50_sfs.lr = SFS(transformers = PRCOMP(num_components = 50), model_class = 'CLS.SCIKIT.LR', model_config = list(penalty = 'l1'))

dm_genlg

