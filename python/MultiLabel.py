#! /usr/bin/python3.5

import readDataSet as dIn

import numpy as np

from sklearn.naive_bayes import MultinomialNB
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MultiLabelBinarizer
from sklearn.multiclass import OneVsRestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.svm import LinearSVC

from sklearn.metrics import zero_one_loss
from sklearn.metrics import hamming_loss
from sklearn.metrics import jaccard_similarity_score
from sklearn.metrics import accuracy_score

from sklearn.externals import joblib

mlb = MultiLabelBinarizer()


X = np.matrix(dIn.genMatrix('fMatrix'), dtype='float64')
y = mlb.fit_transform(dIn.genMatrix('lMatrix'))

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=None)

def GBC():
	# Default values except verbose
	clf = OneVsRestClassifier(GradientBoostingClassifier(n_estimators=100, learning_rate=.1, max_depth=10, random_state=0, verbose = 1), n_jobs= -2).fit(X_train, y_train)
	joblib.dump(clf, dIn.genLoc('Class-GBCModel').replace('.txt', '.sav'))
	return clf

def LSVC():
	clf = OneVsRestClassifier(LinearSVC(verbose=True, max_iter=100000), n_jobs = -2).fit(X_train, y_train)
	joblib.dump(clf, dIn.genLoc('Class-SVCModel').replace('.txt', '.sav'))
	return clf

def MNB():
	clf = OneVsRestClassifier(MultinomialNB()).fit(X_train, y_train)
	joblib.dump(clf, dIn.genLoc('Class-MNBModel').replace('.txt', '.sav'))
	return clf

def GBCScore():
	clf = GBC()
	scoring( clf.predict(X_test) )
	return

def LSVCScore():
	clf = LSVC()
	scoring( clf.predict(X_test) )
	return

def MNBScore():
	clf = MNB()
	scoring( clf.predict(X_test) )
	return

def readLSVC():
	return joblib.load(dIn.genLoc('Class-SVCModel').replace('.txt', '.sav'))

def readMNB():
	return joblib.load(dIn.genLoc('Class-MNBModel').replace('.txt', '.sav'))

def readGBC():
	return joblib.load(dIn.genLoc('Class-GBCModel').replace('.txt', '.sav'))

def scoring( y_pred ):
	print ("Accuracy Score: " ,  accuracy_score(y_test, y_pred))
	print ("Jaccard Accuracy Score: " ,  jaccard_similarity_score(y_test, y_pred))
	print ("Hamming Loss: " , hamming_loss(y_test, y_pred))
	print ("Zero One Loss: " ,  zero_one_loss(y_test, y_pred))
	return

def scoreAll():
	print 'Linear SVC:'
	scoring( readLSVC().predict(X_test) )
	print 'Multinomial Naive Bayes:'
	scoring( readMNB().predict(X_test) )
	print 'Gradient Boosing Classifier:'
	scoring( readGBC().predict(X_test) )
	return

GBC()
LSVC()
MNB()

scoreAll()