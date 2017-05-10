#! /usr/bin/python3.5

import readDataSet as dIn

from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MultiLabelBinarizer
from sklearn.multiclass import OneVsRestClassifier
from sklearn.ensemble import GradientBoostingClassifier
from sklearn.svm import SVC

# mlb = MultiLabelBinarizer(dIn.genList("Labels"))
mlb = MultiLabelBinarizer()


X = dIn.genMatrix('fMatrix')
y = mlb.fit_transform(dIn.genMatrix('lMatrix'))

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.3, random_state=0)

# Might Work
def GBC():
	# Default values except verbose
	return GradientBoostingClassifier(n_estimators=100, learning_rate=.1, max_depth=3, random_state=0, verbose = 1).fit(X_train, y_train)

# Currently Testing
def OvRC():
	return OneVsRestClassifier(SVC(kernel='linear', probability=True), n_jobs = -2).fit(X_train, y_train)

def GBCScore():
	clf = GBC()
	print clf.score(X_test, y_test)
	return

def OvRCScore():
	clf = OvRC()
	print clf.score(X_test, y_test)
	return

OvRCScore()