#! /usr/bin/python3.5

import readDataSet as dIn

import numpy as np

from sklearn.externals import joblib

from sklearn.cluster import DBSCAN
from sklearn.cluster import KMeans
from sklearn.cluster import AgglomerativeClustering

from sklearn.metrics import silhouette_samples, silhouette_score

from sklearn.model_selection import train_test_split

from sklearn.preprocessing import MultiLabelBinarizer

mlb = MultiLabelBinarizer()

X = mlb.fit_transform(dIn.genMatrix('fMatrix'))

X_train, X_test = train_test_split(X, test_size=0.3, random_state=None)

# Implement a variable step
def sScore():
	mScore = 0
	mSze = 0
	d = dict([(0,0)])

	skip = 500
	prevS = 0
	val = 0

	while( skip != 0) :
		cluster_labels = KMC(val + skip)
		silhouette_avg = silhouette_score(X_train, cluster_labels)
		d[val + skip] = silhouette_avg
		print "For n_clusters =", val + skip, "The average silhouette_score is :", silhouette_avg
	
		if(silhouette_avg - prevS <= 0):
			skip *= .5
			skip = int(skip)
		else:
			val += skip
		
		prevS = silhouette_avg
	return d

def tst():
	return mlb.transform([[40, 110]])

def KMC( c ):
	kM = KMeans(n_clusters=c, random_state=None, n_jobs=-1 )
	labels = kM.fit_predict(X_train)
	centers = kM.cluster_centers_
	np.savetxt(dIn.genLoc('KMClabels' + `c`), labels)
	np.savetxt(dIn.genLoc('KMCarray' + `c`), centers)
	# saveKMC(kM, c)
	# print kM.score(X_test)
	return labels

def readKMC( c ):
	kM = joblib.load(dIn.genLoc('KMCModel-' + `c`).replace('.txt', '.sav'))
	return kM

def saveKMC( kM, c ):
	joblib.dump(kM, dIn.genLoc('KMCModel-' + `c`).replace('.txt', '.sav'))
	return


def Agglomerative():
	clf = AgglomerativeClustering(n_clusters=20, linkage='ward').fit(X_train)
	joblib.dump(kM, dIn.genLoc('AgglModel').replace('.txt', '.sav'))
	print clf.score(X_test)
	return clf

def readAgglomerative():
	return joblib.load(dIn.genLoc('AgglModel').replace('.txt', '.sav'))

# sScore()
k = KMC(1500)
