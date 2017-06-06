#! /usr/bin/python3.5

import readDataSet as dIn

import numpy as np

from sklearn.externals import joblib

from sklearn.cluster import DBSCAN
from sklearn.cluster import KMeans
from sklearn.cluster import AgglomerativeClustering

from sklearn.model_selection import train_test_split

from sklearn.preprocessing import MultiLabelBinarizer

from sklearn.metrics import hamming_loss
from sklearn.metrics import zero_one_loss
from sklearn.metrics import accuracy_score
from sklearn.metrics import jaccard_similarity_score
from sklearn.metrics import silhouette_samples, silhouette_score

mlb = MultiLabelBinarizer()

X = mlb.fit_transform(dIn.genMatrix('fMatrix'))

X_train, X_test = train_test_split(X, test_size=0.3, random_state=None)

def sScore():
	mScore = 0
	mSze = 0
	d = dict([(0,0)])

	for n_clusters in range(1000, 100000, 100):
		cluster_labels = KMC(n_clusters)
		silhouette_avg = silhouette_score(X_train, cluster_labels)
		d[n_clusters] = silhouette_avg
		print "For n_clusters =", n_clusters, "The average silhouette_score is :", silhouette_avg
		if ( silhouette_avg > mScore):
			mScore = silhouette_avg
			mSze = n_clusters

	print "Optimal clusters =", mSze, "With a silhouette_score of :", mScore
	return d

def tst():
	return mlb.transform([[40, 110]])

def KMC( c ):
	kM = KMeans(n_clusters=c, random_state=None, n_jobs=-1 ).fit(X_train)
	centers = kM.cluster_centers_
	np.savetxt('array.txt', centers)
	# saveKMC(kM)
	# print kM.score(X_test)
	return centers

def readKMC():
	kM = joblib.load(dIn.genLoc('Class-KMCModel').replace('.txt', '.sav'))
	kM.predict(tst())
	return kM

def saveKMC( kM ):
	joblib.dump(kM, dIn.genLoc('Class-KMCModel').replace('.txt', '.sav'))
	return


def Agglomerative():
	clf = AgglomerativeClustering(n_clusters=20, linkage='ward').fit(X_train)
	joblib.dump(kM, dIn.genLoc('Class-Cluster').replace('.txt', '.sav'))
	print clf.score(X_test)
	return clf

def readAgglomerative():
	return joblib.load(dIn.genLoc('Class-KMCModel').replace('.txt', '.sav'))

# Works on KMC not Agglomerative
def scoring( y_pred ):
	print ("Accuracy Score: " ,  accuracy_score(y_test, y_pred))
	print ("Jaccard Accuracy Score: " ,  jaccard_similarity_score(y_test, y_pred))
	print ("Hamming Loss: " , hamming_loss(y_test, y_pred))
	print ("Zero One Loss: " ,  zero_one_loss(y_test, y_pred))
	return
