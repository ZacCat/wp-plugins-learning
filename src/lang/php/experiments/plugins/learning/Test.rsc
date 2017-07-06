module lang::php::experiments::plugins::learning::Test

import lang::php::experiments::plugins::learning::Utils;
import lang::php::experiments::plugins::Locations; 
import lang::php::util::Config;
import lang::php::experiments::plugins::learning::ClusterTransactions;
import lang::php::experiments::plugins::learning::Association;
import lang::php::experiments::plugins::learning::KDTree;

import IO;
import List;
import Set;
import ValueIO;

import util::Math;
  
void testAll(str version)
{
	Matrix[int] fullM = readBinaryValueFile(#Matrix[int], baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.bin");
	Key key = readBinaryValueFile(#Key, baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.bin");
	
	//binM = binarize(fullM);
	unBinM = [ dup(s) | s <- fullM];
	
	int sz = size(binM[0]);
	
	list[real] recallList = [];
	list[real] precisionList = [];

	list[real] recallListW = [];
	list[real] precisionListW = [];
	
	list[real] recallListA = [];
	list[real] precisionListA = [];
	
	//list[real] recallListK = [];
	//list[real] precisionListK = [];
	
	for( i <- [0 .. 1])
	{
		/* train/test split for kNN */
		//tuple[Matrix[int] train, Matrix[int] tst] tT = splitSet(binM, i);
		//Cluster[real] C = buildClusterBinarized(tT[0], 1.0);	
		//KDT K = genKDT([ <w, [ n +0.0 | n <- s]> |<w, s> <- C<1,0>]);

		/* train/test split for Apriori & BMN & dwBMN */
		tuple[Matrix[int] train, Matrix[int] tst] unBinTT = splitSet(unBinM, i);

		Cluster[int] uC = unBinarizedBuildCluster(unBinTT[0],1);	

		list[AprioriMap] M = Apriori(uC);

		lrel[list[int], list[int]] testSplit = dropHalf(unBinTT[1]);	
		int testS = size(testSplit);
		println("Fold <i> size <testS>");

		int count = -1;
		for( <q, m> <- testSplit )
		{
			count += 1;
			println("<count> of <testS>");
			/* BMN */
			list[int] results = BMNunB(q, uC, key)<2>;
	 		recallList += recall(results, m);
	 		precisionList += precision(results, m);
	 			
	 		/* Weighted BMN */
			list[int] resultsW = unBinDistWeightedBMN(q, uC, key)<2>;
	 		recallListW += recall(resultsW, m);
	 		precisionListW += precision(resultsW, m);
	 		
	 		/* Apriori */
	 		list[list[int]] resultsAMatrix = predictApriori(M, q, key)<2>;
	 		list[int] resultsA = [];
	 		for( n <- resultsAMatrix ) resultsA += n;
	 		resultsA = dup(resultsA);
	 		if(size(resultsA) > hVal) resultsA = head(resultsA, hVal);
	 		recallListA += recall(resultsA, m);
	 		precisionListA += precision(resultsA, m);
	 		
	 		/* kNN */
	 		//list[int] resultsK = predictKNN(K, q, key, sz)<2>;
	 		//recallListK += recall(resultsK, m);
	 		//precisionListK += precision(resultsK, m);
	 	}
	}
	
	calcResults( "BMN", version, recallList, precisionList);
	calcResults( "Distance Weighted BMN", version, recallListW, precisionListW);
	calcResults( "Apriori", version, recallListA, precisionListA);
	//calcResults( "kNN", version, recallListK, precisionListK);
}

void calcResults(str function, str version, list[real] recallList, list[real] precisionList)
{
	real avgR = listAvg(recallList);
	real avgP = listAvg(precisionList);
	printResults(version, function, avgR, avgP, FMeasure(avgR, avgP));
}

/********************************************************************
							Scoring Functions 
********************************************************************/

real recall(list[int] pred, list[int] missing)
{
	real szM = size(missing) + 0.0;
	
	if(szM == 0.0) return 0.0;
	
	return (size( pred & missing ) + 0.0) / szM;
}

real precision(list[int] pred, list[int] missing){
	real szP = size(pred) + 0.0;
	
	if(szP == 0.0) return 0.0;
	
	return(size( pred & missing ) + 0.0) /  szP;
}
/* F1 Measure by default */
real FMeasure(real precision, real recall, real beta = 1.0) 
{
	real betaSq = beta * beta;
	real denom = betaSq * precision + recall;
	
	if(denom == 0.0) return 0.0;
	
 	return ((1.0 + betaSq) * precision * recall) / denom;
}

/********************************************************************
						Query Split Functions 
********************************************************************/

/* returns lrel[ query, missing indexe(s) ] */
lrel[list[int], list[int]] dropHalfBinarized(Matrix[int] tst)
{
	lrel[list[int], list[int]] split = [];
	list[int] testVector = [ 0 | i <- tst[0]];
	
	for( s <- tst )
	{
		set[int] oneIndex = { i | i <- index(s), s[i] == 1}; 
		int qSz = ceil(size(oneIndex) / 2);
		
		list[int] q = testVector;
		list[int] missing = [ i | i <- oneIndex];
		
		for( n <- [ 0 .. qSz ])
		{
			tuple[int, set[int]] newOnes = takeOneFrom(oneIndex);
			oneIndex = newOnes[1];
			q[newOnes[0]] = 1;
		}
		split += <q, missing>;
	}

	return split;
}

/* returns lrel[ query, missing indexe(s) ] */
lrel[list[int], list[int]] dropHalf(Matrix[int] tst)
{
	lrel[list[int], list[int]] split = [];
	int count = 0;
	for( s <- tst )
	{
		int qSz = ceil(((size(s) * 2) / 4.0));

		list[int] q = s;
		list[int] missing = [];
		for( n <- [ 0 .. qSz ])
		{
			tuple[int, list[int]] e = takeOneFrom(q);
			q = e[1];
			missing += e[0];
		}
		if(size(missing) == 0) count += 1;
		split += <q, missing>;
	}
	return split;
}


/* returns lrel[ query, missing index ] */
lrel[list[int], list[int]] dropOne(Matrix[int] tst)
{
	lrel[list[int], list[int]] split = [];
	list[int] testVector = [ 0 | i <- tst[0]];
	
	for( s <- tst )
	{
		int missing = takeOneFrom({ i | i <- indexOf(s), s[i] == 1})[0];
			
		list[int] q = s;
		
		q[missing] = 0;
		
		split += <q, missing>;
	}

	return split;
}

/********************************************************************
					Train/Test Split Functions 
********************************************************************/

/* splitNum indicates the split 0-9 for testing */
tuple[Matrix[int], Matrix[int]] splitSet(Matrix[int] M, int splitNum)
{
	int splitSize = floor(size(M) / 10);
	int splitStart = splitSize * splitNum;
	int splitEnd = splitStart + splitSize;

	Matrix[int] tst = M[ splitStart .. splitEnd];
	Matrix[int] train = M[.. splitStart];
	train += M[splitEnd ..];
	
	return <train, tst>;
}

/********************************************************************
							Save Functions 
********************************************************************/

void printResults(str version, str alg, real avgRecall, real avgPrecision, real F)
{
	str res = "Results for <alg> on WordPress <version>\n
	          'Average recall: <avgRecall>
	          'Average precision: <avgPrecision>
	          'F1 Score: <F>\n\t\t";
	
	println(res);
	
	loc txt = baseLoc + "test.txt";
	if(exists(txt)) appendToFile(txt, res);
	else writeFile(txt, res);
}


