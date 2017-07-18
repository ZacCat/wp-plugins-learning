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

list[str] wpV4() = ["4.0", "4.0.1", "4.0.2", "4.0.3", "4.0.4", "4.0.5", "4.0.6", "4.0.7", "4.0.8", "4.0.9", "4.0.10", "4.0.11", "4.0.12", "4.0.13", "4.0.14", "4.0.15", "4.0.16", "4.0.17", "4.1", "4.1.1", "4.1.2", "4.1.3", "4.1.4", "4.1.5", "4.1.6", "4.1.7", "4.1.8", "4.1.9", "4.1.10", "4.1.11", "4.1.12", "4.1.13", "4.1.14", "4.1.15", "4.1.16", "4.1.17", "4.2", "4.2.1", "4.2.2", "4.2.3", "4.2.4", "4.2.5", "4.2.6", "4.2.7", "4.2.8", "4.2.9", "4.2.10", "4.2.11", "4.2.12", "4.2.13", "4.2.14", "4.3", "4.3.1", "4.3.2", "4.3.3", "4.3.4", "4.3.5", "4.3.6", "4.3.7", "4.3.8", "4.3.9", "4.3.10", "4.4", "4.4.1", "4.4.2", "4.4.3", "4.4.4", "4.4.5", "4.4.6", "4.4.7", "4.4.8", "4.4.9", "4.5", "4.5.1", "4.5.2", "4.5.3", "4.5.4", "4.5.5", "4.5.6", "4.5.7", "4.5.8", "4.6", "4.6.1", "4.6.2", "4.6.3", "4.6.4", "4.6.5", "4.7", "4.7.1", "4.7.2", "4.7.3", "4.7.4", "4.7.5", "4.8"];

void testAllVersions(str clustType)
{
	for(v <- wpV4())
		testAll(v, clustType);
}

/* Where clustType is 'Class' or 'Plugin' */
void testAll(str version, str clustType) 
{ 
	loc cLoc = baseLoc + "/training/Unsupervised/TrainBy<clustType>-fCluster-<version>.bin";
	loc kLoc = baseLoc + "/training/Unsupervised/TrainBy<clustType>-Features-<version>.bin";
	if(!exists(cLoc) || !exists(kLoc)) 
	{
		println("Version <version> not parsed");
		return;
	}
	
	Cluster[int] fullM = readBinaryValueFile(#Cluster[int], cLoc); 
	unWM = [ <dup(s), 1> | <s, _> <- fullM]; 
	Key key = readBinaryValueFile(#Key,kLoc); 
	 
	//binM = binarize(fullM); 
	Cluster[int] unBinM = [ <dup(s), w> | <s, w> <- fullM]; 
	 
	//int sz = size(binM[0]); 
	 
	list[real] recallList = []; 
	list[real] precisionList = []; 
	list[real] MRRList = []; 

	list[real] recallListW = []; 
	list[real] precisionListW = []; 
	list[real] MRRListW = []; 
	 
	list[real] recallListWD = []; 
	list[real] precisionListWD = []; 
	list[real] MRRListWD = []; 
	 
	list[real] recallListA = []; 
	list[real] precisionListA = []; 
	list[real] MRRListA = []; 
	
	//list[real] recallListK = []; 
	//list[real] precisionListK = []; 
	
	for( i <- [0 .. 10]) 
	{ 
		/* train/test split for kNN */ 
		//tuple[Matrix[int] train, Matrix[int] tst] tT = splitSet(binM, i); 
		//Cluster[real] C = buildClusterBinarized(tT[0], 1.0);	 
		//KDT K = genKDT([ <w, [ n +0.0 | n <- s]> |<w, s> <- C<1,0>]); 
 
		/* train/test split for Apriori & BMN & dwBMN */ 
		tuple[Cluster[int] train, Cluster[int] tst] unBinTT = splitSet(unBinM, i); 
		 
		Cluster[int] unWTrain = splitSet(unWM, i)[0]; 
		Cluster[int] uC = unBinTT[0];	 
		 
		list[AprioriMap] M = Apriori(unWTrain); 
 
		lrel[list[int], list[int]] testSplit = dropHalf(unBinTT[1]);	 
		int testS = size(testSplit); 
		println("Fold <i> size <testS>"); 
 
		for( <q, m> <- testSplit ) 
		{ 
			/* BMN */
			list[int] results = BMNunB(q, unWTrain, key, 0.05)<2>; 
			recallList += recall(results, m); 
			precisionList += precision(results, m); 
			MRRList += MRR(results, m);
				
			/* Weighted with DL BMN */
			list[int] resultsWD = unBinDistWeightedBMN(q, uC, key, 0.2)<2>; 
			recallListWD += recall(resultsWD, m); 
			precisionListWD += precision(resultsWD, m); 
			MRRListWD += MRR(resultsWD, m);
		
			 /* Weighted BMN */
			list[int] resultsW = unBinDistWeightedBMN(q, unWTrain, key, 0.3)<2>; 
			recallListW += recall(resultsW, m); 
			precisionListW += precision(resultsW, m); 
			MRRListW += MRR(resultsW, m);
			
			/* Apriori */ 
			list[list[int]] resultsAMatrix = predictApriori(M, q, key, pThres = 0.05)<2>; 
			list[int] resultsA = []; 
			for( n <- resultsAMatrix ) resultsA += n; 
			resultsA = dup(resultsA); 
			if(size(resultsA) > hVal) resultsA = head(resultsA, hVal); 
			recallListA += recall(resultsA, m); 
			precisionListA += precision(resultsA, m); 
			MRRListA += MRR(resultsA, m);

			/* kNN */ 
			//list[int] resultsK = predictKNN(K, q, key, sz)<2>; 
			//recallListK += recall(resultsK, m); 
			//precisionListK += precision(resultsK, m); 
		 } 
	} 
	
	loc txt = baseLoc + "test.txt"; 
	
	calcResults( "BMN <clustType>", version, recallList, precisionList, MRRList); 
	calcResults( "Distance Weighted BMN w/ DL <clustType>", version, recallListWD, precisionListWD, MRRListWD); 
	calcResults( "Distance Weighted BMN <clustType>", version, recallListW, precisionListW, MRRListW); 
	calcResults( "Apriori <clustType>", version, recallListA, precisionListA, MRRListA); 
	//calcResults( "kNN", version, recallListK, precisionListK); 
	
	 appendToFile(txt, "\n"); 
} 
 
void calcResults(str function, str version, list[real] recallList, list[real] precisionList, list[real] MRR) 
{ 
	real avgR = listAvg(recallList); 
	real avgP = listAvg(precisionList); 
	real avgMRR = listAvg(MRR);
	printResults(version, function, avgR, avgP, FMeasure(avgR, avgP), avgMRR); 
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
	 
	return(size( pred & missing ) + 0.0) /	szP; 
} 
/* F1 Measure by default */ 
real FMeasure(real precision, real recall, real beta = 1.0)	
{ 
	real betaSq = beta * beta; 
	real denom = betaSq * precision + recall; 
	 
	if(denom == 0.0) return 0.0; 
	 
	 return ((1.0 + betaSq) * precision * recall) / denom; 
} 

list[real] MRR(list[int] pred, list[int] missing)
{
	list[real] ret = [];
	for( m <- missing)
	{
		real val = (indexOf(pred, m) + 1.0);
		if(val > 0) ret += 1 / val;
		else ret += 0.0; 
		pred -= m;
	}
	return ret;
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
lrel[list[int], list[int]] dropHalf(Cluster[int] tst) 
{ 
	lrel[list[int], list[int]] split = []; 
	int count = 0; 
	for( <s, _><- tst ) 
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
tuple[Cluster[int], Cluster[int]] splitSet(Cluster[int] M, int splitNum) 
{ 
	int splitSize = floor(size(M) / 10); 
	int splitStart = splitSize * splitNum; 
	int splitEnd = splitStart + splitSize; 
 
	Cluster[int] tst = M[ splitStart .. splitEnd]; 
	Cluster[int] train = M[.. splitStart]; 
	train += M[splitEnd ..]; 
	 
	return <train, tst>; 
} 
 
/******************************************************************** 
							Save Functions	
********************************************************************/ 
 
void printResultsOld(str version, str alg, real avgRecall, real avgPrecision, real F, real avgMRR) 
{ 
	str res = "Results for <alg> on WordPress <version>\n 
						'Average recall: <avgRecall> 
						'Average precision: <avgPrecision> 
						'F1 Score: <F>
						'MRR: <avgMRR>\n\t\t"; 
	 
	println(res); 
	 
	loc txt = baseLoc + "test.txt"; 
	if(exists(txt)) appendToFile(txt, res); 
	else writeFile(txt, res); 
} 

void printResults(str version, str alg, real avgRecall, real avgPrecision, real F, real avgMRR) 
{ 
	str res = "<avgRecall>\t<avgPrecision>\t<F>\t<avgMRR>\t"; 
	
	loc txt = baseLoc + "test.txt"; 
	if(exists(txt)) appendToFile(txt, res); 
	else writeFile(txt, res); 
	
	println("Results for <alg> on WordPress <version>\n 
				'Average recall: <avgRecall> 
				'Average precision: <avgPrecision> 
				'F1 Score: <F>
				'MRR: <avgMRR>");
}
