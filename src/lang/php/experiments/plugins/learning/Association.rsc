module lang::php::experiments::plugins::learning::Association

import lang::php::util::Utils;
import lang::php::util::Config;
import lang::php::ast::AbstractSyntax;
import lang::php::experiments::plugins::Summary;
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Locations; 
import lang::php::experiments::plugins::learning::Utils;
import lang::php::experiments::plugins::learning::ClusterTransactions;

import IO;
import Set;
import Map;
import List;
import String;
import ValueIO;
import Relation;
import ListRelation;

import util::Math;

real minSup = .01;

alias AprioriMap = map[list[int], int];

/********************************************************************
							Testing functions
********************************************************************/

tuple[Cluster[int], Key] readTrans(int w, str version)
{
	Matrix[int] M = readBinaryValueFile(#Matrix[int], baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.bin");
	Key k = readBinaryValueFile(#Key, baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.bin");
	
	Cluster[int] C = unBinarizedBuildCluster(M, w);
	
	
	return <C, k>;
}

tuple[list[AprioriMap], Key] genApriori(tuple[Cluster[int], Key] A, real sup) = <Apriori(A[0]), A[1]>;

lrel[real, list[str], list[int]] tstApriori(tuple[list[AprioriMap], Key] A, list[int] q, int minSup) = predictApriori(A[0], q, A[1]); 

/********************************************************************
						Prediction Functions
********************************************************************/

/* TODO: Hash Tree */
/* Build a list of frequent itemsets following the 
   Apriori Principle given a Cluster of transactions.
   Uses unbinarized data. */
list[AprioriMap] Apriori(Cluster[int] T)
{
  real minW = sum(T<1> + [0.00]) * minSup + 0.0; 

	/* Generate Initial Candidates */
	AprioriMap C = ( [s] : 0 | s <- {i | <s, _> <- T, i <- s});	
	C = calcWeights(C, T);
	C = prune(C, minW);

	/* list [ lrel [ indexes, weight ] ]
	   Frequent item sets */
	list[AprioriMap] L = [];

	/* While candidates still exist generate a new candidate set 
	   and add the valid subset to the frequent itemset list */
	while( size(C) != 0 )
	{
		L += [ C ];

		C = genCombos(L[-1]<0>);	
		C = calcWeights(C, T);
		C = prune(C, minW);
	}

	return L;
}

/* Build a prediction given a list of frequent item sets, a query point,
   a key ( to translate plugin indexes to names ) and a prediction 
   threshold. The prediction threshold is the lowest value a prediction
   can be and still be included in the prediction. */
lrel[real, list[str], list[int]] predictApriori(list[AprioriMap] M, list[int] q, Key key, real pThres = .2) 
{
	lrel[list[int], real] p = [];
	int querySize = size(q);
	
	/* if q isn't frequent */
	if( querySize >= size(M) || q notin M[querySize - 1] ) return []; 
	
	real querySupport = M[i - 1][q] + 0.0;
	
	/* Scan M for frequent itemsets containing q
	   that have a support >= the minimum support */
	for( k <- [0 .. size(M) - querySize ], n <- M[k] ) 
	{
		int patternSize = querySize + k;
		list[int] itemSet = merge(n, q);
	
		if(e notin M[patternSize] || M[patternSize][itemSet] < minSup) continue;

		p += <e, M[patternSize][itemSet] / querySupport>;
	}
	
	return sort([ <e + 0.0, [key[f] | f <- s], s> | <n, e> <- p, s := n - q , e >= pThres ],bool(tuple[real, list[str], list[int]] a, tuple[real, list[str], list[int]]  b){ return a<0> > b<0>;}); 
}

/********************************************************************
						Subset Functions
********************************************************************/

/* Calculate the weight for each candidate in C using
   the Cluster of transactions T */ 
AprioriMap calcWeights( AprioriMap C, Cluster[int] T)
{
	for( <s, w> <- T,  n <- C,  n <= s)
			C[n] += w;
	
	return C;
}

/* Prune the candidate set C by removing anything
   below the minimum required weight */
AprioriMap prune( AprioriMap C, real minW ) = ( n : w | n <- C, w := C[n], w >= minW );

/* Generate the possible combinations given the previous 
   candidate set. this uses the F(k -1) * F(k - 1) method.
   The resulting map is each new combination assigned an
   arbitrary weight. */
AprioriMap genCombos( set[list[int]] C) = (dup(e) : -1 | <s, s1> <- C * C, s[..-1] == s1[..-1], s != s1, e:= merge(s, s1)); 

