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
list[AprioriMap] Apriori(Cluster[int] T)
{
	real minW = sum(T<1>) * minSup + 0.0;

	/* Candidates */
	AprioriMap C = ( [s] : 0 | s <- {i | <s, _> <- T, i <- s});	
	C = calcWeights(C, T);
	C = getValid(C, minW);

	/* list [ lrel [ indexes, weight ] ]
	   Frequent item sets */
	list[AprioriMap] L = [];

	while( size(C) != 0 )
	{
		L += [ C ];

		C = genCombos(L[-1]<0>);	
		C = calcWeights(C, T);
		C = getValid(C, minW);
	}

	return L;
}

lrel[real, list[str], list[int]] predictApriori(list[AprioriMap] M, list[int] q, Key key)
{
	real pThres = .2;
	
	lrel[list[int], real] p = [];
	int i = size(q);
	
	/* q isn't frequent */
	if( i >= size(M) || q notin M[i - 1] ) return [];
	
	real qS = M[i - 1][q] + 0.0;
	
	for( k <- [0 .. size(M) - i ], n <- M[k],  j := i + k, e:= merge(n, q), e in M[j], v := M[j][e], v >= minSup )
		p += <e, v / qS>;

	return sort([ <e + 0.0, [key[f] | f <- s], s> | <n, e> <- p, s := n - q , e >= pThres ],bool(tuple[real, list[str], list[int]] a, tuple[real, list[str], list[int]]  b){ return a<0> > b<0>;});
}

/********************************************************************
						Subset Functions
********************************************************************/

AprioriMap calcWeights( AprioriMap C, Cluster[int] T)
{
	for( <s, w> <- T,  n <- C,  n <= s)
			C[n] += w;
	
	return C;
}

AprioriMap getValid( AprioriMap T, real minW ) = ( n : w | n <- T, w := T[n], w >= minW );

AprioriMap genCombos( set[list[int]] C) = (dup(e) : 0 | <s, s1> <- C * C, s[..-1] == s1[..-1], s != s1, e:= merge(s, s1));

