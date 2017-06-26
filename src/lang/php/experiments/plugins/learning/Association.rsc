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

alias AprioriMap = map[list[int], int];

/********************************************************************
							Testing functions
********************************************************************/

tuple[Cluster[int], Key] readTrans(int w, str version)
{
	Matrix[int] M = readTextValueFile(#Matrix[int], baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.txt");
	Cluster[int] C = unBinarizedBuildCluster(M, w);
	
	Key k = readTextValueFile(#Key, baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.txt");
		
	return <C, k>;
}

list[AprioriMap] genApriori(tuple[Cluster[int], Key] A, real sup) = Apriori(A[0], sup);

/********************************************************************
						Prediction Functions
********************************************************************/

/* TODO: Weighted Associative Analysis; Hash Tree */
list[AprioriMap] Apriori(Cluster[int] T, real minSupport)
{
	real minW = sum(T<1>) * minSupport + 0.0;

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

AprioriMap genCombos( set[list[int]] C) = (dup(e) : 0 | <s, s1> <- C * C, s[0..-1] == s1[0..-1], s != s1, e:= merge(s, s1));

