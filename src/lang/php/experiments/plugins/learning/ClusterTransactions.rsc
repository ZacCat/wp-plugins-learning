module lang::php::experiments::plugins::learning::ClusterTransactions

import lang::php::util::Config;
import lang::php::experiments::plugins::learning::Utils;

import IO;
import List;
import Map;
import Type;
import ValueIO;
import util::Math;
import ListRelation;

/* Prediction threshold */
real pThres = .1;

/********************************************************************
							Test Functions 
********************************************************************/

tuple[Cluster[&T <: num], int, Key] readClust(&T <: num w, str version)

{
	Matrix[int] M = readBinaryValueFile(#Matrix[int], baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.bin");
	Key k = readBinaryValueFile(#Key, baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.bin");

	Cluster[&T] C = buildCluster(M, w);	

	return <C, size(C<0>[0]), k>;
}

lrel[num,str, int] tstBMN(tuple[Cluster[&T <: num], int, Key] A, list[int] p) = BMN(testBinarize(p, A[1]), A[0], A[2]);
lrel[num,str, int] tstBMNWeighted(tuple[Cluster[&T <: num], int, Key] A, list[int] p) = DistWeightedBMN(testBinarize(p, A[1]), A[0], A[2]);

/********************************************************************
							Build Functions 
********************************************************************/

/* Binarize and build cluster from an unbinarized matrix with 
   uniform weight */
Cluster[&T] buildCluster( Matrix[&E] M, &T <: num w ) = weightedDistribution([ <s ,w> | s <- binarize(M)]);

/* Build cluster from a binarized matrix with uniform weight */
Cluster[&T] buildClusterBinarized( Matrix[&E] M, &T <: num w ) = weightedDistribution([ <s ,w> | s <- M]);

/* Build cluster from a unbinarized matrix with uniform weight */
Cluster[&T] unBinarizedBuildCluster( Matrix[&E] M, &T <: num w ) = weightedDistribution([ <sort(dup(s)) ,w> | s <- M]);

/********************************************************************
						Standard BMN Functions 
********************************************************************/

/* Best Matching Neighbors */
lrel[real, str, int] BMN( list[int] q, Cluster[&T <: num] C, Key key )
{
	list[real] p = [];
	
	for( <s, w>  <- C )
	{
		bool match = true;
		for( n <- index( q ), match, q[n] != 0, s[n] != q[n])	
			match = false;


		if( match )
			p = isEmpty(p) ? [ v * w + 0.0 | v <- s ] : [ f | k <- index( s ), f:= p[k] + (w * s[k]) ];
	}
	real sumW = sum(C<1>) + 0.0;
	
	return sort([ <e, key[n], n> | n <- index(p), e := (p[n] / sumW) + 0.0, e >= pThres], bool(&T a, &T b){ return a > b;});
}

/********************************************************************
					Distance Weighted BMN Functions 
********************************************************************/

/* Perdict Best Matching Neighbors using the nearest
   neighbors containing the query pattern
   May need to add kD-tree if too slow */
lrel[real, str, int] DistWeightedBMN( list[int] q, Cluster[&T <: num] C, Key key )
{
	lrel[ real, num, list[int]] NN = [];
	
	for( <s, w>  <- C)
	{
		bool match = true;
		for( n <- index( q ), match, q[n] != 0, s[n] != q[n])
			match = false;

		if( match )
			NN += <dist(q, s), w, s>;
	}
		
	return predictNN(NN, key);
}

/* Predict the probability of each feature */
lrel[real, str, int] predictNN( lrel[real d, num w, list[int] V]  M, Key key)
{
	real maxD = max(M<0>);
	real minD = min(M<0>);
	
	list[real] ret = [0.0 | n <- index(M[0]<2>)];
	for( <d, w, V> <- M )
	{
		for( n <- index(V), e := V[n], e > 0 )
			ret[n] += sim(maxD, minD, d, w + 0.0, e + 0.0);
	}
	
	num sz = sum(M<1>);
	return sort([ <e + 0.0, key[n], n> | n <- index(ret), e := ret[n] / sz , e >= pThres ], bool(tuple[real,str,int] a, tuple[real,str,int]  b){ return a<0> > b<0>;});
}

