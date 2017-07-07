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
Cluster[&T] buildClusterBinarized( Matrix[int] M, &T <: num w ) = weightedDistribution([ <s ,w> | s <- M]); 

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
	
 if(size(p) == 0) return []; 
   
  lrel[real, str, int]  ret = sort([ <e, key[n], n> | n <- index(p), q[n] == 0, e := (p[n] / sumW) + 0.0, e >= pThres], bool(tuple[real, str, int] a, tuple[real, str, int] b){ return a[0] > b[0];}); 
  int h = size(ret); 
  if(h > hVal) ret = head(ret, hVal); 
   
  return ret; 
} 
 
/* Best Matching Neighbors using unbinarized data */ 
lrel[real, str, int] BMNunB( list[int] q, Cluster[&T <: num] C, Key key ) 
{ 
	real pThres = 0.1; 
 
	map[int, real] p = (); 
	for( <s, w>  <- C, q <= s, n <- s, n notin q ) 
	{ 
		if( n in p) p[n] += w; 
		else p[n] = w  + 0.0; 
	} 

	if(size(p) == 0) return []; 
   
	real sumW = sum(C<1>) + 0.0; 
 
	lrel[real, str, int]  ret = sort([ <e, key[n], n> | n <- p, e := (p[n] +0.0) / sumW, e >= pThres], bool(tuple[real, str, int] a, tuple[real, str, int] b){ return a[0] > b[0];}); 
	int h = size(ret); 
	if(h > hVal) ret = head(ret, hVal); 
   
	return ret; 
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
		
	return predictNN(NN, key, q); 
}

/* Predict the probability of each feature */
lrel[real, str, int] predictNN( lrel[real d, num w, list[int] V]  M, Key key, list[int] q) 
{
	real pThres = 0.1; 
	
	if(size(M) == 0) return []; 
	
	real maxD = max(M<0>);
	real minD = min(M<0>);
	
	list[real] ret = [0.0 | n <- index(M[0]<2>)];
	for( <d, w, V> <- M )
	{
		for( n <- index(V), e := V[n], e > 0 )
			ret[n] += sim(maxD, minD, d, w + 0.0, e + 0.0);
	}
	
	num sz = sum(M<1>);
  lrel[real, str, int] res = sort([ <e + 0.0, key[n], n> | n <- index(ret), q[n] == 0, e := ret[n] / sz , e >= pThres ], bool(tuple[real,str,int] a, tuple[real,str,int]  b){ return a<0> > b<0>;}); 
  int h = size(res); 
  if(h > hVal) res = head(res, hVal); 
   
  return res; 
} 
 
/* Perdict Best Matching Neighbors using the nearest 
   neighbors containing the query pattern. 
   Uses unbinarized data. */ 
lrel[real, str, int] unBinDistWeightedBMN( list[int] q, Cluster[&T <: num] C, Key key, real pThres ) 
{ 
  lrel[ real, num, list[int]] NN = []; 
     
  for( <s, w>  <- C, q <= s ) 
  { 
    /* size((q - s) + (s - q)) == dist(q,s) */ 
    NN += <size((q - s) + (s - q)) + 0.0, w + 0.0, s >; 
  } 
     
  return unBinpredictNN(NN, key, q, pThres); 
} 
 
/* Predict the probability of each feature */ 
lrel[real, str, int] unBinpredictNN( lrel[real d, real w, list[int] V]  M, Key key, list[int] q, real pThres) 
{ 
	if(size(M) == 0) return []; 
	real maxD = max(M<0>); 
	real minD = min(M<0>); 
   
	real maxW = max(M<1>); 
	real minW = min(M<1>); 
   
	map[int, real] ret = (); 
	for( <d, w, V> <- M, s := sim(maxD, minD, d, maxW, minW, w + 0.0, 1.0)) 
	{ 
 		for( n <- V ) 
 		{ 
 			if(n in ret) ret[n] += s; 
			else ret[n] = s; 
			} 
		} 
   
	//real sz = sum(M<1>) + 0.0; 
	real sz = size(M) + 0.0; 
	if(sz == 0) return []; 
   
	lrel[real, str, int]  res = sort([ <e, key[n], n> | n <- ret, n notin q, e := ret[n] / sz, e >= pThres], bool(tuple[real, str, int] a, tuple[real, str, int] b){ return a[0] > b[0];}); 
	int h = size(res); 
	if(h > hVal) res = head(res, hVal); 
   
	return res; 
 }

