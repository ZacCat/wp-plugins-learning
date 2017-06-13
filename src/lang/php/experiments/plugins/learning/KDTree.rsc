module lang::php::experiments::plugins::learning::KDTree

import lang::php::util::Config;
import lang::php::experiments::plugins::learning::Utils;

import IO;
import List;
import String;

import util::Math;

/* Prediction threshold */
real pThres = 0.1;

/* represents a hyperrectangle */ 
alias KRegion = tuple[ list[real] min, list[real] max ];
/* kd-tree datatype */
data KDT = leaf( list[real] V)
			| kdtNode(int dim, list[real] V, KRegion B, KDT L, KDT R) 
			| kdtNode(int dim, list[real] V, KRegion B, KDT L); 

/********************************************************************
							Testing functions
********************************************************************/

KDT readKDT() = genKDT( readPyMatrix(baseLoc + "/training/Unsupervised/TrainByClass-KMCarray1635-4.3.txt") );

lrel[num, str, int] tst(KDT K, list[int] p)
{
	Matrix[real] M =  readPyMatrix(baseLoc + "/training/Unsupervised/TrainByClass-KMCarray1635-4.3.txt");
	
	int sz = size(M[0]);
	list[int] t = testBinarize(p, sz);

	Key key = readMap(baseLoc + "/training/Unsupervised/TrainByClass-Features-4.3.txt");
	
	predictKDT( K, t );
	println(size(neighborPQ));
	return predictNN( t, neighborPQ, key);
}

/********************************************************************
							Build Functions 
********************************************************************/

/* Generate a region that encompases all points */
KRegion startKRegion( Matrix[real] M )
{
	KRegion B =  < M[0], M[0]>;
	for( s <- M, n <- index(s))
	{
		if(s[n] > B.max[n]) B.max[n] = s[n]; 
		else if (s[n] < B.min[n]) B.min[n] = s[n];
	}
	
	return B;
}

// TODO: Split dimension with highest spread to better balance tree
KDT genKDT( Matrix[real] M, int d = 1 ) = genKDT(M, startKRegion(M));
/* Builds a KDT from a matrix */
KDT genKDT( Matrix[real] M, KRegion bound, int d = 1 )
{
	if (size( M ) == 1) return leaf(M[0]);
	
	int dim = d % size(M[0]);

	Matrix[real] L = [];
	Matrix[real] R = [];
	
	list[real] a = midVector(dim, M);
	real cut = a[dim];
	
	for(s <- ( M - [a] ), e := s[dim])
	{
		if (e > cut)
			R += [s];
		else
			L += [s];
	}
	
	int lW = size(L);
	int rW = size(R);
	
	if(lW == 0 && rW != 0)
	{
		KRegion rBound = moveMin(bound, d, cut);
		return kdtNode(dim, a, bound, genKDT(R, rBound, d = d + 1));
	}
	else if(lW != 0 &&  rW == 0)
	{
		KRegion lBound = moveMax(bound, d, cut);
		return kdtNode(dim, a, bound, genKDT(L, lBound, d = d + 1));
	}
	else
	{
		KRegion lBound = moveMax(bound, d, cut);
		KRegion rBound = moveMin(bound, d, cut);
		return kdtNode(dim, a, bound, genKDT(L, lBound,  d = d + 1), genKDT(R, rBound, d = d + 1));
	}
}

/********************************************************************
						Prediction Functions 
********************************************************************/

/* Arbitrarily high number */
num worstNN = 9999999;
/* Maximimum nearest neighbors to calculate, may be less */
int neighbors = 50;
/* Priority Queue holding the distances and coordinates of 
   the closest points */
lrel[num d, list[real] V] neighborPQ = [];

/* Predict 'p' using 'K' and store the results in neighborPQ */
void predictKDT( KDT K, list[int] p) = predictKDT( K, p, K.B);
void predictKDT( KDT K, list[int] p, KRegion bound)
{
	if( !isRegionClose(bound, p) ) return;

	switch( K )
	{
		case leaf(list[real] V): return updatePQ(<dist(K.V, p), V>);
		case kdtNode(int dim, list[real] V, KRegion B, KDT L, KDT R):
		{
			real cut = V[dim];
			if( cut >= p[dim] ){
				predictKDT(R, p, B);
				if(isRegionClose(B, p)) predictKDT(L, p, B);
				return updatePQ( <dist(K.V, p), K.V> );
			}
			else{
				predictKDT(L, p, B);
				if(isRegionClose(B, p)) predictKDT(R, p, B);
				
				return updatePQ( <dist(K.V, p), K.V> );
			}
		}
		case kdtNode(int dim, list[real] V, KRegion B, KDT L): return updatePQ(<dist(K.V, p), K.V>);
	}
}

/* Insert a new point in neighborPQ */
void updatePQ( tuple[num, list[real]] p )
{
	int sz = size(neighborPQ);
	if( sz <= neighbors || p[0] <= worstNN )
	{
		neighborPQ += p;
		neighborPQ = sort(neighborPQ);
		sz += 1;
		if(sz > neighbors) sz = neighbors;
		neighborPQ = head(neighborPQ, sz);
		
		worstNN = neighborPQ<0>[sz - 1];
	}
}

/* Perform Best Matching Neighbors using 
   the nearest Clusters */
lrel[num, str, int] predictNN( list[int] p, lrel[num d, list[real] V] M, Key key)
{
	num maxD = max(M<0>);

	list[real] ret = [0.0 | n <- p];
	
	for( <d, V> <- M )
	{
		for( n <- index(V) )
		{
			ret[n] +=  V[n] * (maxD - d);
		}
	}
	
	num sz = size(M) + sum(M<0>);
	return reverse(sort([ <e, head(key[{n}]), n> | n <- index(ret), e := (ret[n] / sz), e >= pThres  ]));
}

/********************************************************************
						Hyperrectangle Functions 
********************************************************************/

/* Change the maximum value in the specified
   dimension in the provided KRegion */
KRegion moveMax(KRegion B, int dim, real cut)
{
	B.max[dim] = cut;
	
	return B;
}

/* Change the minimum value in the specified
   dimension in the provided KRegion */
KRegion moveMin(KRegion B, int dim, real cut)
{
	B.min[dim] = cut;
	return B;
}

/* Deturmine if the region is close enough to
   the query point to be eligible to be a NN */
bool isRegionClose(KRegion B, list[num] V) 
{
	/** Cant turn down a region when the PQ isn't full **/
	if(size(neighborPQ) < neighbors) return true;
	
	num d = 0.0;
	for(i <- index(V), min := B.min[i], max := B.max[i], c := V[i]) 
	{
		if( min > c)
		{
			d += (min - c);
		} 
		else if( max < c )
		{
			d += (c - max);
		}
		if(d > worstNN) return false;
	}
	
	return true;
}
