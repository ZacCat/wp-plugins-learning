module lang::php::experiments::plugins::learning::KDTree

import lang::php::util::Config;
import lang::php::experiments::plugins::learning::Utils;

import IO;
import List;
import String;
import Set;
import Map;
import ValueIO;
import util::Math;

/* Prediction threshold */
real pThres = 0.01;

/* represents a hyperrectangle */ 
alias KRegion = tuple[ list[real] min, list[real] max ];
/* kd-tree datatype */
data KDT = leaf( real w, list[real] V)
			| kdtNode(int dim, real w, list[real] V, KRegion B, KDT L, KDT R) 
			| kdtNode(int dim, real w, list[real] V, KRegion B, KDT L); 

/********************************************************************
							Testing functions
********************************************************************/

KDT readClusterKDT() = genKDT(readPyList(baseLoc + "/training/Unsupervised/TrainByClass-KMClabels1500-4.3.txt"), readPyMatrix(baseLoc + "/training/Unsupervised/TrainByClass-KMCarray1500-4.3.txt"));
KDT readFeatureKDT(real weight) = genKDT([ <weight, [i + 0.0 | i <- s]> | s <- binarize(readTextValueFile(#Matrix[int], baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-4.3.txt"))]);

lrel[real, str, int] tst(tuple[KDT, Key] K, list[int] p)
{
	int sz = size(K[0].B[0]);
	list[int] t = testBinarize(p, sz);

	Key key = readTextValueFile(#Key, baseLoc + "/training/Unsupervised/TrainByClass-Features-4.3.txt");

	kNN( K[0], t );
	println(size(neighborPQ));
	return predictNN( neighborPQ, K[1]);
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
/* Takes a list containing the label of each object and
   a matrix of clusters and generates a kD-Tree */
KDT genKDT( list[real] L, Matrix[real] M, int d = 1 ) = genKDT(makeWeightRel(L,M));
/* Takes a lrel [ weight, point ] and generates a kD-tree */
KDT genKDT( lrel[real w, list[real] V] M, int d = 1 ) = genKDT(M, startKRegion(M<1>));
/* Builds a KDT from a matrix */
KDT genKDT( lrel[real w, list[real] V] M, KRegion bound, int d = 1 )
{
	if (size( M ) == 1) return leaf(M[0].w, M[0].V);
	
	int dim = d % size(M<1>[0]);

	tuple[lrel[real, list[real]], lrel[real, list[real]]] T = <[],[]>;
	tuple[int l, int r] sz = <0,0>;
	
	tuple[real, list[real]] c = midVector(dim, M);
	real cut = c[1][dim];
	
	for( <w, s> <- (M - [c]), e := ((s[dim] > cut) ? 0 : 1 ))
	{
		T[e] += [<w, s>];
		sz[e] += 1;
	}
	
	if(sz[0] == 0 && sz[1] != 0)
		return kdtNode(dim, c[0], c[1], bound, genKDT(T[1], moveMin(bound, d, cut), d = d + 1));
	else if(sz[0] != 0 && sz[1] == 0)
		return kdtNode(dim, c[0], c[1], bound, genKDT(T[0], moveMax(bound, d, cut), d = d + 1));
	else
		return kdtNode(dim, c[0], c[1], bound, genKDT(T[0], moveMax(bound, d, cut),  d = d + 1), genKDT(T[1], moveMin(bound, d, cut), d = d + 1));
}

/********************************************************************
						Prediction Functions 
********************************************************************/

/* Arbitrarily high number */
real worstNN = 9999999.0;
/* Maximimum nearest neighbors to calculate, may be less */
int neighbors = 50;
/* Priority Queue holding the distances and coordinates of 
   the closest points */
lrel[real d, real w, list[real] V] neighborPQ = [];

/* Predict 'p' using 'K' and store the results in neighborPQ */
void kNN( KDT K, list[int] p) = kNN( K, p, K.B);
void kNN( KDT K, list[int] p, KRegion bound)
{
	if( !isRegionClose(bound, p) ) return;

	switch( K )
	{
		case leaf(real w, list[real] V): return updatePQ(<w * dist(K.V, p), w, V>);
		case kdtNode(int dim, real w, list[real] V, KRegion B, KDT L, KDT R):
		{
			real cut = V[dim];
			if( cut >= p[dim] ){
				updatePQ( < dist(K.V, p), w, K.V> );
				kNN(R, p, B);
				if(isRegionClose(B, p)) kNN(L, p, B);
			}
			else{
				updatePQ( < dist(K.V, p), w, K.V> );
				kNN(L, p, B);
				if(isRegionClose(B, p)) kNN(R, p, B);				
			}
		}
		case kdtNode(int dim, real w, list[real] V, KRegion B, KDT L):{
			updatePQ(<dist(K.V, p), w, K.V>);
			kNN(L, p, B);
		}
	}
}

/* Insert a new point in neighborPQ */
void updatePQ( tuple[real, real, list[real]] p )
{
	int sz = size(neighborPQ);
	if( sz <= neighbors || p[0] <= worstNN )
	{
		neighborPQ += [p];
		neighborPQ = sort(neighborPQ);
		sz += 1;
		if(sz > neighbors) sz = neighbors;
		neighborPQ = head(neighborPQ, sz);
		
		worstNN = neighborPQ<0>[sz - 1];
	}
}

lrel[real, str, int] predictKNN(KDT K, list[int] q, Key key, int sz) 
{ 
  kNN(K, testBinarize(q, sz)); 
  return predictNN(neighborPQ, key); 
} 

/* Perform Best Matching Neighbors using the nearest 
   Clusters/Transactions without narrowing by query 
   pattern*/
lrel[real, str, int] predictNN(lrel[real d, real w, list[real] V]  M, Key key) 
{
	real maxD = max(M<0>);
	real minD = min(M<0>);
	
	list[real] ret = [0.0 | n <- index(M[0]<2>)];
	for( <d, w, V> <- M )
	{
		for( n <- index(V), e := V[n], e > 0 )
			ret[n] += sim(maxD, minD, d, w, e);
	}
  real sz = size(M) + 0.0; 
	return sort([ <e, key[n], n> | n <- index(ret), e := ret[n] / sz , e >= pThres ], bool(tuple[real,str,int] a, tuple[real,str,int]  b){ return a<0> > b<0>;});
}

/********************************************************************
						Hyperrectangle Functions 
********************************************************************/

/* Returns the average value of a dimension in a Matrix */
tuple[real, list[real]] midVector( int d, lrel[real, list[real]] M ) = M[sort([ <e, w, n> | n <- index(M), e:= M<1>[n][d], w := M<0>[n] ])[floor( size(M) / 2 )][2]];

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
   the query point to be eligible as a NN */
bool isRegionClose(KRegion B, list[int] V) 
{
	/* Cant turn down a region when the PQ isn't full */
	if(size(neighborPQ) < neighbors) return true;
	
	real d = 0.0;
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

/********************************************************************
							General Utilities
********************************************************************/

/* Use the density of each cluster as its weight */
lrel[&T, &E] makeWeightRel( list[&T] w, list[&E] b){
	return tolrel([ n + 0.0 | n <- sort(toList(distribution(w)))<1>], b);
}

lrel[&T, &E] tolrel( list[&T] a, list[&E] b)
{
	if(size(a) != size(b))
	{
		println("Incompatable lists");
		return [];
	}
	
	return [<a[i], b[i]> | i <- index(a)];
	
	
}
