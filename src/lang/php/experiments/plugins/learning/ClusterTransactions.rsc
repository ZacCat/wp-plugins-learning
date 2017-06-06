module lang::php::experiments::plugins::learning::ClusterTransactions

import lang::php::experiments::plugins::Locations;
import lang::php::util::Config;
import lang::php::experiments::plugins::learning::Utils;

import List;
import util::Math;
import IO;
import ValueIO;

// prediction threshold
real pThres = .1;

alias Clust = tuple[ int w, list[int] rep, Matrix[int] contents ];
data ClusterTree = leaf(Clust C)| cNode(ClusterTree L, ClusterTree R) | cNode(ClusterTree L); 
	
// lrel [ binarized index, origional index, feature string ]
alias Key = lrel[ int, int, str ];

tuple[list[Clust], Matrix[int], Key] readClust(str version)

{
	Matrix[int] D = binarize( readTextValueFile(baseLoc + "/training/Cluster/TrainByClass-fMatrix-<version>.txt") );
	
	list[Clust] C = unTree(group(D));

	Key k = bMap(readTextValueFile(baseLoc + "/training/Cluster/TrainByClass-fMatrix-4.3.txt"), readMap(baseLoc + "/training/Cluster/TrainByClass-Features-<version>.txt"));
		
	return <C, D, k>;
}

lrel[num,str, int] tst(tuple[list[Clust], Matrix[int], Key] A, list[int] p) = BNN(testBinarize(p, size(A[1][0])), A[0], A[2]);

// p = list of given values; sz = vector size; C = value, k = deBinarization key
//lrel[num,str, int] tst(list[int] p, int sz, list[Clust] C, Key k) = BNN(testBinarize(p, sz), C, k);

// Return the indexes of a list of strings in k
list[int] findKeyIndex( list[str] s, Key k) 
{
	list[int] ret = [];
	for( <_, i, st> <- k, st in s)
	{
		ret += i;
	}
	return ret;
}

// TODO: Improve implementation
// Currently Unused
// Try BNN-esque algorithm with a match tollerance 
list[Clust] absorbNoise( list[Clust] C )
{
	int t = 0;
	list[Clust] ret = [];
	for(i <- index(C), n <- index(C) - i, c1 := C[i],c2 := C[n])
	{
		t += 1;
		if( t % 1000 == 0) println(t);
		// dist 2 == two different features
		if( c1.w > c2.w * 2 && dist(c1.rep, c2.rep) <= 2 )
		{
			println("Absorbed");
			ret += < c1.w + c2.w, c1.rep, dup(c1.contents + c2.contents + c2.rep) >;
			break;
		}
	}
	return ret;
}

// Retreive a list of Clusters from a Cluster Tree
list[Clust] unTree( ClusterTree T )
{
	list[Clust] lst = [];
	visit(T) { case leaf(Clust C): lst += C;}
	lst = [ <a, b, c> |< a, b, c > <- lst, a > 2 ];
	return lst;
}

// Group a matrix and find each groups weight
ClusterTree group ( Matrix[int] M ) = group(<0, [], M>);
ClusterTree group( Clust C )
{
	if (size( C.contents) == 0) return leaf(C);
	
	Clust cZero =  < 0, C.rep + 0, [] >;
	Clust cOne = < 0,  C.rep + 1, [] >;
	
	for( s <- C.contents, e := headTail( s ))
	{
		if ( e[0] == 0 )
		{
			if(size( e[1] ) != 0) cZero.contents += [e[1]];
			cZero.w += 1;
		}
		else
		{
			if(size( e[1] ) != 0)cOne.contents += [e[1]];
			cOne.w += 1;	
		}
	}
	
	if(cZero.w == 0 && cOne.w != 0) return cNode(group(cOne));
	else if(cZero.w != 0 &&  cOne.w == 0) return cNode(group(cZero));
	else return cNode(group(cZero), group(cOne));
}

/* Best Matching Neighbors */
lrel[num, str, int] BNN( list[int] i, list[Clust] C, Key key )
{
	list[num] p = [];
	int sz = 0;
	
	int j = 0;
	for( <w, s, _>  <- C )
	{
		bool match = true;
		for( n <- index( i ), match, i[n] > 0, s[n] != i[n])
			match = false;

		if( match )
		{
			p = size(p) == 0 ? [ n * w | n <- s ] : [ f | k <- index( s ), f:= p[k] + (w * s[k]) ];
			sz += w;
		}
	}
			
	return reverse(sort([ <e, f, n> | n <- index(p), e := (p[n] / sz), e != 1, e >= pThres, s := key[_,n], size(s) > 0, f := head(s) ]));
}

list[str] deBinarize( list[int] V, Key k )
{
	int i = 0;
	list[int] B = [];
	for( n <- V )
	{
		if( n > 0 ) B += i;
		i += 1;
	}
	
	return  [ f | n <- B, e:= k[_,n], size(e) > 0, f := head(e)];
}

// Return the map used to binarize M
// Using an 'S'and an unbinarized M
// generated from Unsupervised.rsc
Key bMap( Matrix[int] M, map[int, str] S )
{
	list[int] R = sort(dup([ i | s <- M, i <- s ]));
	
	// map [ origional index, binarized index ]
	Key K = [ <i, n, S[i]> | n <- index( R ), i := R[n] ];
	
	return K;
}

// Binarize a matrix of integers
Matrix[int] binarize( Matrix[int] M )
{
	list[int] S = sort(dup([ i | s <- M, i <- s ]));
	
	map[ int, int ] K = ( i : n | n <- index( S ), i := S[n] );
	
	Matrix[int] B = [];
	list[int] bVector = [ 0 | n <- index( S ) ];
	
	for( s <- M )
	{
		list[int] tVector = bVector;
		for( n <- s ) tVector[K[n]] = 1;
		B += [tVector];
	}

	return B;
}
