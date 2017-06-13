module lang::php::experiments::plugins::learning::ClusterTransactions

import lang::php::util::Config;
import lang::php::experiments::plugins::learning::Utils;

import IO;
import List;
import ValueIO;
import util::Math;

/* Prediction threshold */
real pThres = .0001;

alias Cluster = tuple[ int w, list[int] rep, Matrix[int] contents ];
data ClusterTree = leaf(Cluster C)| cNode(ClusterTree L, ClusterTree R) | cNode(ClusterTree L); 

/********************************************************************
							Test Functions 
********************************************************************/

tuple[list[Cluster], Matrix[int], Key] readClust(str version)

{
	Matrix[int] D = binarize( readTextValueFile(baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.txt") );
	
	list[Cluster] C = unTree(group(D));

	Key k = readMap(baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.txt");
		
	return <C, D, k>;
}

lrel[num,str, int] tst(tuple[list[Cluster], Matrix[int], Key] A, list[int] p) = BMN(testBinarize(p, size(A[1][0])), A[0], A[2]);

/********************************************************************
							Build Functions 
********************************************************************/

/* Return the indexes of a list of strings in k */
list[int] findKeyIndex( list[str] s, Key k) 
{
	list[int] ret = [];
	for( <_, i, st> <- k, st in s)
	{
		ret += i;
	}
	return ret;
}

/* Retreive a list of Clusters from a Cluster Tree */
list[Cluster] unTree( ClusterTree T )
{
	list[Cluster] lst = [];
	visit(T) { case leaf(Cluster C): lst += C;}
	lst = [ <a, b, c> |< a, b, c > <- lst, a > 2 ];
	return lst;
}

/* Group a matrix and find each groups weight */
ClusterTree group ( Matrix[int] M ) = group(<0, [], M>);
ClusterTree group( Cluster C )
{
	if (size( C.contents) == 0) return leaf(C);
	
	Cluster cZero =  < 0, C.rep + 0, [] >;
	Cluster cOne = < 0,  C.rep + 1, [] >;
	
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

/********************************************************************
						Prediction Functions 
********************************************************************/

/* Best Matching Neighbors */
lrel[num, str, int] BMN( list[int] i, list[Cluster] C, Key key )
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
			
	return reverse(sort([ <e, head(key[{n}]), n> | n <- index(p), e := (p[n] / sz), e >= pThres  ]));
}

/********************************************************************
					Unimplemented Functions 
********************************************************************/

/* TODO: Improve implementation
   Currently Unused
   Try BMN-esque algorithm with a match tollerance */ 
list[Cluster] absorbNoise( list[Cluster] C )
{
	int t = 0;
	list[Cluster] ret = [];
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


