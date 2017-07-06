module lang::php::experiments::plugins::learning::Utils
 
import lang::php::pp::PrettyPrinter;
import lang::php::ast::AbstractSyntax;
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Summary;

import IO;
import Map;
import List;
import String;
import Relation;

import util::Math;

/* Number of predictions to compare */
public int hVal = 10;

/********************************************************************
								Aliases
********************************************************************/

alias RegMap = lrel[int index, NameModel model, str regexp];
alias HookModels = rel[NameOrExpr hookName, loc at, NameModel model];

alias Matrix[&T] = list[list[&T]];

/* map [ index, feature string ] */
alias Key = map[ int, str ];

alias Cluster[&T <: num] = lrel[list[int], &E];

/********************************************************************
 						Binarization Functions
********************************************************************/

/* Binarize a matrix */
Matrix[&T] binarize( Matrix[&T] M)
{
	list[&T] S = sort(dup([ i | s <- M, i <- s ]));
	
	/* map [ old value, new index  ] */
	map[ &T, int ] K = ( i : n | n <- index( S ), i := S[n] );
		
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

/* Create a vector of size 'size' with values in indexes
   'val' represented with a 1 */
list[int] testBinarize ( list[int] val, int sz ) = [ i in val ? 1 : 0 | i <- [0 .. sz]] ;

Matrix[&T] reIndexMatrix( Matrix[&T] M )
{
	list[&T] S = sort(dup([ i | s <- M, i <- s ]));
	
	/* map [ old value, new index  ] */
	map[ &T, int ] K = ( i : n | n <- index( S ), i := S[n] );
		
	Matrix[&T] ret = [[K[n] | n <- s ] | s <- M];
	
	return ret;
}

Key reIndexKey( Matrix[&T] M, map[&T, str] key )
{
	list[&T] S = sort(dup([ i | s <- M, i <- s ]));
	
	/* map [ old value, new index  ] */
	map[ &T, int ] K = ( i : n | n <- index( S ), i := S[n] );

	return ( K[old] : key[old] | old <- K );
}

list[int] unBinarize(list[int] V) = [i | i <- index(V), V[i] == 1];

/********************************************************************
							Mathematic Functions
********************************************************************/

/* Manhattan Distance */
real dist( list[&T <: num] p, list[&T <: num] q )= sum([ d | i <- index(p),d := abs( p[i] - q[i] )]) + 0.0;

/* Similarity Score */
real sim( real maxD, real minD, real d, real w, real v) = ((maxD == minD) ? 1.0 : ((maxD - d)/ (maxD - minD))) * v * w;

real listAvg( list[&T <: num] lst ) = sum(lst) / size(lst) + 0.0;

/********************************************************************
						Matrix Functions
********************************************************************/

/* Returns the aggragete weight of
   each duplicate value M */
lrel[list[&T], &E <: num] weightedDistribution(lrel[list[&T], &E <: num] M) = [ <s, sum(M[s])> | s <- dup(M<0>)];

/* Return the max value in 'M'
   Unused */
&T <: num maxM( Matrix[&T <: num] M )
{
	int m = 0;
	for ( s <- M, i <- s, i > m ) m = i;
	return m;
}

/* Return the average size in a 'M'
   Unused */
&T <: num cAverage( Matrix[&T <: num] M )
{
	num m = 0;
	for ( s <- M, e := size(s) ) m += e;
	return m;
}

/********************************************************************
						Read File Functions
********************************************************************/

/* Read a 2D matrix of reals from file */
Matrix[real] readPyMatrix( loc at )
{
	list[str] lines = readFileLines(at);
	Matrix[real]  ret = [];
	
	for( s <- lines )
		ret += [[toReal(n) | n <- split(" ", s)]];
	
	return ret;
}

/* Read a list of reals from file */
list[real] readPyList( loc at ) = [ toReal(s) | s <- readFileLines(at)];

/********************************************************************
				Feature Generation/Look-up Functions
********************************************************************/

/* Return a list of feature names */
list[NameOrExpr] featureList(PluginSummary psum, loc at) = [hn | <hn, e,_,_> <- (psum.filters + psum.actions), insideLoc(e, at), pp(hn) != ".*"];

/* Returns a RegMap of the hooks defined in WP */
RegMap labelRegMap(PluginSummary wpsum)
{
	int i = 0;
	RegMap regexps = [];
	/* Expressions containing .* need to be placed at the end to promote matching the most specific hook */
	for( <hn, _ > <- (wpsum.providedActionHooks + wpsum.providedFilterHooks), nm :=  nameModel(hn,wpsum), e := regexpForNameModel(nm), e != ".*")
	{
			regexps += <i, nm, e>;
			i += 1;
	}
		
	return regexps;
}

/* Return the index of 'hnUse' in 'regexps' */
int getIndexList(NameOrExpr hnUse, PluginSummary psum, RegMap regexps) = getIndexAndSpecificity(hnUse, psum, regexps)[0];

/* Return the index of 'hnUse' in 'regexps' w/ specificity */
tuple[int, int] getIndexAndSpecificity(NameOrExpr hnUse, PluginSummary psum, RegMap regexps)
{
	useString = stringForMatch(hnUse, psum);
	tuple[int, int] s = <-1,-1>;
	
	for ( < i, nm, n> <- regexps)
	{
		try 
		{
			if (rexpMatch(useString, n))
			{
				int t = specificity(nm,useString);
				if(s[0] < t)
				{
					s = <i, specificity(nm,useString)>;
				}
			}
		} catch v : {
			println("Bad regexp <n> caused by bug in source, skipping");
		}
	}
	
	return s;
}

/* Return the index of the provided string in regexps
   Unused */
int getStringIndex(str k, RegMap regexps){ for ( <i, _, n> <- regexps, n == k) return i; }

/* Return the string value represented index k
   Unused */
str getIndexListString(int k, RegMap regexps){ for ( <i, _, n> <- regexps, i == k) return n; }

