module lang::php::experiments::plugins::learning::Utils
 
import lang::php::ast::AbstractSyntax;
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Summary;

import IO;
import Map;
import List;
import String;
import Relation;

import util::Math;

alias RegMap = lrel[int index, NameModel model, str regexp];
alias HookModels = rel[NameOrExpr hookName, loc at, NameModel model];

alias Matrix[&T] = list[list[&T]];

/* lrel [ index, feature string ] */
alias Key = lrel[ int, str ];

/********************************************************************
 						Binarization Functions
********************************************************************/

/* Binarize a matrix of integers */
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

/* Create a vector of size 'size' with values in indexes
   'val' represented with a 1 */
list[int] testBinarize ( list[int] val, int size )
{
	int i = 0;
	list[int] B = [];
	for( n <- [0.. size] )
	{
		if( i in val ) B += 1;
		else B += 0;
		i += 1;
	}
	
	return B;
}

/********************************************************************
							Mathematic Functions
********************************************************************/

/* Manhattan Distance */
num dist( list[num] p, list[num] q )
{
	num d = 0;
	for ( i <- p, n <- q )
		d += abs( n - i );
	return d;
}

/********************************************************************
						Matrix Functions
********************************************************************/

/* Return the max value in 'M'
   Unused */
int maxM( Matrix[int] M )
{
	int m = 0;
	for ( s <- M, i <- s, i > m ) m = i;
	return m;
}

/* Return the average size in a 'M'
   Unused */
num cAverage( Matrix[int] M )
{
	num m = 0;
	for ( s <- M, e := size(s) ) m += e;
	return m;
}

/* Returns the average value of a dimension in a Matrix */
list[real] midVector( int d, Matrix[real] M ) = M[sort([ <e, n> | n <- index(M), e:= M[n][d] ])[floor( size(M) / 2 )][1]];

/* Returns the average value of a dimension in a Matrix
   Unused */
num avgVal( int d, Matrix[real] M )
{
	num a = 0;
	for( n <- M, e:= n[d] ) a += e;

	return (a / size(M));
}

/********************************************************************
						Read File Functions
********************************************************************/

/* Read a lrel[int, str] from file */
Key readMap(loc at)
{
	/* Should only be one line */
	str lines = readFileLines(at)[0][1..-1];
	
	Key ret = [];
	
	while(/^\<<index:[0-9]+>,\"<name:[^\"]+>\"[\>,]*<next:.*$>/ := lines)
	{
		ret += < toInt(index), name >;
		lines = next;
	}
	
	return ret;
}

/* Read a 2D matrix of reals from file */
Matrix[real] readPyMatrix( loc at )
{
	list[str] lines = readFileLines(at);
	Matrix[real]  ret = [];
	
	for( s <- lines )
		ret += [[toReal(n) | n <- split(" ", s)]];
	
	return ret;
}

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

/********************************************************************
						Visualization Functions
********************************************************************/

/* Print all nonzero matrix items and their index */
void printMatrix(Matrix[int] M)
{
	println("Matrix:");
	
	for( k <- [0 .. size(M) - 1])
	{
		for( n <- [0 .. size(M[k]) - 1] )
			if(M[k][n] > 0) println("M[<k>][<n>]: <M[k][n]>");
	}
}


