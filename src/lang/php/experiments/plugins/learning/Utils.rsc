module lang::php::experiments::plugins::learning::Utils
 
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Summary;
import lang::php::ast::AbstractSyntax;

import IO;
import List;
import Relation;
import Map;
import String;

alias RegMap = lrel[int index, NameModel model, str regexp];
alias HookModels = rel[NameOrExpr hookName, loc at, NameModel model];

alias Matrix[&T] = list[list[&T]];

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

// Create a vector of size 'size' with values in indexes
// in 'val' represented with 1
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

/* Manhattan Distance */
num dist( list[num] p, list[num] q )
{
	num d = 0;
	for ( i <- p, n <- q )
		d += abs( n - i );
	return d;
}

num abs( num i ) = i > 0 ? i : -i;


map[int, str] readMap(loc at)
{
	// Only one line
	str lines = readFileLines(at)[0][1..-1];
	
	//println(lines);
	
	map[int, str] ret = ();
	
	while(/^<index:[0-9]+>\:\"<name:[^\"]+>\",*<next:.*$>/ := lines)
	{
		ret += ( toInt(index) : name );
		lines = next;
	}
	
	return ret;
}

Matrix[real] readPyMatrix( loc at )
{
	list[str] lines = readFileLines(at);
	Matrix[real]  ret = [];
	
	for( s <- lines )
		ret += [[toReal(n) | n <- split(" ", s)]];
	
	return ret;
}

/* Returns the labels defined in WP (hooks) */
RegMap labelRegMap(PluginSummary wpsum)
{
	int i = 0;
	RegMap regexps = [];
	// Expressions containing .* need to be placed at the end to promote matching the most specific hook
	for( <hn, _ > <- (wpsum.providedActionHooks + wpsum.providedFilterHooks), nm :=  nameModel(hn,wpsum), e := regexpForNameModel(nm), e != ".*")
	{
			regexps += <i, nm, e>;
			i += 1;
	}
		
	return regexps;
	//return [ < i,  nm, e > | <hn, _ > <- (wpsum.providedActionHooks + wpsum.providedFilterHooks), nm :=  nameModel(hn,wpsum), e := regexpForNameModel(nm), e != ".*"];
}

/* Return the index of 'hnUse' in 'regexps' */
int getIndexList(NameOrExpr hnUse, PluginSummary psum, RegMap regexps) = getIndexAndSpecificity(hnUse, psum, regexps)[0];

/* Return the index of 'hnUse' in 'regexps' */
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
			println("Bad regexp <r[n]> caused by bug in source, skipping");
		}
	}
	
	return s;
}

/* Return the index of the provided string in M.matrix */
int getStringIndex(str k, RegMap regexps)
{
	// ??    = regexps[ _, , k ]<0>
	for ( <i, _, n> <- regexps, n == k) return i;
	
	return null;
}

/* Return the string value represented 
index k in the provided MultiLabel*/
str getIndexListString(int k, RegMap regexps)
{
	for ( <i, _, n> <- regexps, i == k) return n;
	
	return null;
}

int maxM( Matrix[int] M )
{
	int m = 0;
	for ( s <- M, i <- s, i > m ) m = i;
	return m;
}

num cAverage( Matrix[int] M )
{
	num m = 0;
	for ( s <- M, e := size(s) ) m += e;
	return m;
}
