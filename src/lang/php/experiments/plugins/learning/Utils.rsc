module lang::php::experiments::plugins::learning::Utils
 
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Summary;
import lang::php::ast::AbstractSyntax;

import List;
import Relation;
import Map;

alias RegMap = map[NameModel model, str regexp];
alias HookModels = rel[NameOrExpr hookName, loc at, NameModel model];

/* Print all nonzero matrix items and their index */
void printMatrix(list[list[int]] i)
{
	println("Matrix:");
	
	for( k <- [0 .. size(i) - 1])
	{
		for( n <- [0 .. size(i[k]) - 1] )
			if(i[k][n] > 0) println("M[<k>][<n>]: <i[k][n]>");
	}
}

/* Return the index of 'hnUse' in 'r' */
int getListIndex(NameOrExpr hnUse, PluginSummary psum, RegMap regexps)
{
	int i = 0;
	
	useString = stringForMatch(hnUse, psum);
	
	for ( n <- regexps<1>)
	{
		try 
		{
			if (rexpMatch(useString, n)) return i;
		} catch v : {
			println("Bad regexp <r[n]> caused by bug in source, skipping");
		}
		
		i = i + 1;
	}
	
	return -1;
}


/* Return the list of indexes corresponding
to the list of NameOrExpr provided */
list[int] hookIndexes(list[NameOrExpr] hooks, PluginSummary psum, RegMap regexps)
{
	list[int] hIndex = [];
	
	for( h <- hooks )
		hIndex += getListIndex(h, psum, regexps);
		
	return hIndex;
}

/* Return the index of the provided string in M.matrix */
int getStringIndex(str k, RegMap regexps)
{
	int i = 0;
	
	for ( n <- regexps<1>)
	{
		if(n == k) return i;
		
		i = i + 1;
	}
}

/* Return the string value represented 
index k in the provided MultiLabel*/
str getIndexString(int k, RegMap regexps)
{
	int i = 0;
	
	for ( n <- regexps<1>)
	{
		if(i == k) return n;
		
		i = i + 1;
	}
	
	return null;
}
