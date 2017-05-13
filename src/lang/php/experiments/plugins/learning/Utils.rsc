module lang::php::experiments::plugins::learning::Utils
 
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Summary;
import lang::php::ast::AbstractSyntax;

import IO;
import List;
import Relation;
import Map;
import String;

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

/* Return the index of 'hnUse' in 'regexps' */
int getIndexList(NameOrExpr hnUse, PluginSummary psum, RegMap regexps)
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
str getIndexListString(int k, RegMap regexps)
{
	int i = 0;
	
	for ( n <- regexps<1>)
	{
		if(i == k) return n;
		
		i = i + 1;
	}
	
	return null;
}
