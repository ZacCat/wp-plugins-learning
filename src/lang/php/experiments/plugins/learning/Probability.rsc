module lang::php::experiments::plugins::learning::Probability

import lang::php::util::Config;
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Summary;
import lang::php::ast::AbstractSyntax;
import lang::php::experiments::plugins::Locations;
import lang::php::util::Utils;
import lang::php::pp::PrettyPrinter;
import lang::php::experiments::plugins::learning::Utils;

import IO;
import Set;
import List;
import Relation;
import ValueIO;
import Map;
import ListRelation;
import String;

import util::Math;

int q = 1;

alias HookMatrix = tuple[list[list[int]] matrix, RegMap regexps, list[int] totals];

/* Populate an empty HookMatrix */
HookMatrix initMatrix(str version)
{
	PluginSummary wpsum = loadWordpressPluginSummary(version);
	
	// Generate a relationship of all RegMaps in wpsum
	RegMap regexps = (  nm : regexpForNameModel(nm) | <hn, _ > <- (wpsum.providedActionHooks + wpsum.providedFilterHooks), nm :=  nameModel(hn,wpsum));
		
	list[int] i = [0];
	list[int] t = [];
	list[list[int]] k = [];
	
	// Initialize M.matrix and M.totals to 0:
	// M.matrix is a lower triangular adjacency matrix
	for(n <- [0 .. size(regexps)] )
	{
		t += [0];
		k = k + [i];
		i += [0];
	}

	return <k, regexps, t>;
}

/* Read a HookMatrix from a file */
HookMatrix getFromFile(str v)
{
	return readBinaryValueFile(#HookMatrix, |file:///home/zac/corpus/HookMatrix<v>|);
}

/* Create a new HookMatrix populated with information generated 
from all plugins compatible with the given WordPress version */
HookMatrix trainWithAllPlugins(str version)
{
	HookMatrix M = initMatrix(version);

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
 
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version)
		{
			println("Training with: <l.file>");	
			// Extract all hooks from the plugin
			list[NameOrExpr] hNames = [hn | <hn, _,_,_> <- (psum.filters + psum.actions)];		
			
			M = insertClusterHooks(hNames, psum, M);
		}
	}

	// Save M to file
	writeBinaryValueFile(|file:///home/zac/corpus/HookMatrix-Plugin-<version>|, M);

	return M;
}

/* Create a new HookMatrix populated with information generated 
from all plugins compatible with the given WordPress version,
relations deturmined by classes */
HookMatrix trainWithAllClasses(str version)
{
	HookMatrix M = initMatrix(version);

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
 
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version)
		{
			println("Training with: <l.file>");
			
			for(<_, at,_> <- psum.classes )
			{
				// Extract all hooks from the class
				list[NameOrExpr] hNames = [hn | <hn, e,_,_> <- (psum.filters + psum.actions), insideLoc(e, at)];		
			
				M = insertClusterHooks(hNames, psum, M);
			}
		}
	}

	// Save M to file
	writeBinaryValueFile(|file:///home/zac/corpus/HookMatrix-Class-<version>|, M);

	return M;
}

/* Insert all hook relationships shown in the provided PluginSummary */
HookMatrix insertClusterHooks(list[NameOrExpr] hNames, PluginSummary psum, HookMatrix M)
{	
	// Create a list of each hooks index in M
	list[int] hooks = getIndexList(hNames, psum, M.regexps);
	
    if(q != 0) println("\tInserting clustered hooks: size <size(hooks)>");
    
    // If there is only one hook no permutations will be made by the following loop
	if(size(hooks) == 1 && hooks[0] >=0) return insertEdge(<hooks[0], hooks[0]>, M); 
	
	// Add all combonations of 'hooks' to M
	while ( !isEmpty(hooks) )
	{	
		int tloc = hooks[0];

		hooks = drop(1, hooks);

		for( h <- hooks )
		{							
			if(h >= 0 && tloc >= 0)
				M = insertEdge(<h, tloc> , M);
		}
	}
	
	if(q != 0)println("\tCluster Resolved");
	
	return M;
}

/* Insert an edge between the two indexes provided into
the HookMatrix */
HookMatrix insertEdge(tuple[int, int] i, HookMatrix M)
{
	if( i[0] < 0 || i[1] < 0 ) 
		return M; //Possible need to add new row
	elseif( i[0] == i[1] ) // self-loop
	{
		M.totals[i[0]] += 1;
		M.matrix[i[0]][i[0]] += 1;
	}
	elseif( i[0] < i[1] ) // Ensure the indexes are in the correct order
	{
		M.totals[i[0]] += 1;
		M.totals[i[1]] += 1;
		M.matrix[i[1]][i[0]] += 1;
	}
	else 
	{
		M.totals[i[0]] += 1;
		M.totals[i[1]] += 1;
		M.matrix[i[0]][i[1]] += 1;
	}
	
	return M;
}

/* Find all the top 10 most likley plugins */
lrel[str,num, num] findProbable(HookMatrix M)
{
	lrel[int, num] i = [];
	
	for( k <- [0 .. size(M.totals) ] )
		i += <k, totalProbability(k, M)>;
	
	// Sort by decreasing probability, and return the top 10
	// TODO: Do this without 5 function calls.
	i = reverse(tail(invert(sort(invert(i))), 10));

	// Return < Name of Plugin, Plugin Index, Probability >
	return  [ <n, e, p> | <e, p> <- i , n:= getIndexListString(e, M.regexps)];
}

/* Find the conditional probability of the top 
10 most likley plugins */
lrel[str, num, num] findProbable(list[int] given, HookMatrix M)
{
	lrel[int, num] i = [];
	for( k <- given )
	{
		for( n <- [ k .. size(M.matrix) - 1] - given)
			if(M.matrix[n][k] > 0) i = addProbable(i, <n, conditionalProb(n, given, M)>);

		for( n <- [ 0 .. k ] - given )
			if(M.matrix[k][n] > 0) i = addProbable(i, <n, conditionalProb(n, given, M)>);
	}
	
	// Sort by decreasing probability, and return the top 10
	// TODO: Do this without 5 function calls.
	i = reverse(tail(invert(sort(invert(i))), 10));
	
	// Return < Name of Plugin, Plugin Index, Probability >
	return [ <n, e, p> | <e, p> <- i , n:= getIndexListString(e, M.regexps)];
}

/* Insert k to i if k<0> doesn't exist in i, or
add the new probability to the existing value  */
lrel[int, num] addProbable(lrel[int, num] i, tuple[int, num] k)
{
	int j = 0;
	
	for( <v, n> <- i)
	{
		if(k[0] == v)
		{
			i[j] = <v, n + i[j][1]>;
			return i;
		}
		
		j += 1;
	}
	
	return i + k;
}

/* Find the probability of a plugin in
relation to the entire matrix */
num totalProbability(int of, HookMatrix M)
{
	num P = 0;
	
	P = total(of, M);
	
	int t = 0;
		
	return P / total([0 .. size(M.matrix) - 1], M);
}

/* Find the probability of P( of | given ) */
num conditionalProb(int of, int given, HookMatrix M)
{
	num P = 0;
	
	//Ensure the indexes are in the correct order
	if( of > given ) P = M.matrix[of][given];
	else P = M.matrix[given][of];
	
	return 	P / total([given, of], M);
}

/* Find the sum of all P( of, g ) | g <- given */
num conditionalProb(int of, list[int] given, HookMatrix M)
{
	num P = 0; // total( given + [of], M );
	
	for( i <- given )
	{
		//Ensure the indexes are in the correct order
		if( of > i ) P += M.matrix[of][i];
		else P += M.matrix[i][of];
	}
	
	return P / total( given + [of], M );
}

/* Find the sum of the number of times each plugin
is referenced in the provided HookMatrix */
int total(list[int] i, HookMatrix M)
{
	int t = 0;
	
	for( n <- i )
		t += total(n, M);
	
	return t;
}

/* Find the number of times the provided plugin
is referenced in the provided HookMatrix */
int total(int i, HookMatrix M)
{	
	return M.totals[i];
}
