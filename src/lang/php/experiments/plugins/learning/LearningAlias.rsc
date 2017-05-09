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
import String;

import util::Math;

int q = 1;

alias AdjMatrix = tuple[HookModels L, list[list[int]] matrix, RegMap regexps];
alias RegMap = map[NameModel model, str regexp];
alias HookModels = rel[NameOrExpr hookName, loc at, NameModel model];


AdjMatrix initMatrix(str version)
{
	PluginSummary wpsum = loadWordpressPluginSummary(version);
	
	AdjMatrix adjM;

	HookModels wpActionModels = { < hn, at, nameModel(hn,wpsum) > | < hn, at > <- wpsum.providedActionHooks };
	HookModels wpFilterModels = { < hn, at, nameModel(hn,wpsum) > | < hn, at > <- wpsum.providedFilterHooks };
	
	RegMap regexps = ( nm : regexpForNameModel(nm) | nm <- (wpActionModels<2> + wpFilterModels<2>) );

	//rel[str hookName, loc at] wpActionsJustNames = { < s, at > | < _, at, [ literalPart(str s) ] > <- wpActionModels };
	//rel[str hookName, loc at] wpFiltersJustNames = { < s, at > | < _, at, [ literalPart(str s) ] > <- wpFilterModels };
	
	//rel[str hookName, loc at] comb =  wpActionsJustNames + wpFiltersJustNames;
	
	//list[str] e =  [ pp(hn) | < hn, _ > <- wpsum.providedActionHooks +  wpsum.providedFilterHooks];
	//set[str] s = { n | <n, _>  <- comb };
	//list[str] sortS = sort(e);
	
	list[int] z = [0];
	
	list[list[int]] M = [];
		
	for(n <- [0 .. size(wpFilterModels + wpActionModels)] )
	{
		z += [0];
		M = M + [z];
	}
		
	//println("<size(wpsum.providedActionHooks + wpsum.providedFilterHooks)>");
	adjM = <(wpFilterModels + wpActionModels), M, regexps>;
	
	return adjM;
	//adjM = adjM + { < n, k + [m] > | n <- (wpsum.providedFilterHooks<0> + wpsum.providedActionHooks<0>), m = m + [0] };
}


AdjMatrix trainWithPlugin(PluginSummary psum, PluginSummary wpsum, AdjMatrix adjM)
{	
	HookModels actionModels = { < hn, at, nameModel(hn,psum) > | < hn, at > <- psum.providedActionHooks };
	HookModels filterModels = { < hn, at, nameModel(hn,psum) > | < hn, at > <- psum.providedFilterHooks };
	
	HookModels psumModels = actionModels + filterModels;

	//HookUses localFilterMatches = { };
	//for ( < hnUse, useloc, _, reg > <- psum.filters ) {
	//		useString = stringForMatch(hnUse,psum);
	//		for ( < hnDef, defloc, nm > <- filterModels) {
	//			try {
	//				if (rexpMatch(useString, regexps[nm])) {
	//					localFilterMatches = localFilterMatches + < useloc, hnUse, defloc, hnDef, reg, specificity(nm,useString) >;
	//				}
	//			} catch v : {
	//				println("Bad regexp <regexps[nm]> caused by bug in source, skipping");
	//			}
	//		}
	//	//}
	//}
	
	int i = 0;
	while( i < size(hooks))
	{
		useString = stringForMatch(hnUse,psum);
		try {
			if (rexpMatch(useString, regexps[nm])) {
				break;
			}
		} catch v : {
			println("Bad regexp <regexps[nm]> caused by bug in source, skipping");
		}
	}
		
	list[str] e =  [ pp(hn) | < hn, _ > <- psum.providedActionHooks +  psum.providedFilterHooks];

	return insertClusterHooks(sort(e), adjM); 
}

// Start function
AdjMatrix trainWithAllPlugins(str version)
{
	AdjMatrix adjM = initMatrix(version);
	
	//AdjMatrix adjM = readBinaryValueFile(#AdjMatrix, |file:///home/zac/corpus/AdjMatrix|);
   	
   	//int i = 0;
   	
    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
    	PluginSummary psum = loadPluginSummary(l.file);
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version)
    	{
    		//if( i > 3 ) break;
    		//i = i + 1;
    	
			HookUses hu = readBinaryValueFile(#HookUses, infoBin+"<l.file>-hook-uses.bin");
						
			HookModels models = { < hn, at, nameModel(hn,psum) > | < hn, at > <- psum.providedActionHooks + psum.providedFilterHooks };
		
			RegMap regexps = ( nm : regexpForNameModel(nm) | nm <- models<2> );
	
			println("Training with: <l.file>");
			
			adjM = insertClusterHooks(models, psum, regexps, adjM);
		}
	}

	writeBinaryValueFile(|file:///home/zac/corpus/AdjMatrix|, adjM);

	return adjM;
}



AdjMatrix insertClusterHooks(HookModels models, PluginSummary psum, RegMap regexps, AdjMatrix adjM)
{	
	list[NameOrExpr] hooks = [hn | <hn, _,_,_> <- (psum.filters + psum.actions)];
	
    if(q != 0) println("\tInserting clustered hooks");
    
	//for( h <- hooks ) println(h);

	NameOrExpr temp;

	while ( !isEmpty(hooks) )
	{	
		temp = hooks[0];
		
		hooks = drop(1, hooks);
					
		int tloc = getAdjListIndex(temp, psum, adjM);
		
		if(size(hooks) == 0 && tloc >=0) insertEdge(<tloc, tloc>, adjM); 
		
		for(h <- hooks)
		{	
			i = <getAdjListIndex(h, psum, adjM), tloc>;
			
			//println("<h>, <temp>  =  <i[0]>, <i[1]>");
			
			if(i[0] >= 0 && i[1] >= 0) //else it is a local hook
				adjM = insertEdge(i , adjM);
		}
	}
	
	if(q != 0)println("\tCluster Resolved");
	return adjM;
}

real conditionalProb(int of, int given, AdjMatrix adjM)
{
	real P = 0;
	
	if( of < given ) P = adjM.matrix[of][given];
	else P = adjM.matrix[given][of];
	
	return P / total(given, adjM);
}

real conditionalProb(int of, list[int] given, AdjMatrix adjM)
{
	real P = 0;
	
	for( n <- given )
	{
		if( of < n ) P = adjM.matrix[of][n];
		else P = adjM.matrix[n][of]; 
	}
	
	return P / total( given, adjM );
}

int total(list[int] i, AdjMatrix adjM)
{
	int t = 0;
	for( n <- i )
	{
		t += total(n, adjM);
	}
	
	return t;
}

int total(int i, AdjMatrix adjM)
{
	int t = 0;
	for( n <- [ i .. size(adjM[2]) - 1])
		t += adjM.matrix[n][i];
		
	for( n <- [ 0 .. i ] )
		t += adjM.matrix[i][n];
		
	return t;
}

AdjMatrix insertEdge(tuple[int, int] i, AdjMatrix adjM)
{
	if( i[0] < 0 || i[1] < 0 ) {
		println("Failed");
		return adjM;
	}
	elseif( i[0] < i[1] )
	{
		int t = i[0];
		i[0] = i[1];
		i[1] = t;
	}
	
	adjM.matrix[i[0]][i[1]] += 1;
		
	return adjM;
}

int getAdjListIndex(NameOrExpr hnUse, PluginSummary psum, AdjMatrix adjM)
{
	int i = 0;
	useString = stringForMatch(hnUse ,psum);
	
	for ( n <- adjM.regexps<1>) {
		try {
			if (rexpMatch(useString, n)) 
			{
				return i;
			}
		} catch v : {
			println("Bad regexp <regexps[nm]> caused by bug in source, skipping");
		}
		i = i + 1;
	}
	
	return -1;
}