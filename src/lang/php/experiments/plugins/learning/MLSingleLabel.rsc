module lang::php::experiments::plugins::learning::MLSingleLabel

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

alias SingleLabel = tuple[list[list[int]] matrix, RegMap regexps, list[int] labels];

/* Populate an empty SingleLabel */
SingleLabel initMatrix(str version)
{
	PluginSummary wpsum = loadWordpressPluginSummary(version);
	
	// Generate a relationship of all RegMaps in wpsum
	RegMap regexps = (  nm : regexpForNameModel(nm) | <hn, _ > <- (wpsum.providedActionHooks + wpsum.providedFilterHooks), nm :=  nameModel(hn,wpsum));
		
	list[int] t = [];
	list[list[int]] k = [];
	
	for(n <- [ 0 .. size(regexps) - 1 ] )
		t += [0];
	
	for(n <- [ 0 .. size(regexps) - 1 ] )
		k = k + [t];
	
	return <k, regexps, t>;
}

/* Read a SingleLabel from a file */
SingleLabel getFromFile(str v)
{
	return readBinaryValueFile(#SingleLabel, |file:///home/zac/corpus/SingleLabel<v>|);
}

/* Create a new SingleLabel populated with information generated 
from all plugins compatible with the given WordPress version */
SingleLabel trainWithAllPlugins(str version)
{
	SingleLabel M = initMatrix(version);

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
	writeTextValueFile(|file:///home/zac/corpus/training/Training-Plugin-<version>.txt|, M);

	return M;
}

/* Create a new SingleLabel populated with information generated 
from all plugins compatible with the given WordPress version,
relations deturmined by classes */
SingleLabel trainWithAllClasses(str version)
{
	SingleLabel M = initMatrix(version);

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);

	int count = 0;
	
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{
			count = count + 1;
			if(count > 10) break;
			
			println("Training with: <l.file>");
			
			for(<_, at,_> <- psum.classes )
			{
				// Extract all hooks from the class
				list[NameOrExpr] hNames = [hn | <hn, e,_,_> <- (psum.filters + psum.actions), insideLoc(e, at)];		
			
				M = insertClusterHooks(hNames, psum, M);
			}
		}
		//if( c> 10) break;
	}

	// Save M to file
	writeTextValueFile(|file:///home/zac/corpus/training/TrainByClass-Features-<version>.txt|, M.matrix);
	writeTextValueFile(|file:///home/zac/corpus/training/TrainByClass-Labels-<version>.txt|, [ e | n <- M.regexps, e := M.regexps[n] ] );

	return M;
}

/* Insert all hook relationships shown in the provided PluginSummary */
SingleLabel insertClusterHooks(list[NameOrExpr] hNames, PluginSummary psum, SingleLabel M)
{	
	// Create a list of each hooks index in M
	list[int] hooks = hookIndexes(hNames, psum, M.regexps);
	
    if(q != 0) println("\tBuilding Plugin Vector: size <size(hooks)>");
    	
	// Add all combonations of 'hooks' to M
	for( label <- hooks )
	{	
		for( feature <- ( hooks - [label] ) )
		{					
			if(feature >= 0 && label >= 0)
				M = insertEdge(<label, feature> , M);
		}
	}
	
	if(q != 0)println("\tVector Resolved");	
	
	return M;
}

/* Insert an feature, i[1], with lable 'i[0]' */
SingleLabel insertEdge(tuple[int, int] i, SingleLabel M)
{
	if( i[0] < 0 || i[1] < 0 ) 
		return M; //Possible need to add new row
	else
		M.matrix[i[0]][i[1]] += 1;
	
	return M;
}
