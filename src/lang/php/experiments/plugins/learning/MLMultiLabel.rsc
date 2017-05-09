module lang::php::experiments::plugins::learning::MLMultiLabel

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
import Type;

import util::Math;

int q = 1;

alias MultiLabel = tuple[ list[list[int]] fMatrix, RegMap fReg ,  list[list[int]] lMatrix, RegMap lReg];

list[NameOrExpr] includedFeatures(PluginSummary psum, loc container = |file:///|)
{
	bool useLoc = container != |file:///|;	
	
	list[NameOrExpr] fL = [ f | < f, at, _> <- ( psum.postMetaKeys + psum.userMetaKeys ), useLoc ? true : insideLoc(at, container) ];
	
	return fL;
}

RegMap featureRegMap(PluginSummary wpsum)
{
	RegMap fL = ( nm : regexpForNameModel(nm) | <hn, _ > <- (wpsum.providedActionHooks + wpsum.providedFilterHooks), nm :=  nameModel(hn,wpsum));	
	
	fL += ( fN : regexpForNameModel(fN) | < f, _, _> <- ( wpsum.postMetaKeys + wpsum.userMetaKeys ), fN :=  nameModel(f,wpsum));
	
	return fL;
}

/* Populate an empty MultiLabel */
MultiLabel initMatrix(str version)
{
	PluginSummary wpsum = loadWordpressPluginSummary(version);
	
	// Generate a relationship of all RegMaps in wpsum, also serves as a label list
	RegMap lReg = ( nm : regexpForNameModel(nm) | <hn, _ > <- (wpsum.providedActionHooks + wpsum.providedFilterHooks), nm :=  nameModel(hn,wpsum));
	
	
	list[int] lVector  = [ 0 | n <- [ 0 .. ( size( lReg ) - 1 ) ] ]; 	
	
	// Generate list of feature's regex
	RegMap fReg = featureRegMap(wpsum);
	
	list[int] fVector = [ 0 | n <- [ 0 .. ( size( fReg ) - 1 ) ] ]; 
	
	return <[fVector], fReg, [lVector], lReg>;
}

/* Create a new MultiLabel populated with information generated 
from all plugins compatible with the given WordPress version */
MultiLabel trainWithAllPlugins(str version)
{
	MultiLabel M = initMatrix(version);

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
 
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version)
		{
			println("Training with: <l.file>");	
			
			// Extract all hooks (labels) from the class
			list[NameOrExpr] lNames = [hn | <hn, e,_,_> <- (psum.filters + psum.actions)];	
			// Extract all non-hook features from the class	
			list[NameOrExpr] fNames = includedFeatures(psum);	
			
			M = insertClusterHooks(lNames, fNames, psum, M);
		}
	}

	// Save M to file
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/Training-Plugin-<version>.txt|, M);

	return M;
}

/* Create a new MultiLabel populated with information generated 
from all plugins compatible with the given WordPress version,
relations deturmined by classes */
MultiLabel trainWithAllClasses(str version)
{
	MultiLabel M = initMatrix(version);

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
	
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{			
			println("Training with: <l.file>");
			
			for(<_, at,_> <- psum.classes )
			{
				// Extract all hooks (labels) from the class
				list[NameOrExpr] lNames = [hn | <hn, e,_,_> <- (psum.filters + psum.actions), insideLoc(e, at)];	
				// Extract all non-hook features from the class	
				list[NameOrExpr] fNames = includedFeatures(psum, container = at);
				
				M = insertClusterHooks(lNames, fNames, psum, M);
			}
		}
	}

	// Save M to file
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByClass-Features-<version>.txt|, M.fMatrix);
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByClass-Labels-<version>.txt|, [ e | n <- M.lReg, e := M.lReg[n] ] );

	return M;
}

/* Insert all hook relationships shown in the provided PluginSummary */
MultiLabel insertClusterHooks(list[NameOrExpr] lNames, list[NameOrExpr] fNames, PluginSummary psum, MultiLabel M)
{	
	list[int] fV = M.fMatrix[0];
	list[int] lV = M.lMatrix[0];
	
	// Create a list of each label's index in M
	list[int] lIndex = getIndexList(lNames, psum, M.lReg);
	// Create a list of each feature's index in M
	list[int] fIndex = getIndexList(fNames, psum, M.fReg);
	
    if(q != 0) println("\tBuilding Feature and List Vectors: size <size(fIndex + lIndex)>");
    	
	// Generate label values
	for( l <- ( lIndex ) )	
	{
		fV[l] += 1;	
		lV[l] += 1;
	}
	
	// Generate feature values
	for( f <- ( fIndex ) )				
		fV[f] += 1;


	if(q != 0)println("\tVectors Resolved");	

	M.lMatrix += [lV];
	M.fMatrix += [fV];
	
	return M;
}

