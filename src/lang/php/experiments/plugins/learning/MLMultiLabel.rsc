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

int q = 0;

alias MultiLabel = tuple[ list[list[int]] fMatrix, RegMap fReg ,  list[list[str]] lMatrix, RegMap lReg];

/* Returns the features chosen to represent a plugin
Currently unused */
list[NameOrExpr] includedFeatures(PluginSummary psum, loc container = |file:///|)
{
	bool useLoc = container != |file:///|;
		
	// Removes ".*" regex expressions
	list[NameOrExpr] fL = [ f | < f, at, _> <- ( psum.postMetaKeys + psum.userMetaKeys ), useLoc ? true : insideLoc(at, container), pp(f) != ".*" ];

	return fL;
}

/* Returns the chosen features defined in WP (used for indexing)
 Currently unused */
RegMap featureRegMap(PluginSummary wpsum)
{
	// Removes ".*" regex expressions	
	return ( fN : e | < f, _, _> <- ( wpsum.postMetaKeys + wpsum.userMetaKeys ), fN :=  nameModel(f,wpsum), e := regexpForNameModel(fN), e != ".*");
}

/* Returns the labels defined in WP (hooks) */
RegMap labelRegMap(PluginSummary wpsum)
{
	// Removes ".*" regex expressions
	return ( nm : e | <hn, _ > <- (wpsum.providedActionHooks + wpsum.providedFilterHooks), nm :=  nameModel(hn,wpsum), e := regexpForNameModel(nm), e != ".*");
}

/* Populate an empty MultiLabel */
MultiLabel initMatrix(str version)
{
	PluginSummary wpsum = loadWordpressPluginSummary(version);
	
	// Generate a relationship of all RegMaps in wpsum, also serves as a label list
	RegMap lReg = labelRegMap(wpsum);
		
	// Generate list of feature's regex
	RegMap fReg = lReg; //+ featureRegMap(wpsum);
	
	list[int] fVector = [ 0 | n <- [ 0 .. ( size( fReg ) - 1 ) ] ]; 
	
	// Has a 0 vector of size |fReg| in fMatrix[0]
	return <[fVector], fReg, [], lReg>;
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
			// list[NameOrExpr] fNames = includedFeatures(psum);	
			
			/** Training with hooks as features and labels for testing **/
			M = insertClusteredFeatures(lNames, lNames, psum, M);
		}
	}

	// Save M to file
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByPlugin-fMatrix-<version>.txt|, drop(1, M.fMatrix)); // Dont print fMatrix[0]
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByPlugin-Features-<version>.txt|, [ e | n <- M.fReg, e := M.fReg[n] ] );
	
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByPlugin-lMatrix-<version>.txt|, M.lMatrix);
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByPlugin-Labels-<version>.txt|, [ e | n <- M.lReg, e := M.lReg[n] ] );
	return M;
}

/* Create a new MultiLabel populated with information generated 
from all plugins compatible with the given WordPress version,
relations determined by classes */
MultiLabel trainWithAllClasses(str version)
{
	MultiLabel M = initMatrix(version);

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
	int c = 0;
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{		
			c += 1;
			//if( c == 5 ) break;	
			println("Training with: <l.file>");
			
			for(<_, at,_> <- psum.classes )
			{
				// Extract all hooks (labels) from the class
				list[NameOrExpr] lNames = [hn | <hn, e,_,_> <- (psum.filters + psum.actions), insideLoc(e, at), pp(hn) != ".*"];	
				// Extract all non-hook features from the class	
				//list[NameOrExpr] fNames = includedFeatures(psum, container = at);
				
				/** Training with hooks as features and labels for testing **/
				M = insertClusteredFeatures(lNames, lNames, psum, M);
			}
		}
	}
	
	// Save M to file
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByClass-fMatrix-<version>.txt|, drop(1, M.fMatrix)); // Dont print fMatrix[0]
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByClass-Features-<version>.txt|, [ e | n <- M.fReg, e := M.fReg[n] ] );
	
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByClass-lMatrix-<version>.txt|, M.lMatrix);
	writeTextValueFile(|file:///home/zac/corpus/training/MultiLabel/TrainByClass-Labels-<version>.txt|, [ e | n <- M.lReg, e := M.lReg[n] ] );

	return M;
}

/* Insert all feature relationships shown in the provided PluginSummary */
MultiLabel insertClusteredFeatures(list[NameOrExpr] lNames, list[NameOrExpr] fNames, PluginSummary psum, MultiLabel M)
{	
	list[int] fV = M.fMatrix[0];
	
	// Create a list of each label's index in M
	list[int] lIndex = [ e | h <- lNames, e:= getIndexList(h, psum, M.lReg), e >= 0]; 
	// Create a list of each label in M
	list[str] lStr = [ getIndexListString(n , M.lReg) | n <- lIndex];
	
	// Do not add cluster with no hooks
	if(size(lIndex) == 0) return M;
		
	// Create a list of each feature's index in M
	// list[int] fIndex = lIndex + [ e | h <- fNames, e :=  getIndexList(h, psum, M.fReg), e >=0 ];

	/** Since only hooks are being considered **/
	list[int] fIndex = lIndex;

    if(q != 0) println("\tBuilding Feature and List Vectors: size <size(fIndex + lIndex)>");
	
	// Generate feature values
	for( f <- ( fIndex ), f >= 0 )
		fV[f] += 1;

	if(q != 0)println("\tVectors Resolved");	

	M.lMatrix += [lStr];
	M.fMatrix += [fV];
	
	return M;
}