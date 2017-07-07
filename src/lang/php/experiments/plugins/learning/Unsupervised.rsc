module lang::php::experiments::plugins::learning::Unsupervised

import lang::php::util::Utils;
import lang::php::util::Config;
import lang::php::ast::AbstractSyntax;
import lang::php::experiments::plugins::Summary;
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Locations; 
import lang::php::experiments::plugins::learning::Utils;

import IO;
import Map;
import List;
import ValueIO;
import ListRelation;

alias MatrixReg = tuple[ Cluster[int] fCluster, RegMap fReg]; 

/********************************************************************
							Build Functions 
********************************************************************/

/* Populate an empty Cluster */
MatrixReg initMatrix(str version) =  <[],  labelRegMap( loadWordpressPluginSummary(version))>;

/* Create a new MatrixReg populated with information generated 
   from all plugins compatible with the given WordPress version,
   relations determined by classes */
MatrixReg trainWithAllClasses(str version)
{
	MatrixReg M = initMatrix(version);
	map[str, int] DL = readDL(); 

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{		
			println("Training with: <l.file>");
			
			for(<_, at,_> <- psum.classes )
			{
				list[NameOrExpr] fNames = featureList(psum, at);	
				
				M = insertSampleFeatures(fNames, psum, M, DL[l.file]); 
			}
		}
	}
	
	Key key = reIndexKey( M.fCluster, ( i : e   | <i,_,e> <- M.fReg )); 
	M.fCluster = reIndexMatrix(M.fCluster); 	
	/* Save M to file */
	writeTextValueFile(baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.txt", M.fCluster);
	writeTextValueFile(baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.txt", key);
	
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.bin", M.fCluster);
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.bin", key);
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-MatrixReg-<version>.bin", M);
	return M ;
}

/* Insert all feature relationships shown in the provided PluginSummary */
MatrixReg insertSampleFeatures(list[NameOrExpr] fNames, PluginSummary psum, MatrixReg M, int w) 
{	
	/* Create a list of each feature's index in M */
	list[int] fIndex = dup([ e | h <- fNames, e :=  getIndexList(h, psum, M.fReg), e >=0 ]);

	/* Do not add sample with < 2 hooks */
	if(size(fIndex) <= 1) return M;	

	M.fCluster += [<fIndex, w>]; 
	
	return M;
}
