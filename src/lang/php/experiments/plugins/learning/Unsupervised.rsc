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
list[str] wpV4() = ["4.0", "4.0.1", "4.0.2", "4.0.3", "4.0.4", "4.0.5", "4.0.6", "4.0.7", "4.0.8", "4.0.9", "4.0.10", "4.0.11", "4.0.12", "4.0.13", "4.0.14", "4.0.15", "4.0.16", "4.0.17", "4.1", "4.1.1", "4.1.2", "4.1.3", "4.1.4", "4.1.5", "4.1.6", "4.1.7", "4.1.8", "4.1.9", "4.1.10", "4.1.11", "4.1.12", "4.1.13", "4.1.14", "4.1.15", "4.1.16", "4.1.17", "4.2", "4.2.1", "4.2.2", "4.2.3", "4.2.4", "4.2.5", "4.2.6", "4.2.7", "4.2.8", "4.2.9", "4.2.10", "4.2.11", "4.2.12", "4.2.13", "4.2.14", "4.3", "4.3.1", "4.3.2", "4.3.3", "4.3.4", "4.3.5", "4.3.6", "4.3.7", "4.3.8", "4.3.9", "4.3.10", "4.4", "4.4.1", "4.4.2", "4.4.3", "4.4.4", "4.4.5", "4.4.6", "4.4.7", "4.4.8", "4.4.9", "4.5", "4.5.1", "4.5.2", "4.5.3", "4.5.4", "4.5.5", "4.5.6", "4.5.7", "4.5.8", "4.6", "4.6.1", "4.6.2", "4.6.3", "4.6.4", "4.6.5", "4.7", "4.7.1", "4.7.2", "4.7.3", "4.7.4", "4.7.5", "4.8"];

void trainAllVersions()
{
	for(v <- wpV4())
		trainWithAllPlugins(v);
}

/* Populate an empty Cluster */
MatrixReg initMatrix(str version) =  <[],  labelRegMap(loadWordpressPluginSummary(version))>;

/* Create a new MatrixReg populated with information generated 
   from all plugins compatible with the given WordPress version,
   relations determined by classes */
MatrixReg trainWithAllClasses(str version)
{
	MatrixReg M = initMatrix(version);
	map[str, int] downloadCounts = readDL(|file:///home/zac/git/wp-plugin-learning/extract/plugins/downloadCounts/downloadCount.txt|); 
	
    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
    /* For each plugin that is parsed and has resolved hooks */
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 		
 		/* if the plugin is tested upto the required version */
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{		
			println("Training with: <l.file>");
			
			for(<_, at,_> <- psum.classes )
			{
				list[NameOrExpr] fNames = featureList(psum, at);	
				
				int dC = 1;
				if( l.file in downloadCounts) dC = downloadCounts[l.file];

				M = insertSampleFeatures(fNames, psum, M, dC); 
			}
		}
	}
	
	Key key = reIndexKey( M.fCluster, ( i : e   | <i,_,e> <- M.fReg )); 
	M.fCluster = reIndexMatrix(M.fCluster); 	
	println("Version <version> has size <size(M.fCluster)>");

	if(size(M.fCluster) == 0)return <[], []>;
	
	loc txt = |file:///home/zac/versionCountClasses.txt|; 
	str res = "<version>\t<size(M.fCluster)>\n";
	if(exists(txt)) appendToFile(txt, res); 
	else writeFile(txt, res); 
	
	/* Save M to file */
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-fCluster -<version>.bin", M.fCluster);
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.bin", key);
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-MatrixReg-<version>.bin", M);
	return M ;
}

/* Create a new MatrixReg populated with information generated 
   from all plugins compatible with the given WordPress version,
   relations determined by classes */
MatrixReg trainWithAllPlugins(str version)
{
	MatrixReg M = initMatrix(version);
	map[str, int] downloadCounts = readDL(|file:///home/zac/git/wp-plugin-learning/extract/plugins/downloadCounts/downloadCount.txt|); 
	
    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
    /* For each plugin that is parsed and has resolved hooks */
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 		
 		/* if the plugin is tested upto the required version */
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{		
			println("Training with: <l.file>");
			
			list[NameOrExpr] fNames = featureList(psum);	
				
			int dC = 1;
			if( l.file in downloadCounts) dC = downloadCounts[l.file];

			M = insertSampleFeatures(fNames, psum, M, dC); 
		}
	}
	
	Key key = reIndexKey( M.fCluster, ( i : e   | <i,_,e> <- M.fReg )); 
	M.fCluster = reIndexMatrix(M.fCluster); 	
	println("Version <version> has size <size(M.fCluster)>");

	if(size(M.fCluster) == 0)return <[], []>;
	
	loc txt = |file:///home/zac/versionCountPlugins.txt|; 
	str res = "<version>\t<size(M.fCluster)>\n";
	if(exists(txt)) appendToFile(txt, res); 
	else writeFile(txt, res); 
	
	/* Save M to file */
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByPlugin-fCluster-<version>.bin", M.fCluster);
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByPlugin-Features-<version>.bin", key);
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByPlugin-MatrixReg-<version>.bin", M);
	return M ;
}

/* Insert all feature relationships shown in the provided PluginSummary */
MatrixReg insertSampleFeatures(list[NameOrExpr] fNames, PluginSummary psum, MatrixReg M, int weight) 
{	
	/* Create a list of each feature's index in M */
	list[int] fIndex = dup([ e | h <- fNames, e :=  getIndexList(h, psum, M.fReg), e >=0 ]);

	/* Do not add sample with < 2 hooks */
	if(size(fIndex) <= 1) return M;	

	M.fCluster += [<fIndex, weight>]; 
	
	return M;
}
