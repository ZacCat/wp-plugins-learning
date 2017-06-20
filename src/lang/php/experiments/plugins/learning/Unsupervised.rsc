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

/* Prediction threshold */
real pThres = .5;

alias MatrixReg = tuple[ Matrix[int] fMatrix, RegMap fReg];

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

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{		
			println("Training with: <l.file>");
			
			for(<_, at,_> <- psum.classes )
			{
				list[NameOrExpr] fNames = featureList(psum, at);	
				
				M = insertSampleFeatures(fNames, psum, M);
			}
		}
	}
	
	M = reduceDimensionality(M);
	
	/* Save M to file */
	writeTextValueFile(baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.txt", M.fMatrix);
	writeTextValueFile(baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.txt",  [ <i , e>  | <i,_,e> <- M.fReg ]);
	
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-fMatrix-<version>.bin", M.fMatrix);
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-Features-<version>.bin",  [ <i , e>  | <i,_,e> <- M.fReg ]);
	writeBinaryValueFile(baseLoc + "/training/Unsupervised/TrainByClass-MatrixReg-<version>.bin", M);
	return M ;
}

/* Insert all feature relationships shown in the provided PluginSummary */
MatrixReg insertSampleFeatures(list[NameOrExpr] fNames, PluginSummary psum, MatrixReg M)
{	
	/* Create a list of each feature's index in M */
	list[int] fIndex = dup([ e | h <- fNames, e :=  getIndexList(h, psum, M.fReg), e >=0 ]);

	/* Do not add sample with < 2 hooks */
	if(size(fIndex) <= 1) return M;	

	M.fMatrix += [fIndex];
	
	return M;
}

/********************************************************************
					Unimplemented Functions
********************************************************************/

/* TODO: Implement real dimensionality reduction
         algorithm.
   Currently: Doesn't remove much; used to re-index
              fReg.  */
MatrixReg reduceDimensionality( MatrixReg M )
{
	/* ( plugin index : < # of classes with plugin, # of total plugins in each of the classes > )*/ 
	map[ int, tuple[ int, int ] ] usage = ( i : < 0, 0> | < i, _,_ > <- M.fReg );
	
	for( r <- M.fMatrix, k <- r, e := usage[k], s := sum(r) ) usage[k] = < e<0> + 1, e<1> + s >;

	for ( i <- usage, t := usage[i],  t<0> == t<1> )
	{
		for( k <- index(M.fMatrix) ,i in M.fMatrix[k])
		{
			M.fMatrix[k] -= i;
			print("Removed");
		}

		lrel[NameModel, str] j = M.fReg[{i}];
		for( <n, s>  <- j) M.fReg = M.fReg - <i, n, s>;
		
	}
	
	RegMap newReg = [];
		
	int i = 0;
	for( < _, n, s> <- M.fReg)
	{
		newReg += <i, n, s>;
		i += 1;
	}
	
	M.fReg = newReg;
	return  M;
}
