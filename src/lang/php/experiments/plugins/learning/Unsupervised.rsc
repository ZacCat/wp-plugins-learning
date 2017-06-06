module lang::php::experiments::plugins::learning::Unsupervised

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
real pThres = .5;

alias Cluster = tuple[ Matrix[int] fMatrix, list[int] fVector, RegMap fReg];

void run(str version)
{
	//Cluster C = readBinaryValueFile(baseLoc + "/training/Cluster/TrainByClass-Cluster-<version>.bin");
	Matrix[real] K = getPy();
	
	int sz = size(K[0]);
	println("FindCluster; Size K: <size(K)>");
	findNearestCluster(testBinarize([8], sz), K);
}

// Read a list of clusters generated in python
Matrix[real] getPy()
{
	return readPyMatrix(|file:///home/zac/git/wp-plugin-learning/python/array.txt|);
}

/* Populate an empty Cluster */
Cluster initMatrix(str version)
{
	PluginSummary wpsum = loadWordpressPluginSummary(version);
	
	// Generate a relationship of all RegMaps in wpsum, also serves as a label list
	RegMap fReg = labelRegMap(wpsum);

	return <[], [ 0 | n <- [ 0 .. size( fReg ) ] ],  fReg>;
}

/* Create a new Cluster populated with information generated 
from all plugins compatible with the given WordPress version,
relations determined by classes */
Cluster trainWithAllClasses(str version)
{
	Cluster M = initMatrix(version);

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);
    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{		
			println("Training with: <l.file>");
			
			for(<_, at,_> <- psum.classes )
			{
				// Extract all non-hook features from the class	
				list[NameOrExpr] fNames = [hn | <hn, e,_,_> <- (psum.filters + psum.actions), insideLoc(e, at), pp(hn) != ".*"];	
				
				M = insertSampleFeatures(fNames, psum, M);
			}
		}
	}
			
	// Save M to file
	writeTextValueFile(baseLoc + "/training/Cluster/TrainByClass-fMatrix-<version>.txt", M.fMatrix);
	writeTextValueFile(baseLoc + "/training/Cluster/TrainByClass-Features-<version>.txt",  ( i : e  | <i,_,e> <- M.fReg ));
	writeBinaryValueFile(baseLoc + "/training/Cluster/TrainByClass-Cluster-<version>.bin", M);
	return M ;
}

/* Insert all feature relationships shown in the provided PluginSummary */
Cluster insertSampleFeatures(list[NameOrExpr] fNames, PluginSummary psum, Cluster M)
{	
	// Create a list of each feature's index in M
	list[int] fIndex = dup([ e | h <- fNames, e :=  getIndexList(h, psum, M.fReg), e >=0 ]);

	if(size(fIndex) <= 1) return M;	

	if(q != 0)println("\tVectors Resolved");	

	M.fMatrix += [fIndex];
	
	return M;
}

// Find the nearest cluster from a predefined list of clusters M 
// Using Manhattan Distance
list[tuple[num, int]] findNearestCluster( list[int] V, Matrix[real] M )
{
	list[tuple[num, int]] D = [];
	int i = 0;
	for( n <- M, e:= dist(V, n))
	{
		if(e >= pThres) D += [ <e, i> ];
		i += 1;
	}
	
	return reverse(sort(D));
}

// Predict using the nearest cluser
list[int] predictKNN( int i, Matrix[real] M )
{
	list[int] ret = [];
	int k = 0;
	for( n <- M[i] )
	{
		if( n > pThes ) ret += k;
		k += 1;
	}
	return ret;
}

// Unused
Cluster reduceDimensionality( Cluster M )
{
	// ( plugin index : < # of classes with plugin, # of total plugins in each of the classes > ) 
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

	return  M;
}
