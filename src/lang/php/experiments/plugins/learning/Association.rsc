module lang::php::experiments::plugins::learning::Association

import lang::php::util::Utils;
import lang::php::util::Config;
import lang::php::ast::AbstractSyntax;
import lang::php::experiments::plugins::Summary;
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::Locations; 
import lang::php::experiments::plugins::learning::Utils;
import lang::php::experiments::plugins::learning::ClusterTransactions;

import IO;
import Set;
import Map;
import List;
import String;
import ValueIO;
import Relation;
import ListRelation;

import util::Math;

alias Associative = tuple[ Transactions T, list[num] tVector, num totalW, RegMap tReg ];
alias Transactions = lrel[num, list[num]];
/********************************************************************
							Testing functions
********************************************************************/

//Associative readA(str version) = readBinaryValueFile(baseLoc + "/training/Associative/TrainByClass-Association-<version>.bin");
Associative readA(str version) = readBinaryValueFile(#Associative, baseLoc + "/training/Associative/TrainByClass-Association-<version>.bin");
/********************************************************************
						Build Functions
********************************************************************/

/* Populate an empty Associative */
Associative initMatrix(str version)
{
	PluginSummary wpsum = loadWordpressPluginSummary(version);
	
	RegMap tReg = labelRegMap(wpsum);
		
	return <[], [ 0.0 | _ <- [0 .. size(tReg)] ], 0.0, tReg>;
}

/* Create a new Associative populated with information generated 
   from all plugins compatible with the given WordPress version,
   relations determined by classes */
Associative trainWithAllClasses(str version)
{
	Associative M = initMatrix(version);

    pluginDirs = sort([l | l <- pluginDir.ls, isDirectory(l) ]);

    for (l <- pluginDirs, exists(getPluginBinLoc(l.file)), exists(infoBin+"<l.file>-hook-uses.bin")) {
 		
    	PluginSummary psum = loadPluginSummary(l.file);
 
    	if (psum.pInfo is pluginInfo && just(maxVersion) := psum.pInfo.testedUpTo && maxVersion == version ) 
		{		
			println("Training with: <l.file>");
			for(<_, at,_> <- psum.classes )
			{
				list[NameOrExpr] tNames = featureList(psum, at);	
				M = insertSampleFeatures(tNames, psum, M);
			}
		}
	}
		
	/* Save M to file */
	writeTextValueFile(baseLoc + "/training/Associative/TrainByClass-Labels-<version>.txt",  [ <i , e>  | <i,_,e> <- M.tReg ]);
	writeTextValueFile(baseLoc + "/training/Associative/TrainByClass-Transactions-<version>.txt", M.T);
	writeBinaryValueFile(baseLoc + "/training/Associative/TrainByClass-Association-<version>.bin", M);

	return M;
}

/* Insert all feature relationships shown in the provided PluginSummary */
Associative insertSampleFeatures(list[NameOrExpr] tNames, PluginSummary psum, Associative M)
{
	list[num] tV = M.tVector;
	
	/* lrel [ specificity, index ] */
	lrel[int, int] tIndex = [ e | h <- tNames, e:= getIndexAndSpecificity(h, psum, M.tReg), e<0> >= 0]; 
	
	/* Do not add sample with < 2 hooks */
	if(size(tIndex) <= 1) return M;

	int k = 0;
	for( < i, s > <- tIndex )
	{
		if( tV[i] >= 0 ) 
		{
			k += 1;
			//tV[i] += s;
		}
	}

	//num w = sum(tV);
	//num d = w / k;
	
	/** Not using average specificivity as weight **/
	M.T += [ <w, 1> ];
	/** Moving from using totalW **/
	//M.totalW += ( d );
		
	return M;
}

//alias Cluster = tuple[ int w, list[int] rep, Matrix[int] contents ];
//data ClusterTree = leaf(Cluster C)| cNode(ClusterTree L, ClusterTree R) | cNode(ClusterTree L); 
//alias Transactions = lrel[num, list[num]];

Transactions clusterTransactions( Transactions T )
{
	println("ClusterTransactions");
	println("group");
	ClusterTree CT = group(<0, [], T<1>>);
	println("unTree");
	list[Cluster] CL = untree(CT);
	println("toTransaction");
	Transactions T = [<w, s> | <w, s, _> <- CL];
	return T;
}

/********************************************************************
						Prediction Functions
********************************************************************/

/* TODO: Weighted Associative Analysis; Hash Tree */
list[Transactions] Apriori(Transactions T, num minSupport)
{	
	println("Apriori");
	real totalW = sum(T<0>);
	num minW = totalW * minSupport;
	
	T = clusterTransactions(T);
	
	/* list [ lrel [ indexes : weight ] ]
	   Frequent item sets */
	list[Transactions] L = [];
	
	/* Candidates */
	Transactions C = [ <0, [s]> | s <- index(T[0][1]) ];
	
	int k = 1;

	while( size(C) != 0 )
	{
		println(k);
		L += [ calcValid( T, C, minW) ];
		println(L);
		k += 1;
		if(size(L) > 0) C = genAndPrune( L, k );
	}
	
	return L;
}

Transactions calcValid( Transactions M, Transactions C, num minW) = getValid( calcWeights( M, C), minW );


Transactions calcWeights( Transactions M, Transactions cVector){
	println("calcWeight");	
	return [<sum([w | < w, s > <- M, s >= d] + [0.0]), d> | d <-cVector<1>];
}


/********************************************************************
						Subset Functions
********************************************************************/


Transactions getValid( Transactions cVector, num minW )
{
	println("getValid");
	return [ <w, s> | <w, s> <- cVector, w >= minW ];
}

Transactions genAndPrune( Transactions C, int i ) = prune( genCombos( C, i ), i );

Transactions genCombos( Transactions C, int i)
{
	println("genCombos");
	return dup([<0, dup(e)> | t := C<1>, <s, s1> <- t *t, s[0..-1] == s1[0..-1], s != s1, e:= merge(s, s1)]);
}

/* Removes sets with at least one infrequent superset */
Transactions prune( Transactions C, int i )
{
	println("prune");
	
	Transactions L = []; 
	//if( ( size(C) < 2) || (size(C[i - 2]) == 0)) return [];
	
	Matrix[int] prevL =  [ s| <s,_>  <- C[i - 2] ];
	
	set[list[int]] setL = toSet(prevL);
	
	for( s <- prevL )
	{
		bool isIn = false;
		for( k <- s )
		{
			if( (s - k) <= prevL ) isIn = true;
			else 
			{
				isIn = false;
				break;
			}
		}
		if( isIn ) L += [ 0, s ];
	}
	
	return L;
}

