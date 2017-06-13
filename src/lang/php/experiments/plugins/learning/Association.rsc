module lang::php::experiments::plugins::learning::Association

import lang::php::util::Utils;
import lang::php::util::Config;
import lang::php::ast::AbstractSyntax;
import lang::php::experiments::plugins::Summary;
import lang::php::experiments::plugins::Plugins;
import lang::php::experiments::plugins::learning::Utils;

import IO;
import Set;
import Map;
import List;
import String;
import ValueIO;
import Relation;
import ListRelation;

alias Associative = tuple[ lrel[num, list[num]] transactions, list[num] tVector, num totalW, RegMap tReg ];

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
	writeTextValueFile(baseLoc + "/training/Associative/TrainByClass-Association-<version>.bin", M);

	return M;
}

/* Insert all feature relationships shown in the provided PluginSummary */
Associative insertSampleFeatures(list[NameOrExpr] tNames, PluginSummary psum, Associative M)
{	
	list[num] tV = M.tVector;
	
	/* lrel [ specificity, index ] */
	lrel[int, int] tIndex = { e | h <- tNames, e:= getIndexAndSpecificity(h, psum, M.tReg), e<0> >= 0}; 
	
	/* Do not add sample with < 2 hooks */
	if(size(tIndex) <= 1) return M;

	int k = 0;
	for( < i, s > <- tIndex )
	{
		if( tV[i] >= 0 ) 
		{
			k += 1;
			tV[i] += s;
		}
	}

	num w = sum(tV);
	num d = w / k;
	
	M.transactions += [ <w, tV> ];
	M.totalW += ( d );
		
	return M;
}

/********************************************************************
						Prediction Functions
********************************************************************/

/* TODO: Weighted Associative Analysis; Hash Tree */
list[lrel[list[int], num]] Apriori(Associative M, num minSupport)
{	
	num minW = M.totalW * minSupport;
	
	/* list [ lrel [ indexes : weight ] ]
	   Frequent item sets */
	list[lrel[list[int], num]] L = [];
	
	/* Candidates */
	lrel[list[int], num] C = [ <[s] , 0> | s <- [0 .. size(M.tReg) - 1] ];
		
	int k = 1;

	while( size(C) != 0 )
	{
		println(k);
		L += [ calcValid( M, C, minW) ];
		println(L);
		k += 1;
		C = genAndPrune( L, k );
	}
	
	return L;
}

lrel[list[int], num] calcValid( Associative M, lrel[list[int], num] cVector, num minW) = getValid( calcWeights( M, cVector), minW );

lrel[list[int], num] calcWeights( Associative M, lrel[list[int], num] cVector)
{
	println("calcWeights");
	int k = 0;
	/* For each weight w and set s */
	for( < w, s > <- M.transactions )
	{	
		if( k % 1000 == 0 ) println("Trans: <k>");
		int i = 0;
		/* for each domain d */	
		for( d <- cVector<0> )
		{
			/* False if the set is empty */
			bool isIn = size(s) > 0;
			/* for each int n in the domain set */
			for( n <- d, isIn)
			{
				if( s[n] == 0 ) isIn = false;
			}
			if( isIn ) cVector[i] = <d, w>;
			i += 1;
		}
		k += 1;
	}
	
	println(cVector);
	return cVector;
}

/********************************************************************
						Subset Functions
********************************************************************/


lrel[list[int], num] getValid( lrel[list[int], num] cVector, num minW ) = [ <s, w> | <s, w> <- cVector, w >= minW ];

lrel[list[int], num] genAndPrune( list[lrel[list[int], num]] C, int i ) = prune( genCombos( C, i ), i );

/* Use List multiplication */
list[lrel[list[int], num]] genCombos( list[lrel[list[int], num]] C, int i )
{
	println("genCombos");

	Matrix[int] prevL = [ s | <s, _> <- C[i - 2] ];

	int sizeL = size( prevL );
	if(sizeL == 0 ) return [];
	
	Matrix[int] t = [];
	
	int sz = size( prevL[0] ) - 1;
		
	if(sz == 0)
	{
		for( s <- index(prevL) )
		{
			for( n <- [ s + 1 .. sizeL - 1 ], n < sizeL)	
			{
				// n gives index out of bounds error
				list[int] f = prevL[s] + prevL[n];
				t += [f];
			}
		}
	}
	else
	{
		Matrix[int] h = [ e | s <- prevL, e := delete( s, sz ) ];
		
		/***** Check logic ****/
		for( s <- [0 .. sizeL - 2])
		{
			for( n <- [ s + 1 .. sizeL - 1 ] )
			{
				if (h[i] == h[n]) t += [sort(prevL[i] + tail(prevL[n], 1))];
				else break;
				//t += ( e : 0 | n <- [ s .. sizeL - 1 ], e := prevL[s] + prevL[n], size(e) == i );
			}
		}
	}
	
	t = dup(t);
	println(t);
	return C + [[ <n, 0> | n <- t]];
}

/* Removes sets with at least one infrequent superset */
lrel[list[int], num] prune( list[lrel[list[int], num]] C, int i )
{
	println("prune");
	
	lrel[set[int], num] L = []; 
	if( ( size(C) < 2) || (size(C[i - 2]) == 0)) return [];
	
	Matrix[int] prevL =  [ s| <s,_>  <- C[i - 2] ];
	
	set[list[int]] setL = toSet(prevL);
	
	for( s <- prevL )
	{
		bool isIn = false;
		for( k <- s )
		{
			if( indexOf(prevL, s - k) >= 0 ) isIn = true;
			else 
			{
				isIn = false;
				break;
			}
		}
		if( isIn ) L += [ s , 0 ];
	}
	
	return L;
}

