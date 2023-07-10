#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3				// Use modern global access method and strict wave access
#pragma DefaultTab={3,20,4}		// Set default tab width in Igor Pro 9 and later
#include <HeatMap_and_Dendrogram>
#include "PXPUtils"

// the goal is to compare 2D shapes (contours) of PCA results
// in R we do a PCA and then plot all points onto PC2 vs PC1
// the density of points can be visualised using a contour plot
// the bins describe the shape of points at that bin level
// We can export the contours as csv and read into IGOR

////////////////////////////////////////////////////////////////////////
// Menu items
////////////////////////////////////////////////////////////////////////

Menu "Macros"
	Submenu "Cluster Shapes"
		"Single Bin", /Q, ClusterShapes()
		"Outer Bin Only", /Q, ClusterShapesOuter()
		"Bin to Bin", /Q, ClusterShapesAll()
	End
End

////////////////////////////////////////////////////////////////////////
// Master functions and wrappers
////////////////////////////////////////////////////////////////////////

// ClusterShapes() was written to compare 2D shapes of a single bin size
// if used on multibin data the results are valid but compare across bins
// For bin-to-bin comparison use ClusterShapesAlt()

Function ClusterShapes()
	if(LoadCSVFromR() < 0)
		return -1
	endif
	ParseContours()
	MakeUniTracks(0) // smaller than smallest contour
	CompareTracks()
	ComputeAndMakeTheDendrogram()
End

Function ClusterShapesAll()
	if(LoadCSVFromR() < 0)
		return -1
	endif
	ParseContours()
	PushContoursToUniTracks()
	CompareTracksPerBin()
	ComputeAndMakeTheDendrogram()
End

Function ClusterShapesOuter()
	if(LoadCSVFromR() < 0)
		return -1
	endif
	ParseContours()
	String cList = Wavelist("cond_*", ";","")
	cList = RemoveFromList(WaveList("cond_*_0",";",""),cList)
	PXPUtils#KillTheseWaves(cList)
	PushContoursToUniTracks()
	CompareTracksPerBin()
	ComputeAndMakeTheDendrogram()
End

////////////////////////////////////////////////////////////////////////
// Main functions
////////////////////////////////////////////////////////////////////////

STATIC Function LoadCSVFromR()
	Variable refNum
	String fileName
	String filters = "csv File (*.csv):.csv;"
	filters += "All Files:.*;"
	Open/D/R/F=filters/M="Select csv file from R" refNum
	fileName = S_fileName			// S_fileName is set by Open/D
	if (strlen(fileName) == 0)		// User cancelled?
		return -1
	endif
	LoadWave/A/W/J/D/O/K=0/L={0,1,0,0,0}/Q fileName
	
	return 0
End

// This function will make individual contours from the long list of data
Function ParseContours()
	PXPUtils#KillAllExcept("condition;xW;yW;piece;") // changed from bins
	
	WAVE/Z/T condition
	WAVE/Z xW,yW,piece // changed from bins
	
	// find unique names of condition
	FindDuplicates/RT=uCondW condition
	Wave/Z/T uCondW
	Variable nCond = numpnts(uCondW)
	
	String wName
	
	Variable i,j
	
	if(!WaveExists(piece)) // changed from bins
		// slice up the tracks
		for(i = 0; i < nCond; i += 1)
			wName = "cond_" + num2str(i)
			Duplicate/O xW, xC
			Duplicate/O yW, yC
			xC[] = (cmpstr(uCondW[i],condition[p]) == 0) ? xW[p] : NaN
			yC[] = (cmpstr(uCondW[i],condition[p]) == 0) ? yW[p] : NaN
			WaveTransform zapnans, xC
			WaveTransform zapnans, yC
			Concatenate/O/NP=1/KILL {xC,yC}, $wName
		endfor
	else
		FindDuplicates/RN=uBinW piece // changed from bins
		Wave/Z uBinW
		Variable nBin = numpnts(uBinW)
		// slice up the tracks per bin
		for(i = 0; i < nCond; i += 1)
			for(j = 0; j < nBin; j += 1)
				wName = "cond_" + num2str(i) + "_" + num2str(j)
				Duplicate/O xW, xC
				Duplicate/O yW, yC
				xC[] = (cmpstr(uCondW[i],condition[p]) == 0 && piece[p] == uBinW[j]) ? xW[p] : NaN
				yC[] = (cmpstr(uCondW[i],condition[p]) == 0 && piece[p] == uBinW[j]) ? yW[p] : NaN
				WaveTransform zapnans, xC
				WaveTransform zapnans, yC
				Concatenate/O/NP=1/KILL {xC,yC}, $wName
			endfor
		endfor
	endif
	
	return 0
End

// this function will make uniformly sampled 2d coords
// use nPoints = 0 for auto mode
Function MakeUniTracks(nPoints)
	Variable nPoints
	String wList = WaveList("cond_*",";","")
	Variable nWaves = ItemsInList(wList)
	String xyName, distName
	
	Variable i
	
	// if nPoints is 0, it means auto = determine points
	if(nPoints == 0)
		Variable newPoints = 20000 // set to something big
		for(i = 0; i < nWaves; i += 1)
			xyName = StringFromList(i, wList)
			Wave xyW = $xyName
			newPoints = min(DimSize(xyW,0),newPoints)
		endfor
		nPoints = floor(0.9 * newPoints)
	endif
	
	for(i = 0; i < nWaves; i += 1)
		xyName = StringFromList(i, wList)
		Wave xyW = $xyName
		// temp xy as 1D
		Duplicate/O/RMD=[][0]/FREE xyW, txW
		Duplicate/O/RMD=[][1]/FREE xyW, tyW
		// calc point-to-point distance to get cumulative distance
		Duplicate/O/FREE xyW, tempDist
		Differentiate/METH=2/DIM=0 tempDist
		MatrixOp/O/FREE tempNorm = sqrt(sumRows(tempDist * tempDist))
		Integrate/METH=0 tempNorm
		if(DimSize(xyW,0) > 2)
			Interpolate2/T=1/N=(nPoints)/Y=xuW tempNorm, txW
			Interpolate2/T=1/N=(nPoints)/Y=yuW tempNorm, tyW
			Concatenate/O/NP=1 {xuW,yuW}, $("u_" + xyName)
		else
			Make/O/N=(nPoints,2) $("u_" + xyName)
			Wave fakeW = $("u_" + xyName)
			fakeW[][0] = txW[0]
			fakeW[][1] = tyW[0]
		endif
	endfor
	
	return 0
End

// this function will simply push each bin to MakeUniTracksPerBin()
Function PushContoursToUniTracks()
	WAVE/Z uBinW
	Variable nBin = numpnts(uBinW)
	Variable i
	for(i = 0; i < nBin; i += 1)
		MakeUniTracksPerBin(i)
	endfor
End

// make uniformly sampled 2d coords per bin
Function MakeUniTracksPerBin(bin)
	Variable bin
	String wList = WaveList("cond_*_" + num2str(bin),";","")
	Variable nWaves = ItemsInList(wList)
	
	// if there are no waves, break
	if(nWaves == 0)
		return -1
	endif
		
	String xyName, distName
	
	Variable i,j
	
	// determine number of points for resampling
	// we use a %age of the shortest track
	Variable newPoints = 20000 // set to something big
	for(i = 0; i < nWaves; i += 1)
		xyName = StringFromList(i, wList)
		Wave xyW = $xyName
		newPoints = min(DimSize(xyW,0),newPoints)
		if(newPoints < 20) // if there are too few points, abort
			newPoints = 0
			break
		endif
	endfor
	Variable nPoints = floor(0.9 * newPoints)
	
	if(nPoints == 0)
		return -1
	endif
	
	for(i = 0; i < nWaves; i += 1)
		xyName = StringFromList(i, wList)
		Wave xyW = $xyName
		// temp xy as 1D
		Duplicate/O/RMD=[][0]/FREE xyW, txW
		Duplicate/O/RMD=[][1]/FREE xyW, tyW
		// calc point-to-point distance to get cumulative distance
		Duplicate/O/FREE xyW, tempDist
		Differentiate/METH=2/DIM=0 tempDist
		MatrixOp/O/FREE tempNorm = sqrt(sumRows(tempDist * tempDist))
		Integrate/METH=0 tempNorm
		if(DimSize(xyW,0) > 2)
			Interpolate2/T=1/N=(nPoints)/Y=xuW tempNorm, txW
			Interpolate2/T=1/N=(nPoints)/Y=yuW tempNorm, tyW
			Concatenate/O/NP=1 {xuW,yuW}, $("u_" + xyName)
		else
			Make/O/N=(nPoints,2) $("u_" + xyName)
			Wave fakeW = $("u_" + xyName)
			fakeW[][0] = txW[0]
			fakeW[][1] = tyW[0]
		endif
	endfor
	
	return 0
End

// This is a wrapper to compare each uniTrack to every other track
// to generate a dissimilarity matrix
Function CompareTracks()
	String wList = WaveList("u_cond*",";","")
	Variable nWaves = ItemsInList(wList)
	Make/O/D/N=(nWaves,nWaves) dissMatrix = 0 // assign 0 rather than NaN here so hClust doesn't complain
	String wName0, wName1
	
	Variable i, j
	
	for(i = 0; i < nWaves; i += 1)
		wName0  = StringFromList(i, wList)
		Wave w0 = $wName0
		
		for(j = 0; j < nWaves; j += 1)
			if(i == j)
				continue
			endif
			wName1  = StringFromList(j, wList)
			Wave w1 = $wName1
			dissMatrix[i][j] = GetDissimilarity(w0,w1)
		endfor
	endfor
	
	return 0
End

// This is a wrapper to compare each uniTrack to every other track of that bin size
// to generate a dissimilarity matrix which is a sum of all dissimilarities
Function CompareTracksPerBin()
	WAVE/Z/T uCondW
	WAVE/Z uBinW
	Variable nCond = numpnts(uCondW)
	Variable nBin = numpnts(uBinW)
	// make the overall dissMatrix
	Make/O/D/N=(nCond,nCond) dissMatrix = 0 // assign 0 rather than NaN here so hClust doesn't complain
//	Make/O/D/N=(nCond,nCond,nBin) dissMatrix = 0 // assign 0 rather than NaN here so hClust doesn't complain	

	// setup
	String wList, wName0, wName1
	Variable nWaves	
	Variable i, j, k
	
	for(i = 0; i < nBin; i += 1)
		wList = WaveList("u_cond*" + "_" + num2str(i),";","")
		nWaves = ItemsInList(wList)
		if(nWaves < nCond)
			// if nWaves is 0, we didn't make any u_cond waves because
			// at least one condition had no data
			// if we are missing a wave for any condition we do not want the comparison
			continue
		endif
		Make/O/D/N=(nCond,nCond)/FREE tempDissMatrix = 0
		
		for(j = 0; j < nWaves; j += 1)
			wName0  = StringFromList(j, wList)
			Wave w0 = $wName0
			
			for(k = 0; k < nWaves; k += 1)
				if(j == k)
					continue
				endif
				wName1  = StringFromList(k, wList)
				Wave w1 = $wName1
				tempDissMatrix[j][k] = GetDissimilarity(w0,w1)
			endfor
		endfor
		dissMatrix[][] = dissMatrix[p][q] + tempDissMatrix[p][q]
//		dissMatrix[][][i] = tempDissMatrix[p][q]
	endfor
	
	return 0
End

STATIC Function GetDissimilarity(w0,w1)
	Wave w0,w1
	MatrixOp/O/FREE tempW = w0 - w1
	MatrixOp/O/FREE dissW = sqrt(sumRows(tempW * tempW))
//	return mean(dissW)
	return sum(dissW)
End

Function ComputeAndMakeTheDendrogram()
	// calculate dendrogram
	WAVE/Z dissMatrix
	HCluster/ITYP=DMatrix/OTYP=Dendrogram/DEST=treeMat/O dissMatrix
	LoadNiceCTableW()
	WAVE/Z treeMat
	WAVE/T/Z uCondW
	PlotDendrogram(treeMat, dissMatrix, uCondW, 0, 1)
	ModifyImage dissMatrix_ordered ctab= {*,*,root:Packages:ColorTables:Matplotlib:inferno,0}
	ModifyGraph width=0,height={Plan,1,left,bottom}
End

// to look at an outline, use this function
// to see u_cond_3_2, use LookAt("3_2")
// to see u_cond_1, use LookAt("1")
Function LookAt(suffix)
	String suffix
	WAVE/Z zW
	String trackName = "u_cond_" + suffix
	Wave/Z w = $trackName
	if(!WaveExists(w))
		DoAlert 0, "No shape called " + trackName + " found"
		return -1
	endif
	if(!WaveExists(zW))
		Make/O/N=(DimSize(w,0)) zW=p
	endif
	KillWindow/Z lookTrack
	Display/N=lookTrack w[][1] vs w[][0]
	ModifyGraph/W=lookTrack width={Aspect,1}
	ModifyGraph/W=lookTrack noLabel=2,axThick=0
	ModifyGraph/W=lookTrack margin=14
	ModifyGraph zColor($trackName)={zW,*,*,Rainbow,0}
End

// to look at all outlines, use this function
// to see u_cond_3_*, use LookAt("3")
Function LookAtAll(suffix)
	String suffix
	String wList = WaveList("cond_" + suffix + "*",";","")
//	String wList = WaveList("u_cond_*_" + suffix,";","")
	Variable nWaves = ItemsInList(wList)
	if(nWaves == 0)
		return -1
	endif
	String wName
	
	KillWindow/Z lookTrack
	Display/N=lookTrack
	Variable i
	
	for(i = 0; i < nWaves; i += 1)
		wName = StringFromList(i,wList)
		AppendToGraph/W=lookTrack $wName[][1] vs $wName[][0]
	endfor
	ModifyGraph/W=lookTrack width={Aspect,1}
	ModifyGraph/W=lookTrack noLabel=2,axThick=0
	ModifyGraph/W=lookTrack margin=14
End

////////////////////////////////////////////////////////////////////////
// Utility functions
////////////////////////////////////////////////////////////////////////

STATIC Function LoadNiceCTableW()
	NewDataFolder/O root:Packages
	NewDataFolder/O root:Packages:ColorTables
	String/G root:Packages:ColorTables:oldDF = GetDataFolder(1)
	NewDataFolder/O/S root:Packages:ColorTables:Matplotlib
	LoadWave/H/O/P=Igor/Q ":Color Tables:Matplotlib:inferno.ibw"
	KillStrings/Z/A
	SetDataFolder root:
	KillStrings/Z root:Packages:ColorTables:oldDF
	KillVariables/Z root:Packages:ColorTables:Matplotlib:V_flag
End