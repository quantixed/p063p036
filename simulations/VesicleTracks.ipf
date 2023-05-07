#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3				// Use modern global access method and strict wave access
#pragma DefaultTab={3,20,4}		// Set default tab width in Igor Pro 9 and later
#include "PXPUtils"
#include <Waves Average>
#include <3DWaveDisplay>
#include <ImageSlider>
#include <Graph Utility Procs>

////////////////////////////////////////////////////////////////////////
// Menu items
////////////////////////////////////////////////////////////////////////
Menu "Macros"
	"Vesicle Tracks...", /Q, VTWrapperPtOne()
	"Vesicle Tracks Only", /Q, SimpleImport(0)
	"Vesicle Tracks Simple", /Q, SimpleImport(1)
End

////////////////////////////////////////////////////////////////////////
// Master functions and wrappers
////////////////////////////////////////////////////////////////////////
Function VTWrapperPtOne()
	SetDataFolder root:
	// kill all windows and waves before we start
	PXPUtils#CleanSlate()
	
	VesicleTracks_Panel()
End

Function VTWrapperPtTwo()
	TrackEngine(0)
	BuildMovieDisplay()
End

Function SimpleImport(opt)
	Variable opt
	SetDataFolder root:
	// kill all windows and waves before we start
	PXPUtils#CleanSlate()
	
	Variable tStep = 0.06 // time in seconds between frames
	Variable pxSize = 0.04 // pixel size in microns
	Variable thickness = 0.2 // confocal section thickness in microns
	Variable width = 375 // width of movie in pixels
	Variable height = 375 // height of movie in pixels
		
	Prompt tStep, "Time interval (s)"
	Prompt pxSize, "Pixel size (\u03BCm)"
	Prompt thickness, "Slice depth (\u03BCm)"
	Prompt width, "Width of image (px)"
	Prompt height, "Height of image (px)"
	DoPrompt "Specify", tStep, pxSize, thickness, width, height
	
	if (V_flag) 
		return -1
	endif
	Make/O/N=5 paramWave={tStep, pxSize, thickness, width, height}
	// find file
	Variable refNum
	String filters = "CSV File (*.csv):.csv;"
	filters += "All Files:.*;"
	Open/D/R/F=filters/M="Select csv file" refnum
	String fileName = S_fileName			// S_fileName is set by Open/D
	if (strlen(fileName) == 0)		// User cancelled?
		return -1
	endif
	LoadWave/A/W/J/K=1/L={0,1,0,0,0}/O/Q fileName
	// this will give 4 waves: TrackID, xW, yW, frame
	// if 3D it will give 5 waves: TrackID, xW, yW, zW, frame
	TrackEngine(opt)
End

////////////////////////////////////////////////////////////////////////
// Main functions
////////////////////////////////////////////////////////////////////////

Function TrackEngine(simple)
	Variable simple
	WAVE/Z paramWave = root:paramWave
	if(!WaveExists(paramWave))
		DoAlert 0, "Setup has failed. Missing paramWave."
		return -1
	endif

	WAVE/Z frame = root:frame, TrackID = root:TrackID, xW = root:xW, yW = root:yW, zW = root:zW
	if(!WaveExists(frame) || !WaveExists(TrackID) || !WaveExists(xW) || !WaveExists(yW))
		DoAlert 0, "Error loading data, check file"
		return -1
	endif
	
	Variable dimensions
	if(!WaveExists(zW))
		dimensions = 2
	else
		dimensions = 3
	endif
	
	// pick up global values needed
	Variable tStep = paramWave[0]
	Variable pxSize = paramWave[1]
	
	// we cannot take the wavemax of trackID and use that to loop with, because some IDs may have been removed
	FindDuplicates/RN=uTrackID TrackID
	Wave uTrackID = root:uTrackID
	Variable trackMax = numpnts(uTrackID)
	
	String dataFolderName = "root:data"
	NewDataFolder/O $dataFolderName // make root:data: but don't put anything in it yet
	// make subfolder for tracks
	dataFolderName = "root:data:tracks"
	NewDataFolder/O/S $dataFolderName
	
	Variable iTrack
	String tkName
	
	String plotName = "p_allTracks"
	KillWindow/Z $plotName
	Display/N=$plotName/HIDE=1
	if(dimensions == 3)
		KillWindow/Z p_allTracks_XZ
		Display/N=p_allTracks_XZ/HIDE=1
		KillWindow/Z p_allTracks_YZ
		Display/N=p_allTracks_YZ/HIDE=1
	endif
	
	Variable i
//	Variable timer
//	timer = startmstimer
	for(i = 0; i < trackMax; i += 1)
		iTrack = uTrackID[i]
		tkName = "tk_" + num2str(iTrack)
		// this strategy was faster than Extract and way faster than deleting the matched rows
		Duplicate/O xW, tkX
		Duplicate/O yW, tkY
		Duplicate/O frame, tkT
		tkX[] = (trackID[p] == iTrack) ? tkX[p] : NaN
		tkY[] = (trackID[p] == iTrack) ? tkY[p] : NaN
		tkT[] = (trackID[p] == iTrack) ? tkT[p] : NaN
		WaveTransform zapnans tkX
		WaveTransform zapnans tkY
		WaveTransform zapnans tkT
		if(dimensions == 3)
			Duplicate/O zW, tkZ
			tkZ[] = (trackID[p] == iTrack) ? tkZ[p] : NaN
			WaveTransform zapnans tkZ
			Concatenate/O/KILL {tkX,tkY,tkZ,tkT}, $tkName
		else
			Concatenate/O/KILL {tkX,tkY,tkT}, $tkName
		endif
		Wave w = $tkName
		// we no longer plot x on y-axis and y and x-axis to match msp-tracker
		AppendToGraph/W=$plotName w[][1] vs w[][0]
		if(dimensions == 3)
			AppendToGraph/W=p_allTracks_XZ w[][2] vs w[][0]
			AppendToGraph/W=p_allTracks_YZ w[][2] vs w[][1]
		endif
	endfor
	
	// format plots
	Variable axMax, axMin
	if(dimensions == 3)
		axMax = max(WaveMax(xW),WaveMax(yW),WaveMax(zW))
		axMin = min(WaveMin(xW),WaveMin(yW),WaveMax(zW))
	else
		axMax = max(WaveMax(xW),WaveMax(yW))
		axMin = min(WaveMin(xW),WaveMin(yW))
	endif
	axMax = ceil(axMax / 10) * 10
	axMin = floor(axMin / 10) * 10
	SetAxis/W=$plotName bottom axMin, axMax
	SetAxis/W=$plotName left axMax, axMin
	ModifyGraph/W=$plotName width={Aspect,1}
	ModifyGraph/W=$plotName mirror=1,tick=3,noLabel=2,standoff=0,margin=3
	if(dimensions == 3)
		SetAxis/W=p_allTracks_XZ bottom axMin, axMax
		SetAxis/W=p_allTracks_XZ left axMax, axMin
		ModifyGraph/W=p_allTracks_XZ width={Aspect,1}
		ModifyGraph/W=p_allTracks_XZ mirror=1,tick=3,noLabel=2,standoff=0,margin=3
		SetAxis/W=p_allTracks_YZ bottom axMin, axMax
		SetAxis/W=p_allTracks_YZ left axMax, axMin
		ModifyGraph/W=p_allTracks_YZ width={Aspect,1}
		ModifyGraph/W=p_allTracks_YZ mirror=1,tick=3,noLabel=2,standoff=0,margin=3
	endif
	
//	Print stopmstimer(timer)/1e6

	// run other procedures
	CalcDistancesAndSpeeds()
	CollectStats()
	if(simple == 0)
		HowManyVesicles()
		CalcRatios()
	endif
	CalcMSD()
	CalcSqd()
	if(simple == 0)
		CompareAlgorithms()
	endif
	
	SetDataFolder root:
	PXPUtils#MakeTheLayouts("p",5,3, rev = 1, saveIt = 0)
End

STATIC Function CalcDistancesAndSpeeds()
	String wList = WaveList("tk_*",";","")
	Variable nWaves = ItemsInList(wList)
	
	Variable nRows
	Variable i
	
	WAVE/Z paramWave = root:paramWave
	Variable tStep = paramWave[0]
	Variable pxSize = paramWave[1]
	String wName, newName
	
	for(i = 0; i < nWaves; i += 1)
		// tk_ wave columns are x, y, frame or x, y, z, frame
		wName = StringFromList(i, wList)
		Wave w0 = $wName
		newName = ReplaceString("tk_",wName,"sp_")
		// we need speed to be dist, time, speed
		// calc distance
		if(DimSize(w0,1) == 4)
			Duplicate/O/RMD=[][0,2]/FREE w0, tempDist
			Duplicate/O/RMD=[][3]/FREE w0, tempTime
		else
			Duplicate/O/RMD=[][0,1]/FREE w0, tempDist
			Duplicate/O/RMD=[][2]/FREE w0, tempTime
		endif
		Differentiate/METH=2/DIM=0 tempDist
		tempDist[0][] = 0
		MatrixOp/O/FREE tempNorm = sqrt(sumRows(tempDist * tempDist))
		tempNorm[] *= pxSize // convert to real distance
		// calc time interval, cannot assume that frames are consecutive due to gap-linking
		Differentiate/METH=2 tempTime
		tempTime[0] = 0
		tempTime[] *= tStep // convert to real distance
		MatrixOp/O/FREE tempSpeed = tempNorm / tempTime
		tempSpeed[0] = 0
		// assemble into 2D wave
		Concatenate/O/KILL {tempNorm,tempTime,tempSpeed}, $newName
	endfor
	
	return 0
End

STATIC Function CollectStats()
	Wave uTrackID = root:uTrackID
	Variable nWaves = numpnts(uTrackID)
	Make/O/N=(nWaves,3) root:sum_traj
	Wave sum_traj = root:sum_traj
	String wName
	
	Variable i
	
	for(i = 0; i < nWaves; i += 1)
		wName = "sp_" + num2str(uTrackID[i])
		Wave w0 = $wName
		MatrixOp/O colSum = sumCols(w0)
		sum_traj[i][] = colSum[0][q]
		// speed is incorrect at this point
	endfor
	KillWaves/Z colSum
	sum_traj[][2] = sum_traj[p][0] / sum_traj[p][1]
	
	NewDataFolder/O/S root:data:stats
	Duplicate/O sum_traj, all_tracks
	SplitWave all_tracks
	WAVE/Z all_tracks0,all_tracks1,all_tracks2
	
	String plotName = "p_dist"
	KillWindow/Z $plotName
	Display/N=$plotName
	Make/N=100/O all_tracks0_Hist;
	Histogram/B=1 all_tracks0, all_tracks0_Hist
	AppendToGraph/W=$plotName all_tracks0_Hist
	SetAxis/W=$plotName/A/N=1 left
	SetAxis/W=$plotName/A/N=1 bottom
	ModifyGraph/W=$plotName mode=5,hbFill=4,rgb=(0,0,0)
	Label/W=$plotName left "Frequency"
	Label/W=$plotName bottom "Track length (\u03BCm)"
	
	plotName = "p_duration"
	KillWindow/Z $plotName
	Display/N=$plotName
	Make/N=100/O all_tracks1_Hist;
	Histogram/B=1 all_tracks1, all_tracks1_Hist
	AppendToGraph/W=$plotName all_tracks1_Hist
	SetAxis/A/N=1 left
	SetAxis/A/N=1 bottom
	ModifyGraph/W=$plotName mode=5,hbFill=4,rgb=(0,0,0)
	Label/W=$plotName left "Frequency"
	Label/W=$plotName bottom "Duration (s)"
	
	plotName = "p_speed"
	KillWindow/Z $plotName
	Display/N=$plotName
	Make/N=100/O all_tracks2_Hist;
	Histogram/B=1 all_tracks2, all_tracks2_Hist
	AppendToGraph/W=$plotName all_tracks2_Hist
	SetAxis/W=$plotName/A/N=1 left
	SetAxis/W=$plotName/A/N=1 bottom
	ModifyGraph/W=$plotName mode=5,hbFill=4,rgb=(0,0,0)
	Label/W=$plotName left "Frequency"
	Label/W=$plotName bottom "Average speed (\u03BCm/s)"
	
	return 0
End

STATIC Function HowManyVesicles()
	SetDataFolder root:
	Wave paramWave = root:paramWave
	
	Wave frame = root:frame
	FindDuplicates/RN=uFrame frame
	Duplicate/O/FREE frame, tempFrame
	Variable nFrames = numpnts(uFrame)
	Make/O/N=(nFrames) uCount
	
	Variable i
	
	for(i = 0; i < nFrames; i += 1)
		tempFrame[] = (frame[p] == uFrame[i]) ? 1 : 0
		uCount[i] = sum(tempFrame)
	endfor
	
	Concatenate/O/KILL {uFrame,uCount}, sum_vpf
	
	String plotName = "p_vesiclesPerFrame"
	KillWindow/Z $plotName
	Display/N=$plotName sum_vpf[][1] vs sum_vpf[][0]
	SetAxis/W=$plotName/A/N=1 left
	SetAxis/W=$plotName/A/N=1 bottom
	Label/W=$plotName left "Vesicles"
	Label/W=$plotName bottom "Frame"
	// get average count from first ten frames
	WaveStats/Q/RMD=[1,11][1] sum_vpf
	Print "There are", V_avg, "vesicles per frame on average."
	Variable roivol = paramWave[3] * paramWave[4] * paramWave[1]^2 * paramWave[2]
	Print "Density is", V_avg / roivol, "vesicles per \u03BCm^3."
	Print "Assuming cell is 4000 \u03BCm^3 and nucleus occupies 10%, this would be approx.", (V_avg / roivol) * (4000 * 0.9), "vesicles."
	
	return 0
End

Function CalcRatios()
	Wave uTrackID = root:uTrackID
	Variable nWaves = numpnts(uTrackID)
	Make/O/N=(nWaves) root:directTracks_dd, root:directTracks_span
	Wave directTracks_dd = root:directTracks_dd
	Wave directTracks_span = root:directTracks_span
	Duplicate/O uTrackID, root:uTrackID_dd
	Wave trk_dd = root:uTrackID_dd
	Duplicate/O uTrackID, root:uTrackID_span
	Wave trk_span = root:uTrackID_span
	
	Wave paramWave = root:paramWave
	Variable tStep = paramWave[0]
	Variable pxSize = paramWave[1]

	// calculate d/D directionality ratio
	String plotName = "p_dDplot"
	KillWindow/Z $plotName	// setup plot
	Display/N=$plotName/HIDE=0
	
	SetDataFolder root:data:tracks:
	String wName
	
	Variable i
		
	for(i = 0; i < nWaves; i += 1)
		wName = "tk_" + num2str(uTrackID[i])
		Wave tkW = $wName
		// calculate d/D for the track
		Duplicate/O/FREE tkW, tempMat
		tempMat[][] -= tkW[0][q]
		if(DimSize(tkW,1) == 3)
			tempMat[][0,1] *= pxSize
			tempMat[][2] *= tStep
		else
			tempMat[][0,2] *= pxSize
			tempMat[][3] *= tStep
		endif
		Wave spW = $(ReplaceString("tk_",wName,"sp_"))
		// calc umulative distance
		Duplicate/O/RMD=[][0]/FREE spW, tempDist
		Integrate/DIM=0/METH=0 tempDist
		Make/O/N=(DimSize(tkW,0),2) $(ReplaceString("tk_",wName,"dD_"))
		Wave dDW = $(ReplaceString("tk_",wName,"dD_"))
		// this is d/D calc
		if(DimSize(tkW,1) == 3)
			dDW[][0] = sqrt(tempMat[p][0]^2 + tempMat[p][1]^2) / tempDist[p]
			// add time as second column
			dDW[][1] = tempMat[p][2]
		else
			dDW[][0] = sqrt(tempMat[p][0]^2 + tempMat[p][1]^2 + tempMat[p][2]^2) / tempDist[p]
			// add time as second column
			dDW[][1] = tempMat[p][3]
		endif
		dDW[0][0] = NaN
		
		if(DimSize(dDW,0) > 12)
			AppendToGraph/W=$plotName dDW[][0] vs dDW[][1]
			WaveStats/RMD=[][0]/Q dDW
			directTracks_dd[i] = V_avg
		else
			directTracks_dd[i] = 0
		endif
		
		// while in this loop let's calculate the span of the track
		if(DimSize(dDW,0) > 12)
			if(DimSize(tkW,1) == 3)
				Duplicate/O/RMD=[][0,1]/FREE tkW, tempMat // xy coords in pixels
				MatrixOp/O/FREE meanMat = averageCols(tempMat)
				tempMat[][] -= meanMat[0][q]
				Make/O/N=(DimSize(tempMat,0))/FREE distW
				distW[] = sqrt(tempMat[p][0]^2 + tempMat[p][1]^2)
			else
				Duplicate/O/RMD=[][0,2]/FREE tkW, tempMat // xyz coords in pixels
				MatrixOp/O/FREE meanMat = averageCols(tempMat)
				tempMat[][] -= meanMat[0][q]
				Make/O/N=(DimSize(tempMat,0))/FREE distW
				distW[] = sqrt(tempMat[p][0]^2 + tempMat[p][1]^2 + tempMat[p][2]^2)
			endif
			directTracks_span[i] = wavemax(distW) / numpnts(distW)
		else
			directTracks_span[i] = 0
		endif
	endfor
	ModifyGraph/W=$plotName rgb=(0,0,0,6554)
	SetAxis/W=$plotName/A/N=1 left
	SetAxis/W=$plotName/A/N=1 bottom
	Label/W=$plotName left "Directionality ratio (d/D)"
	Label/W=$plotName bottom "Time (s)"
	
	// sort by the value in directTracks_dd and select the top thirty
	Sort/R directTracks_dd, directTracks_dd, trk_dd
	Duplicate/O/FREE/RMD=[0,29] trk_dd, tw
	String trkList
	wfprintf trkList, "tk_%g;", tw
	PlotTheseTracks(trkList,"p_dDtopThirty")
	TextBox/W=p_dDtopThirty/C/N=text0/F=0/S=3/B=1/X=0.00/Y=0.00 "d/D Selected"
	PrintStats(tw, "d/D Selected")

	// sort by the value in directTracks_span and select the top thirty
	Sort/R directTracks_span, directTracks_span, trk_span
	Duplicate/O/FREE/RMD=[0,29] trk_span, tw
	wfprintf trkList, "tk_%g;", tw
	PlotTheseTracks(trkList,"p_spanTopThirty")
	TextBox/W=p_spanTopThirty/C/N=text0/F=0/S=3/B=1/X=0.00/Y=0.00 "footprint Selected"
	PrintStats(tw, "Footprint Selected")

	SetDataFolder root:
End

STATIC Function PrintStats(w, labelStr)
	Wave w
	String labelStr
	Variable nTracks = numpnts(w)
	WAVE/Z sum_traj = root:sum_traj
	WAVE/Z uTrackID = root:uTrackID
	Make/O/N=(nTracks,3)/FREE statsW
	Variable i
	
	for(i = 0; i < nTracks; i += 1)
		FindValue/V=(w[i]) uTrackID
		statsW[i][] = sum_traj[V_Value][q]
	endfor
	
	MatrixOp/O/FREE finalW = averageCols(statsW)
	
	Print LabelStr,"average values:"
	Print "   Distance:",finalW[0][0],"µm"
	Print "   Duration:",finalW[0][1],"s"
	Print "   Speed:",finalW[0][2],"µm/s"
End

Function CalcMSD()
	Wave paramWave = root:paramWave
	Variable tStep = paramWave[0]
	Variable pxSize = paramWave[1]

	Wave uTrackID = root:uTrackID
	Variable nWaves = numpnts(uTrackID)
	Make/O/N=(nWaves) root:directTracks_msd
	Wave directTracks_msd = root:directTracks_msd
	Duplicate/O uTrackID, root:uTrackID_msd
	Wave trk_msd = root:uTrackID_msd
	
	// calculate MSD (overlapping method)
	String plotName = "p_MSDplot"
	KillWindow/Z $plotName
	Display/N=$plotName/HIDE=0
	
	SetDataFolder root:data:tracks:
	String tkList = WaveList("tk_*", ";","")
	nWaves = ItemsInList(tkList)
	String wName, newName, msdXName, msdYName
	String xWavesToRemove = "", yWavesToRemove = ""
	Variable len
	
	Variable i
	
	for(i = 0; i < nWaves; i += 1)
		wName = StringFromList(i,tkList)	// tk wave
		WAVE tkW = $wName
		len = DimSize(tkW,0)
		if(DimSize(tkW,1) == 3)
			Duplicate/O/RMD=[][0,1]/FREE tkW, coordW // take xy coords
			coordW *= pxSize
			msdXName = ReplaceString("tk",wName,"MSDX")
			Duplicate/O/RMD=[1,*][2] tkW, $msdXName // take frame and make time
			Wave timeW = $msdXName
			timeW[] = (tkW[p + 1][2] - tkW[0][2]) * tStep
			newName = ReplaceString("tk",wName,"MSD")	// for results of MSD per track
			Make/O/N=(len-1,len-1,2)/FREE tempMat0,tempMat1
			// make 2 3D waves. 0 is end point to measure MSD, 1 is start point
			// layers are x and y
			tempMat0[][][] = (p >= q) ? coordW[p+1][r] : 0
			tempMat1[][][] = (p >= q) ? coordW[p-q][r] : 0
			MatrixOp/O/FREE tempMat2 = (tempMat0 - tempMat1) * (tempMat0 - tempMat1))
			Make/O/N=(len-1)/FREE countOfMSDPnts = (len-1)-p
			msdYName = ReplaceString("tk",wName,"MSDY")
			MatrixOp/O $msdYName = sumcols(sumbeams(tempMat2))^t / countOfMSDPnts
			Wave tempMSD = $msdYName
		else
			Duplicate/O/RMD=[][0,2]/FREE tkW, coordW // take xyz coords
			coordW *= pxSize
			msdXName = ReplaceString("tk",wName,"MSDX")
			Duplicate/O/RMD=[1,*][3] tkW, $msdXName // take frame and make time
			Wave timeW = $msdXName
			timeW[] = (tkW[p + 1][3] - tkW[0][3]) * tStep
			newName = ReplaceString("tk",wName,"MSD")	// for results of MSD per track
			Make/O/N=(len-1,len-1,3)/FREE tempMat0, tempMat1
			// make 2 3D waves. 0 is end point to measure MSD, 1 is start point
			// layers are x, y, z
			tempMat0[][][] = (p >= q) ? coordW[p+1][r] : 0
			tempMat1[][][] = (p >= q) ? coordW[p-q][r] : 0
			MatrixOp/O/FREE tempMat2 = (tempMat0 - tempMat1) * (tempMat0 - tempMat1))
			Make/O/N=(len-1)/FREE countOfMSDPnts = (len-1)-p
			msdYName = ReplaceString("tk",wName,"MSDY")
			MatrixOp/O $msdYName = sumcols(sumbeams(tempMat2))^t / countOfMSDPnts
			Wave tempMSD = $msdYName
		endif
		// make a 2D wave to display for tidiness but we'll temporarily keep time and MSD to make average
		Concatenate/O/NP=1 {timeW, tempMSD}, $newName
		Wave w = $newName
		if(DimSize(w,0) > 10)
			AppendtoGraph/W=$plotName w[][1] vs w[][0]
			if(DimSize(tkW,1) == 2)
				directTracks_msd[i] = MSDalpha(w,2)
			else
				directTracks_msd[i] = MSDalpha(w,3)
			endif
		else
			xWavesToRemove += msdXName + ";"
			yWavesToRemove += msdYName + ";"
			directTracks_msd[i] = 0
		endif
	endfor
	ModifyGraph/W=$plotName rgb=(32768,32768,32768,6554)
	
	// add average - we'll do this by wave in folder not waves on graph
	String avList, avName, errName, xList
	avList = Wavelist("MSDY*",";","")
	avList = RemoveFromList(yWavesToRemove,avList)
	xList = Wavelist("MSDX*",";","")
	xList = RemoveFromList(xWavesToRemove,xList)
	avName = "W_Ave_MSD"
	errName = ReplaceString("Ave", avName, "Err")
	fWaveAverage(avList, xList, 1, 1, avName, errName)
	AppendToGraph/W=$plotName $avName
	ModifyGraph/W=$plotName log=1
	SetAxis/W=$plotName/A/N=1 left
	len = numpnts($avName) * tStep
	SetAxis/W=$plotName bottom tStep,(len/2)
	Label/W=$plotName left "MSD"
	Label/W=$plotName bottom "Time (s)"
	ErrorBars/W=$plotName $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=$plotName lsize($avName)=2,rgb($avName)=(0,0,0)
	// add free diffusion trendline. E = expected diffusion = 4Dt
	// using average MSD at first time interval to find D
	Wave avW = $avName
	Make/O/N=(20) W_Trend_MSD
	Variable Dee
	if(DimSize(tkW,1) == 3)
		Dee = avW[0] / (4 * tStep)
		W_Trend_MSD[] = 4 * Dee * ((p + 1) * tStep)
	else
		Dee = avW[0] / (6 * tStep)
		W_Trend_MSD[] = 6 * Dee * ((p + 1) * tStep)
	endif
	Print "MSD trendline D =", Dee
	SetScale/P x tStep, tStep, "", W_Trend_MSD
	AppendToGraph/W=$plotName W_Trend_MSD
	ModifyGraph/W=$plotName lstyle(W_Trend_MSD)=3
	// tidy up
	PXPUtils#KillTheseWaves(avList)
	PXPUtils#KillTheseWaves(xList)
	
	String trkList
	// sort by the value in directTracks_msd and select the top thirty
	Sort/R directTracks_msd, directTracks_msd, trk_msd
	Duplicate/O/FREE/RMD=[0,29] trk_msd, tw
	wfprintf trkList, "tk_%g;", tw
	PlotTheseTracks(trkList,"p_msdTopThirty")
	TextBox/W=p_msdTopThirty/C/N=text0/F=0/S=3/B=1/X=0.00/Y=0.00 "MSD Selected"
	PrintStats(tw, "MSD Selected")
	// let's look at a random sample for comparison
	// remove the short tracks and the ones we've already picked
	Duplicate/O/FREE trk_msd, tw
	tw[] = (directTracks_msd[p] > 0 ) ? trk_msd[p] : NaN
	tw[0,29] = NaN
	WaveTransform zapnans tw
	Make/O/N=(numpnts(tw))/FREE indexW = 30 + p
	StatsResample/Q/N=(30) indexW
	Wave/Z W_Resampled
	Make/O/N=(30)/FREE tw = trk_msd[W_Resampled[p]]
	wfprintf trklist, "tk_%g;", tw
	PlotTheseTracks(trkList,"p_msdRandomThirty")
	TextBox/W=p_msdRandomThirty/C/N=text0/F=0/S=3/B=1/X=0.00/Y=0.00 "Random"
	PrintStats(tw, "Random")

	SetDataFolder root:
End

STATIC Function MSDAlpha(w,dimension)
	Wave w // 2D wave with x = t = col 0, y = msd = col 1
	Variable dimension
	Variable alpha, dee, poi
	
	if(dimension == 2)
		// MSD = 4Dt so D = MSD / 4t
		dee = w[0][1] / 4 * w[0][0]
		// alpha = MSD / E(msd) and E(msd) = 4 * dee * t
		// so alpha = 1 at t0
		// we will take the second to last point of each track
		poi = DimSize(w,0) - 3 // point of interest
		alpha = w[poi][1] / (4 * dee * w[poi][0])
	else
		dee = w[0][1] / 6 * w[0][0]
		// alpha = MSD / E(msd) and E(msd) = 4 * dee * t
		// so alpha = 1 at t0
		// we will take the second to last point of each track
		poi = DimSize(w,0) - 3 // point of interest
		alpha = w[poi][1] / (6 * dee * w[poi][0])
	endif
	
	return alpha
End

Function CalcSqD()
	Wave paramWave = root:paramWave
	Variable tStep = paramWave[0]
	Variable pxSize = paramWave[1]

	Wave uTrackID = root:uTrackID
	Variable nWaves = numpnts(uTrackID)
	Make/O/N=(nWaves) root:calcDW
	Wave calcDW = root:calcDW
	
	// calculate SqD (Squared Displacement)
	String plotName = "p_SqDplot"
	KillWindow/Z $plotName
	Display/N=$plotName/HIDE=0
	
	SetDataFolder root:data:tracks:
	String tkList = WaveList("tk_*", ";","")
	nWaves = ItemsInList(tkList)
	String wName, newName, sqdXName, sqdYName
	String xWavesToRemove = "", yWavesToRemove = ""
	Variable len
	
	Variable i
	
	for(i = 0; i < nWaves; i += 1)
		wName = StringFromList(i,tkList)	// tk wave
		WAVE tkW = $wName
		len = DimSize(tkW,0)
		if(DimSize(tkW,1) == 3)
			Duplicate/O/RMD=[][0,1]/FREE tkW, coordW // take xy coords
			coordW[][] -= tkW[0][q] // offset to 0
			coordW *= pxSize // scale (coords can now be used for SqD since they are offset)
			SqdXName = ReplaceString("tk",wName,"SQDX")
			// we need a time wave because there may be gaps so cannot use scaling
			Duplicate/O/RMD=[][2] tkW, $SqdXName // take frame and make time
			Wave timeW = $sqdXName
			//timeW[] = (tkW[p][2] - tkW[0][2]) * tStep
			timeW[] *= tStep // compare similar parts of movie in case this effect of illumination
			newName = ReplaceString("tk",wName,"SqD")	// for results of SqD per track
			SqdYName = ReplaceString("tk",wName,"SqDY")
			MatrixOp/O $SqdYName = sumrows(coordW * coordW)	// squared displacement
			Wave tempSqD = $SqdYName
			// make a 2D wave to display for tidiness but we'll temporarily keep time and sqD to make average
			Concatenate/O/NP=1 {timeW, tempSqD}, $newName
		else
			Duplicate/O/RMD=[][0,2]/FREE tkW, coordW // take xyz coords
			coordW[][] -= tkW[0][q] // offset to 0
			coordW *= pxSize // scale (coords can now be used for SqD since they are offset)
			SqdXName = ReplaceString("tk",wName,"SQDX")
			// we need a time wave because there may be gaps so cannot use scaling
			Duplicate/O/RMD=[][3] tkW, $SqdXName // take frame and make time
			Wave timeW = $sqdXName
			//timeW[] = (tkW[p][2] - tkW[0][2]) * tStep
			timeW[] *= tStep // compare similar parts of movie in case this effect of illumination
			newName = ReplaceString("tk",wName,"SqD")	// for results of SqD per track
			SqdYName = ReplaceString("tk",wName,"SqDY")
			MatrixOp/O $SqdYName = sumrows(coordW * coordW)	// squared displacement
			Wave tempSqD = $SqdYName
			// make a 2D wave to display for tidiness but we'll temporarily keep time and sqD to make average
			Concatenate/O/NP=1 {timeW, tempSqD}, $newName
		endif
		Wave w = $newName
		if(DimSize(w,0) > 10)
			AppendtoGraph/W=$plotName w[][1] vs w[][0]
			calcDW[i] = CalculateDiffusionCoefficient(tkW)
		else
			xWavesToRemove += sqdXName + ";"
			yWavesToRemove += sqdYName + ";"
			calcDW[i] = NaN
		endif
	endfor
	ModifyGraph/W=$plotName rgb=(32768,32768,32768,6554)
	
	// add average - we'll do this by wave in folder not waves on graph
	String avList, avName, errName, xList
	avList = Wavelist("SqDY*",";","")
	avList = RemoveFromList(yWavesToRemove,avList)
	xList = Wavelist("SqDX*",";","")
	xList = RemoveFromList(xWavesToRemove,xList)
	avName = "W_Ave_SqD"
	errName = ReplaceString("Ave", avName, "Err")
	fWaveAverage(avList, xList, 3, 1, avName, errName)
	AppendToGraph/W=$plotName $avName
	Wave avW = $avName
	Wave errW = $errName
	Variable axMax = avW(1.5) + ErrW(1.5)
	SetAxis/W=$plotName left 0, axMax
	SetAxis/W=$plotName bottom 0,1.5 // hard coded
	Label/W=$plotName left "Squared Displacement"
	Label/W=$plotName bottom "Time (s)"
	ErrorBars/W=$plotName $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($errName,$errName)
	ModifyGraph/W=$plotName lsize($avName)=2,rgb($avName)=(0,0,0)
	// find trend
	CurveFit/H="10"/Q line avW[0,10] /W=errW /I=1 /D 
	WAVE/Z W_coef
	// W_coef[1] = gradient which should equal 2 * k^2
	// since k = sqrt(2 * D * tau), gradient = 4 D tau
	// since gradient is a factor of seconds, tau = 1 so
	if(DimSize(tkW,1) == 3)
		Print "Squared displacement trendline D =", W_coef[1] / 4
	else
		Print "Squared displacement trendline D =", W_coef[1] / 6
	endif
	
	// tidy up
	PXPUtils#KillTheseWaves(WaveList("SqDX_*",";",""))
	PXPUtils#KillTheseWaves(WaveList("SqDY_*",";",""))
	
	SetDataFolder root:
End

STATIC Function CalculateDiffusionCoefficient(matA)
	Wave matA // three column wave - x, y, t or four column wave - x, y, z, t
	WAVE/Z paramWave = root:paramWave
	
	Variable dee
	
	if(DimSize(matA,1) == 3)
		Duplicate/O/FREE/RMD=[][0,1] matA, tempXY
		tempXY[][] -= matA[0][q]
		tempXY[][] *= paramWave[1]
		Differentiate/DIM=0/METH=2 tempXY
		DeletePoints 0,1, tempXY
		// because time may be 1 frame or more
		Duplicate/O/FREE/RMD=[][2] matA, tempT
		tempT[] -= matA[0][2]
		Differentiate/DIM=0/METH=2 tempT
		DeletePoints 0,1, tempT
		// now
		MatrixOp/O/FREE dSqDW = sumrows(tempXY * tempXY) / tempT
		dee = mean(dSqDW) / (4 * paramWave[0])
	else
		Duplicate/O/FREE/RMD=[][0,2] matA, tempXY
		tempXY[][] -= matA[0][q]
		tempXY[][] *= paramWave[1]
		Differentiate/DIM=0/METH=2 tempXY
		DeletePoints 0,1, tempXY
		// because time may be 1 frame or more
		Duplicate/O/FREE/RMD=[][3] matA, tempT
		tempT[] -= matA[0][3]
		Differentiate/DIM=0/METH=2 tempT
		DeletePoints 0,1, tempT
		// now
		MatrixOp/O/FREE dSqDW = sumrows(tempXY * tempXY) / tempT
		dee = mean(dSqDW) / (6 * paramWave[0])
	endif
	
	return dee
End

Function PlotTheseTracks(trkList,plotName)
	String trkList, plotName
	
	SetDataFolder root:data:tracks:
	
	KillWindow/Z $plotName
	Display/N=$plotName
	
	Variable nTracks = ItemsInList(trkList)
	Variable i
	
	for(i = 0; i < nTracks; i += 1)
		Wave w = $(StringFromList(i,trkList))
		AppendToGraph/W=$plotName w[][0] vs w[][1]
	endfor
	
	WAVE/Z paramWave = root:paramWave
	SetAxis/W=$plotName left paramWave[4],0
	SetAxis/W=$plotName bottom 0,paramWave[3]
	ModifyGraph/W=$plotName width={Aspect,1}
	ModifyGraph/W=$plotName mirror=1,tick=3,noLabel=2,standoff=0,margin=3
End

Function CompareAlgorithms()
	SetDataFolder root:
	WAVE/Z uTrackID, uTrackID_dd, uTrackID_msd, uTrackID_span
	Variable nTracks = DimSize(uTrackID,0)
	Make/O/N=(nTracks,3) comparisonMat
	Variable trk
	
	Variable i
	
	for(i = 0; i < nTracks; i += 1)
		trk = uTrackID[i]
		FindValue/V=(trk) uTrackID_dd
		comparisonMat[i][0] = V_Value
		FindValue/V=(trk) uTrackID_msd
		comparisonMat[i][1] = V_Value
		FindValue/V=(trk) uTrackID_span
		comparisonMat[i][2] = V_Value
	endfor
	
	Concatenate/O/NP=1 {uTrackID_dd,uTrackID_msd,uTrackID_span}, tempMat
	// just take the top n from all three
	MatrixTranspose tempMat
	Redimension/N=(numpnts(tempMat)) tempMat
	FindDuplicates/RN=uTrackID_all tempMat
	KillWaves/Z tempMat
	
	String trkList
	Duplicate/O/FREE/RMD=[0,99] uTrackID_all, tw
	wfprintf trkList, "tk_%g;", tw
	PlotTheseTracks(trkList,"p_alTopHundred")
	TextBox/W=p_alTopHundred/C/N=text0/F=0/S=3/B=1/X=0.00/Y=0.00 "All Selected"
	PrintStats(tw,"Top 100")
End


////////////////////////////////////////////////////////////////////////
// Panel functions
///////////////////////////////////////////////////////////////////////

Function VesicleTracks_Panel()
	
	// make global waves to store settings
	// Make/O/N=5 paramWave={tStep, pxSize, thickness, width, height}
	Make/O/N=(5) paramWave={0.06,0.04,0.2,0,0}
	Make/O/N=(2)/T pathWave
	String panelName = "MSPTracker"
	KillWindow/Z $panelName
	NewPanel/N=$panelName/K=1/W=(40,40,500,300)
	DrawText/W=$panelName 10,30,"Output from msp-tracker (csv file)"
	DrawText/W=$panelName 10,100,"Image used in msp-tracker (TIFF)"
	DrawText/W=$panelName 10,250,"VesicleTracks"
	// do it button
	Button DoIt,pos={340,220},size={100,20},proc=MSPTrackerButtonProc,title="Do It",fstyle=1,fcolor=(30840,50629,61423)
	// file button
	Button csvSelect,pos={10,40},size={48,20},proc=MSPTrackerButtonProc,title="File"
	// file box - displays path
	SetVariable fileBox,pos={62,43},size={380,14},value= pathWave[0], title=" "
	// file button
	Button tifSelect,pos={10,110},size={48,20},proc=MSPTrackerButtonProc,title="Image"
	// file box - displays path
	SetVariable imageBox,pos={62,113},size={380,14},value= pathWave[1], title=" "
	// specify timing
	SetVariable box0,pos={10,160},size={140,24},title="Frame interval (s)",value=paramWave[0]
	// specify pixelsize
	SetVariable box1,pos={10,180},size={140,30},title="Pixel size (\u03BCm)",value=paramWave[1]
	// specify depth
	SetVariable box2,pos={10,200},size={140,30},title="Section depth (\u03BCm)",value=paramWave[2]
	// flip image (msp has x and y switched, ground truth and trackmate do not) check this box if you get a mismatch
	CheckBox xyOpt,pos={10,140},size={105,14},title="XY flip",value=0
End

// define buttons
Function MSPTrackerButtonProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba

	Wave/T pathWave = root:pathWave
	Wave paramWave = root:paramWave
	Variable refnum, xyFlip = 0
	String filters, fileName

	switch(ba.eventCode)
		case 2:
			ControlInfo/W=msptracker xyopt
			if(V_Value)
				xyflip = 1
			endif
			
			if(CmpStr(ba.ctrlName,"csvSelect") == 0)
				filters = "CSV File (*.csv):.csv;All Files:.*;"
				Open/D/R/F=filters/M="Select csv file" refnum
				fileName = S_fileName
				if (strlen(fileName) == 0)		// User cancelled?
					return -1
				else
					pathWave[0] = fileName
					pathWave[1] = ReplaceString(".csv",fileName,".tif")
					return 0
				endif
			
			elseif(CmpStr(ba.ctrlName,"tifSelect") == 0)
				filters = "TIFF File (*.tif,*.tiff):.tif,.tiff;"
				Open/D/R/F=filters/M="Select image" refnum
				fileName = S_fileName
				if (strlen(fileName) == 0)		// User cancelled?
					return -1
				else
					pathWave[1] = fileName
					return 0
				endif
			
			elseif(CmpStr(ba.ctrlName,"DoIt") == 0)
				if(strlen(pathWave[0]) > 0 && strlen(pathWave[1]) > 0)
					// load the csv file
					LoadWave/A/W/J/K=1/L={0,1,0,0,0}/O/Q pathWave[0]
					// load the image
					ImageLoad/T=tiff/O/S=0/C=-1/LR3D/Q/N=image pathWave[1]
					WAVE/Z image
					if(xyflip == 1)
						ImageRotate/C/O image	
					endif
					paramWave[3] = DimSize(image,0) // width
					paramWave[4] = DimSize(image,1) // height
					KillWindow/Z MSPTracker
					VTWrapperPtTwo()
					return 0
				else
					return -1
				endif
			else
				return -1
			endif
	endswitch
	
	return 0
End


Function BuildMovieDisplay()
	SetDataFolder root:
	WAVE/Z Image, paramWave
	// make vtMovie window
	String graphName = "vtMovie"
	KillWindow/Z $graphName
	NewImage/N=$graphName/S=0 Image
	ControlBar/W=$graphName 50
	ModifyGraph/W=$graphName width={Aspect,1}
	SetAxis/R/W=$graphName left paramWave[4]-0.5,-0.5
	SetAxis/W=$graphName top -0.5,paramWave[4]-0.5
	Slider sld, win=vtMovie,limits={0,DimSize(Image,2)-1,1},size={0.9*paramWave[3],20},vert=0,thumbcolor=(30840,50629,61423),proc=ActionProcName
	// generate the waves for live display
	MakeGappedWaves(0)
	WAVE/Z liveXW,liveYW,liveXTrails,liveYTrails
	AppendToGraph/T/W=$graphName liveYW/TN=spots vs liveXW // recall that X and Y are the other way around
	ModifyGraph/W=$graphName mode(spots)=3,marker(spots)=8,msize(spots)=5,mrkThick(spots)=2
	ModifyGraph/W=$graphName rgb(spots)=(65535,65535,0)
	AppendToGraph/T/W=$graphName liveYTrails/TN=trails vs liveXTrails // recall that X and Y are the other way around
	ModifyGraph/W=$graphName mode(trails)=0,lsize(trails)=2
	ModifyGraph/W=$graphName rgb(trails)=(65535,26985,46260)
	
	moviePanel()

	return 0
End

STATIC Function MakeGappedWaves(optVar)
	Variable optVar // 0 is all spots, 1 is out selection
	WAVE/Z xW,yW,Frame,TrackID
	Duplicate/O xW, gappedXW
	Duplicate/O yW, gappedYW
	Duplicate/O Frame, gappedFrame
	Duplicate/O trackID, gappedTrackID
	
	Variable i
	
	if(optVar == 1)
		WAVE/Z uTrackID_all
		Duplicate/O/FREE/RMD=[0,99] uTrackID_all, tw
		Variable nSpots = numpnts(tw)
		Make/O/N=(numpnts(gappedXW))/FREE keepDelete = 0
		for(i = 0; i < nSpots; i += 1)
			keepDelete[] = (gappedTrackID[p] == tw[i]) ? 1 : keepDelete[p]
		endfor
		gappedXW[] = (keepDelete[p] == 1) ? gappedXW[p] : NaN
		gappedYW[] = (keepDelete[p] == 1) ? gappedYW[p] : NaN
		gappedFrame[] = (keepDelete[p] == 1) ? gappedFrame[p] : NaN
		gappedTrackID[] = (keepDelete[p] == 1) ? gappedTrackID[p] : NaN
		WaveTransform zapnans gappedXW
		WaveTransform zapnans gappedYW
		WaveTransform zapnans gappedFrame
		WaveTransform zapnans gappedTrackID
	endif
	Differentiate/METH=1 gappedTrackID/D=gaps
	Variable startRow = 0
	
	for(i = startRow; i < numpnts(gaps); i += 1)
		if(gaps[i] > 0)
			InsertPoints/V=(NaN) i + 1, 1, gaps, gappedXW, gappedYW, gappedFrame, gappedTrackID
			startRow = i + 2
		endif			
	endfor
	
	Make/O/N=(numpnts(gappedYW)) liveXW, liveYW
	liveXW[] = (gappedFrame[p] == 0) ? gappedXW[p] : NaN
	liveYW[] = (gappedFrame[p] == 0) ? gappedYW[p] : NaN
	Make/O/N=(numpnts(gappedYW)) liveXTrails, liveYTrails
	liveXTrails[] = (gappedFrame[p] == 0) ? gappedXW[p] : NaN
	liveYTrails[] = (gappedFrame[p] == 0) ? gappedYW[p] : NaN
	
	KillWaves/Z gaps
End

Function updateFrame(liveSlice)
	Variable liveSlice
	WAVE/Z liveXW, liveYW, liveXTrails, liveYTrails, gappedXW, gappedYW, gappedFrame
	liveXW[] = (gappedFrame[p] == liveSlice) ? gappedXW[p] : NaN
	liveYW[] = (gappedFrame[p] == liveSlice) ? gappedYW[p] : NaN
	liveXTrails[] = (gappedFrame[p] < liveSlice - 5 || gappedFrame[p] > liveSlice) ? NaN : gappedXW[p]
	liveYTrails[] = (gappedFrame[p] < liveSlice - 5 || gappedFrame[p] > liveSlice) ? NaN : gappedYW[p]
End

Function ActionProcName(sa) : SliderControl
	STRUCT WMSliderAction &sa

	sa.blockReentry=1
	if(sa.eventcode == 9)
		ModifyImage/W=vtMovie Image plane=(sa.curval)
		updateFrame(sa.curval)
	endif
End

// Define the panel that works with the movie window
Function moviePanel()
	KillWindow/Z vtPanel
	NewPanel/N=vtPanel/K=1/W=(0, 0, 200, 200)/HOST=vtMovie/EXT=0
	Button all,pos={25,50},size={150,20},proc=movieControlProc,title="All tracks"
	Button selected,pos={25,80},size={150,20},proc=movieControlProc,title="Selected tracks"
	Button export,pos={35,170},size={130,20},proc=movieControlProc,title="Save movie",fstyle=1,fcolor=(30840,50629,61423)
	
	return 0
End

Function movieControlProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	
	switch(ba.eventCode)
		case 2:
			if(cmpstr(ba.ctrlName,"all") == 0)
				MakeGappedWaves(0)
			elseif(cmpstr(ba.ctrlName,"selected") == 0)
				MakeGappedWaves(1)
			elseif(cmpstr(ba.ctrlName,"export") == 0)
				MovieMaker()
				return 0
			endif
	endswitch	
End

Function MovieMaker()
	NewPath/Q/O OutputPath
	NewMovie/P=OutputPath/CF=1/F=10 as "output.mp4"
	WAVE/Z image
	Variable nFrames = DimSize(Image,2)
	String tiffName, iString
	String sprintStr = "%0" + num2str(ceil(log(nFrames))) + "d"
	
	Variable i
	
	for(i = 0 ; i < nFrames; i += 1)
		ModifyImage/W=vtMovie Image plane=(i)
		updateFrame(i)
		AddMovieFrame
		// movie is low quality - make tifs too
		sprintf iString, sprintStr, i
		tiffName = "tracks" + iString + ".tif"
		SavePICT/WIN=vtMovie/P=OutputPath/E=-7/RES=300/Z as tiffName
		Print "Saving movie frames..."
	endfor
	CloseMovie
	Print "Done"
End


////////////////////////////////////////////////////////////////////////
// Utility functions
////////////////////////////////////////////////////////////////////////

