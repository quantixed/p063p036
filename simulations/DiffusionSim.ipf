#pragma TextEncoding = "UTF-8"
#pragma rtGlobals=3				// Use modern global access method and strict wave access
#pragma DefaultTab={3,20,4}		// Set default tab width in Igor Pro 9 and later
#include "PXPUtils"
#include <Waves Average>

////////////////////////////////////////////////////////////////////////
// Menu items
////////////////////////////////////////////////////////////////////////
Menu "Macros"
	"Vesicle Panel", /Q, MakeVesiclePanel()
End

////////////////////////////////////////////////////////////////////////
// Master functions and wrappers
////////////////////////////////////////////////////////////////////////
Function SimulateVesicleMovement(flipOpt,graphOpt,repOpt)
	Variable flipOpt,graphOpt,repOpt
	
	WAVE/Z paramWave
	ReadyToRun()
	
	Variable i
	
	if(repOpt == 0)
		// simulate vesicles
		VesiclesAndBoundary()
		// if we are doing flip
		if(flipOpt == 1)
			FindTimeToContact()
		endif
		
		if(graphOpt == 1)
			GraphAllVesicles(flipOpt,paramWave[%rr])
			if(flipOpt == 1)
				GraphContactTimes("contactFracW;",0)
			endif
		endif
	else // we are doing repeats of FLIP
		for(i = 0; i < repOpt; i += 1)
			VesiclesAndBoundary()
			FindTimeToContact()
			WAVE/Z contactFracW
			Duplicate/O contactFracW, $("contactFracW_" + num2str(i))
		endfor
		MakeRepReport(0)
	endif
End

Function AutomateSimulation(iStart,iStop,iPnts,jStart,jStop,jPnts,reps)
	Variable iStart,iStop,iPnts,jStart,jStop,jPnts,reps
	
	WAVE/Z paramWave
	ReadyToRun()
	
	// iWave varies FLIP area (radius)
	Make/O/N=(2)/D iWave = {iStart,iStop}
	iWave[] = log(iWave[p])
	Interpolate2/T=1/N=(iPnts)/Y=iWave iWave
	iWave[] = 10^iWave[p]
	// jWave varies D
	Make/O/N=(2)/D jWave = {jStart,jStop}
	jWave[] = log(jWave[p])
	Interpolate2/T=1/N=(jPnts)/Y=jWave jWave
	jWave[] = 10^jWave[p]
	
	Make/O/N=(numpnts(iWave),numpnts(jWave))/D resultMat, resultFit
	Make/O/N=(reps)/D/FREE resultW
	
	Variable i,j,k
	
	for(i = 0; i < numpnts(iWave); i += 1)
		paramWave[%flipCircleR] = iWave[i]
		for(j = 0;  j < numpnts(jWave); j += 1)
			Print "Running i=", i,"j=", j
			paramWave[%Dee] = jWave[j]
			paramWave[%k] = sqrt(2 * paramWave[%Dee] * paramWave[%tau])
			for(k = 0; k < reps; k += 1)
				VesiclesAndBoundary()
				resultW[k] = FindTimeToContact()
				WAVE/Z contactFracW
				Duplicate/O contactFracW, $("contactFracW_" + num2str(k))
			endfor
			resultMat[i][j] = mean(resultW)
			resultFit[i][j] = MakeRepReport(1)
		endfor
	endfor
	
	KillWindow/Z p_results
	Display/N=p_results
	for(j = 0;  j < numpnts(jWave); j += 1)
		AppendToGraph/W=p_results resultMat[][j] vs iWave
	endfor
	ModifyGraph/W=p_results log(left)=1
	SetAxis/A/N=1/W=p_results left
	Label/W=p_results left "Median Time (s)"
	Label/W=p_results bottom "Radius (m)"
	Plot2DResults()
End


////////////////////////////////////////////////////////////////////////
// Main functions
////////////////////////////////////////////////////////////////////////
Function VesiclesAndBoundary()
	WAVE/Z paramWave
	Variable nVesicles = paramWave[%nVesicles]
	Variable randOpt = paramWave[%randOpt]
	Variable k = paramWave[%k]
	Variable rr = paramWave[%rr]
	Variable nSteps = paramWave[%nSteps]

	Variable i,j
	
	for(i = 0; i < nVesicles; i += 1)
		Make/O/N=(nSteps,2)/D $("xyW_" + num2str(i))
		Wave w = $("xyW_" + num2str(i))
		if(randOpt == 0)
			w[][] = 0
		else
			RandomStart(w,rr)
		endif
		
		for(j = 1; j < nSteps; j += 1)
			do
				w[j][] = w[j-1][q] + (k * gnoise(1))
			while(sqrt( w[j][0]^2 + w[j][1]^2 ) > rr)
		endfor
	endfor
End

Function GraphAllVesicles(flipOpt,maxVal)
	Variable flipOpt, maxVal
	String plotName = "p_vesicles"
	KillWindow/Z $plotName
	Display/N=$plotName/W=(40,40,640,640)
	String wList = WaveList("xyW_*",";","")
	Variable nWaves = ItemsInList(wList)
	String wName
	WAVE/Z fStateMat
	WAVE/Z paramWave
	
	Variable i
	
	for(i = 0; i < nWaves; i += 1)
		wName = StringFromList(i,wList)
		Wave w = $wName
		AppendToGraph/W=$plotName w[][1] vs w[][0]
		if(flipOpt == 1)
			ModifyGraph/W=$plotName zColor($wName)={fStateMat[*][i],0.5,1,Grays,1},zColorMin($wName)=NaN
		endif
	endfor
	SetAxis/W=$plotName left -maxVal, maxVal
	SetAxis/W=$plotName bottom -maxVal, maxVal
	ModifyGraph/W=$plotName mirror=1
	ModifyGraph/W=$plotName width={Aspect,1}
	if(flipOpt == 0)
		ModifyGraph/W=$plotName rgb=(0,0,0,16384)
	else
		// overlay the FLIP area if it is there
		SetDrawEnv/W=$plotName xcoord= bottom,ycoord= left,linefgc= (65535,0,0),fillpat= 0
		if(paramWave[%flipMod] == 0)	
			DrawOval/W=$plotName -paramWave[%FlipRingR],paramWave[%FlipRingR],paramWave[%FlipRingR],-paramWave[%FlipRingR]
		elseif(paramWave[%flipMod] == 1)
			DrawRect/W=$plotName paramWave[%flipLTX],paramWave[%flipLTY],paramWave[%flipRBX],paramWave[%flipRBY]
		elseif(paramWave[%flipMod] == 2)
			DrawOval/W=$plotName paramWave[%flipCircleCentre]-paramWave[%flipCircleR],paramWave[%flipCircleCentre]-paramWave[%flipCircleR],paramWave[%flipCircleCentre]+paramWave[%flipCircleR],paramWave[%flipCircleCentre]+paramWave[%flipCircleR]
		endif
	endif
End

Function FindTimeToContact()
	WAVE/Z paramWave
	String wList = WaveList("xyW_*",";","")
	Variable nWaves = ItemsInList(wList)
	Variable nSteps = paramWave[%nSteps]
	Make/O/N=(nSteps,nWaves) fStateMat = 1
	Variable duration, events
	
	Variable i,j
	
	// make a pattern illumination wave
	Make/O/N=(paramWave[%nSteps])/FREE frameTimeW, bleachW
	if(paramWave[%flipBleachT] >= paramWave[%flipBleachF])
		bleachW[] = 1
	else
		duration = nSteps * paramWave[%tau]
		events = (duration / paramWave[%flipBleachF]) + 1
		Make/O/N=(events)/FREE startW, stopW
		startW[] = p * paramWave[%flipBleachF]
		stopW[] = startW[p] + paramWave[%flipBleachT]
		frameTimeW[] = p * paramWave[%tau]
		for(i = 0; i < paramWave[%nSteps]; i += 1)
			for(j = 0; j < events; j += 1)
				if(frameTimeW[i] >= startW[j] && frameTimeW[i] < startW[j] + paramWave[%flipBleachF])
					if(frameTimeW[i] < stopW[j])
						bleachW[i] = 1
					else
						bleachW[i] = 0
					endif
					continue
				endif
			endfor
		endfor
	endif
	
	for(i = 0; i < nWaves; i += 1)
		Wave w = $(StringFromList(i,wList))
		// what happens next depends on FLIP model and bleach settings
		if(paramWave[%flipMod] == 0) // ring
			fStateMat[][i] = (sqrt(w[p][0]^2 + w[p][1]^2) >= paramWave[%flipRingR] && bleachW[p] == 1) ? 0 : fStateMat[p][i]
		elseif(paramWave[%flipMod] == 1) // square
			fStateMat[][i] = ((w[p][0] >= paramWave[%flipltx]) && (w[p][0] < paramWave[%fliprbx]) && (w[p][1] > paramWave[%fliplty]) && (w[p][1] <= paramWave[%fliprby]) && (bleachW[p] == 1)) ? 0 : fStateMat[p][i]
		elseif(paramWave[%flipMod] == 2) // circle
			fStateMat[][i] = (sqrt((w[p][0] - paramWave[%flipCircleCentre])^2 + (w[p][1] - paramWave[%flipCircleCentre])^2) <= paramWave[%flipCircleR] && bleachW[p] == 1) ? 0 : fStateMat[p][i]
		endif		
		
		// set all values after first 0 to be 0 for this vesicles/column
		FindValue/V=0/RMD=[][i] fStateMat
		if(V_Value != -1)
			fStateMat[V_row,*][i] = 0
		endif
	endfor
	// make a wave to show the contact times in seconds
	MatrixOp/O contactTimeW = sumcols(fStateMat)^t
	Redimension/N=-1 contactTimeW
	contactTimeW *= paramWave[%tau]
	// make a wave of completion (survival style)
	MatrixOp/O contactFracW = sumrows(fStateMat)
	Redimension/N=-1 contactFracW
	// we want time 0 to have all vesicles and then t+1 will equal the outcome of the first frame.
	InsertPoints 0,1,contactFracW
	contactFracW[0] = paramWave[%nVesicles]
	SetScale/P x 0,paramWave[%tau],"s", contactFracW
	
	return StatsMedian(contactTimeW)
End

Function GraphContactTimes(list,auto)
	String list
	Variable auto
	WAVE/Z paramWave
	String plotName = "p_contact"
	KillWindow/Z $plotName
	if(auto == 0)
		Display/N=$plotName/W=(640,40,1040,440)
	else
		Display/N=$plotName/W=(640,40,1040,440)/HIDE=1
	endif
	
	Variable i
	
	for(i = 0; i < ItemsInList(list); i += 1)
		AppendToGraph/W=$plotName $StringFromList(i,list)
	endfor
	
	SetAxis/W=$plotName left 0,paramWave[%nVesicles]
	Label/W=$plotName left "Vesicles"
	Label/W=$plotName bottom "Time (s)"
End

Function MakeRepReport(auto)
	Variable auto // 1 means we are in automated mode
	WAVE/Z paramWave
	if(auto == 0)
		PrintParams()
	endif
	String wList = WaveList("contactFracW_*",";","")
	GraphContactTimes(wList,auto)
	if(ItemsInList(wList) > 2)
		// make average with 1 sd
		String avName = "ave_contactFrac"
		String errName = "err_contactFrac"
		String plotName = "p_contact"
		fWaveAverage(wList, "", 1, 1, avName, errName)
		AppendToGraph/W=$plotName $avName
		ErrorBars/W=$plotName $avName SHADE= {0,0,(0,0,0,0),(0,0,0,0)},wave=($ErrName,$ErrName)
		ModifyGraph/W=$plotName lsize($avName)=2,rgb($avName)=(0,0,0)
	endif
	// do fitting
	Variable/G K0 = 0
	Variable/G K1 = paramWave[%nVesicles]
	if(ItemsInList(wList) > 2)
		CurveFit/H="110" exp_XOffset $avName /W=$errName /I=1 /D
		ModifyGraph/W=$plotName lstyle(fit_ave_contactFrac)=3,rgb(fit_ave_contactFrac)=(0,0,65535)
	else
		// fit to first trace
		CurveFit/H="110" exp_XOffset $(StringFromList(0,wList)) /D
	endif
	WAVE/Z W_coef
	Print "Half-time", W_coef[2] * ln(2)

	return W_coef[2] * ln(2)
End

Function PrintParams()
	WAVE/Z paramWave
	
	Variable i
	
	for(i = 0; i < numpnts(paramWave); i += 1)
		Print GetDimLabel(paramWave,0,i), paramWave[i]
	endfor
End


Function Plot2DResults()
	LoadNiceCTableW()
	WAVE/Z resultFit, resultMat, iWave, jWave
	if(!WaveExists(resultFit) || !WaveExists(resultMat))
		return -1
	endif
	KillWindow/Z p_fit
	KillWindow/Z p_Mat
	NewImage/K=0/N=p_fit root:resultFit
	NewImage/K=0/N=p_Mat root:resultMat
	ModifyImage/W=p_fit resultFit ctab= {0.01,10000,root:Packages:ColorTables:EPFL:Ametrine,0},log=1
	ModifyImage/W=p_Mat resultMat ctab= {0.01,10000,root:Packages:ColorTables:EPFL:Ametrine,0},log=1

	Make/O/N=(numpnts(iWave)) iPos = p
	Make/O/N=(numpnts(jWave)) jPos = p
	Make/O/N=(numpnts(iWave))/T iLabel = num2str(iWave[p])
	Make/O/N=(numpnts(jWave))/T jLabel = num2str(jWave[p])
	ModifyGraph/W=p_fit userticks(left)={jPos,jLabel},userticks(top)={iPos,iLabel}
	ModifyGraph/W=p_Mat userticks(left)={jPos,jLabel},userticks(top)={iPos,iLabel}
	ModifyGraph/W=p_fit width={Plan,1,top,left}
	ModifyGraph/W=p_Mat width={Plan,1,top,left}
End

////////////////////////////////////////////////////////////////////////
// Panel functions
////////////////////////////////////////////////////////////////////////

Function MakeVesiclePanel()
	SetDataFolder root:
	WAVE/Z paramWave
	if(!WaveExists(paramWave))
		if(MakeParamWave() != 1)
			return -1
		endif
		WAVE/Z paramWave
	endif
	KillWindow/Z vesiclepanel
	NewPanel/K=1/W=(15,44,347,670)/N=vesiclepanel as "Vesicle Panel"
	ModifyPanel fixedSize=1
	SetDrawLayer UserBack
	DrawText 15,54,"Frame duration (s)"
	DrawText 15,72,"Diffusion constant (m^2/s)"
	DrawText 15,108,"Cell radius (m)"
	DrawText 15,126,"Number of frames"
	DrawText 15,142,"Number of vesicles"
	DrawText 15,198,"FLIP Ring radius (m)"
	DrawText 15,242,"FLIP Square (m)"
	DrawText 15,296,"FLIP Circle (m)"
	DrawText 15,324,"Bleach duration (s)"
	DrawText 15,342,"Bleach interval (s)"
	TitleBox tb0,pos={15,9},size={52,20},title="Settings for simulation:",fstyle=1,fsize=11,labelBack=(55000,55000,65000),frame=0
	String varName, labStr
	String hideList = "k;randOpt;flipMod;"
	
	Variable i
	
	for(i = 0; i < numpnts(paramWave); i += 1)
		labStr = GetDimLabel(paramWave, 0, i)
		if(WhichListItem(labStr,hideList) >= 0)
			continue
		endif
		varName = "ves_" + labStr
		SetVariable $varName,pos={190 + (20-strlen(varName)),40 + (i * 18)},size={120 - (20-strlen(varName)),18},bodyWidth=80,title=labStr,value= paramWave[i]
	endfor
	CheckBox randCheck,pos={17,152},size={105,14},proc=randOptProc, title="Random start location",value= paramWave[%randOpt]
	PopupMenu FlipPop,pos={180,153},size={145,20},proc=VesiclePopMenuProc,title="FLIP Mode:"
	PopupMenu FlipPop,mode=2,popvalue="Circle",value= #"\"Ring;Square;Circle\""
	CheckBox plotCheck,pos={17,350},size={105,14},title="Visualise",value= 1
	Button vesSim,pos={184,350},size={115,21},proc=vesDoItButtonProc,title="Simulate"
	Button vesFLIP,pos={184,380},size={115,21},proc=vesDoItButtonProc,title="FLIP"
	SetVariable repeat,pos={15,415},size={120,18},title="Repeats",value=_NUM:10
	Button vesFLIPRep,pos={184,410},size={115,21},proc=vesDoItButtonProc,title="FLIP Rep"
	Button vesReset,pos={15,384},size={115,14},proc=vesResetButtonProc,title="Reset"
	DrawLine 15,440,312,440
	// automation part
	TitleBox tb1,pos={15,460},size={52,20},title="Automate analysis:",fstyle=1,fsize=11,labelBack=(55000,65000,55000),frame=0
	DrawText 15,510,"FLIP Circle Radius (m)"
	DrawText 15,565,"Diffusion constant (m^2/s)"
	SetVariable iLeft,pos={190,480},size={120,18},title="Lower",value=_NUM:1e-6
	SetVariable iRight,pos={190,495},size={120,18},title="Upper",value=_NUM:1.5e-5
	SetVariable iSteps,pos={190,510},size={120,18},title="Points",value=_NUM:14
	SetVariable jLeft,pos={190,535},size={120,18},title="Lower",value=_NUM:1e-14
	SetVariable jRight,pos={190,550},size={120,18},title="Upper",value=_NUM:1e-11
	SetVariable jSteps,pos={190,565},size={120,18},title="Points",value=_NUM:8
	Button vesAuto,pos={184,592},size={115,21},proc=vesDoItButtonProc,title="Automate"
End

Function VesiclePopMenuProc(pa) : PopupMenuControl
	STRUCT WMPopupAction &pa

	switch( pa.eventCode )
		case 2: // mouse up
			Variable popNum = pa.popNum
			WAVE/Z paramWave
			switch(popNum)
				case 1:
					paramWave[%flipMod] = 0
				break
				case 2: 
					paramWave[%flipMod] = 1
				break
				case 3:
					paramWave[%flipMod] = 2
				break
			endswitch
			break
	endswitch

	return 0
End

Function randOptProc(CB_Struct) : CheckBoxControl
	STRUCT WMCheckboxAction &CB_Struct
	
	WAVE/Z paramWave
	if(!WaveExists(paramWave))
		return -1
	endif
	
	if(CB_Struct.checked == 1)
		paramWave[%randOpt] = 1
	elseif(CB_Struct.checked == 0)
		paramWave[%randOpt] = 0
	endif
	
	return 0
End

Function vesDoItButtonProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	
	WAVE/Z paramWave
	if(!WaveExists(paramWave))
		return -1
	endif
	Variable plotOpt = 0, reps = 0
	Variable iStart,iStop,iPnts,jStart,jStop,jPnts
	
	switch(ba.eventCode)
		case 2 :
			paramWave[%k] = sqrt(2 * paramWave[%Dee] * paramWave[%tau])
			ControlInfo/W=vesiclepanel plotCheck
			if(V_Value)
				plotOpt = 1
			endif
			ControlInfo/W=vesiclepanel repeat
			reps = V_Value
			
			if(CmpStr(ba.ctrlName,"vesSim") == 0)
				SimulateVesicleMovement(0,plotOpt,0)
				return 0
			elseif(CmpStr(ba.ctrlName,"vesFLIP") == 0)
				SimulateVesicleMovement(1,plotOpt,0)
			elseif(CmpStr(ba.ctrlName,"vesFLIPRep") == 0)
				SimulateVesicleMovement(1,plotOpt,reps)
			elseif(CmpStr(ba.ctrlName,"vesAuto") == 0)
				ControlInfo/W=vesiclepanel iLeft
				iStart = V_Value
				ControlInfo/W=vesiclepanel iRight
				iStop = V_Value
				ControlInfo/W=vesiclepanel iSteps
				iPnts = V_Value
				ControlInfo/W=vesiclepanel jLeft
				jStart = V_Value
				ControlInfo/W=vesiclepanel jRight
				jStop = V_Value
				ControlInfo/W=vesiclepanel jSteps
				jPnts = V_Value
				AutomateSimulation(iStart,iStop,iPnts,jStart,jStop,jPnts,reps)
			else
				return -1
			endif
		case -1:
			break
	endswitch
	
	return 0
End

Function vesResetButtonProc(ba) : ButtonControl
	STRUCT WMButtonAction &ba
	
	switch(ba.eventCode)
		case 2 :
			if(CmpStr(ba.ctrlName,"vesReset") == 0)
				MakeParamWave()
				MakeVesiclePanel()
			else
				return -1
			endif
		case -1:
			break
	endswitch
	
	return 0
End

////////////////////////////////////////////////////////////////////////
// Utility functions
////////////////////////////////////////////////////////////////////////

STATIC Function RandomStart(w,radius)
	WAVE w
	Variable radius
	Variable rr
	Variable tt = 2 * pi * (0.5 + enoise(0.5))
	Variable uu = (0.5 + enoise(0.5)) + (0.5 + enoise(0.5))
	if(uu > 1)
		rr = 2 - uu
	else
		rr = uu
	endif
	w[0][0] = radius * rr * cos(tt)
	w[0][1] = radius * rr * sin(tt)
End

STATIC Function LoadNiceCTableW()
	NewDataFolder/O root:Packages
	NewDataFolder/O root:Packages:ColorTables
	String/G root:Packages:ColorTables:oldDF = GetDataFolder(1)
	NewDataFolder/O/S root:Packages:ColorTables:EPFL
	LoadWave/H/O/P=Igor/Q ":Color Tables:EPFL:Ametrine.ibw"
	KillStrings/Z/A
	SetDataFolder root:
	KillStrings/Z root:Packages:ColorTables:oldDF
	KillVariables/Z root:Packages:ColorTables:EPFL:V_flag
End

STATIC Function MakeParamWave()
	Make/O/N=17/D paramWave = {0.06, 3e-13, 2.683281572999748e-07, 15e-6, 30000, 100, 1, 2, 1e-5, -2.5e-6, -2.5e-6, 2.5e-6, 2.5e-6, 0, 2.7e-6, 0.6, 3}
	String labelStr = "tau;Dee;k;rr;nSteps;nVesicles;randOpt;flipMod;FlipRingR;flipLTX;flipLTY;flipRBX;flipRBY;flipCircleCentre;flipCircleR;flipBleachT;flipBleachF;"
	if(PXPUtils#LabelWaveDimensions(paramWave,0,LabelStr) == 1)
		return 1
	endif
End

STATIC Function ReadyToRun()
	KillWindow/Z p_Vesicles
	KillWindow/Z p_contact
	KillWindow/Z p_fit
	KillWindow/Z p_mat
	KillWindow/Z p_results
	PXPUtils#KillAllExcept("paramWave")
End