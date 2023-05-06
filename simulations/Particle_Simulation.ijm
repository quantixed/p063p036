/* 
 *  Simulation of diffusing particles in ImageJ
 *	Written by Méghane Sittewelle & Stephen J. Royle
*/

macro "Simulation (Multi)" {
	mode = "3D";
	imageDim = 350;     // Set image dimension in pixels
	pixelSize = 0.04;    // What is the pixel size in um/pixel
	timeFrames = 200;   // How many time frames?
	dT = 0.01;          // Time Interval (s)
	particleSize = 5;	// size of each particle
	confocalplane = 0.9 / pixelSize;  // size of the confocal plane (assume 900 nm) in pixels
	noise = 10; // the amount of noise to be added to stack
	
	D = Array.fill(newArray(3),0);
	nParticles = Array.fill(newArray(3),0);
	
	// specify in a loop
	DloopVals = newArray(0.01,0.05,0.1,0.25,0.5,0.75,1.0,1.5,2.0,3.0)
	nPloopVals = newArray(1000,5000,7774,10000,16982,50000);
	outDir = getDirectory("Choose Destination Directory ");
	
	for (i = 0; i < DloopVals.length; i++) {
		D[0] = DloopVals[i];
		for (j = 0; j < nPloopVals.length; j ++) {
			nParticles[0] = nPloopVals[j];
			doTheSim(mode,imageDim,pixelSize,timeFrames,dT,particleSize,confocalplane,noise,D,nParticles,outDir);
		}
	}
}

function doTheSim(mode,imageDim,pixelSize,timeFrames,dT,particleSize,confocalplane,noise,D,nParticles,outDir) {
	if (mode == "3D") {
		titlestr = getValidFileName(outDir,"track_3D_",0,4);
	} else {
		titlestr = getValidFileName(outDir,"track_2D_",0,4);
	}
	resultFilename = outDir + titlestr + ".csv";
	imageFilename = outDir + titlestr + ".tif";
	infoFileName = outDir + titlestr + ".txt";
	f = File.open(resultFilename);
	if (mode == "3D") {
		print(f, "TrackID,x,y,z,frame");
	} else {
		print(f, "TrackID,x,y,frame");
	}
	
	setBatchMode(true);
	newImage("Tracks", "8-bit grayscale-mode black", imageDim, imageDim, 1,1, timeFrames);
	run("Properties...", "unit=um pixel_width="+pixelSize+" pixel_height="+pixelSize+" voxel_depth=1 frame=["+dT+" sec]");
	// Create arrays to hold the coordinates
	posX = newArray(timeFrames);
	posY = newArray(timeFrames);
	posZ = newArray(timeFrames);
	id = 0;
	
	for (k = 0; k < D.length; k ++) {
		
		for (i = 0; i < nParticles[k]; i ++){
			col = floor(128 + (random * 128));
			setForegroundColor(col, col, col);
			for (j = 0; j < timeFrames; j ++){
				if (j == 0){
					// Set starting coordinates
					posX[0] = floor(random * imageDim);
					posY[0] = floor(random * imageDim);
					posZ[0] = floor(random * imageDim);
				}
				else {
					posX[j] = posX[j-1] + (sqrt(2 * D[k] * dT) * random("gaussian")) / pixelSize;
					posY[j] = posY[j-1] + (sqrt(2 * D[k] * dT) * random("gaussian")) / pixelSize;
					posZ[j] = posZ[j-1] + (sqrt(2 * D[k] * dT) * random("gaussian")) / pixelSize;
				}
				if (mode == "3D") {
					if ((posX[j] >= 0 && posX[j] <= imageDim -1) &&
					(posY[j] >= 0 && posY[j] <= imageDim -1) &&
					(posZ[j] >= imageDim/2 - confocalplane/2 && posZ[j] <= imageDim/2 + confocalplane/2)) {
					setSlice(j + 1); // Select the correct frame
					// minus values for xstart and ystart are allowed
					makeOval(posX[j] - (particleSize / 2), posY[j] - (particleSize / 2), particleSize, particleSize);    // Create the selection
					run("Fill", "slice");                // Draw the point into the image (using foreground colour)
					run("Select None");
					}
				} else {
					if ((posX[j] >= 0 && posX[j] <= imageDim -1) &&
					(posY[j] >= 0 && posY[j] <= imageDim -1)) {
					setSlice(j + 1); // Select the correct frame
					// minus values for xstart and ystart are allowed
					makeOval(posX[j] - (particleSize / 2), posY[j] - (particleSize / 2), particleSize, particleSize);    // Create the selection
					run("Fill", "slice");
					run("Select None");
					}
				}
				// make a list of all positions, whether in frame or not (in pixels)
				// don't match msp-tracker, keep X and Y
				if (mode == "3D") {
					print(f, d2s(id,0) + "," + d2s(posX[j],16) + "," + d2s(posY[j],16) + "," + d2s(posZ[j],16) + "," + d2s(j,0));
				} else {
					print(f, d2s(id,0) + "," + d2s(posX[j],16) + "," + d2s(posY[j],16) + "," + d2s(j,0));
				}
			}
			id += 1;
		}
	}
	File.close(f);
	
	// save info
	f = File.open(infoFileName);
	dstr = "D1,p1,D2,p2,D3,p3";
	dvalstr = d2s(D[0],6) + "," + d2s(nParticles[0],0) + "," + d2s(D[1],6) + "," + d2s(nParticles[1],0) + "," + d2s(D[2],6) + "," + d2s(nParticles[2],0);
	print(f, "imageDim,pixelSize,timeFrames,dT,particleSize,confocalplane,noise," + dstr);
	print(f, imageDim + "," + pixelSize + "," + timeFrames + "," + dT + "," + particleSize + "," + confocalplane + "," + noise + "," + dvalstr);
	File.close(f);
	
	// add blur and noise according to parameters
	run("Gaussian Blur...", "sigma=" + particleSize/2 + " stack");
	run("Add Specified Noise...", "stack standard=" + noise);
	selectWindow("Tracks");
	save(imageFilename);
	close();
	setForegroundColor(255, 255, 255);
	setBatchMode(false);
}


function getValidFileName(dir, prefix, n, padlen) {
	i = 0;
	do {
		stem = prefix + IJ.pad(n+i, padlen);
		path = dir + stem + ".csv";
		i += 1;
	} while (File.exists(path) == true);
	
	return stem;
}
