#@ File (style = "directory", label = "Input folder") inputFolder
#@ File (style = "directory", label = "Output folder") outputFolder
#@ double (label = "Spot radius", value = 0.15, stepSize = 0.01) radius
#@ double (label = "Quality threshold", value = 1.5, stepSize = 0.1) threshold
#@ int (label = "Max frame gap", value = 1) frameGap


import ij.IJ
import fiji.plugin.trackmate.Model
import fiji.plugin.trackmate.Settings
import fiji.plugin.trackmate.TrackMate
//import fiji.plugin.trackmate.Logger

import fiji.plugin.trackmate.detection.DogDetectorFactory

import fiji.plugin.trackmate.tracking.LAPUtils
import fiji.plugin.trackmate.tracking.sparselap.SimpleSparseLAPTrackerFactory

import fiji.plugin.trackmate.io.TmXmlWriter

def main() {
	inputFolder.eachFileRecurse {
		name = it.getName()
		if (name.endsWith(".tif")) {
			process(it, inputFolder, outputFolder)
		}
	}
}

def process(file, src, dst) {
	println "Processing $file"

	// Opening the image
	imp = IJ.openImage(file.getAbsolutePath())
	fname = file.getName()
	fname = fname.replaceAll("track_3D_","")
	fname = fname.replaceAll(".tif","")
	num = fname.toDouble()
	dlist = [0.01,0.05,0.1,0.25,0.5,0.75,1.0,1.5,2.0,3.0]
	linkingMax = Math.sqrt(dlist[Math.floor(num / 6)]) * 0.66
	closingMax = Math.sqrt(dlist[Math.floor(num / 6)]) * 0.99
	
	// Swap Z and T dimensions if T=1
	dims = imp.getDimensions() // default order: XYCZT
	if (dims[4] == 1) {
		imp.setDimensions( dims[2,4,3] )
	}
	
	// Setup settings for TrackMate
	settings = new Settings(imp)
	
	settings.detectorFactory = new DogDetectorFactory()
	settings.detectorSettings = settings.detectorFactory.getDefaultSettings()
	settings.detectorSettings['RADIUS'] = radius
	settings.detectorSettings['THRESHOLD'] = threshold
	settings.detectorSettings['DO_SUBPIXEL_LOCALIZATION'] = true
	settings.detectorSettings['DO_MEDIAN_FILTERING'] = true
	println settings.detectorSettings
	
	settings.trackerFactory = new SimpleSparseLAPTrackerFactory()
	settings.trackerSettings = LAPUtils.getDefaultLAPSettingsMap()
	settings.trackerSettings['MAX_FRAME_GAP']  = frameGap
	settings.trackerSettings['LINKING_MAX_DISTANCE']  = linkingMax
	settings.trackerSettings['GAP_CLOSING_MAX_DISTANCE']  = closingMax
	println settings.trackerSettings
	
	settings.addAllAnalyzers()
	
	// Run TrackMate and store data into Model
	model = new Model()
//	model.setLogger(Logger.IJ_LOGGER)
	model.setPhysicalUnits("µm","s")
	trackmate = new TrackMate(model, settings)
	
	println trackmate.checkInput()
	println trackmate.process()
	println trackmate.getErrorMessage()
	
	println model.getSpots().getNSpots(true)
	println model.getTrackModel().nTracks(true)
	
	// Save tracks as XML
	
	filename = file.getName()
	if (!filename.endsWith(".xml")) {
		filename += ".xml"
	}
	outFile = new File(outputFolder, filename)
//	ExportTracksToXML.export(model, settings, outFile)
	writer = new TmXmlWriter(outFile) //a File path object
//	writer.appendLog(logPanel.getTextContent())
	writer.appendModel(model) //trackmate instantiate like this before trackmate = TrackMate(model, settings)
	writer.appendSettings(settings)
	writer.writeToFile()

	// Clean up
	imp.close()
}

main()
