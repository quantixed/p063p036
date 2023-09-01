# R Code and data for recreation of plots

### Main Figures

- Relocalization kinetics `MS137`
- Nocodazole and Latrunculin B plots `MS099_100`
- FRAP fitting and plots `FRAP_Curves`
	- GFP, WT, R159E ± PEG `MS090`
	- WT ± nocodazole `MS077`
	- WT ± ATP depl `MS060` 
- Diffusion coefficient from FRAP `MS055_060_131`
- FLIP fitting and plots `MS060`
- Tracking StayGold-TPD54 INVs `MS124_126` (see below, use `MS124_126_single.R`)
- Comparison of 12 markers using TrackMate `TrackMateR`
	- requires XML files adding to `TrackMateR/Data/`
	- PCA contour similarity and hierarchical clustering is done in Igor using `PCASimilarShapes.ipf`
	- Example Tracksets produced with `plotDatasetTracks.R`, requires output from `compareDatasets()`.
	- Distibution of alpha values with `alphaHistograms.R`, requires output from `compareDatasets()`.
- Exocytosis, relocalization of INVs `MS110_111_114_125` 
- Effect of PEG on exocytosis `MS104_105`
- Diffusion coefficient from FRAP, control vs dongle `MS130_131`
- Calculated diffusion `MS130_131`
- Exocytosis, control vs dongle `MS126_130_131` and `SJR221`

### Supplementary Figures

- Evaluation of TrackMate using synthetic data `SJR194` (requires XML files adding to `SJR194/Data/`, these files are large and can be generated using the code in `../simulations`)
- Comparison of StayGold-TPD54 pre and post bleach `MS124_126` (requires XML files adding to `MS124_126/Data/`)
- PCA of all TrackMate data, outputs with `TrackMateR`, n table production.
- Intensity distributions are generated with `TrackMateR`, `processTrackMate.R`

TrackMate XML files are available as described in the main README.