# p063p036

Data and code for manuscript p063p036

## plots

R code for plot recreation.
For details, see the README in `plots/`

## simulations

### Modelling particle diffusion

For FLIP simulation, `DiffusionSim.ipf` is written for IGOR Pro.

<details>
	<summary>A GUI is available to guide simulations. Click for details</summary>
	
<img src="img/VesiclePanel.png" width="300">

- Most variables are self-explanatory, please note the units.
- *FLIP mode* selects between a circle (used in the paper), square or a ring.
- *Random start location* when checked will start vesicles at random locations throughout the cell. When unchecked, vesicles will start at the cell centre.
- Specify bleach location for circle, square and ring. FlipRingR stipulates the radius of the ring (intended to be used without Random start location checked) so that vesicles diffuse away from the origin before entering the ring. For square specify the left-top (LT) and right-bottom (RB) XY locations relative to the cell centre `(0,0)`. For circle, an offset can be specified as well as a radius.
- *Simulate* will show the movement of vesicles without FLIP
- *FLIP* will do a single FLIP simulation
- *FLIP Rep* will do repeated FLIP simulations
- *Visualise* if checked will show the tracks when FLIP Rep is clicked.

The bottom part of the panel allows the simulations to be automated by taking a variable number of steps from a start and stop point for FLIP circle radius and for the diffusion coefficient (as described in the paper. Click *Automate* to start after setting all parameters.


</details>

### Simulating particle diffusion and benchmarking TrackMate

Particle movies were generated using `Particle_Simulation.ijm` in FIJI.
TrackMate automation is via the groovy script `TrackMateAutomation.groovy`.
The ground truth outputs can be evaluated in IGOR using `VesicleTracks.ipf`.
Although not documented in the paper, this program will also display movies and tracking data in a neat widget for visualization.
It will also select the most mobile of tracks using a few different algorithms.
The TrackMate XML outputs can be analyzed using `TrackMateR`.
R code to evaluate TrackMate XML files from these routines is available in `plots/SJR194/`

Some IGOR procedure files require `PXPUtils.ipf` to run properly, available [here](https://github.com/quantixed/PXPUtils)