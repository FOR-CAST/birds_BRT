---
title: "birds_BRT Manual"
subtitle: "v.2.0.0"
date: "Last updated: 2025-02-20"
output:
  bookdown::html_document2:
    toc: true
    toc_float: true
    theme: sandstone
    number_sections: false
    df_print: paged
    keep_md: yes
editor_options:
  chunk_output_type: console
  bibliography: citations/references_birds_BRT.bib
link-citations: true
always_allow_html: true
---

# birds_BRT Module

<!-- the following are text references used in captions for LaTeX compatibility -->
(ref:birds-BRT) *birds_BRT*



#### Authors:

Tati Micheletti <tati.micheletti@gmail.com> [aut], Diana Stralberg <dstralberg@gmail.com> [aut], Alex M Chubaty <achubaty@or-cast.ca> [aut, cre]
<!-- ideally separate authors with new lines, '\n' not working -->

## Module Overview

### Module summary

<!-- TODO: update description for ON and other study areas -->

The birds module for loads an existing glm object for each species located in  
<https://drive.google.com/open?id=1obSvU4ml8xa8WMQhQprd6heRrN47buvI>. 
The model uses dynamic (vegetation: biomass of tree species) and static layers (Non-veg: WAT = water (1/0), URBAG = urban/agriculture (1/0), lLED25 = lake edge density with 5x5 moving window (continuous), DEV25 = development proportion within 5x5 moving window (continuous), and landform (categorical).

North American landcover 2005 (MODIS) is
source for all but landform (from AdaptWest land facet datset).
Vector ruggedness (already available - 
<https://drive.google.com/open?id=1dgIw70mDpDYrBExA52SkPoS1TFaZdLE9>) and road density (that should be available from the Anthropogenic module) will be added to the next version of these models.
The birds' prediction is masked to uplands as we do not have data for lowlands.

### Module inputs and parameters

Table \@ref(tab:moduleInputs-birds-BRT) shows the full list of module inputs.

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleInputs-birds-BRT)(\#tab:moduleInputs-birds-BRT)List of (ref:birds-BRT) input objects and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
   <th style="text-align:left;"> sourceURL </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> usrEmail </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> User's e.mail to automatic authentication of GoogleDriveNA </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> waterRaster </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Wetland raster for excluding water from final bird layers. Water == 1 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> wetlandRaster </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Wetland raster for creating upland raster. wetlands == 1 </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> uplandsRaster </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Upland raster for excluding wetlands and water from bird's predictions. LandR has NOT been tested for wetlands. Uplands == 1. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> birdsList </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Bird species to be predicted </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cloudFolderID </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Folder ID for cloud caching </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> urlModels </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Url for the GDrive folder that has all models (used for flat structures) objects. Alternatively, it might be a `data.table` object with the following columns: `Species`, `folderID`, `modelUsed`, which will contain the individual species models inside each of the species folders (`folderID`). </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> urlStaticLayers </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Static Layers (WET, VRUG, WAT, URBAG, lLED25, DEV25 and landform) url </td>
   <td style="text-align:left;"> https://drive.google.com/drive/u/0/folders/1G1VIR3bHZ_HztDToJnMwALuZqZ2ef38Q </td>
  </tr>
  <tr>
   <td style="text-align:left;"> studyArea </td>
   <td style="text-align:left;"> SpatialPolygonDataFrame </td>
   <td style="text-align:left;"> Study area for the prediction. Currently only available for NWT </td>
   <td style="text-align:left;"> https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rasterToMatch </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> All spatial outputs will be reprojected and resampled to it </td>
   <td style="text-align:left;"> https://drive.google.com/open?id=1P4grDYDffVyVXvMjM-RwzpuH1deZuvL3 </td>
  </tr>
  <tr>
   <td style="text-align:left;"> forestOnly </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Raster to match but NA'ed for non-forest pixels </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateLayersBirds </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> List of raster stacks of climate variables for birds such as: AHM, bFFP, CMD, DD_0, DD_18, DD18, DD5, eFFP, EMT, EXT, FFP, MAP, MAT, MCMT, MSP, MWMT, NFFD, PAS, PPT_sm, PPT_wt, SHM, Tave_sm, Tave_wt, TD, </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateDataFolder </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Folder where to look for the climate data. If not provided, set as inputPath(sim). </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> zipClimateDataFilesFolder </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Folder where to look for the climate data '.zip' files if these have not been extracted. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquiv </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> table of species equivalencies. See LandR::sppEquivalencies_CA. </td>
   <td style="text-align:left;"> NA </td>
  </tr>
  <tr>
   <td style="text-align:left;"> sppEquivCol </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> The column in the specie Equivalency table to use as a naming convention </td>
   <td style="text-align:left;"> NA </td>
  </tr>
</tbody>
</table>

Provide a summary of user-visible parameters (Table \@ref(tab:moduleParams-birds-BRT))


<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleParams-birds-BRT)(\#tab:moduleParams-birds-BRT)List of (ref:birds-BRT) parameters and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> paramName </th>
   <th style="text-align:left;"> paramClass </th>
   <th style="text-align:left;"> default </th>
   <th style="text-align:left;"> min </th>
   <th style="text-align:left;"> max </th>
   <th style="text-align:left;"> paramDesc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> scenario </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Are these predictions from a specific scenario? If not, leave it as NA. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> rastersShowingNA </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should the raster present `NA` where wetlands are? This is because LandR doesn't predict for wetlands. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> predictLastYear </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should it schedule events for the last year of simulation if this is not a multiple of interval? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> useStaticPredictionsForNonForest </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> If `TRUE`, it will use the original kNN data to fill up the `NA`s back after if we don't want to leave `NA` pixels in the predictions, independently of having the `pixelGroupMap` being masked to uplands or not. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> useOnlyUplandsForPrediction </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should the bird layers be masked to forest uplands only? masks `pixelGroupMap` with uplands as quality of DUCKS layer is better than `rstLCC` to ID the wetlands. We currently have succession happening in some wetlands because of the low quality of LCC05. This should not be happening. But as the layer is proprietary, we can't use it in LandR. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> .useCache </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should this entire module be run with caching? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> version </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> 6a </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Number of the bird module version to be used </td>
  </tr>
  <tr>
   <td style="text-align:left;"> useParallel </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should bird prediction be parallelized? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> useTestSpeciesLayers </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> TRUE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Use testing layers if forest succesion is not available? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> predictionInterval </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 10 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Time between predictions </td>
  </tr>
  <tr>
   <td style="text-align:left;"> nCores </td>
   <td style="text-align:left;"> characte.... </td>
   <td style="text-align:left;"> auto </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> If parallelizing, how many cores to use? Use 'auto' (90% of available), or numeric. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> baseLayer </td>
   <td style="text-align:left;"> numeric </td>
   <td style="text-align:left;"> 2005 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Which layer should be used? LCC05 or LCC10? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> overwritePredictions </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should overwrite bird predictions thta might be available? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> lowMem </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should the bird predictions return the final rasters (FALSE) or path to these (TRUE). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> vegetationStatic </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should the bird predictions keep vegetation static through time? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateStatic </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Should the bird predictions keep climate layers static through time? </td>
  </tr>
  <tr>
   <td style="text-align:left;"> RCP </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> 85 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Which RCP should be used? Default to 85. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateModel </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> CCSM4 </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Which climate model should be used? Default to CCSM4. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> ensemble </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Which ensemble model should be used? Default to ''. CCSM4 doesn't have ensemble, just CanESM2 (r11i1p1). </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateResolution </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;">  </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> Which DEM resolution was used for generating the climate layers? Default to '3ArcMin'. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> climateFilePath </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> https://.... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> URL to zipped climate file coming from ClimateNA, containing all climate variables for all years of simulation. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> staticLayersNames </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> dev750, .... </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> This is the vector of layer names to indicate which ones are static. Defaults to the ones used in models from BAM. </td>
  </tr>
  <tr>
   <td style="text-align:left;"> onlyLoadModels </td>
   <td style="text-align:left;"> logical </td>
   <td style="text-align:left;"> FALSE </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> NA </td>
   <td style="text-align:left;"> If set to TRUE, the module will only download the models that can be used. </td>
  </tr>
</tbody>
</table>

### Events

<!-- TODO: describe module events -->

Describe what happens for each event type.

### Plotting

<!-- TODO: describe plot outputs -->

Write what is plotted.

### Saving

<!-- TODO: describe file outputs -->

Write what is saved.

### Module outputs

Description of the module outputs (Table \@ref(tab:moduleOutputs-birds-BRT)).

<table class="table" style="color: black; margin-left: auto; margin-right: auto;">
<caption>(\#tab:moduleOutputs-birds-BRT)(\#tab:moduleOutputs-birds-BRT)List of (ref:birds-BRT) outputs and their description.</caption>
 <thead>
  <tr>
   <th style="text-align:left;"> objectName </th>
   <th style="text-align:left;"> objectClass </th>
   <th style="text-align:left;"> desc </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> birdPrediction </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> List per year of the bird species predicted rasters </td>
  </tr>
  <tr>
   <td style="text-align:left;"> birdModels </td>
   <td style="text-align:left;"> list </td>
   <td style="text-align:left;"> List of the bird models for prediction </td>
  </tr>
  <tr>
   <td style="text-align:left;"> staticLayers </td>
   <td style="text-align:left;"> RasterStack </td>
   <td style="text-align:left;"> Raster stack of all static layers (WAT, URBAG, lLED25, DEV25 and landform) for the bird models </td>
  </tr>
  <tr>
   <td style="text-align:left;"> successionLayers </td>
   <td style="text-align:left;"> RasterStack </td>
   <td style="text-align:left;"> Raster stack of all succession layers (species) and total biomass for the bird models </td>
  </tr>
  <tr>
   <td style="text-align:left;"> unavailableModels </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Character vector with all missing models </td>
  </tr>
  <tr>
   <td style="text-align:left;"> biomassMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Total biomass map </td>
  </tr>
  <tr>
   <td style="text-align:left;"> cohortData </td>
   <td style="text-align:left;"> data.table </td>
   <td style="text-align:left;"> Table with cohort information (biomass per species per pixelGroup) </td>
  </tr>
  <tr>
   <td style="text-align:left;"> pixelGroupMap </td>
   <td style="text-align:left;"> RasterLayer </td>
   <td style="text-align:left;"> Mapping raster to pixelGroup </td>
  </tr>
  <tr>
   <td style="text-align:left;"> allVariables </td>
   <td style="text-align:left;"> character </td>
   <td style="text-align:left;"> Vector of all variables that compose all models for each of the species. </td>
  </tr>
</tbody>
</table>

### Links to other modules

Describe any anticipated linkages to other modules, such as modules that supply input data or do post-hoc analysis.

### Getting help

GitHub repository: <https://github.com/FOR-CAST/birds_BRT>
