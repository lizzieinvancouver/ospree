# AppEEARS Area Sample Extraction Readme  

## Table of Contents  

1. Request Parameters  
2. Request File Listing  
3. Area Sample Extraction Process  
4. Area Sample Naming Convention  
5. Data Quality  
    5.1. Moderate Resolution Imaging Spectroradiometer (MODIS)  
    5.2. NASA MEaSUREs Shuttle Radar Topography Mission (SRTM) Version 3 (v3)  
    5.3. Gridded Population of the World (GPW) Version 4 (v4)  
    5.5. Suomi National Polar-orbiting Partnership (S-NPP) NASA Visible Infrared Imaging Radiometer Suite (VIIRS)  
    5.5. Soil Moisture Active Passive (SMAP)  
    5.6. MODIS Simplified Surface Energy Balance (SSEBop) Actual Evapotranspiration (ETa)  
    5.7. eMODIS Smoothed Normalized Difference Vegetation Index (NDVI)  
    5.8. Daymet  
    5.9. U.S. Landsat Analysis Ready Data (ARD)  
    5.10. Ecosystem Spaceborne Thermal Radiometer Experiment on Space Station (ECOSTRESS)  
    5.11. Advanced Spaceborne Thermal Emission and Reflection Radiometer (ASTER) Global Digital Elevation Model (GDEM) Version 3 (v3) and Global Water Bodies Database (WBD) Version 1 (v1)  
    5.12. NASA MEaSUREs NASA Digital Elevation Model (DEM) Version 1 (v1)  
6. Data Caveats  
7. Documentation  
8. Sample Request Retention  
9. Data Product Citations  
10. Software Citation  
11. Feedback  

## 1. Request Parameters  

    Name: global_timeseries  

    Date Completed: 2020-10-09T16:17:22.913207  

    Id: 740e6e91-aa56-4e97-9694-40391e95ed27  

    Details:  

        Input Vector Name:            User-Drawn-Polygon
        Number of Vector Features:    1  

        Start Date:                   01-01-2001  

        End Date:                     12-31-2018  
        
        Layers:  

                Greenup (MCD12Q2.006)  
      
        Output Projection:            Geographic
        Datum:                        WGS84
    
        EPSG:                         4326  
    
        PROJ.4:                       "+proj=longlat +datum=WGS84 +no_defs"  
    
        Output Format:                geotiff  

    Version: This request was processed by AppEEARS version 2.46  

## 2. Request File Listing  

**Supporting Files:**  

- global-timeseries-MCD12Q2-006-metadata.xml
- global-timeseries-granule-list.txt
- global-timeseries-request.json
- MCD12Q2-006-QA-Detailed-0-lookup.csv
- MCD12Q2-006-QA-Detailed-1-lookup.csv
- MCD12Q2-006-QA-Overall-0-lookup.csv
- MCD12Q2-006-QA-Overall-1-lookup.csv
- MCD12Q2-006-QA-Detailed-0-Statistics-QA.csv
- MCD12Q2-006-QA-Detailed-1-Statistics-QA.csv
- MCD12Q2-006-QA-Overall-0-Statistics-QA.csv
- MCD12Q2-006-QA-Overall-1-Statistics-QA.csv
- MCD12Q2-006-Statistics.csv

**Data Files:**  

Number of Extracted Data Files: 108  
Total Size of Extracted Data Files: 10,206.01 MB  

## 3. Area Sample Extraction Process  

When an area sample extraction request is successfully submitted, AppEEARS
implements a series of tools and services to extract the exact data the user is
interested in; or rather, extracts data from specific data layers that
intersect with the input vector file's features. AppEEARS first uploads the
input vector file and reprojects it to the source projection of the data layer
of interest. The PROJ.4 definitions for each data collection available through
AppEEARS are listed below.  

### MODIS (TERRA, AQUA, & Combined)  

    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"

### SRTM v3 (30m & 90m)  

    "+proj=longlat +datum=WGS84 +no_defs"  

### MODIS Snow Products (TERRA & AQUA)  

    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"  

### GPW v4  

    "+proj=longlat +datum=WGS84 +no_defs"  

### NPP NASA VIIRS  

    "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs"  

### SMAP - Global  

    "+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"  

### SMAP - Northern Hemisphere  

    "+proj=laea +lat_0=90 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"  

### SSEBop Actual Evapotranspiration (ETa)  

    "+proj=longlat +datum=WGS84 +no_defs"  

### eMODIS Smoothed Normalized Difference Vegetation Index (NDVI)  

    "+proj=laea +lat_0=45 +lon_0=-100 +x_0=0 +y_0=0 +a=6370997 +b=6370997 +units=m +no_defs"  

### Daymet  

    "+proj=lcc +lat_0=42.5 +lat_1=25 +lat_2=60 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +units=km +no_defs"  

### U.S. Landsat ARD (CONUS)  

    "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"  

### U.S. Landsat ARD (Alaska)  

    "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"  

### U.S. Landsat ARD (Hawaii)  

    "+proj=aea +lat_1=8 +lat_2=18 +lat_0=3 +lon_0=-157 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"  

### ECOSTRESS (see data caveats section below)  

    "+proj=longlat +datum=WGS84 +no_defs"  

### ASTER GDEM v3 and Global Water Bodies Database v1  

    "+proj=longlat +datum=WGS84 +no_defs"  

### NASADEM v1 (30m)  

    "+proj=longlat +datum=WGS84 +no_defs"  

Following the reprojection of the vector file, a bounding box for each feature in the vector file is determined. Each corner point is first determined using the minimum and maximum latitude and longitude values. An additional 1 pixel buffer is applied to each corner point to create the feature bounding box. See below for details on how the bounding box is determined.  

#### Feature Bounding Box Calculation  

- `UpperLeft   =   (maxLat + cellSize), (minLon - cellSize)`  
- `LowerLeft   =   (minLat - cellSize), (minLon - cellSize)`  
- `UpperRight  =   (maxLat + cellSize), (maxLon + cellSize)`  
- `LowerRight  =   (minLat - cellSize), (maxLon + cellSize)`  

For each feature, a series of tools and services are used to determine which spatial tiles intersect with the coordinates of the feature bounding box for the data layer of interest. These tiles are extracted (from OPeNDAP) to a server-side work environment where they are mosaicked into an image. Tile extraction and processing is implemented for each requested composite period (e.g., daily, weekly, 8-day, 16-day, monthly, seasonal, or annual) to create a time series image stack. If the user chooses to have the output projection for each extracted data file match, then the image stack is reprojected into the user-requested projection using the PROJ.4 definition described above. The image stacks are finally clipped to the input feature shape to only maintain the data intersecting the feature shape. Data outside of the feature shape are converted to a product-specific NODATA value. Each clipped image in the time series image stack is saved as a CF-compliant NetCDF file or in a series of Geospatial Tagged Image File Format (GeoTIFF) files with a unique name following the naming conventions described in Section 4 of this README.

AppEEARS implements a strict procedure for reprojecting data outputs. Pixel size and resampling methods are non-customizable in AppEEARS. Reprojected data are produced using the Geospatial Data Abstraction Library (GDAL) gdalwarp function in combination with the PROJ.4 string definition for the user-defined output projection type. Reprojection is performed using nearest neighbor resampling. If the projection units are the same between the native and output projections, the native pixel size is used to reproject the image. If the projection units between the native and output projections are different (e.g. sinusoidal (m) to geographic (degrees), pixel size is calculated by reprojecting the center pixel of the original image, calculating its width and height, and then squaring all pixels. The latitude and longitude of the region of interest are maintained in the conversion.

**NOTE:**  

- Requested date ranges may not match the reference date for multi-day products. AppEEARS takes an inclusive approach when extracting data for sample requests, often returning data that extends beyond the requested date range. This approach ensures that the returned data includes records for the entire requested date range.  
- If selected, the SRTM v3, ASTER GDEM v3 and Global Water Bodies Database v1, and NASADEM v1 products will be extracted regardless of the time period specified in AppEEARS because they are static datasets. The date field in the data tables reflect the nominal date for each of these products.  
- If the visualizations indicate that there are no data to display, proceed to checking the .csv output file. Data products that have both categorical and continuous data values (e.g. MOD15A2H) are not able to be displayed in the visualizations within AppEEARS.  

## 4. Area Sample Naming Convention  

Output data files returned by AppEEARS have the following naming convention:  

`<ProductShortName>.<Version>_<LayerName>_doy<Year><JulianDate>_<AppEEARSFeatureID>.<FileFormat>`  

### Example output file name  

    MOD13Q1.006__250m_16_days_NDVI_doy2005193.aid0002.tif  

**where:**  

    <ProductShortName> .......... MOD13Q1  
    <Version> ................... 006  
    <LayerName> ................. _250m_16_days_NDVI  
    <Year> ...................... 2005  
    <JulianDate> ................ 193  
    <AppEEARSFeatureID> ......... aid0002  
    <FileFormat> ................ tif

The AppEEARS Feature ID is assigned automatically by the system.  

## 5. Data Quality  

When available, AppEEARS extracts and returns quality assurance (QA) data for each data file returned regardless of whether the user requests it. This is done to ensure that the user possesses the information needed to determine the usability and usefulness of the data they get from AppEEARS. Most data products available through AppEEARS have an associated QA data layer. Some products have more than one QA data layer to consult. See below for more information regarding data collections/products and their associated QA data layers.  

### 5.1. MODIS (Terra, Aqua, & Combined)  

All MODIS land products, as well as the MODIS Snow Cover Daily product, include quality assurance (QA) information designed to help users understand and make best use of the data that comprise each product.  

- See the MODIS Land Products QA Tutorials: <https://lpdaac.usgs.gov/resources/e-learning/> for more QA information regarding each MODIS land product suite.
- See the MODIS Snow Cover Daily product user guide for information regarding QA utilization and interpretation.

### 5.2. NASA MEaSUREs SRTM v3 (30m & 90m)  

SRTM v3 products are accompanied by an ancillary "NUM" file in place of the QA/QC files. The "NUM" files indicate the source of each SRTM pixel, as well as the number of input data scenes used to generate the SRTM v3 data for that pixel.  

- See the user guide: <https://lpdaac.usgs.gov/documents/179/SRTM_User_Guide_V3.pdf> for additional information regarding the SRTM "NUM" file.

### 5.3. GPW v4  

The GPW Population Count and Population Density data layers are accompanied by two Data Quality Indicators datasets. The Data Quality Indicators were created to provide context for the population count and density grids, and to provide explicit information on the spatial precision of the input boundary data. The data context grid (data-context1) explains pixels with "0" population estimate in the population count and density grids, based on information included in the census documents. The mean administrative unit area grid (mean-admin-area2) measures the mean input unit size in square kilometers. It provides a quantitative surface that indicates the size of the input unit(s) from which the population count and density grids were created.  

### 5.4. S-NPP NASA VIIRS  

All S-NPP NASA VIIRS land products include quality information designed to help users understand and make best use of the data that comprise each product. For product-specific information, see the link to the S-NPP VIIRS products table provided in section 6.  

**NOTE:**  

- The S-NPP NASA VIIRS Surface Reflectance data products VNP09A1 and VNP09H1 contain two quality layers: `SurfReflect_State` and `SurfReflect_QC`. Both quality layers are provided to the user with the request results. Due to changes implemented on August 21, 2017 for forward processed data, there are differences in values for the `SurfReflect_QC` layer in VNP09A1 and `SurfReflect_QC_500m` in VNP09H1.  
- Refer to the S-NPP NASA VIIRS Surface Reflectance User's Guide Version 1.1: <https://lpdaac.usgs.gov/documents/123/VNP09_User_Guide_V1.1.pdf> for information on how to decode the `SurfReflect_QC` quality layer for data processed before August 21, 2017. For data processed on or after August 21, 2017, refer to the S-NPP NASA VIIRS Surface Reflectance User's guide Version 1.6: <https://lpdaac.usgs.gov/documents/124/VNP09_User_Guide_V1.6.pdf>.  

### 5.5. SMAP  

SMAP products provide multiple means to assess quality. Each data product contains bit flags, uncertainty measures, and file-level metadata that provide quality information. Results downloaded from AppEEARS and/or data directly requested via middleware services contain not only the requested pixel/data values, but also the decoded bit flag information associated with each pixel/data value extracted. For additional information regarding the specific bit flags, uncertainty measures, and file-level metadata contained in this product, refer to the Quality Assessment section of the user guide for the specific SMAP data product in your request: <https://nsidc.org/data/smap/smap-data.html>.  

### 5.6. SSEBop Actual Evapotranspiration (ETa)  

The SSEBop evapotranspiration monthly product does not have associated quality indicators or data layers. The data are considered to satisfy the quality standards relative to the purpose for which the data were collected.  

### 5.7. eMODIS Smoothed Normalized Difference Vegetation Index (NDVI)  

The smoothed eMODIS NDVI product does not have associated quality indicators or data layers. The data are considered to satisfy the quality standards relative to the purpose for which the data were collected.  

### 5.8. Daymet  

Daymet station-level daily weather observation data and the corresponding Daymet model predicted data for three Daymet model parameters: minimum temperature (tmin), maximum temperature (tmax), and daily total precipitation (prcp) are available. These data provide information into the regional accuracy of the Daymet model for the three station-level input parameters. Corresponding comma separated value (.csv) files that contain metadata for every surface weather station for the variable-year combinations are also available. <https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1391>  

### 5.9. U.S. Landsat ARD  

Quality assessment bands for the U.S. Landsat ARD data products are produced from Level 1 inputs with additional calculations derived from higher-level processing. A pixel quality assessment band describing the general state of each pixel is supplied with each AppEEARS request. In addition to the pixel quality assessment band, Landsat ARD data products also have additional bands that can be used to evaluate the usability and usefulness of the data. These include bands that characterize radiometric saturation, as well as parameters specific to atmospheric correction. Refer to the U.S. Landsat ARD Data Format Control Book (DFCB): <https://www.usgs.gov/media/files/landsat-analysis-ready-data-ard-data-format-control-book-dfcb> for a full description of the quality assessment bands for each product (L4-L8) as well as guidance on interpreting each band’s bit-packed data values.  

### 5.10. ECOSTRESS  

Quality information varies by product for the ECOSTRESS product suite. Quality information for ECO2LSTE.001, including the bit definition index for the quality layer, is provided in section 2.4 of the User Guide: <https://lpdaac.usgs.gov/documents/423/ECO2_User_Guide_V1.pdf>. Results downloaded from AppEEARS contain the requested pixel/data values and also the decoded QA information associated with each pixel/data value extracted. No quality flags are produced for the ECO3ETPTJPL.001, ECO4WUE.001, or ECO4ESIPTJPL.001 products. Instead, the quality flags of the source data are available in the ECO3ANCQA.001 data product and a cloud mask is available in the ECO2CLD.001 product. The `ETinst` layer in the ECO3ETPTJPL.001 product does include an associated uncertainty layer that is provided with each request for ‘ETinst’ in AppEEARS. Each radiance layer in the ECO1BMAPRAD.001 product has a linked quality layer (Data Quality Indicators). ECO2CLD.001 and ECO3ANCQA.001 are separate quality products that are also available for download in AppEEARS.  

### 5.11. ASTER GDEM v3 and Global Water Bodies Database v1  

ASTER GDEM v3 data are accompanied by an ancillary "NUM" file in place of the QA/QC files. The "NUM" files refer to the count of ASTER Level-1A scenes that were processed for each pixel or the source of reference data used to replace anomalies. The ASTER Global Water Bodies Database v1 products do not contain QA/QC files.  

- See Section 7 of the ASTER GDEM user guide: <https://lpdaac.usgs.gov/documents/434/ASTGTM_User_Guide_V3.pdf> for additional information regarding the GDEM "NUM" file.  
- See Section 7 of the ASTER Global Water Bodies Database user guide: <https://lpdaac.usgs.gov/documents/436/ASTWBD_User_Guide_V1.pdf> for a comparison with the SRTM Water Body Dataset.

### 5.12 NASA MEaSUREs NASADEM v1 (30m)  

NASADEM v1 products are accompanied by an ancillary "NUM" file in place of the QA/QC files. The "NUM" files indicate the source of each NASADEM pixel, as well as the number of input data scenes used to generate the NASADEM v1 data for that pixel.  

- See the NASADEM user guide: <https://lpdaac.usgs.gov/documents/592/NASADEM_User_Guide_V1.pdf> for additional information regarding the NASADEM "NUM" file.  

## 6. Data Caveats  

### 6.1. SSEBop Actual Evapotranspiration (ETa)  

- A list of granule files is not provided for the SSEBop ETa data product. The source data for this product can be obtained by using the download interface at: <https://earlywarning.usgs.gov/fews/datadownloads/Continental%20Africa/Monthly%20ET%20Anomaly>  

### 6.2 eMODIS Smoothed Normalized Difference Vegetation Index (NDVI)  

- The raw data values within the smoothed eMODIS NDVI product represent scaled byte data with values between 0 and 200. To convert the scaled raw data to smoothed NDVI (smNDVI) data values, the user must apply the following conversion equation:  

      smNDVI = (0.01 * Raw_Data_Value) - 1  

- A list of granule files is not provided for the SSEBop ETa data product. The source data for this product can be obtained by using the download interface at: <https://phenology.cr.usgs.gov/get_data_smNDVI.php>  

### 6.3 ECOSTRESS

- ECOSTRESS data products are natively stored in swath format. To fulfill AppEEARS requests for ECOSTRESS products, the data are first resampled from the native swath format to a georeferenced output. This requires the use of the requested ECOSTRESS product files and the corresponding ECO1BGEO: <https://doi.org/10.5067/ECOSTRESS/ECO1BGEO.001> files for all products except for ECO1BMAPRAD.001. ECO1BMAPRAD.001 contains latitude and longitude arrays within each file that are then used in the resampling process.  
The conversion leverages the pyresample package’s: <https://pyresample.readthedocs.io/en/stable/> kd_tree algorithm: <https://pyresample.readthedocs.io/en/latest/swath.html#pyresample-kd-tree> using nearest neighbor resampling. The conversion resamples to a Geographic (lat/lon) coordinate reference system (EPSG: 4326), which is defined as the ‘native projection’ option for ECOSTRESS products in AppEEARS.  

## 7. Documentation  

The documentation for AppEEARS can be found at <https://lpdaacsvc.cr.usgs.gov/appeears/help>.  

Documentation for data products available through AppEEARS are listed below.  

### 7.1. MODIS Land Products(Terra, Aqua, & Combined)  

- <https://lpdaac.usgs.gov/product_search/?collections=Combined+MODIS&collections=Terra+MODIS&collections=Aqua+MODIS&view=list>  

### 7.2. MODIS Snow Products (Terra and Aqua)  

- <https://nsidc.org/data/modis/data_summaries>  

### 7.3. NASA MEaSUREs SRTM v3  

- <https://lpdaac.usgs.gov/product_search/?collections=MEaSUREs+SRTM&view=list>  

### 7.4. GPW v4  

- <http://sedac.ciesin.columbia.edu/binaries/web/sedac/collections/gpw-v4/gpw-v4-documentation.pdf>  

### 7.5. S-NPP NASA VIIRS Land Products  

- <https://lpdaac.usgs.gov/product_search/?collections=S-NPP+VIIRS&view=list>  

### 7.6. SMAP Products  

- <http://nsidc.org/data/smap/smap-data.html>  

### 7.7. SSEBop Actual Evapotranspiration (ETa)  

- <https://earlywarning.usgs.gov/fews/product/66#documentation>  

### 7.8. eMODIS Smoothed Normalized Difference Vegetation Index (NDVI)  

- <https://phenology.cr.usgs.gov/get_data_smNDVI.php>  

### 7.9. Daymet  

- <https://daac.ornl.gov/cgi-bin/dsviewer.pl?ds_id=1328>  
- <https://daymet.ornl.gov/>  

### 7.10. U.S. Landsat ARD  

- <https://www.usgs.gov/land-resources/nli/landsat/us-landsat-analysis-ready-data?qt-science_support_page_related_con=0#qt-science_support_page_related_con>  

### 7.11. ECOSTRESS  

- <https://lpdaac.usgs.gov/product_search/?collections=ECOSTRESS&view=list>  

### 7.12. ASTER GDEM v3 and Global Water Bodies Database v1  

- <https://doi.org/10.5067/ASTER/ASTGTM.003>  
- <https://doi.org/10.5067/ASTER/ASTWBD.001>  

### 7.13. NASADEM v1  

- <https://doi.org/10.5067/MEaSUREs/NASADEM/NASADEM_NC.001>  
- <https://doi.org/10.5067/MEaSUREs/NASADEM/NASADEM_NUMNC.001>

## 8. Sample Request Retention  

AppEEARS sample request outputs are available to download for a limited amount of time after completion. Please visit <https://lpdaacsvc.cr.usgs.gov/appeears/help?section=sample-retention> for details.  

## 9. Data Product Citations  

- Friedl, M., Gray, J., Sulla-Menashe, D. (2019). MCD12Q2 MODIS/Terra+Aqua Land Cover Dynamics Yearly L3 Global 500m SIN Grid V006. NASA EOSDIS Land Processes DAAC. Accessed 2020-10-09 from https://doi.org/10.5067/MODIS/MCD12Q2.006. Accessed October 9, 2020.

## 10. Software Citation  

AppEEARS Team. (2020). Application for Extracting and Exploring Analysis Ready Samples (AppEEARS). Ver. 2.46. NASA EOSDIS Land Processes Distributed Active Archive Center (LP DAAC), USGS/Earth Resources Observation and Science (EROS) Center, Sioux Falls, South Dakota, USA. Accessed October 9, 2020. https://lpdaacsvc.cr.usgs.gov/appeears

## 11. Feedback  

We value your opinion. Please help us identify what works, what doesn't, and anything we can do to make AppEEARS better by submitting your feedback at <https://lpdaacsvc.cr.usgs.gov/appeears/feedback> or to LP DAAC User Services at <https://lpdaac.usgs.gov/lpdaac-contact-us/>.  
