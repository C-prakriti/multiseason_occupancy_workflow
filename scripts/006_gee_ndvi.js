// Load MODIS NDVI dataset
var ndviCollection = ee.ImageCollection("MODIS/061/MOD13Q1"); 
print(ndviCollection);

// Import your study area
var Grid = ee.FeatureCollection("projects/occupancy-2026/assets/study_area");

// Filter images by year and sampling time period
var ndvi_2017 = ndviCollection
.filterBounds(Grid)
.filterDate('2017-01-01', '2017-02-28')
.select('NDVI');
print(ndvi_2017);

var ndvi_2018 = ndviCollection
.filterBounds(Grid)
.filterDate('2018-01-01', '2018-03-31')
.select('NDVI');

var ndvi_2021 = ndviCollection
.filterBounds(Grid)
.filterDate('2021-01-01', '2021-03-31')
.select('NDVI');

print('NDVI images count:', ndvi_2017.size());

// Generate NDVI composite raster
var ndvi_mean_2017 = ndvi_2017.mean();
var ndvi_mean_2018 = ndvi_2018.mean();
var ndvi_mean_2021 = ndvi_2021.mean();

// Scale NDVI values by a factor of 0.0001
var ndvi_scaled_2017 = ndvi_mean_2017.multiply(0.0001);
var ndvi_scaled_2018 = ndvi_mean_2018.multiply(0.0001);
var ndvi_scaled_2021 = ndvi_mean_2021.multiply(0.0001);

// Import your sample grids
var points = ee.FeatureCollection("projects/occupancy-2026/assets/sample_sites");

// Extract NDVI values to sites
var extracted = ndvi_scaled_2017.reduceRegions({
  collection: points,
  reducer: ee.Reducer.mean(),
  scale: 250,
});

var extracted = ndvi_scaled_2018.reduceRegions({
  collection: points,
  reducer: ee.Reducer.mean(),
  scale: 250,
});

var extracted = ndvi_scaled_2021.reduceRegions({
  collection: points,
  reducer: ee.Reducer.mean(),
  scale: 250,
});

// Export the csv file into drive
Export.table.toDrive({
  collection: extracted,
  description: 'NDVI_2017',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection: extracted,
  description: 'NDVI_2018',
  fileFormat: 'CSV'
});

Export.table.toDrive({
  collection: extracted,
  description: 'NDVI_2021',
  fileFormat: 'CSV'
});

// Create an NDVI map for 2021
Map.centerObject(Grid, 8);
var visParams = {
  min:0,
  max:1,
  palette: ['brown', 'yellow', 'green', 'darkgreen']
};
Map.addLayer(ndvi_scaled_2021, visParams, 'NDVI 2021')

Export.image.toDrive({
  image: ndvi_scaled_2021,
  description: 'NDVI_2021',
  region: Grid,
  scale: 250,
  crs: 'EPSG:4326',
  maxPixels: 1e13
});