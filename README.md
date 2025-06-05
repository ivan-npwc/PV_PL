Here's a comprehensive documentation for the seal monitoring system with a focus on the LRG (Larga Seal) processing pipeline:

Seal Monitoring System - LRG Processing Pipeline Documentation
System Overview
This specialized pipeline processes drone imagery and 3D scans of Larga seal populations to:

Detect and segment individual seals using deep learning

Create 3D models from point cloud data

Calculate seal body volumes

Generate geospatial outputs for population analysis

Core Components
Image Processing

02_ERODE MASK.r: Preprocessing for training masks

Applies morphological operations (erosion) to segmentation masks

Processes mask images in bulk from specified directories

3D Processing

LRG_3d_CROP.r: Main 3D processing script

Imports and processes 3D point clouds (.ply)

Creates watertight 3D meshes using ball pivoting algorithm

Calculates seal body volumes

Visualizes results with rgl

Deep Learning Prediction

LRG_PREDICT.r: Regression model for seal measurements

LRG_Predict_tf2.r: UNet-based segmentation

Batch processing of images (default 32)

Custom dice coefficient metric

Mask creation and blob analysis

Blob Processing

LRG_BlobsToPolygons.r: Converts blob detections to polygons

LRG_SplitPredByBlob.r: Splits predictions by individual blobs

Filters by size thresholds

Extracts individual seal images

Auxiliary Scripts

MaskCreateFromOutlines.r: Creates training masks from manual outlines

Various utility functions for coordinate conversion and visualization

Workflow
Input Requirements

Drone orthophotos

3D point clouds (.ply format) from Agisoft Metashape

KML polygon files for seal boundaries

Coordinate reference parameters (UTM Zone 10N)
