Seal Monitoring System - LRG Processing Pipeline Documentation
![2025-06-05_15-12-49](https://github.com/user-attachments/assets/03e74907-3c2a-4411-84af-8618d2531f14)

The LRG (Phoca largha and Phoca vitulina) processing pipeline is a specialized system designed to analyze drone imagery and 3D scans of seal populations in a marine environment. The system combines computer vision, deep learning, and 3D modeling techniques to detect, segment, and measure individual seals for ecological research and population monitoring. The primary objectives of the pipeline include automated seal detection in aerial imagery, 3D reconstruction of seal bodies from point cloud data, volume estimation, and geospatial mapping of seal distributions.

Core Components

1. Image Processing Module
The image processing module prepares training data and applies preprocessing techniques to enhance seal detection accuracy.
02_ERODE_MASK.r
This script performs morphological erosion on segmentation masks to refine training data. It processes batches of mask images stored in specified directories, applying erosion to remove noise and improve boundary definitions. The script supports bulk operations for efficiency, ensuring consistent mask quality across large datasets.

2. 3D Processing Module
The 3D processing module handles point cloud data to reconstruct and analyze seal morphology.
LRG_3D_CROP.r
The primary script for 3D data processing, this tool imports point clouds in .ply format (typically exported from Agisoft Metashape). It applies filtering to remove noise and outliers before generating watertight 3D meshes using the ball-pivoting algorithm. The script calculates seal body volumes from the reconstructed meshes and provides interactive visualization via the RGL library. Key functionalities include mesh smoothing, hole filling, and volume computation based on mesh geometry.

3. Deep Learning Prediction Module
This module includes scripts for seal detection and segmentation using deep learning models.
LRG_PREDICT.r
A regression-based model that estimates seal body measurements from segmented images. It uses pre-trained weights to predict biometric parameters such as length and girth.
LRG_Predict_tf2.r
A TensorFlow 2 implementation of a U-Net model for semantic segmentation of seals in drone imagery. The script processes images in batches (default batch size: 32) and includes a custom Dice coefficient metric for model evaluation. Post-processing steps include binary mask generation, blob detection, and filtering based on size thresholds to eliminate false positives.

4. Blob Processing Module
This module converts raw detection outputs into structured geospatial data.
LRG_BlobsToPolygons.r
Converts binary blob detections into polygon shapes (e.g., GeoJSON or KML) for GIS integration. The script applies contour detection to blobs, filters them by area, and exports vectorized outlines.
LRG_SplitPredByBlob.r
Splits prediction masks into individual seal detections based on connected components (blobs). It filters blobs by minimum/maximum pixel area to exclude noise and extracts cropped images of each seal for further analysis.

5. Auxiliary Scripts
Supporting tools for data preparation and visualization.
MaskCreateFromOutlines.r
Generates training masks from manually drawn outlines (e.g., KML or shapefile boundaries). Useful for creating labeled datasets when automatic segmentation fails.
Coordinate Conversion Utilities
Functions to transform between pixel coordinates, UTM Zone 10N (the default CRS for drone surveys), and other geographic systems.


Workflow Input Requirements
1.Drone Imagery: High-resolution orthophotos in JPEG format.
2. 3D Point Clouds: .ply files generated from photogrammetry software (e.g., Agisoft Metashape).
3. Geospatial Boundaries: KML files defining survey areas or regions of interest.
4. Metadata: UTM Zone 10N projection parameters, sensor calibration data.

Processing Pipeline Steps

1.Data Ingestion: Drone images and 3D scans are loaded into the system.
2.Seal Detection: U-Net model segments seals in imagery; blobs are converted to polygons.
3.3D Reconstruction: Point clouds are cropped to seal regions, meshed, and volumes computed.
4.Output Generation: Geospatial layers (KML/GeoJSON), volume reports, and cropped seal images are exported.

Technical Dependencies

1.Programming Languages: R (for 3D processing ans deep learning).
2.Libraries: TensorFlow 2, RGL, plyr, sf (for geospatial operations).
3.Hardware: GPU acceleration recommended for deep learning inference.

Notes

1.Volume calculations assume seals are in a relaxed, prone position.
2.For large datasets, batch processing can be distributed across multiple nodes.

Troubleshooting

1. Erosion artifacts in masks: Adjust kernel size in 02_ERODE_MASK.r.
2. Mesh holes in 3D models: Increase point density or use manual hole-filling.
3. False positives in detection: Retrain U-Net with additional negative samples.
