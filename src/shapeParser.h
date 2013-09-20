/****************************************************************************** 
**  File:        shapeParser.h  
**
**  Purpose:     This file contains all the C structs used for parsing
**               .shp files and .dbf files.
**  Programmers: Christian Platt, Tom Kincaid
**  Created:     September 15, 2004
**  Revised:     March 22, 2006
******************************************************************************/

#ifndef SHAPE_PARSER_H
#define SHAPE_PARSER_H

/* these number correspond to the specified shapefile types defined in the */
/* ESRI white paper */
#define POINTS   1
#define POLYLINE 3
#define POLYGON  5
#define POINTS_Z   11
#define POLYLINE_Z 13
#define POLYGON_Z  15
#define POINTS_M   21
#define POLYLINE_M 23
#define POLYGON_M  25

/* struct for storing a Point */
typedef struct pointStruct Point;
struct pointStruct {
  double X;
  double Y;
};

/* struct for storing a PointZ */
typedef struct pointStructZ PointZ;
struct pointStructZ {
  double X;
  double Y;
  double Z;
  double M;
};

/* struct for storing a PointM */
typedef struct pointStructM PointM;
struct pointStructM {
  double X;
  double Y;
  double M;
};

/* struct for storing a Polygon or a Polyline */
typedef struct polyStruct Polygon;
struct polyStruct {
  double box[4];
  int numParts;
  int numPoints;
  int * ringDirs;     /* 1 for clockwies, -1 for counter clockwise, */
                      /* one value for each part */  
  int * parts;
  Point * points;
};

/* struct for storing a PolygonZ or a PolylineZ */
typedef struct polyStructZ PolygonZ;
struct polyStructZ {
  double box[4];
  int numParts;
  int numPoints;
  int * ringDirs;     /* 1 for clockwies, -1 for counter clockwise, */
                      /* one value for each part */  
  int * parts;
  Point * points;
  double zRange[2];
  double * zArray;
  double mRange[2];
  double * mArray;
};

/* struct for storing a PolygonM or a PolylineM */
typedef struct polyStructM PolygonM;
struct polyStructM {
  double box[4];
  int numParts;
  int numPoints;
  int * ringDirs;     /* 1 for clockwies, -1 for counter clockwise */
                      /* one value for each part */  
  int * parts;
  Point * points;
  double mRange[2];
  double * mArray;
};

/* struct used to represent a record and all it's information */
/* this record could either be for a point, polygon, or polyline */
typedef struct record Record;
struct record {
  /* record header info */
  int number;
  int contentLength;
  int shapeType;

  /* record content */
  Polygon * poly;
  PolygonZ * polyZ;
  PolygonM * polyM;
  Point * point;
  PointZ * pointZ;
  PointM * pointM;

  Record * next;
};

/* struct used to store all information and data for a shape file */
typedef struct shape Shape;
struct shape {
  /* header info */
  int fileCode;
  unsigned int fileLength;
  int fileVersion;
  int shapeType;

  /* bounding box */
  double Xmin;
  double Ymin;
  double Xmax;
  double Ymax;
  double Zmin;
  double Zmax;
  double Mmin;
  double Mmax;

  /* linked list of records */
  unsigned int numRecords;
  Record * records; 

  /* total number of parts found in the shape file */
  /* pretty much only used for polygons */
  unsigned int numParts;
};

/* struct used to store info regarding a dbf field, including an array of */
/* strings representing all the data values for this field */
typedef struct field Field;
struct field {
  char name[12];
  char type;
  unsigned int length;
  char ** data;
};

/* struct used to store info regarding a dbf file */
typedef struct dbf Dbf;
struct dbf {
  /* main file header info */
  int version;
  int year;
  int month;
  int day;
  unsigned int numRecords;
  unsigned int headerLength;
  unsigned int recordLength; 

  /* dynamic array of fields found in file */
  int numFields;
  Field * fields;

  Dbf * next;
};

/* struct for storing a line segment node (for the linked list of segments) */
typedef struct segmentStruct Segment;
struct segmentStruct {
  Point p1;
  Point p2;
  int recordNumber;
  double length;
  Segment * next;
};

#endif
