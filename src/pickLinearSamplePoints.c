/****************************************************************************** 
**  Function:    pickLinearSamplePoints
**  Programmer:  Tom Kincaid
**  Date:        January 23, 2011
**  Revised:     February 23, 2015
**  Revised:     May 5, 2015
**  Description:
**    For each value in the set of shapefile record IDs, select a sample point
**    from the shapefile record 
**  Arguments:
**    fileNamePrefix = the shapefile name
**    shpIDsVec = vector of shapefile record IDs to use in the calculations
**    recordIDsVec = vector of the shapefile record ID for each sample point
**    xcsVec = vector of grid cell x-coordinates
**    ycsVec = vector of grid cell y-coordinates
**    dxVal = x-axis size of the grid cells
**    dyVal = y-axis size of the grid cells
**  Results
**    An R list object of named results that contains the following items:
**    xcs = x-coordinates of sample points
**    ycs = y-coordinates of sample points
******************************************************************************/


#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include "shapeParser.h"
#include "grts.h"

#define LEFT   1
#define RIGHT  2
#define BOTTOM 3
#define TOP    4

/* This function is found in shapeParser.c */
extern int parseHeader(FILE * fptr, Shape * shape);
extern int readLittleEndian(unsigned char * buffer, int length);
extern int readBigEndian(unsigned char * buffer, int length);

/* These functions are found in grts.c */
extern int combineShpFiles(FILE * newShp, unsigned int * ids, int numIDs);
extern int createNewTempShpFile(FILE * newShp, char * shapeFileName,
                                unsigned int * ids, int numIDs);

/* These functions are found in grtslin.c */
extern void addSegment( Segment ** head, Segment * seg );
extern void deallocateSegments( Segment * head );
extern double lineLength(double x1, double y1, double x2, double y2, Cell * cell, 
                         Segment ** newSeg);


SEXP pickLinearSamplePoints(SEXP fileNamePrefix, SEXP shpIDsVec,
     SEXP recordIDsVec, SEXP xcVec, SEXP ycVec, SEXP dxVal, SEXP dyVal) {

  int i, k;                    /* loop counters */
  FILE * fptr = NULL;         /* pointer to the shapefile */
  FILE * newShp = NULL;       /* pointer to the temporary .shp file */
  char * restrict shpFileName = NULL;  /* stores full shape file name */
  int singleFile = FALSE;
  Shape shape;           /* used to store shapefile info and data */
  Record * temp = NULL;  /* used for traversing linked list of records */
  int partIndx;          /* index into polyline parts array */
  unsigned int filePosition = 100;  /* byte offset for the beginning of the */
                                    /* record data */
  unsigned char buffer[4];  /* temp buffer for reading from file */
  Polygon * poly;        /* temp Polygon storage */
  PolygonZ * polyZ;      /* temp PolygonZ storage */
  PolygonM * polyM;      /* temp PolygonM storage */
  Record record;         /* record used for parsing records */
  Cell cell;             /* temporary storage for a cell */
  Segment * seg;         /* variable for storing a segment struct */
  Segment * segmentList = NULL;  /* variable for storing a linked list of segment structs */
  Segment * tempSeg;     /* variables for traversing the list of segment structs */
  unsigned int * shpIDs = NULL;     /* array of shapefile record IDs to use */
  int dsgSize = length(shpIDsVec);  /* number of values in the shpIDs array */
  unsigned int sampleSize = length(xcVec); /* sample size */
  unsigned int * recordIDs = NULL;  /* array of shapefile record IDs that get a sample point */
  double * xc = NULL;    /* array that stores values found in xcVec R vector */
  double * yc = NULL;    /* array that stores values found in ycVec R vector */
  double dx;             /* x-axis size of the grid cells */
  double dy;             /* y-axis size of the grid cells */
  SEXP xcsVec;           /* return vector of record IDs */
  SEXP ycsVec;          /* return vector of record clipped areas */
  double tempLength;     /* stores current shapefile record clipped length */
  double sumWl;          /* sum of the segment lengths in a cell */
  double pos;            /* position along the line of all segment lengths in a cell */
  double cumSum;         /* cumulative sum of lengths up to selected point */
  double len;            /* variable for determining coordinates of the sample point */
  double dx2, dy2;       /* variables for determining coordinates of the sample point */
  double lx, ly;         /* variables for determining coordinates of the sample point */
  double * xcs = NULL;   /* array of sample x-coordinates */
  double * ycs = NULL;   /* array array of sample x-coordinates */
  SEXP results = NULL;   /* R object used to return values to R */
  SEXP colNamesVec;      /* vector used to name the columns in the results object */

  /* input the RNG state */
  GetRNGstate();

  /* see if a specific file was sent */
  if(fileNamePrefix != R_NilValue) {

    /* create the full .shp file name */
    if((shpFileName = (char * restrict)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
                                              + strlen(".shp") + 1)) == NULL){
      Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
      PROTECT(results = allocVector(VECSXP, 1));
      UNPROTECT(1);
      return results;
    }
    strcpy(shpFileName, CHAR(STRING_ELT(fileNamePrefix,0)));
    strcat(shpFileName, ".shp");
    singleFile = TRUE;
  }

  /* create the new temporary .shp file */
  if((newShp = fopen(TEMP_SHP_FILE, "wb")) == NULL) {
    Rprintf("Error: Creating temporary .shp file %s in C function pickLinearSamplePoints.\n", TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* copy the shapefile record IDs from the R vector to a C array */
  if((shpIDs = (unsigned int *) malloc(sizeof(unsigned int) * dsgSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    fclose(newShp);
    remove(TEMP_SHP_FILE);
    return results;
  }
  for(i = 0; i < dsgSize; ++i) {
    shpIDs[i] = INTEGER(shpIDsVec)[i];
  }

  if(singleFile == FALSE) {

    /* create a temporary .shp file containing all the .shp files */
    if(combineShpFiles(newShp, shpIDs, dsgSize) == -1) {
      PROTECT(results = allocVector(VECSXP, 1));
      UNPROTECT(1);
      fclose(newShp);
      remove(TEMP_SHP_FILE);
      Rprintf("Error: Combining multiple shapefiles in C function pickLinearSamplePoints.\n");
      return results; 
    }
    fclose(newShp);

  } else {

    /* create a temporary .shp file containing the sent .shp file */
    if(createNewTempShpFile(newShp, shpFileName, shpIDs, dsgSize) == -1) {
      PROTECT(results = allocVector(VECSXP, 1));
      UNPROTECT(1);
      fclose(newShp);
      remove(TEMP_SHP_FILE);
      Rprintf("Error: Creating temporary shapefile in C function pickLinearSamplePoints.\n");
      return results; 
    }
    fclose(newShp);
  }

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;

  /* open the temporary .shp file */
  if((fptr = fopen(TEMP_SHP_FILE, "rb")) == NULL) {
    Rprintf("Error: Opening shape file in C function pickLinearSamplePoints.\n");
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* parse main file header */
  if(parseHeader(fptr, &shape) == -1) {
    Rprintf("Error: Reading main file header in C function pickLinearSamplePoints.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* copy record ID values, record ID index values, and coordinates of the */
  /* cells from the R vectors to C arrays */
  if((recordIDs = (unsigned int *) malloc(sizeof(unsigned int) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((xc = (double *) malloc(sizeof(double) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((yc = (double *) malloc(sizeof(double) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  for(i = 0; i < sampleSize; ++i) {
    recordIDs[i] = INTEGER(recordIDsVec)[i];
    xc[i] = REAL(xcVec)[i];
    yc[i] = REAL(ycVec)[i];
  }

  /* copy x-axis and y-axis size of the grid cells from R values to C values */
  dx = REAL(dxVal)[0];
  dy = REAL(dyVal)[0];

  /* allocate memory and initialize the sample point coordinate arrays */
  if((xcs = (double *) malloc(sizeof(double) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((ycs = (double *) malloc(sizeof(double) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  for(i = 0; i < sampleSize; ++i) {
    xcs[i] = 0.0;
    ycs[i] = 0.0;
  }

  /* select sample points */  
  while (filePosition < shape.fileLength*2) {
    /* read the record number */
    fread(buffer, sizeof(char), 4, fptr);
    record.number = readBigEndian(buffer, 4); 
    filePosition += 4;

    /* ignore content length */
    fread(buffer, sizeof(char), 4, fptr);
    filePosition += 4;

    /* ignore shape type */
    fread(buffer, sizeof(char), 4, fptr);
    filePosition += 4;

    if(shape.shapeType == POLYLINE) {

      /* allocate a new polygon */
      if((poly = (Polygon *) malloc(sizeof(Polygon))) == NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }

      /* read box data */
      for(i = 0; i < 4; ++i) {
        fread(&(poly->box[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      }

      /* read the number of parts */
      fread(buffer, sizeof(char), 4, fptr);
      poly->numParts = readLittleEndian(buffer, 4);
      filePosition += 4;

      /* read the number of points */
      fread(buffer, sizeof(char), 4, fptr);
      poly->numPoints = readLittleEndian(buffer, 4);
      filePosition += 4;

      /* read parts info */ 
      if((poly->parts = (int *) malloc(sizeof(int) * poly->numParts))==NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < poly->numParts; ++i) {
        fread(&(poly->parts[i]), sizeof(char), 4, fptr);
        filePosition += 4;
      } 

      /* read points data */
      if((poly->points = (Point *) malloc(sizeof(Point) * poly->numPoints))
         == NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < poly->numPoints; ++i) {
        fread(&(poly->points[i].X), sizeof(double), 1, fptr);
        filePosition += 8;
        if(fread(&(poly->points[i].Y), sizeof(double), 1, fptr) == 0) {
          Rprintf("Error: reading shape file in C function pickLinearSamplePoints.\n");
          fclose(fptr);
          remove(TEMP_SHP_FILE);
          PROTECT(results = allocVector(VECSXP, 1));
          UNPROTECT(1); 
          return results;  
        }
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.poly = poly;

    } else if(shape.shapeType == POLYLINE_Z) {

      /* allocate a new polygon */
      if((polyZ = (PolygonZ *) malloc(sizeof(PolygonZ))) == NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }

      /* read box data */
      for(i = 0; i < 4; ++i) {
        fread(&(polyZ->box[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      }

      /* read the number of parts */
      fread(buffer, sizeof(char), 4, fptr);
      polyZ->numParts = readLittleEndian(buffer, 4);
      filePosition += 4;

      /* read the number of points */
      fread(buffer, sizeof(char), 4, fptr);
      polyZ->numPoints = readLittleEndian(buffer, 4);
      filePosition += 4;

      /* read parts info */ 
      if((polyZ->parts = (int *) malloc(sizeof(int) * polyZ->numParts))
      	== NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < polyZ->numParts; ++i) {
        fread(&(polyZ->parts[i]), sizeof(char), 4, fptr);
        filePosition += 4;
      } 

      /* read points data */
      if((polyZ->points = (Point *) malloc(sizeof(Point) * polyZ->numPoints))
         == NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < polyZ->numPoints; ++i) {
        fread(&(polyZ->points[i].X), sizeof(double), 1, fptr);
        filePosition += 8;
        if(fread(&(polyZ->points[i].Y), sizeof(double), 1, fptr) == 0) {
          Rprintf("Error: reading shape file in C function pickLinearSamplePoints.\n");
          fclose(fptr);
          remove(TEMP_SHP_FILE);
          PROTECT(results = allocVector(VECSXP, 1));
          UNPROTECT(1); 
          return results;  
        }
        filePosition += 8;
      } 

      /* read Z range */
      for(i = 0; i < 2; ++i) {
        fread(&(polyZ->zRange[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      }

      /* read Z values */
      if((polyZ->zArray = (double *) malloc(sizeof(double)*polyZ->numPoints))
         == NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < polyZ->numPoints; ++i) {
        fread(&(polyZ->zArray[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      } 

      /* read M range */
      for(i = 0; i < 2; ++i) {
        fread(&(polyZ->mRange[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      }

      /* read M values */
      if((polyZ->mArray = (double *) malloc(sizeof(double)*polyZ->numPoints))
         == NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < polyZ->numPoints; ++i) {
        fread(&(polyZ->mArray[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.polyZ = polyZ;

    } else {

      /* allocate a new polygon */
      if((polyM = (PolygonM *) malloc(sizeof(PolygonM))) == NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }

      /* read box data */
      for(i = 0; i < 4; ++i) {
        fread(&(polyM->box[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      }

      /* read the number of parts */
      fread(buffer, sizeof(char), 4, fptr);
      polyM->numParts = readLittleEndian(buffer, 4);
      filePosition += 4;

      /* read the number of points */
      fread(buffer, sizeof(char), 4, fptr);
      polyM->numPoints = readLittleEndian(buffer, 4);
      filePosition += 4;

      /* read parts info */ 
      if((polyM->parts = (int *) malloc(sizeof(int) * polyM->numParts))
      	== NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < polyM->numParts; ++i) {
        fread(&(polyM->parts[i]), sizeof(char), 4, fptr);
        filePosition += 4;
      } 

      /* read points data */
      if((polyM->points = (Point *) malloc(sizeof(Point) * polyM->numPoints))
         == NULL) {
        Rprintf("Error: Allocating memory in C function pickLinearSamplePoints.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < polyM->numPoints; ++i) {
        fread(&(polyM->points[i].X), sizeof(double), 1, fptr);
        filePosition += 8;
        if(fread(&(polyM->points[i].Y), sizeof(double), 1, fptr) == 0) {
          Rprintf("Error: reading shape file in C function pickLinearSamplePoints.\n");
          fclose(fptr);
          remove(TEMP_SHP_FILE);
          PROTECT(results = allocVector(VECSXP, 1));
          UNPROTECT(1); 
          return results;  
        }
        filePosition += 8;
      } 

      /* read M range */
      for(i = 0; i < 2; ++i) {
        fread(&(polyM->mRange[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      }

      /* read M values */
      if((polyM->mArray = (double *) malloc(sizeof(double)*polyM->numPoints))
         == NULL) {
        Rprintf("Error: Allocating memory in C function lintFcn.\n");
        fclose(fptr);
        remove(TEMP_SHP_FILE);
        PROTECT(results = allocVector(VECSXP, 1));
        UNPROTECT(1); 
        return results;  
      }
      for(i = 0; i < polyM->numPoints; ++i) {
        fread(&(polyM->mArray[i]), sizeof(double), 1, fptr);
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.polyM = polyM;

    }

    /* loop through each cell requiring a sample point */
    for(i = 0; i < sampleSize; ++i) {
      if(recordIDs[i] != record.number) {
        continue;
       }
      temp = &record;

      /* create the cell structure */
      cell.xMin = xc[i] - dx;
      cell.yMin = yc[i] - dy;
      cell.xMax = xc[i];
      cell.yMax = yc[i];

      if(shape.shapeType == POLYLINE) {

        /* go through each segment in this record */
        partIndx = 1; 
        for(k = 0; k < temp->poly->numPoints-1; ++k) {

          /* if there are multiple parts, assume the parts are not connected */
          if(temp->poly->numParts > 1 && partIndx < temp->poly->numParts) {
            if((k + 1) == temp->poly->parts[partIndx]) {
              ++partIndx;
              continue;
            }
          }

          /* allocate new segment struct */
          if((seg = (Segment *) malloc(sizeof(Segment))) == NULL) {
            Rprintf("Error: Allocating memory in C function linSample.\n");
            PROTECT(results = allocVector(VECSXP, 1));
            UNPROTECT(1);
            fclose(fptr);
            remove(TEMP_SHP_FILE);
            return results;
          }

          /* get the length of the line that is inside the cell */
          tempLength = lineLength(temp->poly->points[k].X, 
                                  temp->poly->points[k].Y, 
                                  temp->poly->points[k+1].X, 
                                  temp->poly->points[k+1].Y, &cell, &seg);
 
          /* if this segment was inside the cell, then add it to the list */ 
          if(tempLength > 0.0) {
            seg->recordNumber = record.number;
            seg->length = tempLength;
            addSegment(&segmentList, seg);
          } else {
            free( seg );
          }


        }

      } else if(shape.shapeType == POLYLINE_Z) {

        /* go through each segment in this record */
        partIndx = 1; 
        for(k = 0; k < temp->polyZ->numPoints-1; ++k) {

          /* if there are multiple parts, assume the parts are not connected */
          if(temp->polyZ->numParts > 1 && partIndx < temp->polyZ->numParts) {
            if((k + 1) == temp->polyZ->parts[partIndx]) {
              ++partIndx;
              continue;
            }
          }

          /* allocate new segment struct */
          if((seg = (Segment *) malloc(sizeof(Segment))) == NULL) {
            Rprintf("Error: Allocating memory in C function linSample.\n");
            PROTECT(results = allocVector(VECSXP, 1));
            UNPROTECT(1);
            fclose(fptr);
            remove(TEMP_SHP_FILE);
            return results;
          }

          /* get the length of the line that is inside the cell */
          tempLength = lineLength(temp->polyZ->points[k].X, 
                                  temp->polyZ->points[k].Y, 
                                  temp->polyZ->points[k+1].X, 
                                  temp->polyZ->points[k+1].Y, &cell, &seg);

          /* if this segment was inside the cell, then add it to the list */ 
          if(tempLength > 0.0) {
            seg->recordNumber = record.number;
            seg->length = tempLength;
            addSegment(&segmentList, seg);
          } else {
            free( seg );
          }


        }

      } else {

        /* go through each segment in this record */
        partIndx = 1; 
        for (k = 0; k < temp->polyM->numPoints-1; ++k) {

          /* if there are multiple parts, assume the parts are not connected */
          if(temp->polyM->numParts > 1 && partIndx < temp->polyM->numParts) {
            if((k + 1) == temp->polyM->parts[partIndx]) {
              ++partIndx;
              continue;
            }
          }

          /* allocate new segment struct */
          if((seg = (Segment *) malloc(sizeof(Segment))) == NULL) {
            Rprintf("Error: Allocating memory in C function linSample.\n");
            PROTECT(results = allocVector(VECSXP, 1));
            UNPROTECT(1);
            fclose(fptr);
            remove(TEMP_SHP_FILE);
            return results;
          }

          /* get the length of the line that is inside the cell */
          tempLength = lineLength(temp->polyM->points[k].X, 
                                  temp->polyM->points[k].Y, 
                                  temp->polyM->points[k+1].X, 
                                  temp->polyM->points[k+1].Y, &cell, &seg);
       
          /* if this segment was inside the cell, then add it to the list */ 
          if(tempLength > 0.0) {
            seg->recordNumber = record.number;
            seg->length = tempLength;
            addSegment(&segmentList, seg);
          } else {
            free( seg );
          }

        }

      } 

      /* get the total length of all the segments in this cell */
      tempSeg = segmentList;
      sumWl = 0.0;
      while(tempSeg != NULL) {
        sumWl += tempSeg->length;
        tempSeg = tempSeg->next;
      }

      /* randomly pick a point along the line of segments */
      pos = runif(0.0, sumWl);
      tempSeg = segmentList;
      cumSum = 0.0;
      while(TRUE && tempSeg != NULL) {
        cumSum += tempSeg->length;
        if(pos < cumSum) {
          break;
        }
        tempSeg = tempSeg->next;
      }

      /* determine the coordinates for the sample point */
      len = (pos - cumSum);
      dx2 = tempSeg->p2.X - tempSeg->p1.X;
      dy2 = tempSeg->p2.Y - tempSeg->p1.Y;
      if(dx2 != 0) {
        lx = sign(dx2) * sqrt((len*len) / (1 + (dy2*dy2)/(dx2*dx2)));
        ly = lx * (dy2/dx2);
      } else {
        lx = 0.0;
        ly = -sign(dy2) * len;
      }
      xcs[i] = tempSeg->p1.X + lx;
      ycs[i] = tempSeg->p1.Y + ly;

      /* deallocate any memory used for the segment list */
      deallocateSegments(segmentList);
      segmentList = NULL;

    }

    if(shape.shapeType == POLYLINE) {
      free(temp->poly->parts);
      free(temp->poly->points);
      free(temp->poly);
    } else if(shape.shapeType == POLYLINE_Z) {
      free(temp->polyZ->parts);
      free(temp->polyZ->points);
      free(temp->polyZ->zArray);
      free(temp->polyZ->mArray);
      free(temp->polyZ);
    } else {
      free(temp->polyM->parts);
      free(temp->polyM->points);
      free(temp->polyM->mArray);
      free(temp->polyM);
    }

  }

  /* create the return R object */
  PROTECT(results = allocVector(VECSXP, 2));
  PROTECT(colNamesVec = allocVector(STRSXP, 2));
  PROTECT(xcsVec = allocVector(REALSXP, sampleSize));
  PROTECT(ycsVec = allocVector(REALSXP, sampleSize));
  for(i = 0; i < sampleSize; ++i) {
    REAL(xcsVec)[i] = xcs[i];
    REAL(ycsVec)[i] = ycs[i];
  }
  SET_VECTOR_ELT(results, 0, xcsVec);
  SET_VECTOR_ELT(results, 1, ycsVec);
  SET_STRING_ELT(colNamesVec, 0, mkChar("xcs"));
  SET_STRING_ELT(colNamesVec, 1, mkChar("ycs"));
  setAttrib(results, R_NamesSymbol, colNamesVec);

  /* output the RNG state */
  PutRNGstate();
  
  /* clean up */
  if(shpIDs) {
    free(shpIDs);
  }
  if(xc) {
    free(xc);
  }
  if(yc) {
    free(yc);
  }
  fclose(fptr);
  if(singleFile == TRUE) {
    free(shpFileName);
  }
  remove(TEMP_SHP_FILE);
  UNPROTECT(4);

  return results;
}
