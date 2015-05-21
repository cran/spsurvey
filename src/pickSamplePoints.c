/****************************************************************************** 
**  Function:    pickSamplePoints
**  Programmer:  Tom Kincaid
**  Date:        February 12, 2010
**  Description:
**    For each grid cell, this function determines the set of shapefile records
**    contained in the cell and returns the shapefile record IDs and the clipped
**    area of the polygons in the records 
**  Arguments:
**    fileNamePrefix = the shapefile name
**    shpIDsVec = vector of shapefile record IDs to use in the calculations
**    recordIDsVec = vector of shapefile record IDs
**    xcsVec = vector of grid cell x-coordinates
**    ycsVec = vector of grid cell y-coordinates
**    dxVal = x-axis size of the grid cells
**    dyVal = y-axis size of the grid cells
**    maxTryVal = maximum number of tries to obtain a sample point
**  Results
**    An R list object named results that contains the following items:
**    cellID = vector of grid cell IDs
**    recordID = vector of shapefile record IDs
**    recordArea = clipped area of the polygons in the shapefile record
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

/* This function is found in grtsarea.c */
extern int insidePolygon(Point * polygon, int N , double x, double y);


SEXP pickSamplePoints(SEXP fileNamePrefix, SEXP shpIDsVec, SEXP recordIDsVec,
     SEXP xcVec, SEXP ycVec, SEXP dxVal, SEXP dyVal, SEXP maxTryVal) {

  int i, j, k, l;             /* loop counters */
  FILE * fptr = NULL;         /* pointer to the shapefile */
  FILE * newShp = NULL;       /* pointer to the temporary .shp file */
  char * shpFileName = NULL;  /* stores full shape file name */
  int singleFile = FALSE;
  Shape shape;           /* used to store shapefile info and data */
  Point * part = NULL;   /* stores points in a part */
  int partSize;          /* number of points in part */
  int pidx;              /* index into part array */
  Record * temp = NULL;  /* used for traversing linked list of records */
  unsigned int filePosition = 100;  /* byte offset for the beginning of the */
                                    /* record data */
  unsigned char buffer[4];  /* temp buffer for reading from file */
  Polygon * poly;        /* temp Polygon storage */
  PolygonZ * polyZ;      /* temp PolygonZ storage */
  PolygonM * polyM;      /* temp PolygonM storage */
  Record record;         /* record used for parsing records */
  Cell cell;             /* temporary storage for a cell */
  unsigned int * shpIDs = NULL;  /* array of shapefile record IDs to use */
  int dsgSize = length(shpIDsVec);  /* number of values in the shpIDs array */
  unsigned int * recordIDs = NULL;  /* array of shapefile record IDs that get a sample point */
  double * xc = NULL;    /* array that stores values found in xcVec R vector */
  double * yc = NULL;    /* array that stores values found in ycVec R vector */
  double dx;             /* x-axis size of the grid cells */
  double dy;             /* y-axis size of the grid cells */
  unsigned int sampleSize = length(xcVec); /* sample size */
  int * bp = NULL;       /* array of logical values for sample point selection */
  double * xcs = NULL;   /* array of sample x-coordinates */
  double * ycs = NULL;   /* array array of sample x-coordinates */
  unsigned int maxTry;   /* maximum number of tries to obtain a sample point */
  double xMin;           /* minimum x-axis value for the sample point bounding box */
  double yMin;           /* minimum y-axis value for the sample point bounding box */
  double xMax;           /* maximum x-axis value for the sample point bounding box */
  double yMax;           /* maximum y-axis value for the sample point bounding box */
  int check  = 0;        /* value for determining validity of a potential sample point */
  double xtemp = 0.0;    /* x-coordinate for potential sample point */
  double ytemp = 0.0;    /* y-coordinate for potential sample point */
  SEXP results = NULL;   /* R object used to return values to R */
  SEXP colNamesVec;      /* vector used to name the columns in the results object */
  SEXP bpVec;            /* return vector of cell IDs */
  SEXP xcsVec;           /* return vector of record IDs */
  SEXP ycsVec;          /* return vector of record clipped areas */

  /* input the RNG state */
  GetRNGstate();

  /* see if a specific file was sent */
  if(fileNamePrefix != R_NilValue) {

    /* create the full .shp file name */
    if((shpFileName = (char *)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
                                              + strlen(".shp") + 1)) == NULL){
      Rprintf("Error: Allocating memory in C function insideGridCell.\n");
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
    Rprintf("Error: Creating temporary .shp file %s in C function insideGridCell.\n", TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* copy the shapefile record IDs from the R vector to a C array */
  if((shpIDs = (unsigned int *) malloc(sizeof(unsigned int) * dsgSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideGridCell.\n");
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
      Rprintf("Error: Combining multiple shapefiles in C function insideGridCell.\n");
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
      Rprintf("Error: Creating temporary shapefile in C function insideGridCell.\n");
      return results; 
    }
    fclose(newShp);
  }

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;

  /* open the temporary .shp file */
  if((fptr = fopen(TEMP_SHP_FILE, "rb")) == NULL) {
    Rprintf("Error: Opening shape file in C function insideGridCell.\n");
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* parse main file header */
  if(parseHeader(fptr, &shape) == -1) {
    Rprintf("Error: Reading main file header in C function insideGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* copy record ID values and coordinates of the cells from the R vectors to */
  /* C arrays  and initialize the bp array */
  if((recordIDs = (unsigned int *) malloc(sizeof(unsigned int) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((xc = (double *) malloc(sizeof(double) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((yc = (double *) malloc(sizeof(double) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideGridCell.\n");
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

  /* copy maxTry from the R value to a C value */
  maxTry = INTEGER(maxTryVal)[0];

  /* allocate memory and initialize the sample point logical array and the */
  /* sample point coordinate arrays */
  if((bp = (int *) malloc(sizeof(int) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((xcs = (double *) malloc(sizeof(double) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((ycs = (double *) malloc(sizeof(double) * sampleSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  for(i = 0; i < sampleSize; ++i) {
    bp[i] = TRUE;
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

    if(shape.shapeType == POLYGON) {

      /* allocate a new polygon */
      if((poly = (Polygon *) malloc(sizeof(Polygon))) == NULL) {
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
          Rprintf("Error: reading shape file in C function insideShape.\n");
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

    } else if(shape.shapeType == POLYGON_Z) {

      /* allocate a new polygon */
      if((polyZ = (PolygonZ *) malloc(sizeof(PolygonZ))) == NULL) {
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
          Rprintf("Error: reading shape file in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
        Rprintf("Error: Allocating memory in C function insideShape.\n");
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
          Rprintf("Error: reading shape file in C function insideShape.\n");
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
      if(bp[i] == FALSE || recordIDs[i] != record.number) {
        continue;
       }
      temp = &record;

      /* create the cell structure */
      cell.xMin = xc[i] - dx;
      cell.yMin = yc[i] - dy;
      cell.xMax = xc[i];
      cell.yMax = yc[i];

      if(shape.shapeType == POLYGON) {

        /* assign the sample point bounding box */
        if(temp->poly->box[0] < cell.xMin) {
          xMin = cell.xMin;
        } else {
          xMin = temp->poly->box[0];
        }
        if(temp->poly->box[1] < cell.yMin) {
          yMin = cell.yMin;
        } else {
          yMin = temp->poly->box[1];
        }
        if(temp->poly->box[2] > cell.xMax) {
          xMax = cell.xMax;
        } else {
          xMax = temp->poly->box[2];
        }
        if(temp->poly->box[3] > cell.yMax) {
          yMax = cell.yMax;
        } else {
          yMax = temp->poly->box[3];
        }

      /* make maxTry attempts to obtain a sample point */
      for(j = 0; j < maxTry; ++j) {
        check = 0;
        xtemp = runif(xMin, xMax);
        ytemp = runif(yMin, yMax);

        /* if there are more than one part we need to check them separately*/
        if(temp->poly->numParts > 1) {
          for(k = 0; k < temp->poly->numParts; ++k) {
            if(part) {
              free(part);
            }

            /* check to see if this is the last part */ 
            if(k == temp->poly->numParts - 1) {

              /* store part data into part array */
              partSize = temp->poly->numPoints - temp->poly->parts[k];
              if((part = (Point *) malloc(sizeof(Point) * partSize)) == NULL) {
                Rprintf("Error: Allocating memory in C function insideShape.\n");
                fclose(fptr);
                remove(TEMP_SHP_FILE);
                PROTECT(results = allocVector(VECSXP, 1));
                UNPROTECT(1); 
                return results;  
              }
              pidx = 0;
              for(l=temp->poly->parts[k]; l < temp->poly->numPoints; ++l) {
                part[pidx] = temp->poly->points[l];
                ++pidx; 
              }

            /* not the last part */
            } else {

              /* store part data into part array */
              partSize = temp->poly->parts[k+1] - temp->poly->parts[k];
              part = (Point *) malloc(sizeof(Point) * partSize);
              pidx = 0;
              for(l=temp->poly->parts[k]; l < temp->poly->parts[k+1]; ++l) {
                part[pidx] = temp->poly->points[l];
                ++pidx; 
              }

            }

            /* determine whether the point is inside this part */
            if(insidePolygon(part,partSize, xtemp, ytemp ) == 1) {
              ++check;
            }

          }  

        /* only one part so check the entire record */
        } else {

          /* determine whether the point is inside polygon */
          if(insidePolygon( temp->poly->points, temp->poly->numPoints, xtemp, ytemp) == 1) {
            ++check;
          }
        }

        if((check % 2) == 1) {
          bp[i] = FALSE;
          xcs[i] = xtemp;
          ycs[i] = ytemp;
          break;
        }

      }

      } else if(shape.shapeType == POLYGON_Z) {

      /* if there are more than one part we need to check them separately */
        if(temp->polyZ->numParts > 1) {
            for(k = 0; k < temp->polyZ->numParts; ++k) {
            if(part) {
              free(part);
            }

            /* check to see if this is the last part */ 
            if(k == temp->polyZ->numParts - 1) {

              /* store part data into part array */
              partSize = temp->polyZ->numPoints - temp->polyZ->parts[k];
              if((part = (Point *) malloc(sizeof(Point) * partSize)) == NULL) {
                Rprintf("Error: Allocating memory in C function insideShape.\n");
                fclose(fptr);
                remove(TEMP_SHP_FILE);
                PROTECT(results = allocVector(VECSXP, 1));
                UNPROTECT(1); 
                return results;  
              }
              pidx = 0;
              for(l=temp->polyZ->parts[k]; l < temp->polyZ->numPoints; ++l) {
                part[pidx] = temp->polyZ->points[l];
                ++pidx; 
              }

            /* not the last part */
            } else {

              /* store part data into part array */
              partSize = temp->polyZ->parts[k+1] - temp->polyZ->parts[k];
              part = (Point *) malloc(sizeof(Point) * partSize);
              pidx = 0;
              for(l=temp->polyZ->parts[k]; l < temp->polyZ->parts[k+1]; ++l) {
                part[pidx] = temp->polyZ->points[l];
                ++pidx; 
              }

            }

            /* determine whether the point is inside this part */
            if(insidePolygon(part,partSize, xtemp, ytemp) == 1) {
              ++check;
            }

          }  

          /* only one part so check the entire record */
        } else {

          /* determine whether the point is inside polygon */
          if(insidePolygon(temp->polyZ->points, temp->polyZ->numPoints, xtemp, ytemp) == 1) {
            ++check;
          }
        }

        if((check % 2) == 0) {
          bp[i] = FALSE;
          break;
        }

      } else {

      /* if there are more than one part we need to check them separately*/
        if(temp->polyM->numParts > 1) {
            for(k = 0; k < temp->polyM->numParts; ++k) {
            if(part) {
              free(part);
            }

            /* check to see if this is the last part */ 
            if(k == temp->polyM->numParts - 1) {

              /* store part data into part array */
              partSize = temp->polyM->numPoints - temp->polyM->parts[k];
              if((part = (Point *) malloc(sizeof(Point) * partSize)) == NULL) {
                Rprintf("Error: Allocating memory in C function insideShape.\n");
                fclose(fptr);
                remove(TEMP_SHP_FILE);
                PROTECT(results = allocVector(VECSXP, 1));
                UNPROTECT(1); 
                return results;  
              }
              pidx = 0;
              for(l=temp->polyM->parts[k]; l < temp->polyM->numPoints; ++l) {
                part[pidx] = temp->polyM->points[l];
                ++pidx; 
              }

            /* not the last part */
            } else {

              /* store part data into part array */
              partSize = temp->polyM->parts[k+1] - temp->polyM->parts[k];
              part = (Point *) malloc(sizeof(Point) * partSize);
              pidx = 0;
              for(l=temp->polyM->parts[k]; l < temp->polyM->parts[k+1]; ++l) {
                part[pidx] = temp->polyM->points[l];
                ++pidx; 
              }

            }

            /* determine whether the point is inside this part */
            if(insidePolygon(part,partSize, xtemp, ytemp) == 1) {
              ++check;
            }

          }  

          /* only one part so check the entire record */
        } else {

          /* determine whether the point is inside polygon */
          if(insidePolygon(temp->polyM->points, temp->polyM->numPoints, xtemp, ytemp) == 1) {
            ++check;
          }
        }

        if((check % 2) == 0) {
          bp[i] = FALSE;
          break;
        }

      } 

    }

    if(shape.shapeType == POLYGON) {
      free(temp->poly->parts);
      free(temp->poly->points);
      free(temp->poly);
    } else if(shape.shapeType == POLYGON_Z) {
      free(temp->polyZ->parts);
      free(temp->polyZ->points);
      free(temp->polyZ);
    } else {
      free(temp->polyM->parts);
      free(temp->polyM->points);
      free(temp->polyM);
    }

  }

  /* create the return R object */
  PROTECT(results = allocVector(VECSXP, 3));
  PROTECT(colNamesVec = allocVector(STRSXP, 3));
  PROTECT(bpVec = allocVector(LGLSXP, sampleSize));
  PROTECT(xcsVec = allocVector(REALSXP, sampleSize));
  PROTECT(ycsVec = allocVector(REALSXP, sampleSize));
  for(i = 0; i < sampleSize; ++i) {
    LOGICAL(bpVec)[i] = bp[i];
    REAL(xcsVec)[i] = xcs[i];
    REAL(ycsVec)[i] = ycs[i];
  }
  SET_VECTOR_ELT(results, 0, bpVec);
  SET_VECTOR_ELT(results, 1, xcsVec);
  SET_VECTOR_ELT(results, 2, ycsVec);
  SET_STRING_ELT(colNamesVec, 0, mkChar("bp")); 
  SET_STRING_ELT(colNamesVec, 1, mkChar("xcs"));
  SET_STRING_ELT(colNamesVec, 2, mkChar("ycs"));
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
  if(bp) {
    free(bp);
  }
  fclose(fptr);
  if(singleFile == TRUE) {
    free(shpFileName);
  }
  remove(TEMP_SHP_FILE);
  UNPROTECT(5);

  return results;
}
