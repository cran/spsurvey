/****************************************************************************** 
**  Function:    insideAreaGridCell
**  Programmer:  Tom Kincaid
**  Date:        May 19, 2010
**  Revised:     February 23, 2015
**  Revised:     May 5, 2015
**  Revised:     June 15, 2015
**  Description:
**    For each grid cell, this function determines the set of shapefile records
**    contained in the cell and returns the shapefile record IDs and the clipped
**    area of the polygons in the records 
**  Arguments:
**    fileNamePrefix = the shapefile name
**    dsgnmdIDVec = vector of shapefile record IDs to use in the calculations
**    cellIDsVec = vector of grid cell IDs
**    xcsVec = vector of grid cell x-coordinates
**    ycsVec = vector of grid cell y-coordinates
**    dxVal = x-axis size of the grid cells
**    dyVal = y-axis size of the grid cells
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

/* These functions are found in shapeParser.c */
extern int parseHeader(FILE * fptr, Shape * shape);
extern unsigned int readLittleEndian(unsigned char * buffer, int length);
extern unsigned int readBigEndian(unsigned char * buffer, int length);

/* These functions are found in grts.c */
extern int combineShpFiles(FILE * newShp, unsigned int * ids, int numIDs);
extern int createNewTempShpFile(FILE * newShp, char * shapeFileName,
                                unsigned int * ids, int numIDs);

/* These functions are found in grtsarea.c */
extern double polygonArea(Point * polygon, int size);
extern Point intersect(Cell * cell, Point * p1, Point * p2, int side);
extern int inside(Cell * cell, Point * p, int side);
extern double clipPolygonArea(Cell * cell, Point * points, int start, int end);


SEXP insideAreaGridCell(SEXP fileNamePrefix, SEXP dsgnmdIDVec, SEXP cellIDsVec,
     SEXP xcVec, SEXP ycVec, SEXP dxVal, SEXP dyVal) {

  int i, j, k;                /* loop counters */
  FILE * fptr = NULL;         /* pointer to the shapefile */
  FILE * newShp = NULL;       /* pointer to the temporary .shp file */
  unsigned int fileNameLen = 0;  /* length of the shapefile name */
  const char * shpExt = ".shp";  /* shapefile extension */
  char * restrict shpFileName = NULL;  /* stores the full .shp file name */
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
  unsigned int * dsgnmdID = NULL;  /* array of shapefile record IDs to use */
  unsigned int dsgSize = length(dsgnmdIDVec);  /* number of values in the dsgnmdID array */
  unsigned int numCells = length(xcVec); /* number of cells */
  unsigned int * recordIDs[numCells];  /* array that stores record IDs for each cell */
  double * recordAreas[numCells];      /* array that stores record areas for each cell */
  unsigned int * numIDs = NULL;   /* array that stores the number of record IDs for each cell */
  unsigned int * cellIDs = NULL;  /* array that stores values found in cellIDsVec R vector */
  double * xc = NULL;   /* array that stores values found in xcVec R vector */
  double * yc = NULL;   /* array that stores values found in ycVec R vector */
  double dx;             /* x-axis size of the grid cells */
  double dy;             /* y-axis size of the grid cells */
  unsigned int tempID;   /* stores current shapefile record ID */
  double tempArea;       /* stores current shapefile record clipped area */
  unsigned int * newIDs = NULL;    /* stores new set of record IDs in a cell */
  double * newAreas = NULL;        /* stores new set of record areas in a cell */
  int nID;               /* total number of record IDs */
  SEXP results = NULL;   /* R object used to return values to R */
  SEXP colNamesVec;      /* vector used to name the columns in the results object */
  SEXP cellVec;          /* return vector of cell IDs */
  SEXP recordIDsVec;     /* return vector of record IDs */
  SEXP recordAreasVec;   /* return vector of record clipped areas */

  /* see if a specific file was sent */
  if(fileNamePrefix != R_NilValue) {

    /* create the full .shp file name */
    fileNameLen = strlen(CHAR(STRING_ELT(fileNamePrefix, 0))) + strlen(shpExt);
    if ((shpFileName = (char * restrict)malloc(fileNameLen + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in C function insideAreaGridCell\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return results;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix, 0)));
    strcat( shpFileName, shpExt );
    singleFile = TRUE;
  }

  /* create the new temporary .shp file */
  if((newShp = fopen(TEMP_SHP_FILE, "wb")) == NULL) {
    Rprintf("Error: Creating temporary .shp file %s in C function insideAreaGridCell.\n", TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* copy the record IDs from the R vector to a C array */
  if((dsgnmdID = (unsigned int *) malloc(sizeof(unsigned int) * dsgSize)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    fclose(newShp);
    remove(TEMP_SHP_FILE);
    return results;
  }
  for(i = 0; i < dsgSize; ++i) {
    dsgnmdID[i] = INTEGER(dsgnmdIDVec)[i];
  }

  if(singleFile == FALSE) {

    /* create a temporary .shp file containing all the .shp files */
    if(combineShpFiles(newShp, dsgnmdID, dsgSize) == -1) {
      PROTECT(results = allocVector(VECSXP, 1));
      UNPROTECT(1);
      fclose(newShp);
      remove(TEMP_SHP_FILE);
      Rprintf("Error: Combining multiple shapefiles in C function insideAreaGridCell.\n");
      return results; 
    }
    fclose(newShp);

  } else {

    /* create a temporary .shp file containing the sent .shp file */
    if(createNewTempShpFile(newShp, shpFileName, dsgnmdID, dsgSize) == -1) {
      PROTECT(results = allocVector(VECSXP, 1));
      UNPROTECT(1);
      fclose(newShp);
      remove(TEMP_SHP_FILE);
      Rprintf("Error: Creating temporary shapefile in C function insideAreaGridCell.\n");
      return results; 
    }
    fclose(newShp);
  }

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;

  /* open the temporary .shp file */
  if((fptr = fopen(TEMP_SHP_FILE, "rb")) == NULL) {
    Rprintf("Error: Opening shape file in C function insideAreaGridCell.\n");
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* parse main file header */
  if(parseHeader(fptr, &shape) == -1) {
    Rprintf("Error: Reading main file header in C function insideAreaGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1);
    return results;
  }

  /* copy coordinates of the cells from the R vectors to C arrays */
  if((cellIDs = (unsigned int *) malloc(sizeof(unsigned int) * numCells)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((xc = (double *) malloc(sizeof(double) * numCells)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  if((yc = (double *) malloc(sizeof(double) * numCells)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  for(i = 0; i < numCells; ++i) {
    cellIDs[i] = INTEGER(cellIDsVec)[i];
    xc[i] = REAL(xcVec)[i];
    yc[i] = REAL(ycVec)[i];
  }

  /* copy x-axis and y-axis size of the grid cells from R values to C values */
  dx = REAL(dxVal)[0];
  dy = REAL(dyVal)[0];

  /* create the array for number of record IDs in each cell */  
  if((numIDs = (unsigned int *) malloc(sizeof(unsigned int) * numCells)) == NULL) {
    Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
    fclose(fptr);
    remove(TEMP_SHP_FILE);
    PROTECT(results = allocVector(VECSXP, 1));
    UNPROTECT(1); 
    return results;  
  }
  for(i = 0; i < numCells; ++i) {
    numIDs[i] = 0;
  }

  /* fill the arrays with the record IDs and clipped areas for each cell */  
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
          Rprintf("Error: reading shape file in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
          Rprintf("Error: reading shape file in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
        Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
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
          Rprintf("Error: reading shape file in C function insideAreaGridCell.\n");
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

    /* build the cell record IDs array */
    for(i = 0; i < numCells; ++i) {
      temp = &record;
      tempID = 0;
      tempArea = 0.0;

      /* create the cell structure */
      cell.xMin = xc[i] - dx;
      cell.yMin = yc[i] - dy;
      cell.xMax = xc[i];
      cell.yMax = yc[i];

      if(shape.shapeType == POLYGON) {

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
                Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
                fclose(fptr);
                remove(TEMP_SHP_FILE);
                PROTECT(results = allocVector(VECSXP, 1));
                UNPROTECT(1); 
                return results;  
              }
              pidx = 0;
              for(j=temp->poly->parts[k]; j < temp->poly->numPoints; ++j){
                part[pidx] = temp->poly->points[j];
                ++pidx; 
              }

            /* not the last part */
            } else {

              /* store part data into part array */
              partSize = temp->poly->parts[k+1] - temp->poly->parts[k];
              part = (Point *) malloc(sizeof(Point) * partSize);
              pidx = 0;
              for(j=temp->poly->parts[k]; j < temp->poly->parts[k+1]; ++j) {
                part[pidx] = temp->poly->points[j];
                ++pidx; 
              }

            }
            tempArea += clipPolygonArea(&cell, part, 0, partSize-1);

          }  
          if(tempArea > 0) {
            tempID = record.number;
          }

        /* only one part so check the entire record */
        } else {

          /* check whether the polygon is inside the cell */
          tempArea += clipPolygonArea(&cell, temp->poly->points, 0, temp->poly->numPoints-1);
          if(tempArea > 0) {
            tempID = record.number;
          }
        }

        if(tempID == 0) {
          continue;
        }

      } else if(shape.shapeType == POLYGON_Z) {

      /* if there are more than one part we need to check them separately*/
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
                Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
                fclose(fptr);
                remove(TEMP_SHP_FILE);
                PROTECT(results = allocVector(VECSXP, 1));
                UNPROTECT(1); 
                return results;  
              }
              pidx = 0;
              for(j=temp->polyZ->parts[k]; j < temp->polyZ->numPoints; ++j){
                part[pidx] = temp->polyZ->points[j];
                ++pidx; 
              }

            /* not the last part */
            } else {

              /* store part data into part array */
              partSize = temp->polyZ->parts[k+1] - temp->polyZ->parts[k];
              part = (Point *) malloc(sizeof(Point) * partSize);
              pidx = 0;
              for(j=temp->polyZ->parts[k]; j < temp->polyZ->parts[k+1]; ++j){
                part[pidx] = temp->polyZ->points[j];
                ++pidx; 
              }

            }
            tempArea += clipPolygonArea(&cell, part, 0, partSize-1);

          }  
          if(tempArea > 0) {
            tempID = record.number;
          }

          /* only one part so check the entire record */
        } else {

          /* check whether the polygon is inside the cell */
          tempArea += clipPolygonArea(&cell,temp->polyZ->points,0,temp->polyZ->numPoints-1);
          if(tempArea > 0) {
            tempID = record.number;
          }
        }

        if(tempID == 0) {
          continue;
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
                Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
                fclose(fptr);
                remove(TEMP_SHP_FILE);
                PROTECT(results = allocVector(VECSXP, 1));
                UNPROTECT(1); 
                return results;  
              }
              pidx = 0;
              for(j=temp->polyM->parts[k]; j < temp->polyM->numPoints; ++j){
                part[pidx] = temp->polyM->points[j];
                ++pidx; 
              }

            /* not the last part */
            } else {

              /* store part data into part array */
              partSize = temp->polyM->parts[k+1] - temp->polyM->parts[k];
              part = (Point *) malloc(sizeof(Point) * partSize);
              pidx = 0;
              for(j=temp->polyM->parts[k]; j < temp->polyM->parts[k+1]; ++j){
                part[pidx] = temp->polyM->points[j];
                ++pidx; 
              }

            }
            tempArea += clipPolygonArea(&cell, part, 0, partSize-1);

          }  
          if(tempArea > 0) {
            tempID = record.number;
          }

          /* only one part so check the entire record */
        } else {

          /* check whether the polygon is inside the cell */
          tempArea += clipPolygonArea(&cell,temp->polyM->points,0,temp->polyM->numPoints-1);
          if(tempArea > 0) {
            tempID = record.number;
          }
        }

        if(tempID == 0) {
          continue;
        }

      } 

      /* if found, add record ID to recordIDs array */
      if(tempID > 0) {
        ++numIDs[i];
        if(numIDs[i] > 1) {
          if((newIDs = (unsigned int *) malloc(sizeof(unsigned int) * numIDs[i])) == NULL) {
            Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
            PROTECT(results = allocVector(VECSXP, 1));
            UNPROTECT(1);
            fclose(newShp);
            remove(TEMP_SHP_FILE);
            return results;
          }
          if((newAreas = (double *) malloc(sizeof(double) * numIDs[i])) == NULL) {
            Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
            PROTECT(results = allocVector(VECSXP, 1));
            UNPROTECT(1);
            fclose(newShp);
            remove(TEMP_SHP_FILE);
            return results;
          }
          for(j = 0; j < (numIDs[i]-1); ++j) {
            newIDs[j] = *(recordIDs[i] + j);
            newAreas[j] = *(recordAreas[i] + j);
          }
          newIDs[(numIDs[i]-1)] = tempID;
          newAreas[(numIDs[i]-1)] = tempArea;
          recordIDs[i] = newIDs;
          recordAreas[i] = newAreas;
        } else {
          if((newIDs = (unsigned int *) malloc(sizeof(unsigned int))) == NULL) {
            Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
            PROTECT(results = allocVector(VECSXP, 1));
            UNPROTECT(1);
            fclose(newShp);
            remove(TEMP_SHP_FILE);
            return results;
          }
          if((newAreas = (double *) malloc(sizeof(double))) == NULL) {
            Rprintf("Error: Allocating memory in C function insideAreaGridCell.\n");
            PROTECT(results = allocVector(VECSXP, 1));
            UNPROTECT(1);
            fclose(newShp);
            remove(TEMP_SHP_FILE);
            return results;
          }
          newIDs[0] = tempID;
          newAreas[0] = tempArea;
          recordIDs[i] = newIDs;
          recordAreas[i] = newAreas;
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

  /* determine the total number of record IDs */
  nID = 0;
  for(i = 0; i < numCells; ++i) {
    nID = nID + numIDs[i];
  }

  /* create the return R object */
  PROTECT(results = allocVector(VECSXP, 3));
  PROTECT(colNamesVec = allocVector(STRSXP, 3));
  PROTECT(cellVec = allocVector(INTSXP, nID));
  PROTECT(recordIDsVec = allocVector(INTSXP, nID));
  PROTECT(recordAreasVec = allocVector(REALSXP, nID));
  k = 0;
  for(i = 0; i < numCells; ++i) {
    for(j = 0; j < numIDs[i]; ++j) {
      INTEGER(cellVec)[k] = cellIDs[i];
      INTEGER(recordIDsVec)[k] = *(recordIDs[i] + j);
      REAL(recordAreasVec)[k] = *(recordAreas[i] + j);
      ++k;
    }
  }
  SET_VECTOR_ELT(results, 0, cellVec);
  SET_VECTOR_ELT(results, 1, recordIDsVec);
  SET_VECTOR_ELT(results, 2, recordAreasVec);
  SET_STRING_ELT(colNamesVec, 0, mkChar("cellID")); 
  SET_STRING_ELT(colNamesVec, 1, mkChar("recordID"));
  SET_STRING_ELT(colNamesVec, 2, mkChar("recordArea"));
  setAttrib(results, R_NamesSymbol, colNamesVec);

  /* clean up */
  if(dsgnmdID) {
    free(dsgnmdID);
  }
  if(xc) {
    free(xc);
  }
  if(yc) {
    free(yc);
  }
  if(numIDs) {
    free(numIDs);
  }
  fclose(fptr);
  remove(TEMP_SHP_FILE);
  UNPROTECT(5);

  return results;
}
