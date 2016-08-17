/****************************************************************************** 
**  File:         grtslin.c
** 
**  Purpose:      This file contains the code for the lintFcn() and linSample()
**                functions and pertain to polyline shape types.  lintFcn() is 
**                called from the numLevels() function found in grts.c.  The
**                lineSample() function is used to pick sample points from 
**                the cells.
**  Programmers:  Christian Platt, Tom Kincaid
**  Created:      October 18, 2004
**  Revised:      October 18, 2007
**  Revised:      February 23, 2015
**  Revised:      May 5, 2015
**  Revised:      June 15, 2015
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>           /* for runif function */
#include "shapeParser.h"
#include "grts.h"

#define BOTTOM  1
#define TOP     2
#define LEFT    4
#define RIGHT   8

/* these functions are found in shapeParser.c */
extern int parseHeader( FILE * fptr, Shape * shape );
extern unsigned int readLittleEndian( unsigned char * buffer, int length );
extern unsigned int readBigEndian( unsigned char * buffer, int length );

/* these functions are found in grts.c */
extern int combineShpFiles( FILE * newShp, unsigned int * ids, int numIDs );
extern int createNewTempShpFile( FILE * newShp, char * shapeFileName, 
		                       unsigned int * ids, int numIDs );


/**********************************************************
** Function:   addSegment
**
** Purpose:    Adds the sent segment to the linked list of segments.
** Notes:      It is assumed that the sent seg struct has already been
**             allocated memory and has had the necessary data written
**             to it.
** Arguments:  head,  pointer to the head node in the linked list 
**             seg,   segment to be added to the linked list 
** Return:     void
***********************************************************/
void addSegment( Segment ** head, Segment * seg ) {

  seg->next = *head;
  *head = seg;

  return;
}


/**********************************************************
** Function:   deallocateSegments
**
** Purpose:    Deallocates all the memory used by the segment nodes
**             in the linked list pointed to by head.
** Arguments:  head,   the head node in the linked list 
** Return:     void
***********************************************************/
void deallocateSegments( Segment * head ) {
  Segment * temp;

  while ( head ) {
    temp = head->next;
    free( head );
    head = temp;
  }
  
  return;
}


/**********************************************************
** Function:   compOutCode
**
** Purpose:    Computes and returns the out code describing where
**             the sent point is (x,y) compared to the cell.
** Notes:      This code is taken modeled after the Cohen-Sutherland
**             algorithm for line clipping which was found at
**             http://www.cc.gatech.edu/grads/h/Hao-wei.Hsieh/Haowei.Hsieh/
** Arguments:  x,      x coordinate of the point
**             y,      y coordinate of the point
**             cell,   holds the coordinates for the cell
** Return:     outCode,  integer that describes which area the point is
**                       in in relationship to the cell.  It has 9 possbile
**                       values: top left, top, top right, left, inside
**                       right, bottom left, bottom, bottom right.
***********************************************************/
int compOutCode( double x, double y, Cell * cell ) {
  int outCode = 0;
  
  if ( y > cell->yMax ) {
    outCode += TOP;
  } else if ( y < cell->yMin ) {
    outCode += BOTTOM;
  }

  if ( x > cell->xMax ) {
    outCode += RIGHT;
  } else if ( x < cell->xMin ) {
    outCode += LEFT;
  }

  return outCode;
}


/**********************************************************
** Function:   lineLength
**
** Purpose:    Calculates the lenght of the sent line that is inside the
**             sent cell.
** Notes:      This code is taken modeled after the Cohen-Sutherland
**             algorithm for line clipping which was found at
**             http://www.cc.gatech.edu/grads/h/Hao-wei.Hsieh/Haowei.Hsieh
**             The Cohen-Sutherland algorithm is used to calculate the new
**             end points of the sent line clipped by the cell's boundary.
**             This function then uses those new end points to return the
**             length of the new line segment.
** Arguments:  x1,      x coordinate of end point 1 of the line
**             y1,      y coordinate of end point 1 of the line
**             x2,      x coordinate of end point 2 of the line
**             y2,      y coordinate of end point 2 of the line
**             cell,    holds the coordinates for the cell
** Return:     the length of the sent line that is inside the sent cell.
**             It will return 0 if none of the line falls within the cell.
***********************************************************/
double lineLength( double x1, double y1, double x2, double y2, Cell * cell, 
                                        Segment ** newSeg ){
  double x = -4.0;
  double y = -4.0;
  double dx, dy;
  int outCode1, outCode2, outCodeOut;

  outCode1 = compOutCode( x1, y1, cell );
  outCode2 = compOutCode( x2, y2, cell );

  while ( outCode1 != 0 || outCode2 != 0 ) {
    if ( (outCode1 & outCode2) != 0 ) {
      return 0.0;    /* trivial reject */
    }

    if ( outCode1 > 0 ) {
      outCodeOut = outCode1;
    } else {
      outCodeOut = outCode2;
    }

    if ( (outCodeOut & BOTTOM) == BOTTOM ) {
      y = cell->yMin;
      x = x1 + (x2 - x1) * (y - y1) / (y2 - y1);
    } else if ( (outCodeOut & TOP) == TOP ) {
      y = cell->yMax;
      x = x1 + (x2 - x1) * (y - y1) / (y2 - y1);
    } else if ( (outCodeOut & RIGHT) == RIGHT ) {
      x = cell->xMax;
      y = y1 + (y2 - y1) * (x - x1) / (x2 - x1);
    } else if ( (outCodeOut & LEFT) == LEFT ) {
      x = cell->xMin;
      y = y1 + (y2 - y1) * (x - x1) / (x2 - x1);
    }

    if ( outCodeOut == outCode1 ) {
      x1 = x;
      y1 = y;
      outCode1 = compOutCode( x1, y1, cell);
    } else {
      x2 = x;
      y2 = y;
      outCode2 = compOutCode( x2, y2, cell);
    }

  }

  /* calculate length of new line */
  dx = x2 - x1;
  dy = y2 - y1;

  if ( newSeg != NULL ) {
    (*newSeg)->p1.X = x1;
    (*newSeg)->p1.Y = y1;
    (*newSeg)->p2.X = x2;
    (*newSeg)->p2.Y = y2;
  }

  return sqrt( dx*dx + dy*dy );
}


/**********************************************************
** Function:   lintFcn
**
** Purpose:    This function implements the lint.fcn and is called from
**             the numLevels() function found in grts.c.  It is used to
**             determine the total length of segments found in each cell
**             and is used on polyline shape types. These lengths are 
**             used to determine the cell weights.
** Algorithm:  The function reads one record at a time from the sent shape
**             file and processes it to save on memory.
** Arguments:  celWts,   array of doubles representing the cell weights
**             xc,       array of x coordinates
**             yc,       array of y coordiantes
**             dx,       amount to shift x coordinates
**             dy,       amount to shift y coordiantes
**             size,     length of the xc and yc arrays
**             shape,    shape struct that contains header info for the
**                       shape file we are working with. It is used to store
**                       a subset of the records at a time.
**             fptr,     pointer to the shape file we are using
**             dsgnmdID, array of record IDs which have weights and should be
**                       used in the calculations
**             dsgnmd,   array of weights corresponding to the above IDs
**             dsgSize,  number of IDs in the dsgnmdID array
** Return:     1,  on success
**             -1, on error
***********************************************************/
int lintFcn ( double ** celWts, double * xc, double * yc, double dx, double dy,
              int size, Shape * shape, FILE * fptr, unsigned int * dsgnmdID,
              double * dsgnmd, int dsgSize ) { 

  int i, w;                     /* loop counter */
  int row;                      /* loop counter */
  int partIndx;                 /* index into polyline parts array */
  Cell cell;                    /* temp storage for a cell */
  unsigned int filePosition;    /* byte offset within the shape file */
  Record record;                /* temp record storage */
  unsigned char buffer[4];      /* temp buffer for reading from file */
  Polygon * poly;               /* temp Polygon storage */
  PolygonZ * polyZ;             /* temp PolygonZ storage */
  PolygonM * polyM;             /* temp PolygonM storage */

  /* initialize the shape struct */
  shape->records = NULL;

  /* initialize all the cell weights */
  for ( row = 0; row < size; ++row ) {
    (*celWts)[row] = 0.0;
  }
 
  /* get to the correct spot in the shape file to read in all the records */ 
  fseek( fptr, 100, SEEK_SET );
  filePosition = 100;

  /* read through all the records found in the file */
  while ( filePosition < shape->fileLength*2 ) {

    /* read the record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 ); 
    filePosition += 4;

    /* skip over content length */
    fread( buffer, sizeof(char), 4, fptr );
    filePosition += 4;

    /* skip over shape type */
    fread( buffer, sizeof(char), 4, fptr );
    filePosition += 4;

    if ( shape->shapeType == POLYLINE ) {

      /* allocate a new polygon */
      if ( (poly = (Polygon *) malloc( sizeof(Polygon) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }

      /* read in box data */
      for ( i=0; i < 4; ++i ) {
        fread( &(poly->box[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read in the number of parts */
      fread( buffer, sizeof(char), 4, fptr );
      poly->numParts = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read in the number of points */
      fread( buffer, sizeof(char), 4, fptr );
      poly->numPoints = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read in parts info */ 
      if ((poly->parts = (int *) malloc( sizeof(int) * poly->numParts ))==NULL){
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < poly->numParts; ++i ) {
        fread( &(poly->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read in points data */
      if ((poly->points = (Point *) malloc( sizeof(Point) * poly->numPoints ))
                                                                  == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < poly->numPoints; ++i ) {
        fread( &(poly->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(poly->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function lintFcn.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.poly = poly;
      record.polyZ = NULL;
      record.polyM = NULL;

    } else if ( shape->shapeType == POLYLINE_Z ) {

      /* allocate a new polygon */
      if ( (polyZ = (PolygonZ *) malloc( sizeof(PolygonZ) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }

      /* read in box data */
      for ( i=0; i < 4; ++i ) {
        fread( &(polyZ->box[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read in the number of parts */
      fread( buffer, sizeof(char), 4, fptr );
      polyZ->numParts = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read in the number of points */
      fread( buffer, sizeof(char), 4, fptr );
      polyZ->numPoints = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read in parts info */ 
      if ((polyZ->parts = (int *) malloc( sizeof(int) * polyZ->numParts))
      	== NULL) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < polyZ->numParts; ++i ) {
        fread( &(polyZ->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read in points data */
      if ((polyZ->points = (Point *) malloc( sizeof(Point) * polyZ->numPoints ))
                                                                  == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(polyZ->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function lintFcn.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* read Z range */
      for ( i=0; i < 2; ++i ) {
        fread( &(polyZ->zRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read Z values */
      if ((polyZ->zArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
                                                               == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->zArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* read M range */
      for ( i=0; i < 2; ++i ) {
        fread( &(polyZ->mRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read M values */
      if ((polyZ->mArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
                                                               == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->mArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.polyZ = polyZ;
      record.poly = NULL;
      record.polyM = NULL;

    } else {

      /* allocate a new polygon */
      if ( (polyM = (PolygonM *) malloc( sizeof(PolygonM) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }

      /* read in box data */
      for ( i=0; i < 4; ++i ) {
        fread( &(polyM->box[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read in the number of parts */
      fread( buffer, sizeof(char), 4, fptr );
      polyM->numParts = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read in the number of points */
      fread( buffer, sizeof(char), 4, fptr );
      polyM->numPoints = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read in parts info */ 
      if ((polyM->parts = (int *) malloc( sizeof(int) * polyM->numParts))
      	== NULL) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < polyM->numParts; ++i ) {
        fread( &(polyM->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read in points data */
      if ((polyM->points = (Point *) malloc( sizeof(Point) * polyM->numPoints ))
           == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < polyM->numPoints; ++i ) {
        fread( &(polyM->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(polyM->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function lintFcn.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* read M range */
      for ( i=0; i < 2; ++i ) {
        fread( &(polyM->mRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read M values */
      if ((polyM->mArray = (double *) malloc( sizeof(double)*polyM->numPoints))
           == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i=0; i < polyM->numPoints; ++i ) {
        fread( &(polyM->mArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.polyM = polyM;
      record.poly = NULL;
      record.polyZ = NULL;

    }

    /* find the dsgnmd weight array position for this record ID */
    for ( w = 0; w < dsgSize; ++w ) {
      if ( dsgnmdID[w] == record.number ) {
        break; 
      } 
    }
           
    if ( shape->shapeType == POLYLINE ) {

      /* go through each segment in this record */
      partIndx = 1; 
      for ( i = 0; i < record.poly->numPoints-1; ++i ) {

        /* if there are multiple parts, assume the parts are not connected */
        if ( record.poly->numParts > 1 && partIndx < record.poly->numParts ) {
          if ( (i + 1) == record.poly->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        /* check each segment in each cell */
        for ( row = 0; row < size; ++row ) {

          /* form the cell's points */
          cell.xMin = xc[row] - dx;
          cell.yMin = yc[row] - dy;
          cell.xMax = xc[row];
          cell.yMax = yc[row];

          /* add the lenght of the segment that is in the cell */
          if ( w >= dsgSize ) {
            (*celWts)[row] += 0;
          } else {
            (*celWts)[row] += lineLength(record.poly->points[i].X, 
                                         record.poly->points[i].Y, 
                                         record.poly->points[i+1].X, 
                                         record.poly->points[i+1].Y,
                                         &cell, NULL) * dsgnmd[w];
          }
        }
      }

      free( record.poly->parts );
      free( record.poly->points );
      free( record.poly );

    } else if ( shape->shapeType == POLYLINE_Z ) {

      /* go through each segment in this record */
      partIndx = 1; 
      for ( i = 0; i < record.polyZ->numPoints-1; ++i ) {

        /* if there are multiple parts, assume the parts are not connected */
        if ( record.polyZ->numParts > 1 && partIndx < record.polyZ->numParts ) {
          if ( (i + 1) == record.polyZ->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        /* check each segment in each cell */
        for ( row = 0; row < size; ++row ) {

          /* form the cell's points */
          cell.xMin = xc[row] - dx;
          cell.yMin = yc[row] - dy;
          cell.xMax = xc[row];
          cell.yMax = yc[row];

          /* add the length of the segment that is in the cell */
          if ( w >= dsgSize ) {
            (*celWts)[row] += 0;
          } else {
            (*celWts)[row] += lineLength(record.polyZ->points[i].X, 
                                         record.polyZ->points[i].Y, 
                                         record.polyZ->points[i+1].X, 
                                         record.polyZ->points[i+1].Y,
                                         &cell, NULL) * dsgnmd[w];
          }
        }
      }

      free( record.polyZ->parts );
      free( record.polyZ->points );
      free( record.polyZ );

    } else {

      /* go through each segment in this record */
      partIndx = 1; 
      for ( i = 0; i < record.polyM->numPoints-1; ++i ) {

        /* if there are multiple parts, assume the parts are not connected */
        if ( record.polyM->numParts > 1 && partIndx < record.polyM->numParts ) {
          if ( (i + 1) == record.polyM->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        /* check each segment in each cell */
        for ( row = 0; row < size; ++row ) {

          /* form the cell's points */
          cell.xMin = xc[row] - dx;
          cell.yMin = yc[row] - dy;
          cell.xMax = xc[row];
          cell.yMax = yc[row];

          /* add the length of the segment that is in the cell */
          if ( w >= dsgSize ) {
            (*celWts)[row] += 0;
          } else {
            (*celWts)[row] += lineLength(record.polyM->points[i].X, 
                                         record.polyM->points[i].Y, 
                                         record.polyM->points[i+1].X, 
                                         record.polyM->points[i+1].Y,
                                         &cell, NULL) * dsgnmd[w];
          }
        }
      }

      free( record.polyM->parts );
      free( record.polyM->points );
      free( record.polyM );

    }

  }

  return 1;
}


/**********************************************************
** Function:   linSample
**
** Purpose:    To select a sample from each of the sent
**             cells( sent x,y coordinates for cell corner and x,y offsets )
** Notes:      It is assumed that if a record has multiple parts 
**             they are not connected.
**             To save memory one record at a time is read from the shape
**             file and processed before reading in the next.
** Arguments:  fileNamePrefix, name of shape file not including the 
**                             .shp extension
**             xcVec, vector of x coordinates for all the cell corners
**             ycVec, vector of y coordiantes for all the cell corners
**             dxVec, x offset to form the cell corners
**             dyVec, y offset to form the cell corners
**             dsgnmdIDVec, vector of the record IDs that should be used
**                          in the calculations
**             dsgnmdVec, weights corresponding to the above IDs
** Return:     results,  R vectors of x and y coordinates making up the
**                       selected sample along with the IDs that the
**                       samples belong to.
***********************************************************/
SEXP linSample( SEXP fileNamePrefix, SEXP xcVec, SEXP ycVec, SEXP dxVec, 
                SEXP dyVec, SEXP dsgnmdIDVec, SEXP dsgnmdVec ) {


  int i, j, w;                /* loop counters */
  int partIndx;               /* index into polyline parts array */

  /* C variables that store the sent R object's values */
  double dx, dy;
  double * xc = NULL;
  double * yc = NULL;
  double * dsgnmd = NULL;
  unsigned int * dsgnmdID = NULL;
  unsigned int dsgSize = length( dsgnmdIDVec );

  double * realPtr;    /* temp reald pointer */
  FILE * fptr = NULL;  /* pointer to .shp file */
  Shape shape;         /* temp storage of shape data */
  unsigned int vecSize = length( xcVec );  /* number of cells */
  unsigned int filePosition;  /* byte offset into shape file */
  Cell cell;                  /* temp storage of cell coordinates */
  Record record;              /* temp storage of record data */
  Polygon * poly;             /* temp Polygon storage */
  PolygonZ * polyZ;           /* temp PolygonZ storage */
  PolygonM * polyM;           /* temp PolygonM storage */
  unsigned char buffer[4];    /* buffer used for reading from file */

  /* variables for storing linked list of segment structs and traversing them */
  Segment * seg;
  Segment * segmentList = NULL;
  Segment * temp;

  /* vars used for picking sample point in cell */
  double pos;
  double cumSum;       /* accumulative sum of lengths up to selected point */
  double sumWl;        /* sum of all the lengths of lines in a cell */
  double lx, ly;
  double length;       /* length of segment found in a cell */
  unsigned int * samp = NULL;  
  double * x = NULL;
  double * y = NULL;
  unsigned int sampInd = 0;
  double len;
  double dx2, dy2;

  /* vars used for converting results to an R object */
  SEXP xVec, yVec, IDVec, colNamesVec;
  SEXP results = NULL;

  FILE * newShp = NULL;   /* pointer to the temp .shp file that will consist */
                          /* of the data found in all the .shp files found in */
                          /* the current working directory */
  unsigned int fileNameLen = 0;  /* length of the shapefile name */
  const char * shpExt = ".shp";  /* shapefile extension */
  char * restrict shpFileName = NULL;  /* stores the full .shp file name */
  int singleFile = FALSE;


  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;

  /* see if a specific file was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full .shp file name */
    fileNameLen = strlen(CHAR(STRING_ELT(fileNamePrefix, 0))) + strlen(shpExt);
    if ((shpFileName = (char * restrict) malloc(fileNameLen + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in C function linSample\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return results;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix, 0)));
    strcat( shpFileName, shpExt );
    singleFile = TRUE;
  }

  /* create the new temporary .shp file */
  if ( ( newShp = fopen( TEMP_SHP_FILE, "wb" )) == NULL ) {
    Rprintf( "Error: Creating temporary .shp file %s in C function linSample.\n", TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* copy the dsgnmd poly IDs into a C array */
  if ( (dsgnmdID = (unsigned int *) malloc( sizeof( unsigned int ) * dsgSize))
                                                         == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSample.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    fclose( newShp );
    remove( TEMP_SHP_FILE );
    return results;
  }
  for ( i = 0; i < dsgSize; ++i ) {
    dsgnmdID[i] = INTEGER( dsgnmdIDVec )[i];
  }

  if ( singleFile == FALSE ) {

    /* create a temporary .shp file containing all the .shp files */
    if ( combineShpFiles( newShp, dsgnmdID, dsgSize ) == -1 ) {
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( newShp );
      remove( TEMP_SHP_FILE );
      Rprintf( "Error: Combining multiple shapefiles in C function linSample.\n" );
      return results; 
    }
    fclose( newShp );

  } else {

    /* create a temporary .shp file containing the sent .shp file */
    if (createNewTempShpFile(newShp,shpFileName,dsgnmdID,dsgSize) == -1 ){
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( newShp );
      remove( TEMP_SHP_FILE );
      Rprintf( "Error: Creating temporary shapefile in C function linSample.\n" );
      return results; 
    }
    fclose( newShp );
  }

  /* open the temporary .shp file */
  if ( (fptr = fopen( TEMP_SHP_FILE, "rb" )) == NULL ) {
    Rprintf( "Error: Opening shape file in C function linSample.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* parse main file header */
  if ( parseHeader( fptr, &shape ) == -1 ) {
    Rprintf( "Error: Reading main file header in C function linSample.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }

  /* copy the dsgnmd mdm weights into an C array */
  if ( (dsgnmd = (double *) malloc( sizeof( double ) * dsgSize )) == NULL ){
    Rprintf( "Error: Allocating memory in C function linSample.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }
  for ( i = 0; i < dsgSize; ++i ) {
    dsgnmd[i] = REAL( dsgnmdVec )[i];
  }

  /* copy incoming R arguments to C variables */
  PROTECT( dxVec = AS_NUMERIC( dxVec ) );
  realPtr = NUMERIC_POINTER( dxVec );
  dx = *realPtr;
  UNPROTECT(1);

  PROTECT( dyVec = AS_NUMERIC( dyVec ) );
  realPtr = NUMERIC_POINTER( dyVec );
  dy = *realPtr;
  UNPROTECT(1);

  /* copy the points into C arrays */
  if ( (xc = (double *) malloc( sizeof( double ) * vecSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSample.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }
  for ( i = 0; i < vecSize; ++i ) {
    xc[i] = REAL( xcVec )[i];
  }

  if ( (yc = (double *) malloc( sizeof( double ) * vecSize ) ) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSample.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }
  for ( i = 0; i < vecSize; ++i ) {
    yc[i] = REAL( ycVec )[i];
  }

  /* input the RNG state */
  GetRNGstate();

  if ( ( samp = (unsigned int *) malloc( sizeof( unsigned int ) * vecSize ))
                                                             == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSample.c\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }
  if ( ( x = (double *) malloc( sizeof( double ) * vecSize ) ) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSample.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }
  if ( ( y = (double *) malloc( sizeof( double ) * vecSize ) ) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSample.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }

  /* get sample from each cell */
  for ( j = 0; j < vecSize; ++j ) {

    /* set up the cell coordinates */
    cell.xMin = xc[j] - dx;
    cell.yMin = yc[j] - dy;
    cell.xMax = xc[j];
    cell.yMax = yc[j];

    /* get to the beginning of the records in the .shp file */ 
    fseek( fptr, 100, SEEK_SET );
    filePosition = 100;

    /* deallocate any memory used for the segment list */
    deallocateSegments( segmentList );
    segmentList = NULL;

    /* go through the shapefile to determine which segments are in each cell */
    while ( filePosition < shape.fileLength*2 ) {

      /* read the record number */
      fread( buffer, sizeof(char), 4, fptr );
      record.number = readBigEndian( buffer, 4 ); 
      filePosition += 4;

      /* skip over content length */
      fread( buffer, sizeof(char), 4, fptr );
      filePosition += 4;

      /* skip over shape type */
      fread( buffer, sizeof(char), 4, fptr );
      filePosition += 4;

      if ( shape.shapeType == POLYLINE ) {

        /* allocate a new polygon */
        if ( (poly = (Polygon *) malloc( sizeof(Polygon) )) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }

        /* read in box data */
        for ( i=0; i < 4; ++i ) {
          fread( &(poly->box[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        }

        /* read in the number of parts */
        fread( buffer, sizeof(char), 4, fptr );
        poly->numParts = readLittleEndian( buffer, 4 );
        filePosition += 4;

        /* read in the number of points */
        fread( buffer, sizeof(char), 4, fptr );
        poly->numPoints = readLittleEndian( buffer, 4 );
        filePosition += 4;

        /* read in parts info */ 
        if ((poly->parts = (int *) malloc( sizeof(int) * poly->numParts ))==NULL) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < poly->numParts; ++i ) {
          fread( &(poly->parts[i]), sizeof(char), 4, fptr );
          filePosition += 4;
        } 

        /* read points data */
        if ((poly->points = (Point *) malloc( sizeof(Point) * poly->numPoints ))
                                                                       == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < poly->numPoints; ++i ) {
          fread( &(poly->points[i].X), sizeof(double), 1, fptr );
          filePosition += 8;
          if ( fread( &(poly->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
            Rprintf( "Error: reading shape file in C function linSample.\n" );
            PROTECT( results = allocVector( VECSXP, 1 ) );
            UNPROTECT( 1 );
            fclose( fptr );
            remove( TEMP_SHP_FILE );
            return results;
          }
          filePosition += 8;
        } 

        /* add new polygon to the record struct */
        record.poly = poly;
        record.polyZ = NULL;
        record.polyM = NULL;

      } else if ( shape.shapeType == POLYLINE_Z ) {

        /* allocate a new polygon */
        if ( (polyZ = (PolygonZ *) malloc( sizeof(PolygonZ) )) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }

        /* read box data */
        for ( i=0; i < 4; ++i ) {
          fread( &(polyZ->box[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        }

        /* read the number of parts */
        fread( buffer, sizeof(char), 4, fptr );
        polyZ->numParts = readLittleEndian( buffer, 4 );
        filePosition += 4;

        /* read the number of points */
        fread( buffer, sizeof(char), 4, fptr );
        polyZ->numPoints = readLittleEndian( buffer, 4 );
        filePosition += 4;

        /* read parts info */ 
        if ((polyZ->parts = (int *) malloc( sizeof(int) * polyZ->numParts))
             == NULL) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < polyZ->numParts; ++i ) {
          fread( &(polyZ->parts[i]), sizeof(char), 4, fptr );
          filePosition += 4;
        }

        /* read points data */
        if ((polyZ->points = (Point *) malloc( sizeof(Point)*polyZ->numPoints ))
             == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < polyZ->numPoints; ++i ) {
          fread( &(polyZ->points[i].X), sizeof(double), 1, fptr );
          filePosition += 8;
          if ( fread( &(polyZ->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
            Rprintf( "Error: reading shape file in C function linSample.\n" );
            PROTECT( results = allocVector( VECSXP, 1 ) );
            UNPROTECT( 1 );
            fclose( fptr );
            remove( TEMP_SHP_FILE );
            return results;
          }
          filePosition += 8;
        } 

        /* read Z range */
        for ( i=0; i < 2; ++i ) {
          fread( &(polyZ->zRange[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        }

        /* read Z values */
        if ((polyZ->zArray = (double *) malloc( sizeof(double) *
        	   polyZ->numPoints)) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < polyZ->numPoints; ++i ) {
          fread( &(polyZ->zArray[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        } 

        /* read M range */
        for ( i=0; i < 2; ++i ) {
          fread( &(polyZ->mRange[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        }

        /* read M values */
        if ((polyZ->mArray = (double *) malloc( sizeof(double) *
        	   polyZ->numPoints)) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < polyZ->numPoints; ++i ) {
          fread( &(polyZ->mArray[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        } 

        /* add new polygon to the record struct */
        record.polyZ = polyZ;
        record.poly = NULL;
        record.polyM = NULL;

      } else {

        /* allocate a new polygon */
        if ( (polyM = (PolygonM *) malloc( sizeof(PolygonM) )) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }

        /* read box data */
        for ( i=0; i < 4; ++i ) {
          fread( &(polyM->box[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        }

        /* read the number of parts */
        fread( buffer, sizeof(char), 4, fptr );
        polyM->numParts = readLittleEndian( buffer, 4 );
        filePosition += 4;

        /* read the number of points */
        fread( buffer, sizeof(char), 4, fptr );
        polyM->numPoints = readLittleEndian( buffer, 4 );
        filePosition += 4;

        /* read parts info */ 
        if ((polyM->parts = (int *) malloc( sizeof(int) * polyM->numParts))
             == NULL) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < polyM->numParts; ++i ) {
          fread( &(polyM->parts[i]), sizeof(char), 4, fptr );
          filePosition += 4;
        }

        /* read points data */
        if ((polyM->points = (Point *) malloc( sizeof(Point)*polyM->numPoints ))
             == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < polyM->numPoints; ++i ) {
          fread( &(polyM->points[i].X), sizeof(double), 1, fptr );
          filePosition += 8;
          if ( fread( &(polyM->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
            Rprintf( "Error: reading shape file in C function linSample.\n" );
            PROTECT( results = allocVector( VECSXP, 1 ) );
            UNPROTECT( 1 );
            fclose( fptr );
            remove( TEMP_SHP_FILE );
            return results;
          }
          filePosition += 8;
        } 

        /* read M range */
        for ( i=0; i < 2; ++i ) {
          fread( &(polyM->mRange[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        }

        /* read M values */
        if ((polyM->mArray = (double *) malloc( sizeof(double) *
        	   polyM->numPoints)) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSample.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }
        for ( i=0; i < polyM->numPoints; ++i ) {
          fread( &(polyM->mArray[i]), sizeof(double), 1, fptr );
          filePosition += 8;
        } 

        /* add new polygon to the record struct */
        record.polyM = polyM;
        record.poly = NULL;
        record.polyZ = NULL;

      }

      /* find the dsgnmd weight array position for this record ID */
      for ( w = 0; w < dsgSize; ++w ) {
        if ( dsgnmdID[w] == record.number ) {
          break; 
        } 
      }

      if ( shape.shapeType == POLYLINE ) {

        /* go through each segment in this record */
        partIndx = 1; 
        for ( i = 0; i < record.poly->numPoints-1; ++i ) {

          /* if there are multiple parts, assume the parts are not connected */
          if ( record.poly->numParts > 1 && partIndx < record.poly->numParts ) {
            if ( (i + 1) == record.poly->parts[partIndx] ) {
              ++partIndx;
              continue;
            }
          }

          /* allocate new segment struct */
          if ( (seg = (Segment *) malloc( sizeof( Segment ) )) == NULL ) {
            Rprintf( "Error: Allocating memory in C function linSample.\n" );
            PROTECT( results = allocVector( VECSXP, 1 ) );
            UNPROTECT( 1 );
            fclose( fptr );
            remove( TEMP_SHP_FILE );
            return results;
          }

          /* get the length of the line that is inside the cell */
          length = lineLength( record.poly->points[i].X, 
                               record.poly->points[i].Y, 
                               record.poly->points[i+1].X, 
                               record.poly->points[i+1].Y, &cell, &seg );
       
          /* if this segment was inside the cell and there is a dsg weight */
          /* for it then add it to the list */ 
          if ( length > 0.0 && w < dsgSize ) {
            seg->recordNumber = record.number;
            seg->length = length * dsgnmd[w];
            addSegment( &segmentList, seg );
          } else {
            free( seg );
          }

        }

        /* deallocate memory for this record */
        free( record.poly->points );
        free( record.poly->parts );
        free( record.poly );

      } else if ( shape.shapeType == POLYLINE_Z ) {

        /* go through each segment in this record */
        partIndx = 1; 
        for ( i = 0; i < record.polyZ->numPoints-1; ++i ) {

          /* if there are multiple parts, assume the parts are not connected */
          if ( record.polyZ->numParts > 1 && partIndx < record.polyZ->numParts ) {
            if ( (i + 1) == record.polyZ->parts[partIndx] ) {
              ++partIndx;
              continue;
            }
          }

          /* allocate new segment struct */
          if ( (seg = (Segment *) malloc( sizeof( Segment ) )) == NULL ) {
            Rprintf( "Error: Allocating memory in C function linSample.\n" );
            PROTECT( results = allocVector( VECSXP, 1 ) );
            UNPROTECT( 1 );
            fclose( fptr );
            remove( TEMP_SHP_FILE );
            return results;
          }

          /* get the length of the line that is inside the cell */
          length = lineLength( record.polyZ->points[i].X, 
                               record.polyZ->points[i].Y, 
                               record.polyZ->points[i+1].X, 
                               record.polyZ->points[i+1].Y, &cell, &seg );
       
          /* if this segment was inside the cell and there is a dsg weight */
          /* for it then add it to the list */ 
          if ( length > 0.0 && w < dsgSize ) {
            seg->recordNumber = record.number;
            seg->length = length * dsgnmd[w];
            addSegment( &segmentList, seg );
          } else {
            free( seg );
          }

        }

        /* deallocate memory for this record */
        free( record.polyZ->mArray );
        free( record.polyZ->zArray );
        free( record.polyZ->points );
        free( record.polyZ->parts );
        free( record.polyZ );

      } else {

        /* go through each segment in this record */
        partIndx = 1; 
        for ( i = 0; i < record.polyM->numPoints-1; ++i ) {

          /* if there are multiple parts, assume the parts are not connected */
          if ( record.polyM->numParts > 1 && partIndx < record.polyM->numParts ) {
            if ( (i + 1) == record.polyM->parts[partIndx] ) {
              ++partIndx;
              continue;
            }
          }

          /* allocate new segment struct */
          if ( (seg = (Segment *) malloc( sizeof( Segment ) )) == NULL ) {
            Rprintf( "Error: Allocating memory in C function linSample.\n" );
            PROTECT( results = allocVector( VECSXP, 1 ) );
            UNPROTECT( 1 );
            fclose( fptr );
            remove( TEMP_SHP_FILE );
            return results;
          }

          /* get the length of the line that is inside the cell */
          length = lineLength( record.polyM->points[i].X, 
                               record.polyM->points[i].Y, 
                               record.polyM->points[i+1].X, 
                               record.polyM->points[i+1].Y, &cell, &seg );
       
          /* if this segment was inside the cell and there is a dsg weight */
          /* for it then add it to the list */ 
          if ( length > 0.0 && w < dsgSize ) {
            seg->recordNumber = record.number;
            seg->length = length * dsgnmd[w];
            addSegment( &segmentList, seg );
          } else {
            free( seg );
          }

        }

        /* deallocate memory for this record */
        free( record.polyM->mArray );
        free( record.polyM->points );
        free( record.polyM->parts );
        free( record.polyM );

      }

    }

    temp = segmentList;

    /* get the total length of all the segments in this cell */
    sumWl = 0.0;
    while ( temp ) {
      sumWl += temp->length;
      temp = temp->next;
    }

    /* randomly pick a point somewhere along the line of segments */
    pos = runif( 0.0, sumWl );

    temp = segmentList;
    cumSum = 0.0;
    while ( TRUE && temp != NULL ) {
      cumSum += temp->length;
      if ( pos < cumSum ) {
        break;
      }
      temp = temp->next;
    }
    samp[sampInd] = temp->recordNumber;

    /* find the dsgnmd weight array position for this record ID */
    for ( w = 0; w < dsgSize; ++w ) {
      if ( dsgnmdID[w] == temp->recordNumber ) {
        break; 
      } 
    }

    /* determine the coordinates for the sample point */
    len = (pos - cumSum) / dsgnmd[w];
    dx2 = temp->p2.X - temp->p1.X;
    dy2 = temp->p2.Y - temp->p1.Y;
    if ( dx2 != 0 ) {
      lx = sign(dx2) * sqrt( (len*len) / ( 1 + (dy2*dy2)/(dx2*dx2) ) );
      ly = lx * (dy2/dx2);
    } else {
      lx = 0.0;
      ly = -sign(dy2) * len;
    }
    x[sampInd] = temp->p1.X + lx;
    y[sampInd] = temp->p1.Y + ly;
    ++sampInd;
    deallocateSegments( segmentList );
    segmentList = NULL;
  }

  /* output the RNG state */
  PutRNGstate();

  /* convert arrays to an R object */
  PROTECT( results = allocVector( VECSXP, 3 ) );
  PROTECT( xVec = allocVector( REALSXP, vecSize ) );
  PROTECT( yVec = allocVector( REALSXP, vecSize ) );
  PROTECT( IDVec = allocVector( INTSXP, vecSize ) );
  for ( i = 0; i < vecSize; ++i ) {
    REAL( xVec )[i] = x[i];
    REAL( yVec )[i] = y[i];
    INTEGER( IDVec )[i] = samp[i];
  }
  SET_VECTOR_ELT( results, 0, xVec );
  SET_VECTOR_ELT( results, 1, yVec );
  SET_VECTOR_ELT( results, 2, IDVec );

  /* setup vector labels */
  PROTECT( colNamesVec = allocVector( STRSXP, 3 ) );
  SET_STRING_ELT( colNamesVec, 0, mkChar( "x" ) );
  SET_STRING_ELT( colNamesVec, 1, mkChar( "y" ) );
  SET_STRING_ELT( colNamesVec, 2, mkChar( "lid" ) );
  setAttrib( results, R_NamesSymbol, colNamesVec );

  /* clean up */
  if ( xc ) {
    free( xc );
  }
  if ( yc ) {
    free( yc );
  }
  if ( dsgnmd ) {
    free( dsgnmd );
  }
  if ( dsgnmdID ) {
    free( dsgnmdID );
  }
  if ( samp ) {
    free( samp );
  }
  if ( x ){
    free( x );
  }
  if ( y ) {
    free( y );
  }
  fclose( fptr );
  remove( TEMP_SHP_FILE );
  UNPROTECT( 5 );

  return results;
}
