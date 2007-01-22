/****************************************************************************** 
**  File:        grtsarea.c
**  
**  Purpose:     This file contains the functions used for the grtsarea R
**               function and are used with polygon shape types.  There is 
**               the pintFcn() function which is called from the numLevels()
**               function and two functions that are used for determineing if 
**               points are inside a polygon: pointInPolygonObj() and 
**               pointInPolygonFile().
**  Programmers: Christian Platt, Tom Kincaid
**  Created:     September 7, 2004
**  Revised:     May 10, 2006
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include "shapeParser.h"
#include "grts.h"

#define MIN(x,y) (x < y ? x : y)
#define MAX(x,y) (x > y ? x : y)

#define LEFT   1
#define RIGHT  2
#define BOTTOM 3
#define TOP    4

/* these functions are found in shapeParser.c */
extern int parseHeader( FILE * fptr, Shape * shape );
extern int readLittleEndian( unsigned char * buffer, int length );
extern int readBigEndian( unsigned char * buffer, int length );

/* these functions are found in grts.c */
extern int combineShpFiles( FILE * newShp, unsigned int * ids, int numIDs );
extern int createNewTempShpFile( FILE * newShp, char * shapeFileName, 
		                       unsigned int * ids, int numIDs );


/**********************************************************
** Function:   insidePolygon
**
** Purpose:    To determine if the sent point is inside the sent
**             polygon.
** Notes:      The code for this function was found at 
**             http://astronomy.swin.edu.au/~pbourke/geometry/insidepoly/
**             by Paul Bourke.
**             The polygon must be fully closed, meaning the first point
**             and last point must be the same.
** Algorithm:  It uses the concept of drawing a ray from the point outward
**             and counting how many lines it crosses.  Odd number means
**             it is inside odd amount means it's outside.
** Arguments:  polygon,  array of points that make up the polygon
**             N, number of points in the sent polygon
**             x, x coordinate of the point
**             y, y coordinate of the point
** Return:     1, if the sent point is inside the sent polygon
**             0, if the sent point is outside the sent polygon
***********************************************************/
int insidePolygon(Point * polygon, int N , double x, double y ) {

  int i;
  int counter = 0;
  double xinters;
  Point * p1;
  Point * p2;

  p1 = &(polygon[0]);
  for ( i = 1; i <= N; ++i ) {
    p2 = &(polygon[i % N]);
    if ( y > MIN(p1->Y, p2->Y) ) {
      if ( y <= MAX(p1->Y, p2->Y) ) {
        if ( x <= MAX(p1->X, p2->X) ) {
          if ( p1->Y != p2->Y ) {
            xinters = (y - p1->Y) * (p2->X - p1->X) / (p2->Y - p1->Y) + p1->X;
            if ( p1->X == p2->X || x <= xinters ) {
              ++counter;
            }
          }
        }
      }
    }
    p1 = p2;
  }

  if (counter % 2 == 0)
    /* outside */
    return 0;
  else
    /* inside */
    return 1;
}


/**********************************************************
** Function:   insideShape
**
** Purpose:    To fill the sent matrix array with 1's or 0's  
**             corresponding to whether each point in the x and
**             y arrays are inside or outside the shape represented 
**             by the sent shape file. These 1's or 0's are then multiplied
**             by the dsgnmd weight that correspondes to the record ID 
**             the point was found in (if it was actually found in one of
**             the records).  This function also writes the record number
**             of the records that each point is inside of to the matrixIDs
**             array.
** Algorithm:  To conserve memory this function reads in one record
**             at a time to process.  The polygons found
**             in these records are sent through the algorithm to see if
**             the points are in any of them.  If a point
**             is found to be in a polygon it's corresponding matrix
**             value is incremented by one.  After all the records have
**             been sent through the process, all the values in the
**             matrix are modded by two to determine whether each point
**             overall were in the shape or not.  The matrix value is then
**             multiplied by the sent dsgnmd weight value that correspondes
**             to the record ID the point was found to be inside of.
** Notes:      The matrix, x, and y arrays are all of the same size
**             which is sent in the size argument.
**             If a point is not in a record a -1 is written to the 
**             matrixIDs array at that position.
**             If we are NOT concerned with the matrixIDs a NULL value can
**             be sent to this function for the matrixIDs.  This function
**             checks to make sure it is not NULL before writing to it.
** Arguments:  matrix,  stores whether the points are in or outside the 
**                      polygon multiplied by the corresponding dsgnmd weight
**             matrixIDs,  stores the record ID of the records that the points
**                         are inside of.  Set to -1 if the point is not in 
**                         any records.  This pointer may be NULL if there is
**                         no need to store record IDs.
**             x,  array of the x coordinates for the points
**             y,  array of the y coordinates for the points
**             size,  size of the x and y arrays
**             shape, struct that stores shape infor and data
**             fptr,   pointer to the shape file
**             dsgnmdID, array of record IDs which have weights and should be
**                       used in the calculations
**             dsgnmd,   array of weights corresponding to the above IDs
**             dsgSize,  number of IDs in the dsgnmdID array
** Return:     1,  on success
**             -1, on error
***********************************************************/
int insideShape( double ** matrix, unsigned int ** matrixIDs, double * x, 
                 double * y, int size, Shape * shape, FILE * fptr, 
                 unsigned int * dsgnmdID, double * dsgnmd, int dsgSize ) {

  int i, j, k;                      /* loop counters */

  /* variable used to iterate through the differnet parts for records */
  /* with more than one part */
  Point * part = NULL;              /* stores points makeing up the part */
  int partSize;                     /* number of points in part */
  int pidx;                         /* index into part array */

  Point bdrBox[5];                  /* temp storage for the record bounding */
                                    /* boxes */
  Record * temp = NULL;             /* used for traversing linked list of */
                                    /* records */
  unsigned int filePosition = 100;  /* current byte offset that fptr is at */
                                    /* 100 byte mark is the beginning of the */
                                    /* record data */
  unsigned char buffer[4];          /* temp buffer for reading from file */
  Polygon * poly;                   /* temp Polygon storage */
  PolygonZ * polyZ;                 /* temp PolygonZ storage */
  PolygonM * polyM;                 /* temp PolygonM storage */
  Record record;                    /* record used for parsing records */
  unsigned int tempID = -1;         /* stores current record ID */

  /* initialize the shape struct */
  shape->records = NULL;

  /* initialize the matrix to all 0's */
  for ( i = 0; i < size; ++i ) {
    (*matrix)[i] = 0.0;
  }

  /* set the correct file position  */
  fseek( fptr, 100, SEEK_SET );
  filePosition = 100;

  /* read through all the records found in the file */
  while ( filePosition < shape->fileLength*2 ) {

    /* read the record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 ); 
    filePosition += 4;

    /* ignore content length */
    fread( buffer, sizeof(char), 4, fptr );
    filePosition += 4;

    /* ignore shape type */
    fread( buffer, sizeof(char), 4, fptr );
    filePosition += 4;

    if ( shape->shapeType == POLYGON ) {

      /* allocate a new polygon */
      if ( (poly = (Polygon *) malloc( sizeof(Polygon) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }

      /* read box data */
      for ( i = 0; i < 4; ++i ) {
        fread( &(poly->box[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read the number of parts */
      fread( buffer, sizeof(char), 4, fptr );
      poly->numParts = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read the number of points */
      fread( buffer, sizeof(char), 4, fptr );
      poly->numPoints = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read parts info */ 
      if ((poly->parts = (int *) malloc( sizeof(int) * poly->numParts))==NULL) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }
      for ( i = 0; i < poly->numParts; ++i ) {
        fread( &(poly->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read points data */
      if ((poly->points = (Point *) malloc( sizeof(Point) * poly->numPoints ))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }
      for ( i = 0; i < poly->numPoints; ++i ) {
        fread( &(poly->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(poly->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function insideShape.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.poly = poly;

    } else if ( shape->shapeType == POLYGON_Z ) {

      /* allocate a new polygon */
      if ( (polyZ = (PolygonZ *) malloc( sizeof(PolygonZ) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }

      /* read box data */
      for ( i = 0; i < 4; ++i ) {
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
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }
      for ( i = 0; i < polyZ->numParts; ++i ) {
        fread( &(polyZ->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read points data */
      if ((polyZ->points = (Point *) malloc( sizeof(Point) * polyZ->numPoints ))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }
      for ( i = 0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(polyZ->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function insideShape.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* read Z range */
      for ( i = 0; i < 2; ++i ) {
        fread( &(polyZ->zRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read Z values */
      if ((polyZ->zArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }
      for ( i = 0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->zArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* read M range */
      for ( i = 0; i < 2; ++i ) {
        fread( &(polyZ->mRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read M values */
      if ((polyZ->mArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }
      for ( i = 0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->mArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.polyZ = polyZ;

    } else {

      /* allocate a new polygon */
      if ( (polyM = (PolygonM *) malloc( sizeof(PolygonM) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }

      /* read box data */
      for ( i = 0; i < 4; ++i ) {
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
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }
      for ( i = 0; i < polyM->numParts; ++i ) {
        fread( &(polyM->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read points data */
      if ((polyM->points = (Point *) malloc( sizeof(Point) * polyM->numPoints ))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function insideShape.\n" );
        return -1;
      }
      for ( i = 0; i < polyM->numPoints; ++i ) {
        fread( &(polyM->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(polyM->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function insideShape.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* read M range */
      for ( i = 0; i < 2; ++i ) {
        fread( &(polyM->mRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read M values */
      if ((polyM->mArray = (double *) malloc( sizeof(double)*polyM->numPoints))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function lintFcn.\n" );
        return -1;
      }
      for ( i = 0; i < polyM->numPoints; ++i ) {
        fread( &(polyM->mArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* add new polygon to the record struct */
      record.polyM = polyM;

    }

    /* build the point in polygon matrix */
    for ( i = 0; i < size; ++i ) {

      /* check to see if this point was already found in a record */
      if( (*matrix)[i] > 0.0 ) {
        continue;
      }

      temp = &record;
      tempID = -1;

      /* check to see if we are using weighted polygons */
      if ( dsgnmdID != NULL ) {

        /* check to make sure the current record is to be used */
        for ( j = 0; j < dsgSize; ++j ) {
          if ( dsgnmdID[j] == temp->number ) {
            tempID = j;
            break; 
          }
        } 

        /* if the record ID wasn't found skip it and go onto the next one */
        if ( tempID == -1 ) {
          continue;
        }
      }

      /* get the records bounding box */
      if ( shape->shapeType == POLYGON ) {
        bdrBox[0].X = temp->poly->box[0];
        bdrBox[0].Y = temp->poly->box[1];
        bdrBox[1].X = temp->poly->box[0];
        bdrBox[1].Y = temp->poly->box[3];
        bdrBox[2].X = temp->poly->box[2];
        bdrBox[2].Y = temp->poly->box[3];
        bdrBox[3].X = temp->poly->box[2];
        bdrBox[3].Y = temp->poly->box[1];
        bdrBox[4].X = temp->poly->box[0];
        bdrBox[4].Y = temp->poly->box[1];
      } else if ( shape->shapeType == POLYGON_Z ) {
        bdrBox[0].X = temp->polyZ->box[0];
        bdrBox[0].Y = temp->polyZ->box[1];
        bdrBox[1].X = temp->polyZ->box[0];
        bdrBox[1].Y = temp->polyZ->box[3];
        bdrBox[2].X = temp->polyZ->box[2];
        bdrBox[2].Y = temp->polyZ->box[3];
        bdrBox[3].X = temp->polyZ->box[2];
        bdrBox[3].Y = temp->polyZ->box[1];
        bdrBox[4].X = temp->polyZ->box[0];
        bdrBox[4].Y = temp->polyZ->box[1];
      } else {
        bdrBox[0].X = temp->polyM->box[0];
        bdrBox[0].Y = temp->polyM->box[1];
        bdrBox[1].X = temp->polyM->box[0];
        bdrBox[1].Y = temp->polyM->box[3];
        bdrBox[2].X = temp->polyM->box[2];
        bdrBox[2].Y = temp->polyM->box[3];
        bdrBox[3].X = temp->polyM->box[2];
        bdrBox[3].Y = temp->polyM->box[1];
        bdrBox[4].X = temp->polyM->box[0];
        bdrBox[4].Y = temp->polyM->box[1];
     }

      /* make sure the point is at least in the bounding box before */
      /* checking the record */
      if ( insidePolygon( bdrBox, 5, x[i], y[i] ) == 1 ) {

        if ( shape->shapeType == POLYGON ) {

        /* if there are more than one part we need to check them separately*/
          if ( temp->poly->numParts > 1 ) {
              for ( k = 0; k < temp->poly->numParts; ++k ) {
              if ( part ) {
                free( part );
              }

              /* check to see if this is the last part */ 
              if ( k == temp->poly->numParts - 1 ) {

                /* store part data into part array */
                partSize = temp->poly->numPoints - temp->poly->parts[k];
                if ( (part = (Point *) malloc( sizeof( Point ) * partSize )) 
                   == NULL ) {
                  Rprintf( "Error: Allocating memory in C function insideShape.\n" );
                  return -1;
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
                part = (Point *) malloc( sizeof( Point ) * partSize );
                pidx = 0;
                for(j=temp->poly->parts[k]; j < temp->poly->parts[k+1]; ++j){
                  part[pidx] = temp->poly->points[j];
                  ++pidx; 
                }

              }

              /* see if the point is inside this part */
              if( insidePolygon(part,partSize, x[i], y[i] ) == 1 ){
                ++((*matrix)[i]);
              }
            }  

          /* only one part so check the entire record */
          } else {

            /* now check the polygon */
            if( insidePolygon( temp->poly->points, temp->poly->numPoints,
               x[i], y[i]) == 1 ) {
              ++((*matrix)[i]);
            }
          }

        } else if ( shape->shapeType == POLYGON_Z ) {

        /* if there are more than one part we need to check them separately*/
          if ( temp->polyZ->numParts > 1 ) {
              for ( k = 0; k < temp->polyZ->numParts; ++k ) {
              if ( part ) {
                free( part );
              }

              /* check to see if this is the last part */ 
              if ( k == temp->polyZ->numParts - 1 ) {

                /* store part data into part array */
                partSize = temp->polyZ->numPoints - temp->polyZ->parts[k];
                if ( (part = (Point *) malloc( sizeof( Point ) * partSize )) 
                   == NULL ) {
                  Rprintf( "Error: Allocating memory in C function insideShape.\n" );
                  return -1;
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
                part = (Point *) malloc( sizeof( Point ) * partSize );
                pidx = 0;
                for(j=temp->polyZ->parts[k]; j < temp->polyZ->parts[k+1]; ++j){
                  part[pidx] = temp->polyZ->points[j];
                  ++pidx; 
                }

              }

              /* see if the point is inside this part */
              if( insidePolygon(part,partSize, x[i], y[i] ) == 1 ){
                ++((*matrix)[i]);
              }
            }  

          /* only one part so check the entire record */
          } else {

            /* now check the polygon */
            if( insidePolygon( temp->polyZ->points, temp->polyZ->numPoints,
               x[i], y[i]) == 1 ) {
              ++((*matrix)[i]);
            }
          }
        } else {

        /* if there are more than one part we need to check them separately*/
          if ( temp->polyM->numParts > 1 ) {
              for ( k = 0; k < temp->polyM->numParts; ++k ) {
              if ( part ) {
                free( part );
              }

              /* check to see if this is the last part */ 
              if ( k == temp->polyM->numParts - 1 ) {

                /* store part data into part array */
                partSize = temp->polyM->numPoints - temp->polyM->parts[k];
                if ( (part = (Point *) malloc( sizeof( Point ) * partSize )) 
                   == NULL ) {
                  Rprintf( "Error: Allocating memory in C function insideShape.\n" );
                  return -1;
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
                part = (Point *) malloc( sizeof( Point ) * partSize );
                pidx = 0;
                for(j=temp->polyM->parts[k]; j < temp->polyM->parts[k+1]; ++j){
                  part[pidx] = temp->polyM->points[j];
                  ++pidx; 
                }

              }

              /* see if the point is inside this part */
              if( insidePolygon(part,partSize, x[i], y[i] ) == 1 ){
                ++((*matrix)[i]);
              }
            }  

          /* only one part so check the entire record */
          } else {

            /* now check the polygon */
            if( insidePolygon( temp->polyM->points, temp->polyM->numPoints,
               x[i], y[i]) == 1 ) {
              ++((*matrix)[i]);
            }
          }
        } 
      }

      /* finalize the matrix values to either in or out */
      (*matrix)[i] = ((int)(*matrix)[i]) % 2;

      /* do the weight adjustment if the point is in this record */
      if ( dsgnmdID != NULL && ((int)(*matrix)[i]) == 1 ) {
        (*matrix)[i] *= dsgnmd[tempID];
      }

      /* add the corresponding record ID to the IDs matrix */
      if ( matrixIDs != NULL ) {
        if ( (*matrix)[i] > 0.0 ) {
          (*matrixIDs)[i] = temp->number;
        } else {
          (*matrixIDs)[i] = -1;
        }
      }
    }

    if ( shape->shapeType == POLYGON ) {
      free( temp->poly->parts );
      free( temp->poly->points );
      free( temp->poly );
    } else if ( shape->shapeType == POLYGON_Z ) {
      free( temp->polyZ->parts );
      free( temp->polyZ->points );
      free( temp->polyZ );
    } else {
      free( temp->polyM->parts );
      free( temp->polyM->points );
      free( temp->polyM );
    }

  }

  /* do cleanup */
  if ( part ) {
    free( part );
  }

  return 1;
}


/**********************************************************
** Function:  polygonArea
**
** Purpose:   returns the area of the polygon defined by
**            the sent array of points (polygon).
** Notes:     This function was found online at 
**            http://astronomy.swin.edu.au/~pbourke/geometry/polyarea/source1.c
**            by Paul Bourke, July 1988
** Arguments: polygon, array of points defining the polygon
**            size,  number of points in array
** Return:    area,   area of polygon
***********************************************************/
double polygonArea( Point * polygon, int size ) {

   int i , j;
   double area = 0;

   for ( i = 0; i < size; i++ ) {
      j = (i + 1) % size;
      area += polygon[i].X * polygon[j].Y;
      area -= polygon[i].Y * polygon[j].X;
   }

   area /= 2;
   return -area;
}


Point intersect( Cell * cell, Point * p1, Point * p2, int side, Point * z ) {

  Point newPoint;
  double slope = 0.0;

  if ( p2->X != p1->X ) {
    slope = (p2->Y - p1->Y) / ( p2->X - p1->X );
  }

  switch ( side ) {

    case LEFT: z->X = cell->xMin;
               z->Y = p2->Y + (cell->xMin - p2->X) * slope;
               break;

    case RIGHT: z->X = cell->xMax;
                z->Y = p2->Y + (cell->xMax - p2->X) * slope;
                break;

    case BOTTOM: z->Y = cell->yMin;
                 if ( p1->X == p2->X ) {
                   z->X = p2->X;
                 } else {
                   z->X = p2->X + (cell->yMin - p2->Y)/slope;
                 }
                 break;

    case TOP:  z->Y = cell->yMax;
               if ( p1->X == p2->X ) {
                 z->X = p2->X;
               } else {
                 z->X = p2->X + (cell->yMax - p2->Y)/slope;
               }
               break;

  } 

  return newPoint;
}


int inside( Cell * cell, Point * p, int side ) {

  int c = 1;

  switch ( side ) {
    case LEFT: if ( p->X < cell->xMin ) {
                 c = 0;
               }
               break;

    case RIGHT: if ( p->X > cell->xMax ) {
                  c = 0;
                }
                break;

    case BOTTOM: if ( p->Y < cell->yMin ) {
                   c = 0;
                 }
                 break;

    case TOP:  if ( p->Y > cell->yMax ) {
                 c = 0;
               }
               break;
  }

  return c;
}


double clipPolygonArea( Cell * cell, Point * points, int start, int end ) {

  int i, j;
  int side;
  int cou;
  int n = (end - start);    /* number of points in polygon part */
  Point * clippedPoly;
  Point * tempPoly;
  Point z;
  double area = 0.0;

  clippedPoly = (Point *) malloc( sizeof(Point) * (( 2 * n ) + 1) );
  tempPoly = (Point *) malloc( sizeof(Point) * (( 2 * n ) + 1) );

  j = 0;
  for ( i = start; i <= end; ++i ) {
    clippedPoly[j].X = points[i].X; 
    clippedPoly[j].Y = points[i].Y; 
    tempPoly[j].X = points[i].X; 
    tempPoly[j].Y = points[i].Y; 
    ++j;
  }

  for ( side = LEFT; side <= TOP; ++side ) {
    cou = -1;

    for ( i = 0; i < n; ++i ) {
  
      if ( (inside(cell, &(clippedPoly[i]), side) == 0)  &&
           (inside(cell, &(clippedPoly[i+1]), side) == 1 ) ) {
        intersect( cell, &(clippedPoly[i]), &(clippedPoly[i+1]), side, &z);
        tempPoly[++cou].X = z.X;
        tempPoly[cou].Y = z.Y;
        tempPoly[++cou].X = clippedPoly[i+1].X;
        tempPoly[cou].Y = clippedPoly[i+1].Y;
      } else if ((inside(cell, &(clippedPoly[i]), side) == 1 ) &&
                 (inside(cell, &(clippedPoly[i+1]), side) == 1 ) ){
        tempPoly[++cou].X = clippedPoly[i+1].X;
        tempPoly[cou].Y = clippedPoly[i+1].Y;
      } else if ((inside(cell, &(clippedPoly[i]), side) == 1 ) &&
                 (inside(cell, &(clippedPoly[i+1]), side)  == 0 ) ) {
        intersect( cell, &(clippedPoly[i]), &(clippedPoly[i+1]), side, &z );
        tempPoly[++cou].X = z.X;
        tempPoly[cou].Y = z.Y;
      }
    }
    tempPoly[++cou].X = tempPoly[0].X;
    tempPoly[cou].Y = tempPoly[0].Y;
    n = cou;

    for ( i = 0; i <= n; ++i ) {
      clippedPoly[i].X = tempPoly[i].X;
      clippedPoly[i].Y = tempPoly[i].Y;
    }
  }

  area = polygonArea( clippedPoly, n+1 );

  free( clippedPoly );
  free( tempPoly );


  return area;
}


/**********************************************************
** Function:   areaIntersection
**
** Purpose:    This implements the pint.fcn and the bi-variate Simpson's
**             rule integration using npt^2 points that are found in the
**             R code equivalents.  
** Algorithm:  Follows the same algorithm used in the R code versions.
** Notes:      This function is called from the numLevels function found in
**             grts.c and is used on polygons shapefiles.
** Arguments:  celWts, array of doubles representing the cell weights
**             xc,     array of x coordinates
**             yc,     array of y coordiantes
**             dx,     amount to shift x coordinates
**             dy,     amount to shift y coordiantes
**             size,   length of the xc and yc arrays
**             shape,  shape struct that contains header info for the
**                     shape file we are working with. It is used to store
**                     a subset of the records at a time.
**             fptr,   pointer to the shape file we are using
**             dsgnmdID, array of record IDs which have weights and should be
**                       used in the calculations
**             dsgnmd,   array of weights corresponding to the above IDs
**             dsgSize,  number of IDs in the dsgnmdID array
** Return:     1,   on success
**             -1,  on error
***********************************************************/
int areaIntersection( double ** celWts, double * xc, double * yc, double dx,
    double dy, int size, Shape * shape, FILE * fptr, unsigned int * dsgnmdID,
    double * dsgnmd,int dsgSize ) { 

  int i, j, k, w;               /* loop counters */
  int row;                      /* loop counter */
  Cell cell;                    /* temp storage for a cell */
  unsigned int filePosition = 100;  /* byte offset within the shape file */
  Record record;                /* temp record storage */
  unsigned char buffer[4];      /* temp buffer for reading from file */
  Polygon * poly;               /* temp Polygon storage */
  PolygonZ * polyZ;             /* temp PolygonZ storage */
  PolygonM * polyM;             /* temp PolygonM storage */
  int partNumber = 0;           /* counts which part we are on for a given */
                                /* record */
  int partSize;                 /* number of points that make up a part */
  int pidx;                     /* index into part array */
  Point * part = NULL;          /* used for determing ring direction for */
                                /* individual parts */
  double * areas;

  /* initialize the shape struct */
  shape->records = NULL;

  /* initialize all the cell weights */
  for ( row = 0; row < size; ++row ) {
    (*celWts)[row] = 0.0;
  }

  areas = (double *) malloc( sizeof(double) * size );
 
  /* get to the correct spot in the shape file to read all the records */ 
  fseek( fptr, 100, SEEK_SET );
  filePosition = 100;

  /* read all the records found in the file */
  while ( filePosition < shape->fileLength*2 ) {

    /* ignore record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 ); 
    filePosition += 4;

    /* ignore content length */
    fread( buffer, sizeof(char), 4, fptr );
    filePosition += 4;

    /* ignore shape type */
    fread( buffer, sizeof(char), 4, fptr );
    filePosition += 4;

    if ( shape->shapeType == POLYGON ) {

      /* allocate a new Polygon */
      if ( (poly = (Polygon *) malloc( sizeof(Polygon) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }

      /* read box data */
      for ( i = 0; i < 4; ++i ) {
        fread( &(poly->box[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read the number of parts */
      fread( buffer, sizeof(char), 4, fptr );
      poly->numParts = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read the number of points */
      fread( buffer, sizeof(char), 4, fptr );
      poly->numPoints = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read parts info */ 
      if ((poly->parts = (int *) malloc( sizeof(int) * poly->numParts ))==NULL){
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < poly->numParts; ++i ) {
        fread( &(poly->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read points data */
      if ((poly->points = (Point *) malloc( sizeof(Point) * poly->numPoints ))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < poly->numPoints; ++i ) {
        fread( &(poly->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(poly->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function areaIntersection.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* calculate areas of the parts */
      /* if there is more than one part, check them separately */

      if ( poly->numParts > 1 ) {
        partNumber = 0;

        /* find the dsgnmd weight array position for this record ID */
        for ( w = 0; w < dsgSize; ++w ) {
          if ( dsgnmdID[w] == record.number ) {
            break; 
          } 
        }

        /* zero out the areas */
        for ( i = 0; i < size; ++i ) {
          areas[i] = 0.0;
        }

        for ( k = 0; k < poly->numParts; ++k ) {
          if ( part ) {
            free( part );
            part = NULL;
          }

          /* check to see if this is the last part */ 
          if ( k == poly->numParts - 1 ) {

            /* store part data into part array */
            partSize = poly->numPoints - poly->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize )) 
               == NULL ) {
              Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
              return -1;
            }
            pidx = 0;
            for( j=poly->parts[k]; j < poly->numPoints; ++j ) {
              part[pidx] = poly->points[j];
              ++pidx; 
            }

          /* not the last part */
          } else {

            /* store part data into part array */
            partSize = poly->parts[k+1] - poly->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize ))
               == NULL ) {
              Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
              return -1;
            }
            pidx = 0;
            for(j = poly->parts[k]; j < poly->parts[k+1]; ++j) {
              part[pidx] = poly->points[j];
              ++pidx; 
            }

          }
 
          /* go through each cell and calc the area within that cell */
          for ( i = 0; i < size; ++i ) {
  
            /* form the cell's points */
            cell.xMin = xc[i] - dx;
            cell.yMin = yc[i] - dy;
            cell.xMax = xc[i];
            cell.yMax = yc[i];
            if ( w < dsgSize ) {
              areas[i] += clipPolygonArea(&cell,part,0,partSize-1) * dsgnmd[w];
            }
          }
          ++partNumber;
        }  

        /* if the total area for all the parts is negative, don't add it */
        for ( i = 0; i < size; ++i ) {
          if ( areas[i] > 0.0 ) {
            (*celWts)[i] += areas[i];
          } 
        }

        if ( part ) {
          free( part );
          part = NULL;
        }

      /* only one part so according to the ESRI docs it must be positive area*/
      } else {

        /* find the dsgnmd weight array position for this record ID */
        for ( w = 0; w < dsgSize; ++w ) {
          if ( dsgnmdID[w] == record.number ) {
            break; 
          } 
        }

        for ( i = 0; i < size; ++i ) {

          /* form the cell's points */
          cell.xMin = xc[i] - dx;
          cell.yMin = yc[i] - dy;
          cell.xMax = xc[i];
          cell.yMax = yc[i];
          if ( w < dsgSize ) {
            (*celWts)[i] +=clipPolygonArea(&cell,poly->points,0,poly->numPoints-
            1) * dsgnmd[w];
          }
        }
      }
      free( poly->parts );
      free( poly->points );
      free( poly );

    } else if ( shape->shapeType == POLYGON_Z ) {

      /* allocate a new PolygonZ */
      if ( (polyZ = (PolygonZ *) malloc( sizeof(PolygonZ) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }

      /* read box data */
      for ( i = 0; i < 4; ++i ) {
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
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < polyZ->numParts; ++i ) {
        fread( &(polyZ->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read points data */
      if ((polyZ->points = (Point *) malloc( sizeof(Point) * polyZ->numPoints ))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(polyZ->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function areaIntersection.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* read Z range */
      for ( i = 0; i < 2; ++i ) {
        fread( &(polyZ->zRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read Z values */
      if ((polyZ->zArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->zArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* read M range */
      for ( i = 0; i < 2; ++i ) {
        fread( &(polyZ->mRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read M values */
      if ((polyZ->mArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->mArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* calculate areas of the parts */
      /* if there is more than one part, check them separately */

      if ( polyZ->numParts > 1 ) {
        partNumber = 0;

        /* find the dsgnmd weight array position for this record ID */
        for ( w = 0; w < dsgSize; ++w ) {
          if ( dsgnmdID[w] == record.number ) {
            break; 
          } 
        }

        /* zero out the areas */
        for ( i = 0; i < size; ++i ) {
          areas[i] = 0.0;
        }

        for ( k = 0; k < polyZ->numParts; ++k ) {
          if ( part ) {
            free( part );
            part = NULL;
          }

          /* check to see if this is the last part */ 
          if ( k == polyZ->numParts - 1 ) {

            /* store part data into part array */
            partSize = polyZ->numPoints - polyZ->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize )) 
               == NULL ) {
              Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
              return -1;
            }
            pidx = 0;
            for( j=polyZ->parts[k]; j < polyZ->numPoints; ++j ) {
              part[pidx] = polyZ->points[j];
              ++pidx; 
            }

          /* not the last part */
          } else {

            /* store part data into part array */
            partSize = polyZ->parts[k+1] - polyZ->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize ))
               == NULL ) {
              Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
              return -1;
            }
            pidx = 0;
            for(j = polyZ->parts[k]; j < polyZ->parts[k+1]; ++j) {
              part[pidx] = polyZ->points[j];
              ++pidx; 
            }

          }
 
          /* go through each cell and calc the area within that cell */
          for ( i = 0; i < size; ++i ) {
  
            /* form the cell's points */
            cell.xMin = xc[i] - dx;
            cell.yMin = yc[i] - dy;
            cell.xMax = xc[i];
            cell.yMax = yc[i];

             if ( w < dsgSize ) {
              areas[i] += clipPolygonArea(&cell,part,0,partSize-1) * dsgnmd[w];
            }
          }
          ++partNumber;
        }  

        /* if the total area for all the parts is negative, don't add it */
        for ( i = 0; i < size; ++i ) {
          if ( areas[i] > 0.0 ) {
            (*celWts)[i] += areas[i];
          } 
        }

        if ( part ) {
          free( part );
          part = NULL;
        }

      /* only one part so according to the ESRI docs it must be positive area*/
      } else {

        /* find the dsgnmd weight array position for this record ID */
        for ( w = 0; w < dsgSize; ++w ) {
          if ( dsgnmdID[w] == record.number ) {
            break; 
          } 
        }

        for ( i = 0; i < size; ++i ) {

          /* form the cell's points */
          cell.xMin = xc[i] - dx;
          cell.yMin = yc[i] - dy;
          cell.xMax = xc[i];
          cell.yMax = yc[i];

          if ( w < dsgSize ) {
            (*celWts)[i] +=clipPolygonArea(&cell,polyZ->points,0,
            polyZ->numPoints-1) * dsgnmd[w];
          }
        }
      }
      free( polyZ->parts );
      free( polyZ->points );
      free( polyZ );

    } else {

      /* allocate a new PolygonM */
      if ( (polyM = (PolygonM *) malloc( sizeof(PolygonM) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }

      /* read box data */
      for ( i = 0; i < 4; ++i ) {
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
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < polyM->numParts; ++i ) {
        fread( &(polyM->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 

      /* read points data */
      if ((polyM->points = (Point *) malloc( sizeof(Point) * polyM->numPoints ))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < polyM->numPoints; ++i ) {
        fread( &(polyM->points[i].X), sizeof(double), 1, fptr );
        filePosition += 8;
        if ( fread( &(polyM->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: reading shape file in C function areaIntersection.\n" );
          return -1;
        }
        filePosition += 8;
      } 

      /* read M range */
      for ( i = 0; i < 2; ++i ) {
        fread( &(polyM->mRange[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      }

      /* read M values */
      if ((polyM->mArray = (double *) malloc( sizeof(double)*polyM->numPoints))
         == NULL ) {
        Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
        return -1;
      }
      for ( i = 0; i < polyM->numPoints; ++i ) {
        fread( &(polyM->mArray[i]), sizeof(double), 1, fptr );
        filePosition += 8;
      } 

      /* calculate areas of the parts */
      /* if there is more than one part, check them separately */

      if ( polyM->numParts > 1 ) {
        partNumber = 0;

        /* find the dsgnmd weight array position for this record ID */
        for ( w = 0; w < dsgSize; ++w ) {
          if ( dsgnmdID[w] == record.number ) {
            break; 
          } 
        }

        /* zero out the areas */
        for ( i = 0; i < size; ++i ) {
          areas[i] = 0.0;
        }

        for ( k = 0; k < polyM->numParts; ++k ) {
          if ( part ) {
            free( part );
            part = NULL;
          }

          /* check to see if this is the last part */ 
          if ( k == polyM->numParts - 1 ) {

            /* store part data into part array */
            partSize = polyM->numPoints - polyM->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize )) 
               == NULL ) {
              Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
              return -1;
            }
            pidx = 0;
            for( j=polyM->parts[k]; j < polyM->numPoints; ++j ) {
              part[pidx] = polyM->points[j];
              ++pidx; 
            }

          /* not the last part */
          } else {

            /* store part data into part array */
            partSize = polyM->parts[k+1] - polyM->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize ))
               == NULL ) {
              Rprintf( "Error: Allocating memory in C function areaIntersection.\n" );
              return -1;
            }
            pidx = 0;
            for(j = polyM->parts[k]; j < polyM->parts[k+1]; ++j) {
              part[pidx] = polyM->points[j];
              ++pidx; 
            }

          }
 
          /* go through each cell and calc the area within that cell */
          for ( i = 0; i < size; ++i ) {
  
            /* form the cell's points */
            cell.xMin = xc[i] - dx;
            cell.yMin = yc[i] - dy;
            cell.xMax = xc[i];
            cell.yMax = yc[i];

             if ( w < dsgSize ) {
              areas[i] += clipPolygonArea(&cell,part,0,partSize-1) * dsgnmd[w];
            }
          }
          ++partNumber;
        }  

        /* if the total area for all the parts is negative, don't add it */
        for ( i = 0; i < size; ++i ) {
          if ( areas[i] > 0.0 ) {
            (*celWts)[i] += areas[i];
          } 
        }

        if ( part ) {
          free( part );
          part = NULL;
        }

      /* only one part so according to the ESRI docs it must be positive area*/
      } else {

        /* find the dsgnmd weight array position for this record ID */
        for ( w = 0; w < dsgSize; ++w ) {
          if ( dsgnmdID[w] == record.number ) {
            break; 
          } 
        }

        for ( i = 0; i < size; ++i ) {

          /* form the cell's points */
          cell.xMin = xc[i] - dx;
          cell.yMin = yc[i] - dy;
          cell.xMax = xc[i];
          cell.yMax = yc[i];

          if ( w < dsgSize ) {
            (*celWts)[i] +=clipPolygonArea(&cell,polyM->points,0,
            polyM->numPoints-1) * dsgnmd[w];
          }
        }
      }
      free( polyM->parts );
      free( polyM->points );
      free( polyM );

    }

  }

  free( areas );
  return 1;
}


/**********************************************************
** Function:   pointInPolygonFile
**
** Purpose:    Create an R vector of 1 or 0's 
**             corresponding to whether each point in the xcsVec and
**             ycsVec vectors are inside or outside the shape represented 
**             by the sent shape file.  These 1's and 0's are then multiplied
**             by the corresponding weights found in the dsgnmdVec vector.
**             The R vector will also contain a vector of all the record IDs
**             of the records each point was found in.  Values of -1 are 
**             listed for those points that are not inside any records.
** Algorithm:  The function reads in the header information from the sent
**             file and then sends the matrix aray to the insideShape() 
**             function along with the file ptr and points.  The insideShape()
**             function parses the rest of the shape file a piece at a time
**             to determine which points are inside the shape and which are
**             outside.  The results are written to the matrix array.
** Notes:      The matrix array will be the same size as the xcs and ycs
**             arrays/vectors.
** Arguments:  xcsVec,   vector of all the x coordinates of the points
**             ycsVec,   vector of all the y coordinates of the points
**             dsgnmdIDVec, vector of the record IDs that should be used
**                          in the calculations
**             dsgnmdVec,  weights corresponding to the above IDs
** Return:     results,  R vector of 1 and 0's (multiplied by the corresponding
**                       dsgnmd weight value) corresponding to whether 
**                       each point is inside or outside the shape and a 
**                       vector of record IDs that each point is inside of.
***********************************************************/
SEXP pointInPolygonFile( SEXP fileNamePrefix, SEXP xcsVec, SEXP ycsVec, 
                         SEXP dsgnmdIDVec, SEXP dsgnmdVec ) {

  int i;                 /* loop counter */
  double * matrix = NULL;   /* used to store array of 1 or 0's represnting */
                         /* whether corresponding point is inside or outside */
                         /* polygon */
  unsigned int * matrixIDs = NULL; /*IDs of the records that each point is in*/
  FILE * fptr = NULL;    /* pointer to the shape file */
  Shape shape;           /* used to store shape file info and data */
  unsigned int vecSize = length( xcsVec );
  double * xcs = NULL;   /* array that stores values found in xcsVec R vector */
  double * ycs = NULL;   /* array that stores values found in ycsVec R vector */
  SEXP results = NULL;   /* R object used to return the "matrix" back to R */
  SEXP matrixVec, matrixIDsVec, colNamesVec;
  unsigned int * dsgnmdID = NULL; /*array of the ID numbers that have weights */
  double * dsgnmd = NULL;       /* array of weights that corresponde to the */
                                /* to the array of ID numbers */
  int dsgSize = length( dsgnmdIDVec ); /* number of IDs in the dsgnmdID array */
  FILE * newShp = NULL;   /* pointer to the temporary .shp file */
  char * shpFileName = NULL;  /* stores full shape file name */
  int singleFile = FALSE;

  /* see if a specific file was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full .shp file name */
    if ((shpFileName = (char *)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
                                              + strlen(".shp") + 1)) == NULL ){
      Rprintf( "Error: Allocating memory in C function pointInPolygonFile.\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return results;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix,0)));
    strcat( shpFileName, ".shp" );
    singleFile = TRUE;
  }

  /* create the new temporary .shp file */
  if ( ( newShp = fopen( TEMP_SHP_FILE, "wb" )) == NULL ) {
    Rprintf( "Error: Creating temporary .shp file %s in C function pointInPolygonFile.\n", TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* copy the dsgnmd poly IDs into a C array */
  if ( (dsgnmdID = (unsigned int *) malloc( sizeof( unsigned int ) * dsgSize))
                                                         == NULL ) {
    Rprintf( "Error: Allocating memory in C function numLevels.\n" );
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
      Rprintf( "Error: Combining multiple shapefiles in C function pointInPolygonFile.\n" );
      return results; 
    }
    fclose( newShp );

  } else {

    /* create a temporary .shp file containing the sent .shp file */
    if (createNewTempShpFile( newShp, shpFileName, dsgnmdID, dsgSize ) == -1 ) {
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( newShp );
      remove( TEMP_SHP_FILE );
      Rprintf( "Error: Creating temporary shapefile in C function pointInPolygonFile.\n" );
      return results; 
    }
    fclose( newShp );
  }

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;

  /* open the temporary .shp file */
  if ( (fptr = fopen( TEMP_SHP_FILE, "rb" )) == NULL ) {
    Rprintf( "Error: Opening shape file in C function pointInPolygonFile.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* parse main file header */
  if ( parseHeader( fptr, &shape ) == -1 ) {
    Rprintf( "Error: Reading main file header in C function pointInPolygonFile.\n" );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* copy the dsgnmd mdm weights into an C array */
  if ((dsgnmd = (double *) malloc( sizeof( double ) * dsgSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function pointInPolygonFile.\n" );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 ); 
    return results;  
  }
  for ( i = 0; i < dsgSize; ++i ) {
    dsgnmd[i] = REAL( dsgnmdVec )[i];
  }

  /* copy over the values of the points in the R vectors to C arrays */
  if ( (xcs = (double *) malloc( sizeof(double) * vecSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function pointInPolygonFile.\n" );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 ); 
    return results;  
  }
  if ( (ycs = (double *) malloc( sizeof(double) * vecSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function pointInPolygonFile.\n" );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 ); 
    return results;  
  }
  for ( i = 0; i < vecSize; ++i ) {
    xcs[i] = REAL( xcsVec )[i];
    ycs[i] = REAL( ycsVec )[i];
  }


  /* fill in the matrix specifying which points are inside the shape and */
  /* which are outside and the record ID that the point is in */  
  if ( (matrix = (double *) malloc( sizeof(double) * vecSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function pointInPolygonFile.\n" );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 ); 
    return results;  
  }
  if ((matrixIDs = (unsigned int *) malloc( sizeof(unsigned int) * vecSize ))
                                                               == NULL ) {
    Rprintf( "Error: Allocating memory in C function pointInPolygonFile.\n" );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 ); 
    return results;  
  }
  if ( insideShape( &matrix, &matrixIDs, xcs, ycs, vecSize, &shape, fptr, 
                                          dsgnmdID, dsgnmd, dsgSize ) == -1 ) {
    Rprintf( "Error: In C call to insideShape in C function pointInPolygonFile.\n" );
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* convert the matrices into an R object */
  PROTECT( results = allocVector( VECSXP, 2 ) );
  PROTECT( matrixVec = allocVector( REALSXP, vecSize ) );
  PROTECT( matrixIDsVec = allocVector( INTSXP, vecSize ) );
  for ( i = 0; i < vecSize; ++i ) {
    REAL( matrixVec )[i] = matrix[i]; 
    INTEGER( matrixIDsVec )[i] = matrixIDs[i]; 
  }

  SET_VECTOR_ELT( results, 0, matrixVec );
  SET_VECTOR_ELT( results, 1, matrixIDsVec );

  /* setup vector labels */
  PROTECT( colNamesVec = allocVector( STRSXP, 2 ) );
  SET_STRING_ELT( colNamesVec, 0, mkChar( "mdm" ) ); 
  SET_STRING_ELT( colNamesVec, 1, mkChar( "id" ) );
  setAttrib( results, R_NamesSymbol, colNamesVec );

  /* clean up */
  if ( xcs ) {
    free( xcs );
  }
  if ( ycs ) {
    free( ycs );
  }
  if ( matrix ) {
    free( matrix );
  }
  if ( matrixIDs ) {
    free( matrixIDs );
  }
  if ( dsgnmd ) {
    free( dsgnmd );
  }
  if ( dsgnmdID ) {
    free( dsgnmdID );
  }
  fclose( fptr );
  if ( singleFile == TRUE ) {
    free( shpFileName );
  }
  remove( TEMP_SHP_FILE );
  UNPROTECT( 4 );

  return results;
}


/**********************************************************
** Function:   pointInPolygonObj
**
** Purpose:    To return an R vector of 1 or 0's 
**             corresponding to whether each point in the xcsVec and
**             ycsVec vectors are inside or outside the polygon represented 
**             by the sent polyXVec and polyYVec coordinate arrays.
** Algorithm:  This function converts the sent R vectors to the necessary
**             C arrays and then iterates through each point calling the 
**             insidePolygon() function to determine if the point is inside
**             or outside the sent polygon.  As each point is checked the
**             result (1 or 0) is written to the matrix array.
** Notes:      The matrix array will be the same size as the xcs and ycs
**             arrays/vectors.
** Arguments:  ptXVec,   vector of all the x coordinates of the points
**             ptYVec,   vector of all the y coordinates of the points
**             polyXVec,   vector of all the x coordinates of the polygon
**             polyYVec,   vector of all the y coordinates of the polygon
** Return:     results,  R vector of 1 and 0's corresponding to whether 
**                       each point is inside or outside the polygon
***********************************************************/
SEXP pointInPolygonObj(SEXP ptXVec, SEXP ptYVec, SEXP polyXVec, SEXP polyYVec) {

  int i;                  /* loop counter */
  double * matrix = NULL; /* used to store array of 1 or 0's represnting */
                          /* whether corresponding point is inside or outside */
                          /* polygon */
  unsigned int ptVecSize = length( ptXVec );  /* number of points */
  SEXP results = NULL;    /* R object used to return the "matrix" back to R */
  Polygon poly;           /* used to store the sent polygon coordinates */

  /* copy the sent polygon coordinates into a polygon struct */
  poly.numPoints = length( polyXVec );
  if ((poly.points = (Point *) malloc( sizeof( Point ) * poly.numPoints ))
                                                                == NULL ) {
    Rprintf( "Error: Allocating memory in C function pointInPolygonObj.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 ); 
    return results;  
  }
  for ( i = 0; i < poly.numPoints; ++i ) {
    poly.points[i].X = REAL( polyXVec )[i];  
    poly.points[i].Y = REAL( polyYVec )[i];  
  }

  /* allocate memory for the matrix and initialize to all 0's */
  if ( (matrix = (double *) malloc( sizeof(double) * ptVecSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function pointInPolygonObj.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 ); 
    return results;  
  }
  for ( i = 0; i < ptVecSize; ++i ) {
    matrix[i] = 0.0;
  }

  /* build the pt in polygon matrix */
  for ( i = 0; i < ptVecSize; ++i ) {
    if( insidePolygon( poly.points, poly.numPoints , REAL( ptXVec )[i], 
                                                   REAL( ptYVec )[i] ) == 1 ){
      matrix[i] = 1.0;
    }
  }

  /* convert the matrix into an R object */
  PROTECT( results = allocVector( REALSXP, ptVecSize ) );
  for ( i = 0; i < ptVecSize; ++i ) {
    REAL( results )[i] = matrix[i]; 
  }

  /* clean up */
  if ( matrix ) {
    free( matrix );
  }
  if ( poly.points ) {
    free( poly.points );
  }
  UNPROTECT( 1 );

  return results;
}
