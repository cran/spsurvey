/****************************************************************************** 
**  File:        grtspts.c
**  
**  Purpose:     This file contains the function cWtFcn() which is used to 
**               calculate the weights for the sent array of weights for 
**               dealing with a points shape type.  It is called from the 
**               numLevels() function found in grts.c.
**  Programmers: Christian Platt, Tom Kincaid
**  Created:     October 27, 2004
**  Revised:     May 10, 2006
**  Revised:     July 16, 2014
**  Revised:     June 15, 2015
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include "shapeParser.h"

#define MIN(x,y) (x < y ? x : y)
#define MAX(x,y) (x > y ? x : y)

/* found in shapeParser.c */
extern unsigned int readLittleEndian( unsigned char * buffer, int length );
extern unsigned int readBigEndian( unsigned char * buffer, int length );

/* struct for storing a cell's coordinates */
typedef struct cellStruct Cell;
struct cellStruct {
  double xMin;
  double yMin;
  double xMax;
  double yMax;
};


/**********************************************************
** Function:   cWtFcn
**
** Purpose:    This function is used to calculate the weights for 
**             the sent array of weights for dealing with a points
**             shape type.  It is called from the numLevels() function
**             found in grts.c
** Notes:      To conserve memory, one record is read from the shape
**             file and processed at a time.
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
int cWtFcn( double ** celWts, double * xc, double * yc, double dx, double dy, 
                       int size , Shape * shape, FILE * fptr,
                       unsigned int * dsgnmdID, double * dsgnmd, int dsgSize ){
  int i, w;                         /* loop counters */
  Cell cell;                        /* temp storage for cell coordinates */
  unsigned int filePosition = 100;  /* byte offset into .shp file */
  Record record;                    /* temp storage for record info */
  unsigned char buffer[4];          /* buffer for reading from file */
  double tempDouble;                /* variable for reading from file */
  Point point;                      /* temp storage for a Point */
  PointZ pointZ;                    /* temp storage for a PointZ */
  PointM pointM;                    /* temp storage for a PointM */
  int tempID = -1;                  /* temp ID of record we are looking at */


  /* initialize all the celWts to 0 */
  for ( i = 0; i < size; ++i ) {
    (*celWts)[i] = 0.0;
  }

  /* make sure we are back to the beignning of the records in the file */
  fseek( fptr, 100, SEEK_SET );
  filePosition = 100;

  /* go through the shape file to determine which points are in each cell */
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

    if ( shape->shapeType == POINTS ) {

      /* read the point coordinates */
      fread( &(point.X), sizeof(double), 1, fptr );
      if ( fread( &(point.Y), sizeof(double), 1, fptr ) == 0 ) {
        Rprintf( "Error: Reading .shp file in grtspts.c\n" );
        return -1;
      }
      filePosition += 2*sizeof(double);

    } else if ( shape->shapeType == POINTS_Z ) {

      /* read the point coordinates */
      fread( &(pointZ.X), sizeof(double), 1, fptr );
      if ( fread( &(pointZ.Y), sizeof(double), 1, fptr ) == 0 ) {
        Rprintf( "Error: Reading .shp file in grtspts.c\n" );
        return -1;
      }
      filePosition += 2*sizeof(double);

      /* ignore the z value */
        fread( &tempDouble, sizeof(double), 1, fptr );
        filePosition += sizeof(double);

      /* ignore the M value */
        fread( &tempDouble, sizeof(double), 1, fptr );
        filePosition += sizeof(double);

    } else {

      /* read the point coordinates */
      fread( &(pointM.X), sizeof(double), 1, fptr );
      if ( fread( &(pointM.Y), sizeof(double), 1, fptr ) == 0 ) {
        Rprintf( "Error: Reading .shp file in grtspts.c\n" );
        return -1;
      }
      filePosition += 2*sizeof(double);

      /* ignore the M value */
        fread( &tempDouble, sizeof(double), 1, fptr );
        filePosition += sizeof(double);

    }

    /* make sure the record number is in the dsgnmdID array */
    tempID = -1;
    for ( w = 0; w < dsgSize; ++w ) {
      if ( dsgnmdID[w] == record.number ) {
        tempID = record.number;
        break;
      }
    }

    /* if the record number was in dsgnmd then process this record */
    if ( tempID != -1 ) {

      /* look in each cell for the point */
      for ( i = 0; i < size; ++i ) {
        cell.xMin = xc[i] - dx;
        cell.yMin = yc[i] - dy;
        cell.xMax = xc[i];
        cell.yMax = yc[i];

        if ( shape->shapeType == POINTS ) {

          /* see if the point is inside the cell */
          if(tempID != -1 && (cell.xMin < point.X) && (point.X <= cell.xMax) &&
               (cell.yMin < point.Y) && (point.Y <= cell.yMax) ) {

            /* it's inside the cell so add the dsgnmd weight */
            (*celWts)[i] += dsgnmd[w];
          }

        } else if ( shape->shapeType == POINTS_Z ) {

          /* see if the point is inside the cell */
          if(tempID != -1 && (cell.xMin < pointZ.X) && (pointZ.X <= cell.xMax) &&
               (cell.yMin < pointZ.Y) && (pointZ.Y <= cell.yMax) ) {

            /* it's inside the cell so add the dsgnmd weight */
            (*celWts)[i] += dsgnmd[w];
          }

        } else {

          /* see if the point is inside the cell */
          if(tempID != -1 && (cell.xMin < pointM.X) && (pointM.X <= cell.xMax) &&
               (cell.yMin < pointM.Y) && (pointM.Y <= cell.yMax) ) {

            /* it's inside the cell so add the dsgnmd weight */
            (*celWts)[i] += dsgnmd[w];
          }
        }
      }
    }
  }
  
  return 1;
}
