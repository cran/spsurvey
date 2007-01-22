/****************************************************************************** 
**  File:         irslin.c
** 
**  Purpose:      This file contains code for the linSampleIRS() function and
**                pertains to polyline shapefile types.  The linSampleIRS()
**                function is used to determine the x,y coordinates for sample
**                point for an IRS design.
**  Programmer:   Tom Kincaid
**  Created:      November 30, 2005
**  Last Revised: June 22, 2006
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <fcntl.h>
#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include "shapeParser.h"
#include "grts.h"

/* these functions are found in grts.c */
extern int combineShpFiles( FILE * newShp, unsigned int * ids, int numIDs );
extern int createNewTempShpFile( FILE * newShp, char * shapeFileName, 
  unsigned int * ids, int numIDs );

/* these functions are found in shapeParser.c */
extern int parseHeader( FILE * fptr, Shape * shape );
extern int readLittleEndian( unsigned char * buffer, int length );
extern int readBigEndian( unsigned char * buffer, int length );

/* these functions are found in grtslin.c */
extern void addSegment( Segment ** head, Segment * seg );
extern void deallocateSegments( Segment * head );

/****************************************************************************** 
** Function:   linSampleIRS
**
** Purpose:    To determine the x,y coordinates for sample points that 
**             correspond to the sent sample position vector.
** Notes:      It is assumed that if a record has multiple parts 
**             they are not connected.
**             To save memory one record at a time is read from the shape
**             file and processed before reading in the next.
** Arguments:  fileNamePrefix, name of the shapefile (without the .shp
**                             extension), which may be NULL.
**             lenCumSumVec, vector of cumulative sum of polyline (record)
**                           lengths
**             sampPosVec, vector of sample position values
**             dsgnIDVec, vector of the polyline IDs
**             dsgnLenVec, vector of polyline lengths
** Return:     results, R vectors of x and y coordinates making up the
**                      selected sample along with polyline IDs.
******************************************************************************/
SEXP linSampleIRS( SEXP fileNamePrefix, SEXP lenCumSumVec, SEXP sampPosVec,
     SEXP dsgnIDVec, SEXP dsgnLenVec, SEXP dsgnMdmVec ) {

  int i, j;                  /* loop counters */
  int partIndx;              /* index into polyline parts array */

  /* C variables that store the sent R object's values */
  double * lenCumSum = NULL;
  double * sampPos = NULL;
  int smpSize = length( sampPosVec );
  unsigned int * dsgnID = NULL;
  double * dsgnLen = NULL;
  double * dsgnMdm = NULL;
  int dsgnSize = length( dsgnIDVec );

  FILE * newShp = NULL;       /* pointer to the temporary shapefile */
  FILE * fptr = NULL;         /* pointer to the shapefile */
  Shape shape;                /* temporary storage of shape data */
  unsigned int filePosition;  /* byte offset into shapefile */
  Record record;              /* temp storage of record data */
  Polygon * poly;             /* temp Polygon data storage */
  PolygonZ * polyZ;           /* temp PolygonZ data storage */
  PolygonM * polyM;           /* temp PolygonM data storage */
  unsigned char buffer[4];    /* buffer used for reading from file */

  /* variables for storing linked list of segment structs and traversing them */
  Segment * seg;
  Segment * segmentList = NULL;
  Segment * temp;

  /* vars used for picking sample points */
  unsigned int * id = NULL;  /* the polyline ID array for sample positions */
  double * pos = NULL;       /* the sample position within a polyline */
  double dx, dy;
  double length;
  double cumSum;  /* accumulative sum of lengths up to selected point */
  double len;
  double lx, ly;
  unsigned int * samp = NULL;  
  double * x = NULL;
  double * y = NULL;
  unsigned int sampInd = 0;

  /* vars used for converting results to an R object */
  SEXP xVec, yVec, IDVec, colNamesVec;
  SEXP results = NULL;

  int singleFile = FALSE;     /* indicator variable for a single shapefile */
  char * shpFileName = NULL;  /* stores full shapefile name */

  /* copy the cumulative sum of polyline lengths into a C array */
  if((lenCumSum = (double *) malloc( sizeof( double ) * dsgnSize ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  for ( i = 0; i < dsgnSize; ++i ) {
    lenCumSum[i] = REAL( lenCumSumVec )[i];
  }

  /* copy the sample position values into a C array */
  if((sampPos = (double *) malloc( sizeof( double ) * smpSize ))
      == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  for ( j = 0; j < smpSize; ++j ) {
    sampPos[j] = REAL( sampPosVec )[j];
  }

  /* copy the polyline IDs into a C array */
  if((dsgnID = (unsigned int *) malloc( sizeof( unsigned int ) * dsgnSize ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  for ( i = 0; i < dsgnSize; ++i ) {
    dsgnID[i] = INTEGER( dsgnIDVec )[i];
  }

  /* copy the polyline length values into a C array */
  if (( dsgnLen = (double *) malloc( sizeof( double ) * dsgnSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  for ( i = 0; i < dsgnSize; ++i ) {
    dsgnLen[i] = REAL( dsgnLenVec )[i];
  }

  /* copy the polyline multidensity multiplier values into a C array */
  if (( dsgnMdm = (double *) malloc( sizeof( double ) * dsgnSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  for ( i = 0; i < dsgnSize; ++i ) {
    dsgnMdm[i] = REAL( dsgnMdmVec )[i];
  }

  /* allocate memory for the variables used to select sample coordinates */

  if (( id = (unsigned int *) malloc( sizeof( unsigned int ) * smpSize ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  if ( ( pos = (double *) malloc( sizeof( double ) * smpSize ) ) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  if (( samp = (unsigned int *) malloc( sizeof( unsigned int ) * smpSize ))
       == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  if ( ( x = (double *) malloc( sizeof( double ) * smpSize ) ) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  if ( ( y = (double *) malloc( sizeof( double ) * smpSize ) ) == NULL ) {
    Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }

  /* determine the polyline IDs values for the sample positions and the sample
  ** position within each polyline */
  j = 0;
  for ( i = 0; i < dsgnSize; ++i ) {
    while ( sampPos[j] < lenCumSum[i] ) {
      id[j] = dsgnID[i];
      pos[j] = dsgnLen[i] - (lenCumSum[i] - sampPos[j])/dsgnMdm[i];
      ++j;
      if ( j == smpSize ) {
    	   break;
      }
    }
    if ( j == smpSize ) {
    	 break;
    }
  }

  /* see if a specific shapefile was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full shapefile name */
    if ((shpFileName = (char *)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
      + strlen(".shp") + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return results;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix,0)));
    strcat( shpFileName, ".shp" );
    singleFile = TRUE;

  }

  /* create the temporary shapefile */
  if ( ( newShp = fopen( TEMP_SHP_FILE, "wb" )) == NULL ) {
    Rprintf( "Error: Creating temporary shapefile %s in C function linSampleIRS.\n", TEMP_SHP_FILE );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  } 

  if ( singleFile == FALSE ) {

    /* combine all the shapefiles into one shapefile, subset by dsgnID, */
    /* and create a temporary shapefile */
    if ( combineShpFiles( newShp, dsgnID, dsgnSize ) == -1 ) {
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( newShp );
      remove( TEMP_SHP_FILE );
      Rprintf( "Error: Combining multiple shapefiles in C function linSampleIRS.\n" );
      return results; 
    }
    fclose( newShp );

  } else {

    /* else subset the shapefile by dsgnID and create a temporary shapefile */
    if (createNewTempShpFile(newShp, shpFileName, dsgnID, dsgnSize) == -1 ){
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( newShp );
      remove( TEMP_SHP_FILE );
      Rprintf( "Error: Creating temporary shapefile in C function linSampleIRS.\n" );
      return results; 
    }
    fclose( newShp );
  }

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;

  /* open the temporary shapefile */
  if ( (fptr = fopen( TEMP_SHP_FILE, "rb" )) == NULL ) {
    Rprintf( "Error: Opening shapefile in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* parse main file header */
  if ( parseHeader( fptr, &shape ) == -1 ) {
    Rprintf( "Error: Reading main file header in C function linSampleIRS.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }

  /* get to the beginning of the records in the shapefile */ 
  fseek( fptr, 100, SEEK_SET );
  filePosition = 100;

  /* deallocate any memory used for the segment list */
  deallocateSegments( segmentList );
  segmentList = NULL;

  /* determine the coordinates for each sample point */
  while ( filePosition < shape.fileLength*2 && sampInd < smpSize ) {

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
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
        PROTECT( results = allocVector( VECSXP, 1 ) );
        UNPROTECT( 1 );
        fclose( fptr );
        remove( TEMP_SHP_FILE );
        return results;
      }

      /* read box data */
      for ( i=0; i < 4; ++i ) {
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
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
          Rprintf( "Error: Reading shapefile in C function linSampleIRS.\n" );
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

    } else if ( shape.shapeType == POLYLINE_Z ) {

      /* allocate a new polygon */
      if ( (polyZ = (PolygonZ *) malloc( sizeof(PolygonZ) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
      if ((polyZ->points = (Point *) malloc( sizeof(Point) * polyZ->numPoints ))
           == NULL ) {
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
          Rprintf( "Error: Reading shapefile in C function linSampleIRS.\n" );
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
      if ((polyZ->zArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
           == NULL ) {
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
      if ((polyZ->mArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
           == NULL ) {
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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

    } else {

      /* allocate a new polygon */
      if ( (polyM = (PolygonM *) malloc( sizeof(PolygonM) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
      if ((polyM->points = (Point *) malloc( sizeof(Point) * polyM->numPoints ))
           == NULL ) {
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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
          Rprintf( "Error: Reading shapefile in C function linSampleIRS.\n" );
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
      if ((polyM->mArray = (double *) malloc( sizeof(double)*polyM->numPoints))
           == NULL ) {
        Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
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

    }

    if ( shape.shapeType == POLYLINE ) {

      /* go through each segment in this record */
      partIndx = 1; 
      for ( i = 0; i < record.poly->numPoints-1; ++i ) {

        /* if there are multiple parts assume that the parts are not connected*/
        if ( record.poly->numParts > 1 && partIndx < record.poly->numParts ) {
          if ( (i + 1) == record.poly->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        /* allocate new segment struct */
        if ( (seg = (Segment *) malloc( sizeof( Segment ) )) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }

        /* get length of the line segment */
        dx = record.poly->points[i+1].X - record.poly->points[i].X;
        dy = record.poly->points[i+1].Y - record.poly->points[i].Y;
        length = sqrt( dx*dx + dy*dy );
   
        /* assign values and add the segment to the segment list */ 
        seg->p1.X = record.poly->points[i].X;
        seg->p1.Y = record.poly->points[i].Y;
        seg->p2.X = record.poly->points[i+1].X;
        seg->p2.Y = record.poly->points[i+1].Y;
        seg->recordNumber = record.number;
        seg->length = length;
        addSegment( &segmentList, seg );

      }

      /* deallocate memory for the record */
      free( record.poly->points );
      free( record.poly->parts );
      free( record.poly );

    } else if ( shape.shapeType == POLYLINE_Z ) {

      /* go through each segment in this record */
      partIndx = 1; 
      for ( i = 0; i < record.polyZ->numPoints-1; ++i ) {

        /* if there are multiple parts assume that the parts are not connected*/
        if ( record.polyZ->numParts > 1 && partIndx < record.polyZ->numParts ) {
          if ( (i + 1) == record.polyZ->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        /* allocate new segment struct */
        if ( (seg = (Segment *) malloc( sizeof( Segment ) )) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }

        /* get length of the line segment */
        dx = record.polyZ->points[i+1].X - record.polyZ->points[i].X;
        dy = record.polyZ->points[i+1].Y - record.polyZ->points[i].Y;
        length = sqrt( dx*dx + dy*dy );
   
        /* assign values and add the segment to the segment list */ 
        seg->p1.X = record.polyZ->points[i].X;
        seg->p1.Y = record.polyZ->points[i].Y;
        seg->p2.X = record.polyZ->points[i+1].X;
        seg->p2.Y = record.polyZ->points[i+1].Y;
        seg->recordNumber = record.number;
        seg->length = length;
        addSegment( &segmentList, seg );

      }

      /* deallocate memory for the record */
      free( record.polyZ->mArray );
      free( record.polyZ->zArray );
      free( record.polyZ->points );
      free( record.polyZ->parts );
      free( record.polyZ );

    } else {

      /* go through each segment in this record */
      partIndx = 1; 
      for ( i = 0; i < record.polyM->numPoints-1; ++i ) {

        /* if there are multiple parts assume that the parts are not connected*/
        if ( record.polyM->numParts > 1 && partIndx < record.polyM->numParts ) {
          if ( (i + 1) == record.polyM->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        /* allocate new segment struct */
        if ( (seg = (Segment *) malloc( sizeof( Segment ) )) == NULL ) {
          Rprintf( "Error: Allocating memory in C function linSampleIRS.\n" );
          PROTECT( results = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          fclose( fptr );
          remove( TEMP_SHP_FILE );
          return results;
        }

        /* get length of the line segment */
        dx = record.polyM->points[i+1].X - record.polyM->points[i].X;
        dy = record.polyM->points[i+1].Y - record.polyM->points[i].Y;
        length = sqrt( dx*dx + dy*dy );
   
        /* assign values and add the segment to the segment list */ 
        seg->p1.X = record.polyM->points[i].X;
        seg->p1.Y = record.polyM->points[i].Y;
        seg->p2.X = record.polyM->points[i+1].X;
        seg->p2.Y = record.polyM->points[i+1].Y;
        seg->recordNumber = record.number;
        seg->length = length;
        addSegment( &segmentList, seg );

      }

      /* deallocate memory for the record */
      free( record.polyM->mArray );
      free( record.polyM->points );
      free( record.polyM->parts );
      free( record.polyM );

    }

    /* while the sample ID equals the shapefile record number, determine sample
    ** coordinates */
    while ( id[sampInd] == record.number ) {
      temp = segmentList;
      cumSum = 0.0;
      while ( TRUE && temp != NULL ) {
        cumSum += temp->length;
        if ( pos[sampInd] < cumSum ) {
          break;
        }
        temp = temp->next;
      }
      samp[sampInd] = temp->recordNumber;

      /* determine coordinates for the sample point */
      len =  pos[sampInd] - cumSum;
      dx = temp->p2.X - temp->p1.X;
      dy = temp->p2.Y - temp->p1.Y;
      if ( dx != 0 ) {
        lx = sign(dx) * sqrt( (len*len) / ( 1 + (dy*dy)/(dx*dx) ));
        ly = lx * (dy/dx);
      } else {
        lx = 0.0;
        ly = -sign(dy) * len;
      }
      x[sampInd] = temp->p1.X + lx;
      y[sampInd] = temp->p1.Y + ly;
      ++sampInd;
      if ( sampInd == smpSize ) {
    	   break;
      }

    }

    /* deallocate memory for the segment list */
    deallocateSegments( segmentList );
    segmentList = NULL;

  }

  /* convert arrays to an R object */
  PROTECT( results = allocVector( VECSXP, 3 ) );
  PROTECT( xVec = allocVector( REALSXP, smpSize ) );
  PROTECT( yVec = allocVector( REALSXP, smpSize ) );
  PROTECT( IDVec = allocVector( INTSXP, smpSize ) );

  for ( i = 0; i < smpSize; ++i ) {
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
  SET_STRING_ELT( colNamesVec, 2, mkChar( "id" ) );
  setAttrib( results, R_NamesSymbol, colNamesVec );
 
  /* clean up */
  if ( lenCumSum ) {
    free( lenCumSum );
  }
  if ( sampPos ) {
    free( sampPos );
  }
  if ( dsgnID ) {
    free( dsgnID );
  }
  if ( dsgnLen ) {
    free( dsgnLen );
  }
  if ( dsgnMdm ) {
    free( dsgnMdm );
  }
  if ( id ) {
    free( id );
  }
  if ( pos ) {
    free( pos );
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
  if ( singleFile == TRUE ) {
    free( shpFileName );
  }
  remove( TEMP_SHP_FILE );
  UNPROTECT( 5 );

  return results;
}
