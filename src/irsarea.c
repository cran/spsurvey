/****************************************************************************** 
**  File:       irsarea.c
**  
**  Purpose:     This file contains code for the getShapeBox() function and
**               pertains to polygon shapefile types.  The getShapeBox()
**               function is used to obtain shapefile minimum and maximum values
**               for the x and y coordinates.
**  Programmer:  Tom Kincaid
**  Created:     November 30, 2005
**  Last Revised: June 23, 2006
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>
#include <fcntl.h>
#include "shapeParser.h"
#include "grts.h"

/* these functions are found in grts.c */
extern int combineShpFiles( FILE * newShp, unsigned int * ids, int numIDs );
extern int createNewTempShpFile( FILE * newShp, char * shapeFileName, 
  unsigned int * ids, int numIDs );

/* these functions are found in shapeParser.c */
extern int parseHeader( FILE * fptr, Shape * shape );


/****************************************************************************** 
** Function:   getRecordIDs
**
** Purpose:    To obtain the shapefile record IDs for records from which sample
**             points will be selected.
** Notes:      It is assumed that if a record has multiple parts 
**             they are not connected.
**             To save memory one record at a time is read from the shape
**             file and processed before reading in the next.
** Arguments:  areaCumSumVec, vector of cumulative sum of polygon (record)
**                           areas
**             sampPosVec, vector of sample position values
**             dsgnIDVec, vector of the polygon IDs
** Return:     IDVec, R vector of polygon IDs.
******************************************************************************/
SEXP getRecordIDs( SEXP areaCumSumVec, SEXP sampPosVec, SEXP dsgnIDVec ) {

  int i, j;                  /* loop counters */
  unsigned int * id = NULL;  /* the polygon ID array for sample positions */

  /* C variables that store the sent R object's values */
  double * areaCumSum = NULL;
  double * sampPos = NULL;
  unsigned int * dsgnID = NULL;
  int smpSize = length( sampPosVec );
  int dsgnSize = length( dsgnIDVec );

  /* variable used for converting results to an R object */
  SEXP IDVec;

  /* copy the cumulative sum of polygon areas into a C array */
  if((areaCumSum = (double *) malloc( sizeof( double ) * dsgnSize ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function getRecordIDs.\n" );
    PROTECT( IDVec = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return IDVec;
  }
  for ( i = 0; i < dsgnSize; ++i ) {
    areaCumSum[i] = REAL( areaCumSumVec )[i];
  }

  /* copy the sample position values into a C array */
  if((sampPos = (double *) malloc( sizeof( double ) * smpSize ))
      == NULL ) {
    Rprintf( "Error: Allocating memory in C function getRecordIDs.\n" );
    PROTECT( IDVec = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return IDVec;
  }
  for ( j = 0; j < smpSize; ++j ) {
    sampPos[j] = REAL( sampPosVec )[j];
  }

  /* copy the polygon IDs into a C array */
  if((dsgnID = (unsigned int *) malloc( sizeof( unsigned int ) * dsgnSize ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function getRecordIDs.\n" );
    PROTECT( IDVec = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return IDVec;
  }
  for ( i = 0; i < dsgnSize; ++i ) {
    dsgnID[i] = INTEGER( dsgnIDVec )[i];
  }

  /* allocate memory for the ID variable */

  if (( id = (unsigned int *) malloc( sizeof( unsigned int ) * smpSize ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function getRecordIDs.\n" );
    PROTECT( IDVec = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return IDVec;
  }

  /* determine the polygon IDs values for the sample positions */
  j = 0;
  for ( i = 0; i < dsgnSize; ++i ) {
    while ( sampPos[j] < areaCumSum[i] ) {
      id[j] = dsgnID[i];
      ++j;
      if ( j == smpSize ) {
    	   break;
      }
    }
    if ( j == smpSize ) {
    	 break;
    }
  }

  /* convert array to an R object */
  PROTECT( IDVec = allocVector( INTSXP, smpSize ) );
  for ( i = 0; i < smpSize; ++i ) {
    INTEGER( IDVec )[i] = id[i];
  }

  /* clean up */
  if ( areaCumSum ) {
    free( areaCumSum );
  }
  if ( sampPos ) {
    free( sampPos );
  }
  if ( dsgnID ) {
    free( dsgnID );
  }
  if ( id ) {
    free( id );
  }
  UNPROTECT( 1 );

  return IDVec;
}


/**********************************************************
** Function:   getShapeBox
**
** Purpose:    This function obtains the shapefile minimum and maximum values
**             for the x and y coordinates.
** Notes:      The funtion creates a temporary shapefile. If fileNamePrefix is
**             NULL, the combineShpFiles() function is used to combine the data
**             found in all the shapefiles in the current working directory
**             into a single shapefile, subset the file by the values in
**             dsgnmdIDVec, and store the result in the temporary shapefile.
**             If fileNamePrefix is not NULL, the createNewTempShpFile()
**             function is used to subset the file by the values in dsgnmdIDVec 
**             and store the result in the temporary shapefile.
** Arguments:  fileNamePrefix, name of the shapefile (without the .shp
**                             extension), which may be NULL.
**             dsgnIDVec, vector of shapefile IDs that determines the records
**                        included in the temporary shapefile.
** Return:     results, an R object containing the shapefile minimum and
**                      maximum values for the x and y coordinates. If an error
**                      occurs, object is set to NULL.
***********************************************************/

SEXP getShapeBox( SEXP fileNamePrefix, SEXP dsgnIDVec ) {

  unsigned int * dsgnID = NULL;  /*array of record ID numbers */
  int dsgnSize = length( dsgnIDVec );  /* number of IDs in dsgnID */
  int i;  /* loop counter */
  char * shpFileName = NULL;  /* stores full shapefile name */
  int singleFile = FALSE;  /* indicator for the number of shapefiles */
  FILE * newShp = NULL;  /* pointer to the temporary shapefile */
  Shape shape;  /* Shape struct for temporary storage of shape data */
  FILE * fptr = NULL;  /* pointer to the shapefile */

  /* shape minimum and maximum x and y coordinates */
  double Xmin;
  double Ymin;
  double Xmax;
  double Ymax;

  /* variables for converting results into an R object */
  SEXP XminVec, YminVec, XmaxVec, YmaxVec;
  SEXP colNamesVec;

  SEXP results = NULL;  /* R object for returning the results */

/* copy the dsgnmd poly IDs into an C array */
  if ( (dsgnID = (unsigned int *) malloc( sizeof( unsigned int ) * dsgnSize))
    == NULL ) {
    Rprintf( "Error: Allocating memory in grts.c.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }
  for ( i = 0; i < dsgnSize; ++i ) {
    dsgnID[i] = INTEGER( dsgnIDVec )[i];
  }

  /* see if a specific file was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full shapefile name */
    if ((shpFileName = (char *)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
      + strlen(".shp") + 1)) == NULL ){
      Rprintf( "Error: Allocating memory in shapeParser.c\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return results;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix,0)));
    strcat( shpFileName, ".shp" );
    singleFile = TRUE;
  }

  /* create the new temporary shapefile */
  if ( ( newShp = fopen( TEMP_SHP_FILE, "wb" )) == NULL ) {
    Rprintf( "Error: Creating temporary shapefile %s.\n", TEMP_SHP_FILE );
    Rprintf( "Error: Occured in numLevels() in grts.c\n" );
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
      Rf_error( "Error: Combining multiple shapefiles.\n" );
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
      Rf_error( "Error: Creating temporary shapefile.\n" );
      return results; 
    }
    fclose( newShp );
  }

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;

  /* open the temporary shapefile */
  if ( (fptr = fopen( TEMP_SHP_FILE, "rb" )) == NULL ) {
    Rprintf( "Error: Opening shapefile in C function.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* parse main file header */
  if ( parseHeader( fptr, &shape ) == -1 ) {
    Rprintf( "Error: Reading main file header in C function.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    fclose( fptr );
    return results;
  }

  /* get the minimum and maximum x and y coordinates */
  Xmin = shape.Xmin;
  Ymin = shape.Ymin;
  Xmax = shape.Xmax;
  Ymax = shape.Ymax;

  /* write results to the R object */

  PROTECT( XminVec = allocVector( REALSXP, 1 ) );
  REAL( XminVec )[0] = Xmin;

  PROTECT( YminVec = allocVector( REALSXP, 1 ) );
  REAL( YminVec )[0] = Ymin;

  PROTECT( XmaxVec = allocVector( REALSXP, 1 ) );
  REAL( XmaxVec )[0] = Xmax;

  PROTECT( YmaxVec = allocVector( REALSXP, 1 ) );
  REAL( YmaxVec )[0] = Ymax;

  /* copy each data value into the results vector */  
  PROTECT( results = allocVector( VECSXP, 4 ) );
  SET_VECTOR_ELT( results, 0, XminVec); 
  SET_VECTOR_ELT( results, 1, YminVec ); 
  SET_VECTOR_ELT( results, 2, XmaxVec ); 
  SET_VECTOR_ELT( results, 3, YmaxVec ); 

  /* create vector labels */
  PROTECT( colNamesVec = allocVector( STRSXP, 4 ) );
  SET_STRING_ELT( colNamesVec, 0, mkChar( "xmin" ) );
  SET_STRING_ELT( colNamesVec, 1, mkChar( "ymin" ) );
  SET_STRING_ELT( colNamesVec, 2, mkChar( "xmax" ) );
  SET_STRING_ELT( colNamesVec, 3, mkChar( "ymax" ) );
  setAttrib( results, R_NamesSymbol, colNamesVec );

  /* do clean up */
  if ( dsgnID ) {
    free( dsgnID );
  }
  fclose( fptr );
  if ( singleFile == TRUE ) {
    free( shpFileName );
  }
  remove( TEMP_SHP_FILE );
  UNPROTECT(6);

  /* return the results */
  return results;
}
