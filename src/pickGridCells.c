/****************************************************************************** 
**  Function:    pickGridCells
**  Programmer:  Tom Kincaid
**  Date:        October 24, 2005
**  Revised:     June 11, 2015
**  Revised:     August 16, 2016
**  Description:
**    This function determines the grid cells from which sample points will be
**    selected. 
**  Arguments:
**    samplesize = the sample size
**    idxVec = the vector of index values for the grid cells
**  Results
**    An R object named smpdxVec that contains the index values for cells from
**    which sample points will be selected.
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>

SEXP pickGridCells( SEXP samplesize, SEXP idxVec ) { 

  int i, j;                                 /* loop counters */
  unsigned int smpSize;                     /* sample size */
  unsigned int idxSize = length( idxVec );  /* number of values in the idx array */
  unsigned int * idx = NULL;                /* the idx array */
  unsigned int * smpdx = NULL;              /* the smpdx array */

  /* temporary pointer for converting sent R objects to C variables */
  int * intPtr;

  /* R object for returning results to R */
  SEXP smpdxVec = NULL;    

  /* copy the sample size into a C variable */

  PROTECT( samplesize = AS_INTEGER( samplesize ) );
  intPtr = INTEGER_POINTER( samplesize );
  smpSize = *intPtr;
  UNPROTECT(1);

  /* copy the idx array into an C array */

  if ( (idx = (unsigned int *) malloc( sizeof( unsigned int ) * idxSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in pickGridCells.c.\n" );
    PROTECT( smpdxVec = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return smpdxVec;
  }

  for ( i = 0; i < idxSize; ++i ) {
    idx[i] = INTEGER( idxVec )[i];
  }

  /* allocate memory for the smpdx C array */

  if ( (smpdx = (unsigned int *) malloc( sizeof( unsigned int ) * smpSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in pickGridCells.c.\n" );
    PROTECT( smpdxVec = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return smpdxVec;
  }

  /* assign values to the smpdx C array */

  j = 0;
  for ( i = 0; i < smpSize; ++i ) {
    while ( idx[j] < (i + 1) ) {
      ++j;
    }
    smpdx[i] = j;
  }

  /* write results to the R object */

  PROTECT( smpdxVec = allocVector( INTSXP, smpSize ) );
  for ( i = 0; i < smpSize; ++i ) {
    INTEGER( smpdxVec )[i] = smpdx[i];
  }
  UNPROTECT(1);

  /* clean up */

  if(idx) {
    free(idx);
  }
  if(smpdx) {
    free(smpdx);
  }

  return smpdxVec;
}
