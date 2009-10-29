/****************************************************************************** 
**  File:        grts.c
**  
**  Purpose:     This file contains the numLevels function that does the 
**               "Determine the number of levels for hierarchical 
**               randomization" part of the grts function. It works for 
**               points, polylines, and polygon shape types.
**  Programmers: Christian Platt, Tom Kincaid
**  Algorithm:   It uses the same algorithm that is inplemented in the
**               R version.  For determining the weight values, areaIntersection
**               is called for a polygon shapefile, lintFcn is called for a
**               polylines shapefile, and cWtFcn is called for a points
**               shapefile.
**  Created:     October 19, 2004
**  Revised:     May 3, 2007
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <R.h>
#include <Rdefines.h>
#include <Rmath.h>           /* for runif function */
#include <sys/types.h>
#include <dirent.h>
#include "shapeParser.h"
#include "grts.h"

#define MIN(x,y) (x < y ? x : y)
#define MAX(x,y) (x > y ? x : y)

/* these functions are found in shapeParser.c */
extern int fileMatch( char * fileName, char * fileExt );
extern int parseHeader( FILE * fptr, Shape * shape );
extern int readLittleEndian( unsigned char * buffer, int length );
extern int readBigEndian( unsigned char * buffer, int length );

/* this function is found in grtsarea.c */
extern int areaIntersection( double ** celWts, double * xc, double * yc,
                     double dx , double dy, int size, Shape * shape, 
                     FILE * fptr, unsigned int * dsgnmdID, double * dsgnmd, 
                     int dsgSize );

/* this function is found in grtslin.c */
extern int lintFcn ( double ** celWts, double * xc, double * yc, double dx
                         , double dy, int size, Shape * shape, FILE * fptr,
                   unsigned int * dsgnmdID, double * dsgnmd, int dsgSize );

/* this function is found in grtspts.c */
extern int cWtFcn ( double ** celWts, double * xc, double * yc, double dx,
                    double dy, int size, Shape * shape, FILE * fptr,
                    unsigned int * dsgnmdID, double * dsgnmd, int dsgSize );


/**********************************************************
** Function:   rep
**
** Purpose:    This is the C equivalent to the R rep() function. 
**             It replicates the values in x into a new array.
** Example:    If x = 1,2,3  then repVec = 1,2,3,1,2,3,1,2,3
** Notes:      It is assumed that the proper amount of memory has 
**             already been allocated for the sent repVec array.
** Arguments:  repVec,  array of length size which has the results
**                      written to
**             x,  array of doubles that are to be repeated.
**             size, length of the x array.  repVec should have a 
**                   length of size*size
**             add, value to add to each repeated value
** Return:     1,  on success
**             -1, on error
***********************************************************/
int rep( double ** repVec, double * x, int size, double add ) {
  int i, j;                /* loop counters */

  /* write x repeated to the repVec */
  for ( i = 0; i < (size*size); ++i ) {
    for ( j = 0; j < size; ++j ) {
      (*repVec)[i] = x[j] + add;
      ++i;
    }
    --i;
  }

  return 1;
}



/**********************************************************
** Function:   rep2
**
** Purpose:    This is the C equivalent to the R rep() function similar
**             the the C rep() function except that instead of
**             replicating the array one after the other it replicates
**             the individual values in the x array.           
** Example:    If x = 1,2,3  then repVec = 1,1,1,2,2,2,3,3,3
** Notes:      It is assumed that the proper amount of memory has 
**             already been allocated for the sent repVec array.
** Arguments:  repVec,  array of length size which has the results
**                      written to
**             x,  array of doubles that are to be replicated
**             size, length of the x array.  repVec should have a 
**                   length of size*size
**             add, value to add to each repeated value
** Return:     1,  on success
**             -1, on error
***********************************************************/
int rep2( double ** repVec, double * x, int size, double add ) {
  int i,j;          /* loop counters */
  int index;        /* index into x array */

  index = 0;
  for ( i = 0; i < size*size; ++i ) {
    for ( j = 0; j < size; ++j ) {
      (*repVec)[i] = x[index] + add;
      ++i;
    }
    ++index;
    --i; 
  } 

  return 1;
}



/**********************************************************
** Function:   seq 
**
** Purpose:    This function generates a sequence of the sent 
**             length evenly spaced between min and max.  The 
**             sequence is written into the sent seqVec array.
** Notes:      It is assumed that the proper amount of memory has 
**             been already allocated for the seqVec.
** Arguments:  seqVec,  array to write the sequence into
**             min,  starting value of the sequence
**             max,  ending value of the sequence
**             length, number of values in the sequence 
** Return:     1,  on success
**             -1, on error
***********************************************************/
int seq( double ** seqVec, double min, double max, int length ) {
  int i;       /* loop counter */
  double inc;  /* increment value for the even spacing of values in the seq */

  /* error check */
  if ( min > max || length <= 0 ) {
    return -1;
  }

  /* determine the spacing */
  inc = (max - min) / (double)( length - 1 );

  /* generate the sequence */
  (*seqVec)[0] = min;
  for ( i = 1; i < length; ++i ) {
    (*seqVec)[i] = (*seqVec)[i-1] + inc;
  }

  return 1;
}



/**********************************************************
** Function:   any
**
** Purpose:    This function checks to see if any of the sent cell
**             weights divided by sint are greater than the sent value.
** Arguments:  celWts,  array of cell weights
**             size,  size of celWts array
**             sint,  value to divide celWts by
**             value, value that we are checking to see if any celWts/sint
**                    is greater than
** Return:     1,  if any of the cell weigths/sint are greater than
**                 value
**             0,  if none are greater
**             -1, if error
***********************************************************/
int any( double * celWts, int size, double sint, int value ) {
  int i;

  if ( celWts == NULL ) {
    return -1;
  }

  for ( i = 0; i < size; ++i ) {
    if ( (celWts[i]/sint) > value ) {
      return 1;
    }
  }

  return 0;
}



/**********************************************************
** Function:   sum 
**
** Purpose:    This function sums up all the values in the sent 
**             vec array and returns the result
** Arguments:  vec, array of doubles
**             size, size of vec array
** Return:     result,  the sum of all the values in the sent 
**                      vec array
***********************************************************/
double sum( double * vec, int size ) {
  int i;
  double result = 0.0;

  /* error check */
  if ( vec == NULL || size < 0 ) {
    return result;
  }

  for ( i = 0; i < size; ++i ) {
    result += vec[i];
  }

  return result;
}



/**********************************************************
** Function:   maxWt
**
** Purpose:    This function returns the largest weight value in
**             the sent array of weight values.
** Arguments:  wts, array of weight values
**             size, size of vec array
** Return:     max,  the largest weight value in the sent array
***********************************************************/
double maxWt( double * wts, int size ) {
  int i;
  double max = wts[0];

  for ( i = 1; i < size; ++i ) {
    if ( wts[i] > max ) {
      max = wts[i];
    }
  }

  return max;
}


/**********************************************************
** Function:   combineShpFiles
**
** Purpose:    This function writes to the sent .shp file pointer the data found 
**             in all the .shp files in the current working directory.  Only the 
**             record IDs contained in the sent vector of IDs are retained in 
**             the new .shp file. 
** Arguments:  newShp,  pointer to the new shapefile to be created
**             ids, vector of record ID values
**             numIDs, number of record ID values
** Return:     1,  on success
**             -1, on error
***********************************************************/
int combineShpFiles( FILE * newShp, unsigned int * ids, int numIDs ) {


  int i;                        /* loop counter */
  DIR * dirp = NULL;            /* used to open the current directory */
  struct dirent * fileShp;      /* used for reading file names */
  int ptrShp = 0;               /* ptr to .shp files in the current directory */
  char * shpFileName = NULL;    /* stores the full shapefile name */
  int done = FALSE;             /* flag signalling all .shp files have been read */
  FILE * oldShp;                /* pointer to the original shapefiles */
  Shape firstShape;             /* stores data for the first shapefile read */
  Shape shape;                  /* temp storage for each additional shape */
                                /* file data */
  unsigned char byte;           /* used for reading in a byte at a time */
  unsigned char * ptr;          /* ptr used for converting data to the proper */
                                /* endian */
  unsigned char buffer[4];      /* used for writing data to the file */
  int filePosition = 100;
  unsigned int recordNum;
  unsigned int contentLength;
  unsigned int fileLength;
  int found;
  unsigned int numRecords = 0;
  unsigned int tempNumRecords = 0;
  double tempDouble;
  unsigned int numPoints;
  unsigned int numParts;
  int initXmin = FALSE;
  int initYmin = FALSE;
  int initXmax = FALSE;
  int initYmax = FALSE;


  /* initialize the shape struct */
  firstShape.records = NULL;
  firstShape.numRecords = 0;

  /* open the current directory */
  if((dirp = opendir(".")) == NULL) {
    Rprintf( "Error: Opening the current directory in C function combineShpFiles.\n" );
    return -1;
  }

  /* get the name of the first .shp file */
  while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    if ( strlen(fileShp->d_name) > 4 ) {
      ptrShp = fileMatch( fileShp->d_name, ".shp" );
      if ( ptrShp == 1 ) {
        if ( (shpFileName = (char *)malloc(strlen(fileShp->d_name)
              +  1)) == NULL ) {
          Rprintf( "Error: Allocating memory in C function combineShpFiles.\n" );
          closedir( dirp );
          return -1;
        }
        strcpy( shpFileName, fileShp->d_name);
    	 }
    }
  }

  /* make sure a .shp file was found */
  if ( ptrShp == 0 ) {
    Rprintf( "Error: Couldn't find a .shp file in C function combineShpFiles.\n" );
    closedir( dirp );
    return -1;
  }

  /* open the first .shp file */
  if ( (oldShp = fopen( shpFileName, "rb" )) == NULL ) {
    Rprintf( "Error: Opening %s file in C function combineShpFiles.\n", shpFileName );
    closedir( dirp );
    return -1;
  }

  /* parse this first .shp file */
  /* parse main file header */
  if ( parseHeader( oldShp, &firstShape ) == -1 ) {
    Rprintf( "Error: Reading %s file header in C function combineShpFiles.\n", shpFileName );
    fclose( oldShp );
    closedir( dirp );
    return -1;
  }  

  /* copy the first shapefile to the new shapefile */ 
  rewind( oldShp );
  for ( i = 0; i < 100; ++i ) {
    fread( &byte, sizeof(char), 1, oldShp );
    fwrite( &byte, sizeof(char), 1, newShp );
  }
  fileLength = 50;

  while ( filePosition < firstShape.fileLength*2 ) {

    /* read record number */
    fread( buffer, sizeof(char), 4, oldShp );
    recordNum = readBigEndian( buffer, 4 );
    filePosition += 4;

    ++numRecords;

    /* look for record number in the ids array */
    found = FALSE;
    for ( i = 0; i < numIDs; ++i ) {
      if ( recordNum == ids[i] ) {
        found = TRUE;
        break;
      }
    }

    /* write record number */
    if ( found == TRUE ) {
      fwrite( buffer, sizeof(char), 4, newShp );
      fileLength += 2;
    }

    /* read in content length */
    fread( buffer, sizeof(char), 4, oldShp );
    contentLength = readBigEndian( buffer, 4 );
    filePosition += 4;

    if ( found == TRUE ) {
      fwrite( buffer, sizeof(char), 4, newShp );
      fileLength += 2;
    }

    /* ignore shape type */
    fread( buffer, sizeof(char), 4, oldShp );
    if ( found == TRUE ) {
      fwrite( buffer, sizeof(char), 4, newShp );
    }
    filePosition += 4;

    /* Polygon or Polyline shape types */
    if ( firstShape.shapeType == POLYGON || firstShape.shapeType == POLYLINE ||
    	firstShape.shapeType == POLYGON_Z || firstShape.shapeType == POLYLINE_Z || 
    	firstShape.shapeType == POLYGON_M || firstShape.shapeType == POLYLINE_M ) {

      /* copy the bounding box data */
      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initXmin == FALSE ) {
          firstShape.Xmin = tempDouble;
          initXmin = TRUE;
        }
        if ( tempDouble < firstShape.Xmin ) {
          firstShape.Xmin = tempDouble;
        }
      }
      filePosition += 8;

      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initYmin == FALSE ) {
          firstShape.Ymin = tempDouble;
          initYmin = TRUE;
        }
        if ( tempDouble < firstShape.Ymin ) {
          firstShape.Ymin = tempDouble;
        }
      }
      filePosition += 8;
  
      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initXmax == FALSE ) {
          firstShape.Xmax = tempDouble;
          initXmax = TRUE;
        }
        if ( tempDouble > firstShape.Xmax ) {
          firstShape.Xmax = tempDouble;
        }
      }
      filePosition += 8;

      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initYmax == FALSE ) {
          firstShape.Ymax = tempDouble;
          initYmax = TRUE;
        }
        if ( tempDouble > firstShape.Ymax ) {
          firstShape.Ymax = tempDouble;
        }
      }
      filePosition += 8;

      /* read and ignore the number of parts */
      fread( buffer, sizeof(char), 4, oldShp );
      if ( found == TRUE ) {
        fwrite( buffer, sizeof(char), 4, newShp );
      }
      numParts = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read the number of points */
      fread( buffer, sizeof(char), 4, oldShp );
      if ( found == TRUE ) {
        fwrite( buffer, sizeof(char), 4, newShp );
      }
      numPoints = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* copy the part info */
      for ( i=0; i < numParts; ++i ) {
        fread( buffer, sizeof(char), 4, oldShp );
        if ( found == TRUE ) {
          fwrite( buffer, sizeof(char), 4, newShp );
        }
        filePosition += 4;
      } 

      /* copy point coordinates */
      for ( i=0; i < numPoints; ++i ) {
        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
        }
        filePosition += 8;
        if ( fread( &tempDouble, sizeof(double), 1, oldShp ) == 0 ) {
          Rprintf( "Error: Reading shapefile in C function combineShpFiles.\n" );
          fclose( oldShp );
          return -1;
        }
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
        }
        filePosition += 8;
      }

      /* Point shape types */
    } else if ( firstShape.shapeType == POINTS ||
    	           firstShape.shapeType == POINTS_Z ||
    	           firstShape.shapeType == POINTS_M ) {

      /* copy the x coordinate */
      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initXmin == FALSE ) {
          firstShape.Xmin = tempDouble;
          firstShape.Xmax = tempDouble;
          initXmin = TRUE;
        }
        if ( tempDouble < firstShape.Xmin ) {
          firstShape.Xmin = tempDouble;
        }
        if ( tempDouble > firstShape.Xmax ) {
          firstShape.Xmax = tempDouble;
        }
      }
      filePosition += 8;

      /* copy the y coordinate */
      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initYmin == FALSE ) {
          firstShape.Ymin = tempDouble;
          firstShape.Ymax = tempDouble;
          initYmin = TRUE;
        }
        if ( tempDouble < firstShape.Ymin ) {
          firstShape.Ymin = tempDouble;
        }
        if ( tempDouble > firstShape.Ymax ) {
          firstShape.Ymax = tempDouble;
        }
      }
      filePosition += 8;

    /* got a shape number that we can't parse */
    } else {
      Rprintf( "Error: Unrecognized shape type in C function combineShpFiles.\n" );
      return -1; 
    }

    if ( found == TRUE ) {
      fileLength += contentLength;
    }

  }

  fclose( oldShp );

    /* get the name of the next .shp file */
  while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    if ( strlen(fileShp->d_name) > 4 ) {
      ptrShp = fileMatch( fileShp->d_name, ".shp" );
      if ( ptrShp == 1 ) {
        if ( (shpFileName = (char *)malloc(strlen(fileShp->d_name)
              +  1)) == NULL ) {
          Rprintf( "Error: Allocating memory in C function combineShpFiles.\n" );
          closedir( dirp );
          return -1;
        }
        strcpy( shpFileName, fileShp->d_name);
    	 }
    }
  }

  /* determine whether a .shp file was found */
  if ( ptrShp == 0 ) {
        done = TRUE;
  }

/* go through the rest of the .shp files in this directory */
  while ( done == FALSE ) {

    /* open the next .shp file */
    if ( (oldShp = fopen( shpFileName, "rb" )) == NULL ) {
      Rprintf( "Error: Opening %s file in C function combineShpFiles.\n", shpFileName );
      return -1;
    }

    /* initialize the shape struct */
    shape.records = NULL;
    shape.numRecords = 0;

    /* get this next file's header info */
    if ( parseHeader( oldShp, &shape ) == -1 ) {
      Rprintf("Error: Reading %s file header in C function combineShpFiles.\n", shpFileName);
      fclose( oldShp );
      return -1;
    }  

    /* make sure we have consistant shp files */
    if ( shape.shapeType != firstShape.shapeType ) {
      Rprintf("Error: Multiple shapefiles with varying shape types in C function combineShpFiles.\n" );
      fclose( oldShp );
      return -1;
    }

    numRecords += tempNumRecords;
    filePosition = 100;
    tempNumRecords = 0;
    while ( filePosition < shape.fileLength * 2 ) {

      /* read record number */
      fread( buffer, sizeof(char), 4, oldShp );
      recordNum = readBigEndian( buffer, 4 );
      filePosition += 4;
      ++tempNumRecords;
      recordNum += numRecords;

      /* look for record number in the ids array */
      found = FALSE;
      for ( i = 0; i < numIDs; ++i ) {
        if ( recordNum == ids[i] ) {
          found = TRUE;
          break;
        }
      }

      /* write the modified record number */
      if ( found == TRUE ) {
        ptr = (unsigned char *) &(recordNum);
        buffer[0] = ptr[3];
        buffer[1] = ptr[2];
        buffer[2] = ptr[1];
        buffer[3] = ptr[0];
        fwrite( buffer, sizeof(char), 4, newShp );
        fileLength += 2;
      }

      /* read content length */
      fread( buffer, sizeof(char), 4, oldShp );
      contentLength = readBigEndian( buffer, 4 );
      filePosition += 4;

      if ( found == TRUE ) {
        fwrite( buffer, sizeof(char), 4, newShp );
        fileLength += 2;
      }

      /* ignore shape type */
      fread( buffer, sizeof(char), 4, oldShp );
      if ( found == TRUE ) {
        fwrite( buffer, sizeof(char), 4, newShp );
      }
      filePosition += 4;

      /* Polygon or Polyline shape types */
    if ( firstShape.shapeType == POLYGON || firstShape.shapeType == POLYLINE ||
    	firstShape.shapeType == POLYGON_Z || firstShape.shapeType == POLYLINE_Z || 
    	firstShape.shapeType == POLYGON_M || firstShape.shapeType == POLYLINE_M ) {

        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
          if ( tempDouble < firstShape.Xmin ) {
            firstShape.Xmin = tempDouble;
          }
        }
        filePosition += 8;

        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
          if ( tempDouble < firstShape.Ymin ) {
            firstShape.Ymin = tempDouble;
          }
        }
        filePosition += 8;

        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
          if ( tempDouble > firstShape.Xmax ) {
            firstShape.Xmax = tempDouble;
          }
        }
        filePosition += 8;

        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
          if ( tempDouble > firstShape.Ymax ) {
            firstShape.Ymax = tempDouble;
          }
        }
        filePosition += 8;

        /* read and ignore the number of parts */
        fread( buffer, sizeof(char), 4, oldShp );
        if ( found == TRUE ) {
          fwrite( buffer, sizeof(char), 4, newShp );
        }
        numParts = readLittleEndian( buffer, 4 );
        filePosition += 4;

        /* read the number of points */
        fread( buffer, sizeof(char), 4, oldShp );
        if ( found == TRUE ) {
          fwrite( buffer, sizeof(char), 4, newShp );
        }
        numPoints = readLittleEndian( buffer, 4 );
        filePosition += 4;
  
        /* copy the part info */
        for ( i=0; i < numParts; ++i ) {
          fread( buffer, sizeof(char), 4, oldShp );
          if ( found == TRUE ) {
            fwrite( buffer, sizeof(char), 4, newShp );
          }
          filePosition += 4;
        } 

        /* copy point coordinates */
        for ( i=0; i < numPoints; ++i ) {
          fread( &tempDouble, sizeof(double), 1, oldShp );
          if ( found == TRUE ) {
            fwrite( &tempDouble, sizeof(double), 1, newShp );
          }
          filePosition += 8;
          if ( fread( &tempDouble, sizeof(double), 1, oldShp ) == 0 ) {
            Rprintf( "Error: Reading shapefile in in C function combineShpFiles.\n" );
            return -1;
          }
          if ( found == TRUE ) {
            fwrite( &tempDouble, sizeof(double), 1, newShp );
          }
          filePosition += 8;
        }

      /* Point shape types */
      }  else {

        /* copy the x coordinate */
        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
          if ( tempDouble < firstShape.Xmin ) {
            firstShape.Xmin = tempDouble;
          }
          if ( tempDouble > firstShape.Xmax ) {
            firstShape.Xmax = tempDouble;
          }
        }
        filePosition += 8;

        /* copy over Y coordinant */
        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
          if ( tempDouble < firstShape.Ymin ) {
            firstShape.Ymin = tempDouble;
          }
          if ( tempDouble > firstShape.Ymax ) {
            firstShape.Ymax = tempDouble;
          }
        }
        filePosition += 8;

      }

      if ( found == TRUE ) {
        fileLength += contentLength;
      }

    }

    fclose( oldShp );

    /* get the next .shp file */
    ptrShp = 0;
    while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
      if ( strlen(fileShp->d_name) > 4 ) {
        ptrShp = fileMatch( fileShp->d_name, ".shp" );
        if ( ptrShp == 1 ) {
          if ( (shpFileName = (char *)malloc(strlen(fileShp->d_name)
                +  1)) == NULL ) {
            Rprintf( "Error: Allocating memory in C function combineShpFiles.\n" );
            closedir( dirp );
            return -1;
          }
          strcpy( shpFileName, fileShp->d_name);
        }
      }
    }

    /* determine whether there are no more .shp files */
    if ( ptrShp == 0 ) {
      done = TRUE;
    }

  }

  firstShape.fileLength = fileLength;

  /* overwrite the new header information into the new shapefile */
  fseek( newShp, 24, SEEK_SET );
  ptr = (unsigned char *) &(firstShape.fileLength);
  buffer[0] = ptr[3];
  buffer[1] = ptr[2];
  buffer[2] = ptr[1];
  buffer[3] = ptr[0];
  fwrite( buffer, sizeof(char), 4, newShp ) ;
  fseek( newShp, 36, SEEK_SET );

  /* little endian byte order */
  fwrite( &(firstShape.Xmin), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Ymin), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Xmax), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Ymax), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Xmin), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Ymin), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Xmax), sizeof(double), 1, newShp );
  if ( fwrite( &(firstShape.Ymax), sizeof(double), 1, newShp ) == 0 ) {
    Rprintf( "Error: Writing to new .shp file in C function combineShpFile.\n" );
    return -1;
  }

  return 1;
}


/**********************************************************
** Function:   createNewTempShpFile
**
** Purpose:    This function writes to the sent .shp file pointer the data found 
**             in the .shp file indicated by the sent shapefile name.  Only the 
**             record IDs contained in the sent vector of IDs are retained in 
**             the new .shp file. 
** Arguments:  newShp,  pointer to the new shapefile to be created
**             shapeFileName, name of the shapefile
**             ids, vector of record ID values
**             numIDs, number of record ID values
** Return:     1,  on success
**             -1, on error
***********************************************************/
int createNewTempShpFile( FILE * newShp, char * shapeFileName, 
		          unsigned int * ids, int numIDs ) {


  int i;                        /* loop counter */
  FILE * oldShp;                /* pointer to the original shapefiles */
  Shape firstShape;             /* stores data for the first shapefile read */
  unsigned char byte;           /* used for reading in a byte at a time */
  unsigned char * ptr;          /* ptr used for converting data to the proper */
                                /* endian */
  unsigned char buffer[4];      /* used for writing data to the file */
  int filePosition = 100;
  unsigned int recordNum;
  unsigned int contentLength;
  unsigned int fileLength;
  int found;
  unsigned int numRecords = 0;
  double tempDouble;
  unsigned int numPoints;
  unsigned int numParts;
  int initXmin = FALSE;
  int initYmin = FALSE;
  int initXmax = FALSE;
  int initYmax = FALSE;


  /* initialize the shape struct */
  firstShape.records = NULL;
  firstShape.numRecords = 0;

  /* open the shapefile */
  if ( (oldShp = fopen( shapeFileName, "rb" )) == NULL ) {
    Rprintf( "Error: Opening %s file in C function createNewTempShpFile.\n", shapeFileName );
    return -1;
  }

  /* parse the shapefile */
  /* parse main file header */
  if ( parseHeader( oldShp, &firstShape ) == -1 ) {
    Rprintf( "Error: Reading %s file header in C function createNewTempShpFile.\n", shapeFileName );
    fclose( oldShp );
    return -1;
  }  

  /* copy the shapefile to the new shapefile */ 
  rewind( oldShp );
  for ( i = 0; i < 100; ++i ) {
    fread( &byte, sizeof(char), 1, oldShp );
    fwrite( &byte, sizeof(char), 1, newShp );
  }
  fileLength = 50;

  while ( filePosition < firstShape.fileLength*2 ) {

    /* read record number */
    fread( buffer, sizeof(char), 4, oldShp );
    recordNum = readBigEndian( buffer, 4 );
    filePosition += 4;
    ++numRecords;

    /* look for record number in the ids array */
    found = FALSE;
    for ( i = 0; i < numIDs; ++i ) {
      if ( recordNum == ids[i] ) {
        found = TRUE;
        break;
      }
    }

    /* write record number */
    if ( found == TRUE ) {
      fwrite( buffer, sizeof(char), 4, newShp );
      fileLength += 2;
    }  

    /* read content length */
    fread( buffer, sizeof(char), 4, oldShp );
    contentLength = readBigEndian( buffer, 4 );
    filePosition += 4;
    if ( found == TRUE ) {
      fwrite( buffer, sizeof(char), 4, newShp );
      fileLength += 2;
    }

    /* ignore shape type */
    fread( buffer, sizeof(char), 4, oldShp );
    if ( found == TRUE ) {
      fwrite( buffer, sizeof(char), 4, newShp );
    }
    filePosition += 4;


    /* Polygon or Polyline shape types */
    if ( firstShape.shapeType == POLYGON || firstShape.shapeType == POLYLINE ||
    	firstShape.shapeType == POLYGON_Z || firstShape.shapeType == POLYLINE_Z || 
    	firstShape.shapeType == POLYGON_M || firstShape.shapeType == POLYLINE_M ) {

      /* copy the bounding box data */
      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initXmin == FALSE ) {
          firstShape.Xmin = tempDouble;
          initXmin = TRUE;
        }
        if ( tempDouble < firstShape.Xmin ) {
          firstShape.Xmin = tempDouble;
        }
      }
      filePosition += 8;

      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initYmin == FALSE ) {
          firstShape.Ymin = tempDouble;
          initYmin = TRUE;
        }
        if ( tempDouble < firstShape.Ymin ) {
          firstShape.Ymin = tempDouble;
        }
      }
      filePosition += 8;
  
      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initXmax == FALSE ) {
          firstShape.Xmax = tempDouble;
          initXmax = TRUE;
        }
        if ( tempDouble > firstShape.Xmax ) {
          firstShape.Xmax = tempDouble;
        }
      }
      filePosition += 8;

      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initYmax == FALSE ) {
          firstShape.Ymax = tempDouble;
          initYmax = TRUE;
        }
        if ( tempDouble > firstShape.Ymax ) {
          firstShape.Ymax = tempDouble;
        }
      }
      filePosition += 8;

      /* read and ignore the number of parts */
      fread( buffer, sizeof(char), 4, oldShp );
      if ( found == TRUE ) {
        fwrite( buffer, sizeof(char), 4, newShp );
      }
      numParts = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* read the number of points */
      fread( buffer, sizeof(char), 4, oldShp );
      if ( found == TRUE ) {
        fwrite( buffer, sizeof(char), 4, newShp );
      }
      numPoints = readLittleEndian( buffer, 4 );
      filePosition += 4;

      /* copy the part info */
      for ( i=0; i < numParts; ++i ) {
        fread( buffer, sizeof(char), 4, oldShp );
        if ( found == TRUE ) {
          fwrite( buffer, sizeof(char), 4, newShp );
        }
        filePosition += 4;
      } 

      /* copy point coordinates */
      for ( i=0; i < numPoints; ++i ) {
        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
        }
        filePosition += 8;
        if ( fread( &tempDouble, sizeof(double), 1, oldShp ) == 0 ) {
          Rprintf( "Error: Reading shapefile in C function createNewTempShpFile.\n" );
          fclose( oldShp );
          return -1;
        }
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
        }
        filePosition += 8;
      }

      /* for a Polygon_Z or Polyline_Z shapefile, copy Z range and Z values */
      if ( firstShape.shapeType == POLYGON_Z ||
      	 firstShape.shapeType == POLYLINE_Z ) {

        /* copy Z range */
        for ( i=0; i < 2; ++i ) {
          fread( &tempDouble, sizeof(double), 1, oldShp );
          if ( found == TRUE ) {
            fwrite( &tempDouble, sizeof(double), 1, newShp );
          }
          filePosition += 8;
        }

        /* copy Z values */
        for ( i=0; i < numPoints; ++i ) {
          fread( &tempDouble, sizeof(double), 1, oldShp );
          if ( found == TRUE ) {
            fwrite( &tempDouble, sizeof(double), 1, newShp );
          }
          filePosition += 8;
        } 
      }

      /* for a Polygon_Z, Polyline_Z, Polygon_M, or Polyline_M shapefile, copy
         M range and M values */
      if ( firstShape.shapeType == POLYGON_Z ||
      	 firstShape.shapeType == POLYLINE_Z ||
      	 firstShape.shapeType == POLYGON_M ||
      	 firstShape.shapeType == POLYLINE_M ) {

        /* copy M range */
        for ( i=0; i < 2; ++i ) {
          fread( &tempDouble, sizeof(double), 1, oldShp );
          if ( found == TRUE ) {
            fwrite( &tempDouble, sizeof(double), 1, newShp );
          }
          filePosition += 8;
        }

        /* copy M values */
        for ( i=0; i < numPoints; ++i ) {
          fread( &tempDouble, sizeof(double), 1, oldShp );
          if ( found == TRUE ) {
            fwrite( &tempDouble, sizeof(double), 1, newShp );
          }
          filePosition += 8;
        }
      }

    /* Point shape types */
    } else if ( firstShape.shapeType == POINTS ||
    	           firstShape.shapeType == POINTS_Z ) {

      /* copy the x coordinate */
      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initXmin == FALSE ) {
          firstShape.Xmin = tempDouble;
          firstShape.Xmax = tempDouble;
          initXmin = TRUE;
        }
        if ( tempDouble < firstShape.Xmin ) {
          firstShape.Xmin = tempDouble;
        }
        if ( tempDouble > firstShape.Xmax ) {
          firstShape.Xmax = tempDouble;
        }
      }
      filePosition += 8;

      /* copy the y coordinate */
      fread( &tempDouble, sizeof(double), 1, oldShp );
      if ( found == TRUE ) {
        fwrite( &tempDouble, sizeof(double), 1, newShp );
        if ( initYmin == FALSE ) {
          firstShape.Ymin = tempDouble;
          firstShape.Ymax = tempDouble;
          initYmin = TRUE;
        }
        if ( tempDouble < firstShape.Ymin ) {
          firstShape.Ymin = tempDouble;
        }
        if ( tempDouble > firstShape.Ymax ) {
          firstShape.Ymax = tempDouble;
        }
      }
      filePosition += 8;

      /* for a Point_Z shapefile, copy Z value */
      if ( firstShape.shapeType == POINTS_Z ) {

        /* copy Z value */
        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
        }
        filePosition += 8;

      }

      /* for a Point_Z or Point_M shapefile, copy M value */
      if ( firstShape.shapeType == POINTS_Z ||
      	firstShape.shapeType == POINTS_M ) {

        /* copy M value */
        fread( &tempDouble, sizeof(double), 1, oldShp );
        if ( found == TRUE ) {
          fwrite( &tempDouble, sizeof(double), 1, newShp );
        }
        filePosition += 8;

      }

    /* got a shape number that we can't parse */
    } else {
      Rprintf( "Error: Unrecognized shape type in C function createNewTempShpFile.\n" );
      return -1; 
    }

    if ( found == TRUE ) {
      fileLength += contentLength;
    }

  }

  fclose( oldShp );
  firstShape.fileLength = fileLength;

  /* overwrite the new header information into the new shapefile */
  fseek( newShp, 24, SEEK_SET );
  ptr = (unsigned char *) &(firstShape.fileLength);
  buffer[0] = ptr[3];
  buffer[1] = ptr[2];
  buffer[2] = ptr[1];
  buffer[3] = ptr[0];
  fwrite( buffer, sizeof(char), 4, newShp ) ;
  fseek( newShp, 36, SEEK_SET );

  /* little endian byte order */
  fwrite( &(firstShape.Xmin), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Ymin), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Xmax), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Ymax), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Xmin), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Ymin), sizeof(double), 1, newShp );
  fwrite( &(firstShape.Xmax), sizeof(double), 1, newShp );
  if ( fwrite( &(firstShape.Ymax), sizeof(double), 1, newShp ) == 0 ) {
    Rprintf( "Error: Writing to new .shp file in C function createNewTempShpFile.\n" );
    return -1;
  }

  return 1;
}


/**********************************************************
** Function:   numLevels
**
** Purpose:    This function does the "Determine the number of levels for
**             hierarchical randomization" part of the grts function.
**             It works for points, polylines, and polygon shape types.
** Algorithm:  It uses the same algorithm that is inplemented in the
**             R version.  For determining the weight values, areaIntersection
**             is called for a polygon shapefile, lintFcn is called for a
**             polylines shapefile, and cWtFcn is called for a points
**             shapefile.
** Notes:      Before the algorithm is run a temporary .shp file is created
**             and sent to the combineShpFiles() function.  This function will
**             combine the data found in all the .shp files in the current
**             working directory into one large .shp file.  This one combined
**             .shp file is then used in the algorithm.
**             Records that have ID numbers not found in the sent dsgnmdIDVec
**             vector are ignored.
** Arguments:  nsmpVec,  number of points to select in the sample
**             shiftGridVec,  flag signalling whether to do random shift of
**                            grid,  1 shift, 0 don't shift
**             startLevVec,  starting value to use for the number of levels
**                           of the grid.
**             maxLevVec,  maximum value to use for the number of levels of the
**                         grid.
**             dsgnmdIDVec, vector of record IDs that have weights and should
**                          be used in the calculations 
**             dsgnmdVec,  vector of weights corresponding to the aboe IDs
** Return:     results, an R object containing the final cell weights, sint,
**                      xc and yc vectors, dx, dy, and nlev
**                      If an error occurs results will return set to NULL
***********************************************************/
SEXP numLevels( SEXP fileNamePrefix, SEXP nsmpVec, SEXP shiftGridVec, 
                SEXP startLevVec, SEXP maxLevVec, SEXP dsgnmdIDVec, 
                SEXP dsgnmdVec ) {

  int i;            /* loop counter */
  Shape shape;      /* shape struct for holding a section of the records from*/
                    /* the file */
  FILE * fptr;      /* ptr to the shapefile */
  int maxlev;

  /* vars for calculating the cell weights, names taken from R version */
  int nlev = 0;
  int nlv2;
  double dx = 0.0;
  double dy = 0.0;
  double sint;
  double roffX, roffY;
  double * xc = NULL;
  double * yc = NULL;
  double * tempXc = NULL;
  double * tempYc = NULL;
  double * celWts = NULL;   /* cell weights */
  int celWtsSize = 0;       /* size of cell weights array */
  double celMax = 0.0;      /* maximum cell total inclusion probability */
  int celMaxInd = 0;        /* indicator for whether celMax is unchanged */

  /* C versions of sent vars */
  int nsmp;
  int shiftGrid = 0;  

  /* shape maxs, mins, and extents */
  double gridXMin;
  double gridYMin;
  double gridXMax;
  double gridYMax;
  double gridExtent;

  int inc;         /* amount to increase nlev by for each round */

  /* temp pointer for converting sent R objects to C vars */
  int * intPtr;

  /* vars for converting results into an R object */
  SEXP nlevVec, dxVec, dyVec, xcVec, ycVec, celWtsVec, sintVec;
  SEXP colNamesVec;

  SEXP results = NULL;    /* R object for returning final results to R */
  unsigned int * dsgnmdID = NULL; /*array of the ID numbers that have weights */
  double * dsgnmd = NULL;       /* array of weights that corresponde to the */
                                /* to the array of ID numbers */
  int dsgSize = length( dsgnmdIDVec ); /* number of IDs in the dsgnmdID array */
  FILE * newShp = NULL;   /* pointer to the temp .shp file that will consist */
                          /* of the data found in all the .shp files found in */
                          /* the current working directory */
  char * shpFileName = NULL;  /* stores full shapefile name */
  int singleFile = FALSE;

  /* see if a specific file was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full .shp file name */
    if ((shpFileName = (char *)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
                                              + strlen(".shp") + 1)) == NULL ){
      Rprintf( "Error: Allocating memory in C function numLevels.\n" );
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
    Rprintf( "Error: Creating temporary .shp file %s in C function numLevels.\n", TEMP_SHP_FILE );
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
      Rprintf( "Error: Combining multiple shapefiles in C function numLevels.\n" );
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
      Rprintf( "Error: Creating temporary shapefile in C function numLevels.\n" );
      return results; 
    }
    fclose( newShp );
  }

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;

  /* open the temporary .shp file */
  if ( (fptr = fopen( TEMP_SHP_FILE, "rb" )) == NULL ) {
    Rprintf( "Error: Opening shapefile in C function numLevels.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    return results;
  }

  /* parse main file header */
  if ( parseHeader( fptr, &shape ) == -1 ) {
    Rprintf( "Error: Reading main file header in C function numLevels.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    fclose( fptr );
    remove( TEMP_SHP_FILE );
    return results;
  }

  /* get the min and max for x and y and determine grid extent */
  gridXMin = shape.Xmin;
  gridYMin = shape.Ymin;
  gridXMax = shape.Xmax;
  gridYMax = shape.Ymax;
  gridExtent = MAX( (gridXMax - gridXMin), (gridYMax - gridYMin) );
  gridXMin = gridXMin - gridExtent * 0.04;
  gridYMin = gridYMin - gridExtent * 0.04;
  gridXMax = gridXMin + gridExtent * 1.08;
  gridYMax = gridYMin + gridExtent * 1.08;

  /* copy incoming R arguments to C variables */
  PROTECT( nsmpVec = AS_INTEGER( nsmpVec ) );
  intPtr = INTEGER_POINTER( nsmpVec );
  nsmp = *intPtr;
  UNPROTECT(1);

  /* grid shift flag */
  PROTECT( shiftGridVec = AS_INTEGER( shiftGridVec ) );
  intPtr = INTEGER_POINTER( shiftGridVec );
  shiftGrid = *intPtr;
  UNPROTECT(1);

  /* copy the dsgnmd mdm weights into an C array */
  if ( (dsgnmd = (double *) malloc( sizeof( double ) * dsgSize )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function numLevels.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    fclose( fptr );
    return results;
  }
  for ( i = 0; i < dsgSize; ++i ) {
    dsgnmd[i] = REAL( dsgnmdVec )[i];
  }

  /* allocate memory for initial cell weights */
  if ( (celWts = (double *) malloc( sizeof(double) ) ) == NULL ) {
    Rprintf( "Error: Allocating memory in C function numLevels.\n" );
    PROTECT( results = allocVector( VECSXP, 1 ) );
    UNPROTECT(1);
    fclose( fptr );
    return results;
  }
  celWtsSize = 1;

  /* input the RNG state */
  GetRNGstate();

  /* start algorithm */
  PROTECT( maxLevVec = AS_INTEGER( maxLevVec ) );
  intPtr = INTEGER_POINTER( maxLevVec );
  maxlev = *intPtr;
  UNPROTECT(1);
  if ( startLevVec != R_NilValue ) {
    PROTECT( startLevVec = AS_INTEGER( startLevVec ) );
    intPtr = INTEGER_POINTER( startLevVec );
    nlev = *intPtr;
    UNPROTECT(1);
  } else {
    nlev = ceil( log(nsmp)/log(4) );
    if ( nlev == 0 ) {
    	 nlev = 1;
    }
  }
  if ( nlev > maxlev ) {
  	nlev = maxlev;
  }
  Rprintf( "Initial number of levels: %i \n", nlev );
  celWts[0] = 99999.0;
  sint = 1.0;
  while ( any( celWts, celWtsSize, sint, 1 ) && 
        ( celMaxInd < 2 ) &&
        ( nlev <= maxlev ) ) {
    Rprintf( "Current number of levels: %i \n", nlev );
    celMax = maxWt( celWts, celWtsSize );
    nlv2 = pow( 2, nlev );
    dx = gridExtent * 1.08 / nlv2;
    dy = gridExtent * 1.08 / nlv2;

    /* allocate memory for tempXc */
    if ( tempXc ) {
      free( tempXc );
    }
    if ( (tempXc = (double *) malloc( sizeof(double) * (nlv2+1) )) == NULL ) {
      Rprintf( "Error: Allocating memory in C function numLevels.\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( fptr );
      return results;
    }

    /* allocate memory for tempYc */
    if ( tempYc ) {
      free( tempYc );
    }
    if ( (tempYc = (double *) malloc( sizeof(double) * (nlv2+1) )) == NULL ) {
      Rprintf( "Error: Allocating memory in C function numLevels.\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( fptr );
      return results;
    }

    seq( &tempXc, gridXMin, gridXMax, nlv2+1 );
    seq( &tempYc, gridYMin, gridYMax, nlv2+1 );

    /* allocate memory for xc */
    if ( xc ) {
      free( xc );
    }
    if ( (xc = (double *) malloc( sizeof(double) * (nlv2+1) * (nlv2+1) )) 
                                                              == NULL ) {
      Rprintf( "Error: Allocating memory in C function numLevels.\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( fptr );
      return results;
    }

    /* allocate memory for yc */
    if ( yc ) {
      free( yc );
    }
    if ( (yc = (double *) malloc( sizeof(double) * (nlv2+1) * (nlv2+1) )) 
                                                               == NULL ) {
      Rprintf( "Error: Allocating memory in C function numLevels.\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( fptr );
      return results;
    }

    /* as necessary, do the random shift of the grid */
    if ( shiftGrid == 1 ) {
      roffX = runif( 0.0, dx );
      roffY = runif( 0.0, dy );
      rep( &xc, tempXc, nlv2+1, roffX ); 
      rep2( &yc, tempYc, nlv2+1, roffY );
    } else {
      rep( &xc, tempXc, nlv2+1, 0 ); 
      rep2( &yc, tempYc, nlv2+1, 0 );
    }

    /* reallocate memory for the cell weights */
    if ( celWts ) {
      free( celWts );
    }
    celWtsSize = (nlv2+1) * (nlv2+1);
    if ( (celWts = (double *) malloc( sizeof(double) * celWtsSize ) ) == NULL){
      Rprintf( "Error: Allocating memory in C function numLevels.\n" );
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose( fptr );
      return results;
    }

    /* see if this is a Polygon shape type */
    if ( shape.shapeType == POLYGON || shape.shapeType == POLYGON_Z ||
    	    shape.shapeType == POLYGON_M ) { 
      if ( areaIntersection( &celWts, xc, yc, dx, dy, celWtsSize , &shape, fptr,
           dsgnmdID, dsgnmd, dsgSize ) == - 1) {
        Rprintf( "Error: In C function areaIntersection.\n" ); 
        PROTECT( results = allocVector( VECSXP, 1 ) );
        UNPROTECT(1);
        fclose( fptr );
        return results;
      }
   
    /* see if this is a Polyline shape type */
    } else if ( shape.shapeType == POLYLINE || shape.shapeType == POLYLINE_Z ||
    	           shape.shapeType == POLYLINE_M ) {
      if ( lintFcn ( &celWts, xc, yc, dx , dy, celWtsSize, &shape, fptr,
           dsgnmdID, dsgnmd, dsgSize ) == -1 ) {
        Rprintf( "Error: In C function lintFcn.\n" ); 
        PROTECT( results = allocVector( VECSXP, 1 ) );
        UNPROTECT(1); 
        fclose( fptr );
        return results;
      }

    /* see if this is a Point shape type */
    } else if ( shape.shapeType == POINTS || shape.shapeType == POINTS_Z ||
    	           shape.shapeType == POINTS_M ) {
      if ( cWtFcn( &celWts, xc, yc, dx , dy, celWtsSize, &shape, fptr,
                   dsgnmdID, dsgnmd, dsgSize ) == -1 ) {
        Rprintf( "Error: In C function cWtFcn.\n" ); 
        PROTECT( results = allocVector( VECSXP, 1 ) );
        UNPROTECT(1); 
        fclose( fptr );
        return results;
      }

    /* else unrecognized shape type */
    } else {
      Rprintf( "Error: Invalid shapefile type in C function numLevels.\n" ); 
      PROTECT( results = allocVector( VECSXP, 1 ) );
      UNPROTECT(1); 
      fclose( fptr );
      return results;
    }
    sint = sum( celWts, celWtsSize ) / nsmp; 

    /* as, necessary, increment celMaxInd */
    if ( maxWt( celWts, celWtsSize ) == celMax ) {
    	 ++celMaxInd;
    	 if ( celMaxInd == 2 ) {
    	   Rprintf( "Since the maximum value of total inclusion probability for the grid cells was \nnot changing, the algorithm for determining the number of levels for \nhierarchical randomization was terminated.\n" );
      }
    }

    /* determine the increment for nlev */
    inc = 1;
    if ( nlev < (maxlev - 1) ) {
      for ( i = 0; i < celWtsSize; ++i ) {
        if ( celWts[i] > 0 ) {
          inc = MAX( inc, ceil( log(celWts[i]/sint )/log(4) ) );
        }
      }
      if ( (nlev + inc) > maxlev ) {
      	inc = maxlev - nlev;
      }
    }
    nlev = nlev + inc;
  }
  Rprintf( "Final number of levels: %i \n", nlev-1 );

  /* write final results to the R objects */
  PROTECT( results = allocVector( VECSXP, 7 ) );
  PROTECT( nlevVec = allocVector( INTSXP, 1 ) );
  INTEGER( nlevVec )[0] = nlev;
  PROTECT( dxVec = allocVector( REALSXP, 1 ) );
  REAL( dxVec )[0] = dx;
  PROTECT( dyVec = allocVector( REALSXP, 1 ) );
  REAL( dyVec )[0] = dy;
  PROTECT( xcVec = allocVector( REALSXP, celWtsSize ) );
  PROTECT( ycVec = allocVector( REALSXP, celWtsSize ) );
  PROTECT( celWtsVec = allocVector( REALSXP, celWtsSize ) );
  for ( i = 0; i < celWtsSize; ++i ) {
    REAL( xcVec )[i] = xc[i];
    REAL( ycVec )[i] = yc[i];
    REAL( celWtsVec )[i] = celWts[i];
  }
  PROTECT( sintVec = allocVector( REALSXP, 1 ) );
  REAL( sintVec )[0] = sint;

  /* copy each data vector into the final results vector */  
  SET_VECTOR_ELT( results, 0, nlevVec); 
  SET_VECTOR_ELT( results, 1, dxVec ); 
  SET_VECTOR_ELT( results, 2, dyVec ); 
  SET_VECTOR_ELT( results, 3, xcVec ); 
  SET_VECTOR_ELT( results, 4, ycVec ); 
  SET_VECTOR_ELT( results, 5, celWtsVec ); 
  SET_VECTOR_ELT( results, 6, sintVec ); 

  /* create vector labels */
  PROTECT( colNamesVec = allocVector( STRSXP, 7 ) );
  SET_STRING_ELT( colNamesVec, 0, mkChar( "nlev" ) );
  SET_STRING_ELT( colNamesVec, 1, mkChar( "dx" ) );
  SET_STRING_ELT( colNamesVec, 2, mkChar( "dy" ) );
  SET_STRING_ELT( colNamesVec, 3, mkChar( "xc" ) );
  SET_STRING_ELT( colNamesVec, 4, mkChar( "yc" ) );
  SET_STRING_ELT( colNamesVec, 5, mkChar( "cel.wt" ) );
  SET_STRING_ELT( colNamesVec, 6, mkChar( "sint" ) );
  setAttrib( results, R_NamesSymbol, colNamesVec );

  /* output the RNG state */
  PutRNGstate();
  
  /* clean up */
  if ( celWts ) {
    free( celWts );
  }
  if ( tempXc ) {
    free( tempXc );
  }
  if ( tempYc ) {
    free( tempYc );
  }
  if ( xc ) {
    free( xc );
  }
  if ( yc ) {
    free( yc );
  }
  if ( dsgnmdID ) {
    free( dsgnmdID );
  }
  if ( dsgnmd ) {
    free( dsgnmd );
  }
  fclose( fptr );
  if ( singleFile == TRUE ) {
    free( shpFileName );
  }
  remove( TEMP_SHP_FILE );
  UNPROTECT(9);

  return results;
}




