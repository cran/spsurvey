/****************************************************************************** 
**  File:        dbfFileParser.c  
**
**  Purpose:     This is the C version of parsing a dbf file.  There are
**               functions for readin a dbf file and for writing a dbf file.
**  Programmers: Christian Platt, Tom Kincaid
**  Notes:       There is a sketch of the C struct dbf on paper that diagrams
**               how all the information is stored before being written to
**               the R matrix.
**  Created:     August 24, 2004
**  Revised:     January 22, 2007
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <time.h>
#include <sys/types.h>
#include <dirent.h>
#include <ctype.h>
#include <stdarg.h>
#include "shapeParser.h"

/* found in shapeParser.c */
extern int fileMatch( char * fileName, char * fileExt );
extern int readLittleEndian( unsigned char * buffer, int length );
extern SEXP getRecordShapeSizes( SEXP fileNamePrefix ); 
extern int parseHeader( FILE * fptr, Shape * shape );
extern void deallocateRecords( Record * head );
extern int parsePolygon( FILE * fptr, Shape * shape );


/**********************************************************
** Function:   deallocateDbf
**
** Purpose:    Deallocate all dynamic memory created for the
**             dbf struct
** Arguments:  dbf,  Dbf struct that contains dynamically allocated
**                   memory
** Return:     void
***********************************************************/
void deallocateDbf( Dbf * dbf ) {
  int i, j;

  /* deallocate data array */
  for ( i=0; i < dbf->numFields; ++i ) {

    /* deallcate all the data strings */
    for ( j=0; j < dbf->numRecords; ++j ) {
      free( dbf->fields[i].data[j] );
    }

    free( dbf->fields[i].data );
  }
   
  /* deallocate array of fields */ 
  free( dbf->fields );

  return;
}


/**********************************************************
** Function:   parseFields
**
** Purpose:    Parses the fields section and data records of
**             the dbf file.
** Algorithm:  For reading in the data values (records) the 
**             function iterates through the fields array reading
**             in all the data values for the current row.  It
**             then repeats for the next row and so forth.
** Notes:      As far as error checking goes we only check the last fread
**             since if one fails they all will after that so only need to
**             catch one.
**             This function will read in the deleted record flag to 
**             see if the record was deleted or not but even if the
**             record was flagged as deleted it's data is still read in
**             but a warning statement is sent to stdout.
**             The R version of this function does the same thing minus
**             the warning statement.  
**             This function also strips off any leading or trailing white
**             space from the field values.
** Arguments:  fptr,  file pointer to dbf file
**             dbf,   pointer to Dbf struct that stores the
**                    dbf file info and data
** Return:     1,  on success
**            -1,  on error
***********************************************************/
int parseFields( FILE * fptr, Dbf * dbf ) {
  int i, j, k;     /* loop counters */
  int row = 0;     /* row counter for iterating through the records */
  unsigned char byte;  /* temp storage for reading in one byte */
  char * tempStr;  /* temp storage of field values */
  int foundNonSpace = FALSE;   /* flag for signalling when we have skipped */
                               /* leading spaces in field values */

  /* calculate the number of fields and allocate memory */
  dbf->numFields = (dbf->headerLength-32-1)/32;

  if ( (dbf->fields = (Field *) malloc( sizeof(Field) * dbf->numFields )) 
        == NULL) {
    Rprintf( "Error: Allocating memory in C function parseFields.\n" );
    return -1;
  }

  /* read in all the field attributes */
  for ( i=0; i < dbf->numFields; ++i ) {
    fread( dbf->fields[i].name, sizeof(char), 11, fptr );
    dbf->fields[i].name[11] = '\0';
    fread( &(dbf->fields[i].type), sizeof(char), 1, fptr );
    fseek( fptr, 4, SEEK_CUR ); 
    if ( fread( &byte, sizeof(char), 1, fptr ) == 0 ) {
      return -1;
    }
    dbf->fields[i].length = 0;
    dbf->fields[i].length += (unsigned int) byte;
    fseek( fptr, 15, SEEK_CUR ); 

    /* allocate memory for array of data values for each field */
    if ((dbf->fields[i].data = (char **) malloc(sizeof(char *)*dbf->numRecords))
         == NULL ) {
      Rprintf( "Error: Allocating memory in C function parseFields.\n" );
      return -1;
    }
  }

  /* for some reason we need to skip one byte here */
  /* the R version allocates this byte to the first data field to get */
  /* around it */
  fseek( fptr, 1, SEEK_CUR );

  /* now gather field data */
  for ( row = 0; row < dbf->numRecords; ++row ) {

    /* read in the field deleted flag */
    fread( &byte, sizeof(char), 1, fptr );

    /* see if the record has been deleted */
    if ( byte != 0x20 ) {
      Rprintf( "Warning: Encountered deleted record in C function parseFields.\n" );
    }

    /* read in the values */
    for ( i=0; i < dbf->numFields; ++i ) {
      dbf->fields[i].data[row] = (char *) malloc( sizeof(char) * 
         dbf->fields[i].length+1); 
      tempStr = (char *) malloc( sizeof(char) * dbf->fields[i].length+1); 
      if ( fread(tempStr,sizeof(char), dbf->fields[i].length,fptr) == 0 ) {
        return -1;
      }

      tempStr[dbf->fields[i].length] = '\0';

      /* get rid of trailing white space */
      for ( j=dbf->fields[i].length-1; j >= 0; --j ) {
        if ( tempStr[j] == ' ' ) {
          tempStr[j] = '\0';
        } else {
          break;
        }
      }

      /* get rid of leading white space */
      foundNonSpace = FALSE;
      k = 0;
      for ( j = 0; j < dbf->fields[i].length; ++j ) {
        if ( tempStr[j] != ' ' || foundNonSpace == TRUE ) {
          dbf->fields[i].data[row][k] = tempStr[j];
          foundNonSpace = TRUE;
          ++k;
        } 
      }
      dbf->fields[i].data[row][k] = '\0';

      free( tempStr );
    }
  }

  return 1;
}


/**********************************************************
** Function:   parseDbfHeader
**
** Purpose:    Parses all the main file header information
**             and stores it in the sent Dbf struct.
** Arguments:  fptr,  file pointer to the dbf file
**             dbf,   pointer to Dbf struct that stores the
**                    dbf file info and data
** Return:     1,  on success
**             -1, on failure 
***********************************************************/
int parseDbfHeader( FILE * fptr, Dbf * dbf ) {
  unsigned char buffer[4];

  /* read in version number */
  if ( fread( buffer, sizeof(char), 1, fptr ) == 0 ) {
    return -1;
  }
  dbf->version = 0;
  dbf->version += (unsigned int) buffer[0]; 

  /* read in date YYMMDD */
  if ( fread( buffer, sizeof(char), 3, fptr ) == 0 ) {
    return -1;
  }
  dbf->year = dbf->month = dbf->day = 0;
  dbf->year += (unsigned int) buffer[0];
  dbf->month += (unsigned int) buffer[1];
  dbf->day += (unsigned int) buffer[2];

  /* read in number of records */
  if ( fread( buffer, sizeof(char), 4, fptr ) == 0 ) {
    return -1;
  }
  dbf->numRecords = readLittleEndian( buffer, 4 );

  /* read in length of header */
  if ( fread( buffer, sizeof(char), 2, fptr ) == 0 ) {
    return -1;
  }
  dbf->headerLength = readLittleEndian( buffer, 2 );

  /* read in length of record */
  if ( fread( buffer, sizeof(char), 2, fptr ) == 0 ) {
    return -1;
  }
  dbf->recordLength = readLittleEndian( buffer, 2 );

  /* skip next 20 bytes */
  if ( fseek( fptr, 20, SEEK_CUR ) == -1 ) {
    return -1;
  }

  return 1;
}


/**********************************************************
** Function:   printDbf  (ONLY USED FOR DEBUGGING CODE)
**
** Purpose:    Prints to stdout all the information and data
**             stored in the sent dbf struct.
** Arguments:  dbf,   pointer to Dbf struct that stores the
**                    dbf file info and data
** Return:     void
***********************************************************/
void printDbf( Dbf * dbf ) {
  int i, j;
  int row;

  /* dbf file header info */
  printf( "Version: %d\n", dbf->version );
  printf( "Year: %d\n", dbf->year );
  printf( "Month: %d\n", dbf->month );
  printf( "Day: %d\n", dbf->day );
  printf( "Num of Records: %d\n", dbf->numRecords );
  printf( "Header length: %d\n", dbf->headerLength );
  printf( "Record length: %d\n", dbf->recordLength );

  printf( "\n" );

  /* field info */
  for ( i=0; i < dbf->numFields; ++i ) {
    printf( "%d - Name: %s\n", i+1, dbf->fields[i].name );
    printf( "  Type: %c\n", dbf->fields[i].type );
    printf( "  Length: %d\n", dbf->fields[i].length );
  }

  /* records data */
  for ( row = 0; row < dbf->numRecords; ++row ) {
    printf( "NEW RECORD:\n" );
    for ( i=0; i < dbf->numFields; ++i ) {
      for ( j=0; j < dbf->fields[i].length; ++j ) {
        printf( "%c", dbf->fields[i].data[row][j] );
      }
      printf( "\n" );
    }
  }

  return;
}


/**********************************************************
** Function:   readDbfFile
**
** Purpose:    This function is called from R and is the entry point
**             into the C dbf file parser.  It creates a dbf struct
**             and then parses the entire file storing all info and 
**             data from the file into this struct.  It returns a 
**             R object with the data and info written to it.
** Arguments:  filePrefix,  name of the dbf file without the .dbf extension
**                          This argument can be specified as NULL in which
**                          case all the .dbf files in the current working
**                          directory are read in
** Return:     data,    R vector containing vectors of data entries
**                      for each field.  Each column in data will
**                      be labeled with the corresponding field name.
**                      If an error occurs this vector gets returned empty.
***********************************************************/
SEXP readDbfFile( SEXP fileNamePrefix ) {
  int i, col, row;             /* loop counter */
  FILE * fptr;       /* file pointer to shapefile */
  Shape shape;       /* struct to store all info and data found in shapefile */
  SEXP data = NULL;  /* R object to store data in for returning to R */
  char * fileName = NULL; /* full name of dbf file */
  int done = FALSE;  /* flag signalling all .shp files have been reads */
  DIR * dirp = NULL;        /* used to open the current directory */
  struct dirent * fileShp;  /* used for reading file names */
  int ptrShp = 0;           /* ptr to .shp files in the current directory */
  int shapeType = -1;  /* shape type of the first .shp file found */
  int strLen;        /* string length */
  Dbf * headDbf = NULL;  /* head ptr to list of dbf structs */
  Dbf * tempDbf = NULL;  /* used for traversing list of dbf structs */
  Dbf * dbf = NULL;             /* temp dbf strcut */
  char * shpFileName = NULL;  /* stores the full shapefile name */
  int singleFile = FALSE;  /* flag signalling when we are only looking for a */
                           /* single specified shapefile */
  SEXP tempVec;    /* temp vector for writing results to data vector */
  SEXP fieldsVec;  /* vector of field labels */
  unsigned int numRecords = 0;
  SEXP attribs, class, sizesVec;
  char str[20];
  int idx;  /* array index */

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;
  shape.numParts = 0;

  /* see if a specific file was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full .shp file name */
    if ((shpFileName = (char *)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
                                              + strlen(".shp") + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in dbfFileParser.c\n" );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix,0)));
    strcat( shpFileName, ".shp" );
    singleFile = TRUE;

  } else {

    /* open the current directory */
    if((dirp = opendir(".")) == NULL) {
      Rprintf( "Error: Opening the current directory in C function readDbfFile.\n" );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data;
    }

    /* get the name of the first .shp file */
    while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    	 if ( strlen(fileShp->d_name) > 4 ) {
    	   ptrShp = fileMatch( fileShp->d_name, ".shp" );
    	   if ( ptrShp == 1 ) {
    	     if ( (shpFileName = (char *)malloc(strlen(fileShp->d_name)
                +  1)) == NULL ) {
            Rprintf( "Error: Allocating memory in C function readDbfFile.\n" );
            closedir( dirp );
            PROTECT( data = allocVector( VECSXP, 1 ) );
            UNPROTECT( 1 );
            return data;
          }
          strcpy( shpFileName, fileShp->d_name);
    	   }
    	 }
    }

    /* make sure a .shp file was found */
    if ( ptrShp == 0 ) {
      Rprintf( "Error: Couldn't find any .shp files in the current directory.\n");
      Rprintf( "Error: Occured in C function readDbfFile.\n");
      closedir( dirp );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT( 1 );
      return data;
    }
  }

  while ( done == FALSE ) {

    /* open the shapefile */
    fptr = fopen( shpFileName, "rb" );
    if ( fptr == NULL ) {
      Rprintf( "Error: Opening shapefile in C function.\n" );
      Rprintf("Error: Make sure there is a corresponding .shp file for the specified shapefile name.\n");
      Rprintf( "Error: Occured in C function readDbfFile.\n");
      deallocateRecords( shape.records );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT( 1 );
      return data;
    }

    /* parse main file header */
    if ( parseHeader( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading .shp file header in C function.\n" );
        Rprintf( "Error: Occured in C function readDbfFile.\n");
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT( 1 );
        return data;
    }

    /* initialize the shape type if necessary and make sure that if there */
    /* are multiple .shp files that they are of the same shape type */
    if ( shapeType == -1 ) {
      shapeType = shape.shapeType;
    } else if ( shapeType != shape.shapeType ) {
      Rprintf( "Error: Multiple shapefiles have different shape types.\n" );
      Rprintf( "Error: Occured in C function readDbfFile.\n" );
      deallocateRecords( shape.records );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT( 1 );
      return data;
    }

    /* check for valid shape type */
    if ( shape.shapeType != POINTS &&  shape.shapeType != POLYLINE &&
         shape.shapeType != POLYGON && shape.shapeType != POINTS_Z &&
         shape.shapeType != POLYLINE_Z && shape.shapeType != POLYGON_Z && 
         shape.shapeType != POINTS_M && shape.shapeType != POLYLINE_M &&
         shape.shapeType != POLYGON_M ) {
      Rprintf( "Error: Unrecognized shape type.\n" );
      Rprintf( "Error: Occured in C function readDbfFile.\n" );
      deallocateRecords( shape.records );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT( 1 );
      return data; 
    }

    /* close the shapefile */
    fclose( fptr );

    /* build the corresponding .dbf file name */
    strLen = strlen( shpFileName );
    if ( (fileName = (char *) malloc( sizeof( char ) * (strLen+1) ))==NULL) {
      Rprintf( "Error: Allocating memory in C function readDbfFile.\n" );
      Rprintf( "Error: Occured in C function readDbfFile.\n" );
      deallocateRecords( shape.records );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data; 
    }
    strcpy( fileName, shpFileName ); 
    fileName[strLen] = '\0';
    fileName[strLen-1] = 'f';
    fileName[strLen-2] = 'b';
    fileName[strLen-3] = 'd';

    /* open the corresponding .dbf file */
    if ( (fptr = fopen( fileName,  "rb" )) == NULL ) {
      Rprintf( "Error: Couldn't find .dbf file for %s\n", shpFileName );
      deallocateRecords( shape.records );
      free( fileName );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data;
    }
    dbf = (Dbf *) malloc( sizeof( Dbf ) );

    /* parse the file */
    if ( parseDbfHeader( fptr, dbf ) == -1 ) {
      Rprintf( "Error: Reading dbf file header in C function.\n" );
      Rprintf( "Error: Occured in C function readDbfFile.\n" );
      deallocateRecords( shape.records );
      deallocateDbf( dbf );
      free( fileName );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data;
    }

    numRecords += dbf->numRecords;

    if ( parseFields( fptr, dbf ) == -1 ) {
      /* an error has occured */
      Rprintf( "Error: Reading dbf fields in C function.\n" );
      Rprintf( "Error: Occured in C function readDbfFile.\n" );
      deallocateRecords( shape.records );
      deallocateDbf( dbf );
      free( fileName );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data;
    }

    /* make sure this .dbf file's columns match the previous one if there */
    /* was a previous one */
    if ( headDbf != NULL ) {

      /* make sure there is the same number of columns */
      if ( headDbf->numFields != dbf->numFields ) {
        Rprintf("Error: Multiple .dbf files have varying number of fields.\n" );
        Rprintf("Error: Occured in readDbfFile() in C function readDbfFile.\n" );
        deallocateRecords( shape.records );
        deallocateDbf( dbf );
        free( fileName );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 1 ) );
        UNPROTECT( 1 );
        return data;
      }

      /* make sure column names match */
      for ( i = 0; i < headDbf->numFields; ++i ) {
        if ( strcmp( headDbf->fields[i].name, dbf->fields[i].name ) != 0 ) {
          Rprintf("Error: Multiple .dbf files have varying field names.\n" );
          Rprintf("Error: Occured in readDbfFile() in C function readDbfFile.\n" );
          deallocateRecords( shape.records );
          deallocateDbf( dbf );
          free( fileName );
          fclose( fptr );
          PROTECT( data = allocVector( VECSXP, 1 ) );
          UNPROTECT( 1 );
          return data;
        } 
      }
    } 

    /* add dbf to the list */
    dbf->next = NULL;
    if ( headDbf != NULL ) {
      tempDbf = headDbf;
      while ( tempDbf->next ) {
        tempDbf = tempDbf->next;
      }

      tempDbf->next = dbf;
    } else {
      headDbf = dbf;
    }

    /* get the next .shp file */
    if ( singleFile == TRUE )  {
      done = TRUE;
    } else {
      ptrShp = 0;
      while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    	   if ( strlen(fileShp->d_name) > 4 ) {
    	     ptrShp = fileMatch( fileShp->d_name, ".shp" );
    	     if ( ptrShp == 1 ) {
    	       if ( (shpFileName = (char *)malloc(strlen(fileShp->d_name)
                  +  1)) == NULL ) {
              Rprintf( "Error: Allocating memory in C function readDbfFile.\n" );
              closedir( dirp );
              deallocateRecords( shape.records );
              deallocateDbf( dbf );
              free( fileName );
              fclose( fptr );
              PROTECT( data = allocVector( VECSXP, 1 ) );
              UNPROTECT( 1 );
              return data;
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

    free( fileName );
    fclose( fptr );
  }

  /* close the current directory */
  if ( singleFile == FALSE )  {
    closedir( dirp );
  }

  /* write shape to R object */
  if ( shape.shapeType == POLYLINE || shape.shapeType == POLYGON ||
  	  shape.shapeType == POLYLINE_Z || shape.shapeType == POLYGON_Z ||
  	  shape.shapeType == POLYLINE_M || shape.shapeType == POLYGON_M ) {
 
    /* copy data to R object */
    PROTECT( data = allocVector( VECSXP, dbf->numFields+1 ) );
    for ( col = 0; col < dbf->numFields; ++col ) {
      idx = 0;

      /* determine if it is a numeric or string */
      if ( dbf->fields[col].type == 'F' || 
           dbf->fields[col].type == 'N' ) {
        PROTECT( tempVec = allocVector( REALSXP, numRecords ) );
      } else {
        PROTECT( tempVec = allocVector( STRSXP, numRecords ) );
      }

      tempDbf = headDbf;
      while ( tempDbf ) {
        for ( row = 0; row < dbf->numRecords; ++row ) {
          if ( dbf->fields[col].type == 'F' ||
               dbf->fields[col].type == 'N' ) {
            REAL( tempVec )[idx] = atof( tempDbf->fields[col].data[row] );
          } else {
            SET_STRING_ELT(tempVec,idx,mkChar( tempDbf->fields[col].data[row]));
          }
          ++idx;
        }
        tempDbf = tempDbf->next;
      }
      SET_VECTOR_ELT( data, col, tempVec );
      UNPROTECT( 1 );
    }

    /* add the area_mdm or length_mdm values */
    PROTECT( sizesVec = getRecordShapeSizes( fileNamePrefix ) );
    SET_VECTOR_ELT( data, col, sizesVec );
    UNPROTECT( 1 );


    /* add field names (column names) to the R object */
    PROTECT( fieldsVec = allocVector( STRSXP, dbf->numFields+1 ) );
    for ( i = 0; i < dbf->numFields; ++i ) {
      SET_STRING_ELT( fieldsVec, i, mkChar( dbf->fields[i].name ) );
    }
    if ( shape.shapeType == POLYGON || shape.shapeType == POLYGON_Z  ||
    	                                      shape.shapeType == POLYGON_M ) {
      SET_STRING_ELT( fieldsVec, i, mkChar( "area_mdm" ) );
    } else {
      SET_STRING_ELT( fieldsVec, i, mkChar( "length_mdm" ) );
    }
    setAttrib( data, R_NamesSymbol, fieldsVec );
    UNPROTECT( 1 );

  } else {

    /* copy data to R object */
    PROTECT( data = allocVector( VECSXP, dbf->numFields ) );
    for ( col = 0; col < dbf->numFields; ++col ) {
      idx = 0;

      /* determine if it is a numeric or string */
      if ( dbf->fields[col].type == 'F' ||
           dbf->fields[col].type == 'N' ) {
        PROTECT( tempVec = allocVector( REALSXP, numRecords ) );
      } else {
        PROTECT( tempVec = allocVector( STRSXP, numRecords ) );
      }

      tempDbf = headDbf;
      while ( tempDbf ) {
        for ( row = 0; row < tempDbf->numRecords; ++row ) {
          if ( dbf->fields[col].type == 'F' ||
               dbf->fields[col].type == 'N' ) {
            REAL( tempVec )[idx] = atof( tempDbf->fields[col].data[row] );
          } else {
            SET_STRING_ELT(tempVec,idx,mkChar( tempDbf->fields[col].data[row]));
          }
          ++idx;
        }
        tempDbf = tempDbf->next;
      }
      SET_VECTOR_ELT( data, col, tempVec );
      UNPROTECT( 1 );
    }

    /* add field names (column names) to the R object */
    PROTECT( fieldsVec = allocVector( STRSXP, dbf->numFields ) );
    for ( i = 0; i < dbf->numFields; ++i ) {
      SET_STRING_ELT( fieldsVec, i, mkChar( dbf->fields[i].name ) );
    }
    setAttrib( data, R_NamesSymbol, fieldsVec );
    UNPROTECT( 1 );

  }

  /* add the row names */
  PROTECT( attribs = allocVector( STRSXP, numRecords ));
  for ( i = 0; i < numRecords; ++i ) {
    sprintf( str, "%d", i+1 );
    SET_STRING_ELT( attribs, i, mkChar( str ) );
  }
  setAttrib( data, install("row.names"), attribs );
  UNPROTECT( 1 );
 
  /* add the class type */
  PROTECT( class = allocVector( STRSXP, 1 ) );
  SET_STRING_ELT( class, 0, mkChar( "data.frame" ) );
  classgets( data, class );
  UNPROTECT( 1 );

  /* deallocate list of dbf structs */
  tempDbf = headDbf;
  while( headDbf ) {
    tempDbf = headDbf->next;
    deallocateDbf( headDbf );
    headDbf = tempDbf;
  }

  /* clean up */
  deallocateRecords( shape.records );
  if ( singleFile == TRUE ) {
    free( shpFileName );
  } 
  UNPROTECT( 1 );

  return data;
}


/**********************************************************
** Function:   writeDbfFile
**
** Purpose:    This function is sent R vectors of the field names, field
**             values and name of the dbf file to create.  It follows the 
**             dbf file format found at 
**    http://www.clicketyclick.dk/databases/xbase/format/dbf.html#DBF_STRUCT
**
** Arguments:  fieldNames,  vector of the field names (column names)
**             fields,  vector of all the field values
**             filePrefix,  name of the dbf file to be created without
**                          the .dbf extension
** Return:     void
***********************************************************/
void writeDbfFile ( SEXP fieldNames, SEXP fields, SEXP filePrefix ) {
  int i, j, k, z;               /* loop counters */
  char * fileName;              /* stores full name of the file */
  char * prefix;                /* stores the prefix of the file name */
  char * dbf = ".dbf";          /* file name's extension */
  FILE * fptr;                  /* pointer to the file that is created */
  unsigned char byte;           /* for writing a byte at a time to the file */
  char buffer[256];             /* for storing a field value as a string */
                                /* this can never be larger than 256 */
  time_t now_t;                 /* used for determining the current date */
  struct tm date;               /* used for determining the current date */
  unsigned int tempInt;         /* temporary storage */
  unsigned int longestCol;      /* length of the widest column */
  unsigned int recordLength;    /* size of a record */
  unsigned int * colLengths;    /* number of chars in a column */
  unsigned int * decimalLengths;/* number of digits beyond the decimal point */
  unsigned int numRecords;      /* number of records */

  /* copy over the file name's prefix */
  if ( (prefix = (char *) malloc( strlen(CHAR(STRING_ELT(filePrefix,0)))+1)) 
                                                             == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeDbfFile.\n" );
    return;
  }
  strcpy( prefix, CHAR(STRING_ELT(filePrefix,0)) );

  /* create the .dbf file name */
  if ( (fileName = (char *) malloc( strlen(prefix) + strlen(dbf) + 1)) == NULL){
    Rprintf( "Error: Allocating memory in C function writeDbfFile.\n" );
    return;
  }
  strcpy( fileName, prefix );
  strcat( fileName, dbf );

  /* create the dbf file */
  if ( (fptr = fopen( fileName, "wb" )) == NULL ) {
    Rprintf( "Error: Creating dbf file to write to in C function writeDbfFile.\n" );
    return;
  }
 
  /* write the version number */
  byte = 0x03;   /* File without DBT */
  fwrite( &byte, sizeof(char), 1, fptr );

  /* get the current date */
  time(&now_t);
  date = *localtime(&now_t);

  /* write the date of last update which is the current date */
  /* in little endian */
  ++date.tm_mon;   /* months start at 0 so we need to increment by one */
  fwrite( &date.tm_year, sizeof(char), 1, fptr );
  fwrite( &date.tm_mon, sizeof(char), 1, fptr );
  fwrite( &date.tm_mday, sizeof(char), 1, fptr );

  /* write the number of records */
  /* little endian */
  numRecords = length( VECTOR_ELT( fields, 0 ) );
  fwrite( &numRecords, sizeof(char), 4, fptr );

  /* write the length of the header */
  /* little endian */
  tempInt = 32 * (length(fieldNames) + 1) + 1;
  fwrite( &tempInt, sizeof(char), 2, fptr );
 
  /* figure out the lengths for each column and total length for each record */
  recordLength = 0;
  if ( (colLengths = (unsigned int *) malloc( sizeof(unsigned int) 
                                                 * length(fields))) == NULL) {
    Rprintf( "Error: Allocating memory in C function writeDbfFile.\n" );
    return;
  }
  if ( (decimalLengths = (unsigned int *)malloc(sizeof(unsigned int)
                                                  *length(fields))) == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeDbfFile.\n" );
    return;
  }
  for ( i = 0; i < length( fields ); ++i ) {
    tempInt = 0;
    longestCol = 0;
    decimalLengths[i] = 0;

    for ( j = 0; j < length(VECTOR_ELT(fields,i)); ++j ) {

      /* we have a character string */
      if ( IS_CHARACTER( VECTOR_ELT(fields, i)) == 1 ) {
        tempInt = strlen( CHAR(STRING_ELT( VECTOR_ELT(fields,i) , j )) );

      /* we have an integer */
      } else if ( IS_INTEGER( VECTOR_ELT(fields, i)) == 1 ) {
        tempInt = snprintf(buffer, 255, "%d",INTEGER(VECTOR_ELT(fields,i))[j]);

      /* we have a real */
      } else if ( IS_NUMERIC( VECTOR_ELT(fields, i)) == 1 ) {
        tempInt = snprintf( buffer, 255, "%.15f", REAL(VECTOR_ELT(fields,i))[j]);

        /* figure out decimal length */
        for ( k = 0; k < strlen(buffer); ++k ) {
          /* find the decimal point */
          if ( buffer[k] == '.' ) {
            break;
          }
        }
        decimalLengths[i] = (strlen(buffer) - k) - 1;

        /* 15 is the most we can have */
        if ( decimalLengths[i] > 15 ) {
          decimalLengths[i] = 15;
        }
        
      /* we have a logical */
      } else if ( IS_LOGICAL( VECTOR_ELT(fields, i)) == 1 ) {
        tempInt = 1;
      }

      if ( tempInt > longestCol ) {
        longestCol = tempInt;
      }
    }
  
    colLengths[i] = longestCol;

    recordLength += longestCol;
  }

  /* add one for the deletion flag */
  ++recordLength;

 
  /* write the length of the each record */
  /* little endian */
  fwrite( &recordLength, sizeof(char), 2, fptr );

  /* write 0 for the reserved bytes, incomplete transaction, encryption, */
  /* free record thread, dBASE III, and MDX flag bytes */
  byte = 0x00; 
  for ( i = 0; i < 17; ++i ) {
    fwrite( &byte, sizeof(char), 1, fptr );
  }

  /* write the language driver byte */
  /* used 0x1b because sample dbfs did */
  byte = 0x1b;
  fwrite( &byte, sizeof(char), 1, fptr );

  /* write 2 more reserved bytes */
  byte = 0x00; 
  fwrite( &byte, sizeof(char), 1, fptr );
  fwrite( &byte, sizeof(char), 1, fptr );

  /* write the field descriptor array */
  for ( i = 0; i < length( fieldNames ); ++i ) {

    /* write the field name 11 chars max including terminating byte 0x00 */
    for( j = 0; j < 10; ++j){
      if ( j < strlen( CHAR(STRING_ELT( fieldNames, i )) )) {
        byte = CHAR(STRING_ELT( fieldNames, i ) )[j];
      } else {
        byte = 0x00;
      }
      fwrite( &byte, sizeof(char), 1, fptr );
    }
    /* make sure the last byte is a terminating byte */
    byte = 0x00;
    fwrite( &byte, sizeof(char), 1, fptr );

    /* write the field type */
    if ( IS_INTEGER( VECTOR_ELT(fields, i)) == 1 ) {
      byte = 'N';    
    } else if ( IS_NUMERIC( VECTOR_ELT(fields, i)) == 1 ) {
      byte = 'F';    
    } else if ( IS_COMPLEX( VECTOR_ELT(fields, i)) == 1 ) {
      byte = 'F';    
    } else if ( IS_CHARACTER( VECTOR_ELT(fields, i)) == 1 ) {
      byte = 'C';    
    } else if ( IS_LOGICAL( VECTOR_ELT(fields, i)) == 1 ) {
      byte = 'L';    
    } else {
      Rprintf( "Error: Invalid field type for dbf file.\n" );
      Rprintf( "Error: Occurred in C function writeDbfFile.\n" );
    }
    fwrite( &byte, sizeof(char), 1, fptr );

    /* write 0's for the field data address */
    byte = 0x00;
    for ( j = 0; j < 4; ++j ) {
      fwrite( &byte, sizeof(char), 1, fptr );
    }

    /* write the field length */
    if ( colLengths[i] > 255 ) {
      byte = 255;
    } else {
      byte = colLengths[i];
    }
    fwrite( &byte, sizeof(char), 1, fptr );

    /* write the decimal count */
    byte = decimalLengths[i];
    fwrite( &byte, sizeof(char), 1, fptr );

    /* fill out the rest of the descriptor with 0's */
    byte = 0x00;
    for ( j = 0; j < 14; ++j ) {
      fwrite( &byte, sizeof(char), 1, fptr );
    }
  }

  /* write the terminator byte */
  byte = 0x0d;
  fwrite( &byte, sizeof(char), 1, fptr );

  /* write the records (field values) all as ascii text */
  for ( i = 0; i < numRecords; ++i ) {

    /* write the terminator byte */
    byte = 0x20;
    fwrite( &byte, sizeof(char), 1, fptr );

    /* go thrugh eac field */
    for ( j = 0; j < length( fields ); ++j ) {

      if ( IS_CHARACTER( VECTOR_ELT(fields, j)) == 1 ) {
        for(k=0; k<strlen( CHAR(STRING_ELT( VECTOR_ELT(fields,j) , i )) ); ++k){
          byte = CHAR(STRING_ELT(VECTOR_ELT(fields,j),i))[k];
          fwrite( &byte, sizeof(char), 1, fptr );
        }
      
      } else if ( IS_INTEGER( VECTOR_ELT(fields, j)) == 1 ) {
        snprintf(buffer, 255, "%d",INTEGER(VECTOR_ELT(fields,j))[i]);
        for ( k = 0; k < strlen( buffer ); ++k){
          byte = buffer[k];
          fwrite( &byte, sizeof(char), 1, fptr );
        }

      } else if ( IS_NUMERIC( VECTOR_ELT(fields, j)) == 1 ) {
        snprintf( buffer, 255, "%.15f", REAL(VECTOR_ELT(fields,j))[i]);
        for ( k = 0; k < strlen( buffer ); ++k){
          byte = buffer[k];
          fwrite( &byte, sizeof(char), 1, fptr );
        }

      } else if ( IS_LOGICAL( VECTOR_ELT(fields, j)) == 1 ) {
        snprintf( buffer, 255, "%d", LOGICAL(VECTOR_ELT(fields,j))[i]);
        if ( buffer[0] == '0' ) {
          byte = 'F';
        } else if ( buffer[0] == '1' ) {
          byte = 'T';
        } else {
          byte = '?';
        }
        fwrite( &byte, sizeof(char), 1, fptr );
        k = 1;

      } else {
        k = 0;
      }

      /* add padding(spaces) if the string is smaller than the column size */
      if ( k < colLengths[j] ) {
        for ( z = k; z < colLengths[j]; ++z ) {
          byte = 0x20;
          fwrite( &byte, sizeof(char), 1, fptr );
        } 
      }

    }
  }

  /* write the end of file marker */
  byte = 0x1a;
  if ( fwrite( &byte, sizeof(char), 1, fptr ) == 0 ) {
    Rprintf( "Error: Writing dbf file in C function writeDbfFile.\n" );
  }

  /* clean up */
  free( colLengths );
  free( decimalLengths );
  free( fileName );
  free( prefix );
  fclose( fptr );

  return;
}
