/****************************************************************************** 
**  File:        shapeParser.c   
**
**  Purpose:     This file contains the C functions used for reading and
**               and writing shapefiles (.shp) following the ESRI White Paper
**               "ESRI Shapefile Technical Description" published July 1998.
**               There are functions that can be called from R that will
**               read a shapefile, create and write to a shapefile, and one
**               that returns all the areas of the parts in polygon shapefiles.
**  Programmers: Christian Platt, Tom Kincaid
**  Algorithm:   The data is read from the file and stored in a C struct called
**               shape.  This struct contains a linked list of record structs
**               that either contain dynamically allocted arrays of point 
**               structs or polygon structs.  After the shape struct has beed
**               written to, the data from the struct is written to an R
**               object to be returned to the calling R routine.
**  Notes:       When reading shapefiles the functions will attempt to open
**               and read from every .shp in the current working directory.
**               If there are conflicting shape types an error will be returned.
**               These function can parse a points, poylines, and polygons 
**               shapefile.  These correspond to shape types of 1, 3, and 5
**               respectively from the ESRI white paper.
**               Polygons and polylines have the same record format therefore
**               both use the Polygon C struct type.
**               There is a sketch of the C struct Shape on paper that 
**               diagrams how the data is stored and how dynamic memory is 
**               used.  Also on paper is the layout of the R object that is
**               returned to the calling R routine.
**               Shape files to be open and read can not have a '(' or ')'
**               character in them.
**  Created:     August 20, 2004
**  Revised:     April 26, 2011
**  Revised:     July 16, 2014
**  Revised:     February 23, 2015
**  Revised:     May 5, 2015
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <R.h>
#include <Rdefines.h>
#include <sys/types.h>
#include <dirent.h>
#include <ctype.h>
#include <stdarg.h>
#include "shapeParser.h"

/* found in dbfFileParser.c */
extern void writeDbfFile( SEXP fieldNames, SEXP fields, SEXP filePrefix );
extern void readDbfFile( SEXP fileName );
extern int parseFields( FILE * fptr, Dbf * dbf ); 
extern int parseDbfHeader( FILE * fptr, Dbf * dbf );
extern void deallocateDbf( Dbf * dbf );

 
/**********************************************************
** Function:   readBigEndian
**
** Purpose:    returns an integer value found in the sent 
**             buffer assuming it is stored in big endian
**             byte order.
** Arguments:  buffer,  char buffer that contains the value in
**                      binary
**             length,  size of buffer
** Return:     value,   integer value of number stored in buffer 
***********************************************************/
int readBigEndian( unsigned char * buffer, int length ) {

  int i;
  int value = 0;

  for ( i=0; i < length-1; ++i ) {
    value += buffer[i];
    value = value << 8;  
  }
  value += buffer[i];

  return value;
}


/**********************************************************
** Function:   readLittleEndian
**
** Purpose:    returns an integer value found in the sent 
**             buffer assuming it is stored in little endian
**             byte order.
** Arguments:  buffer,  char buffer that contains the value in
**                      binary
**             length,  size of buffer
** Return:     value,   integer value of number stored in buffer 
***********************************************************/
int readLittleEndian( unsigned char * buffer, int length ) {

  int i;
  int value = 0;

  for ( i=length-1; i > 0; --i ) {
    value += buffer[i];
    value = value << 8;  
  }
  value += buffer[i];

  return value;
}


/**********************************************************
** Function:   fileMatch
**
** Purpose:    Determine whether the sent file name has the
**             sent file extension.
** Arguments:  fileName,  file name
**             fileExt,   file extension
** Return:     1,  on success
**             0,  on failure 
***********************************************************/
int fileMatch( char * fileName, char * fileExt ) {
  char str[5];
  int fileLen;

  /* determine length of the file name */
  fileLen = strlen(fileName);

  /* assign the file extension */
  str[4] = '\0';
  str[3] = fileName[fileLen-1];
  str[2] = fileName[fileLen-2];
  str[1] = fileName[fileLen-3];
  str[0] = fileName[fileLen-4];

  /* determine whether the file has the desired extension */
  if(strcmp(fileExt, str) == 0) {
    return 1;
  } else {
    return 0;
  }

}


/**********************************************************
** Function:  polygonArea2
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
double polygonArea2( Point * polygon, int size ) {

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


/**********************************************************
** Function:   addRecord
**
** Purpose:    Adds a record struct to the linked list pointed
**             to by head.
** Algorithm:  Uses the sent lastRecord pointer to add node at
**             the end of the list.
** Arguments:  head,  pointer to pointer to first record in linked list
**             rec,   rec to be copied into new node
**             lastRecord,  pointer to pointer to last node in list
** Return:     1,  on success
**             -1, on error
***********************************************************/
int addRecord( Record ** head, Record * rec, Record ** lastRecord ) {

  Record * temp;

  /* see if there is already something in the list */
  if ( *head ) {
    if ( (temp = (Record *) malloc( sizeof(Record) )) == NULL ) {
      Rprintf( "Error: Allocating memory in C function addRecord.\n" );
      return -1;
    }
    temp->number = rec->number;
    temp->contentLength = rec->contentLength;
    temp->poly = rec->poly;
    temp->polyZ = rec->polyZ;
    temp->polyM = rec->polyM;
    temp->point = rec->point;
    temp->pointZ = rec->pointZ;
    temp->pointM = rec->pointM;
    temp->shapeType = rec->shapeType;
    temp->next = NULL;
    (*lastRecord)->next = temp;
    *lastRecord = temp;

  /* list is empty so create the first node */
  } else {
    if ( (*head = (Record *) malloc( sizeof(Record) )) == NULL ) {
      Rprintf( "Error: Allocating memory in C function addRecord.\n" );
      return -1;
    }
    (*head)->number = rec->number;
    (*head)->contentLength = rec->contentLength;
    (*head)->poly = rec->poly;
    (*head)->polyZ = rec->polyZ;
    (*head)->polyM = rec->polyM;
    (*head)->point = rec->point;
    (*head)->pointZ = rec->pointZ;
    (*head)->pointM = rec->pointM;
    (*head)->shapeType = rec->shapeType;
    (*head)->next = NULL;
    *lastRecord = *head;
  }

  return 1;
}


/**********************************************************
** Function:   deallocateRecords
**
** Purpose:    Deallocate all the dynamic memory allocated for
**             the linked list of records.
** Arguments:  head, pointer to the first node of the linked
**                   list of records.
** Return:     void
***********************************************************/
void deallocateRecords( Record * head ) {

  Record * temp;

  while( head ) {
    temp = head->next;
    if ( head->poly ) {
      free( head->poly->parts );
      free( head->poly->ringDirs );
      free( head->poly->points );
      free( head->poly );
    }

    if ( head->polyZ ) {
      free( head->polyZ->parts );
      free( head->polyZ->ringDirs );
      free( head->polyZ->points );
      free( head->polyZ->zArray );
      free( head->polyZ->mArray );
      free( head->polyZ );
    }

    if ( head->polyM ) {
      free( head->polyM->parts );
      free( head->polyM->ringDirs );
      free( head->polyM->points );
      free( head->polyM->mArray );
      free( head->polyM );
    }

    if ( head->point ) {
      free( head->point );
    }

    if ( head->pointZ ) {
      free( head->pointZ );
    }

    if ( head->pointM ) {
      free( head->pointM );
    }

    free( head );
    head = temp;
  }

  return;
}


/**********************************************************
** Function:   parsePoints
**
** Purpose:    To parse a point shapefile's records.
** Algorithm:  This function keeps reading in point data until it
**             reaches the end of the file, when filePosition 
**             becomes larger than the fileLength.  A complete record
**             is read from the file and stored in the record struct
**             and point struct.  This record is then added to the
**             linked list found in the shape struct before reading the
**             next record from the file.
** Notes:      In cases where there are multiple shapefiles this 
**             function will get called once for each file.  This is
**             why the lastRecord pointer needs to be set first in
**             casee the sent shape struct already contains some records.
** Arguments:  fptr,  file pointer to the shapefile
**             shape, shape struct that stores all the info
**                    and data found in the shapefile
** Return:     1,  on success
**             -1, on failure
***********************************************************/
int parsePoints( FILE * fptr, Shape * shape ) {

  int filePosition = 100;   /* start at 100 bytes, just past the header */
  Record record;            /* temp record struct forstoring data */
  Point * point;            /* point struct for holding point data */
  Record * lastRecord;      /* pointer to last record in linked list */
  unsigned char buffer[4];  /* temp buffer for reading data from the file */
  Record * temp;            /* used for traversing linked list of records */

  /* if this isn't the first shapefile read in then we need to set up */
  /* the lastRecord pointer */
  temp = shape->records;
  if ( temp ) {
    while ( temp->next ) {
      temp = temp->next;
    }
    lastRecord = temp;
  }

  /* go though the rest of the file */ 
  while ( filePosition < (shape->fileLength*2) ) {
    /* read in record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in content length */
    fread( buffer, sizeof(char), 4, fptr );
    record.contentLength = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in shape type */
    fread( buffer, sizeof(char), 4, fptr ); 
    record.shapeType = readLittleEndian( buffer, 4 );
    filePosition += 4;

    /* read in point data */
    if ( record.shapeType == POINTS ) {
      if ( (point = (Point *) malloc( sizeof(Point) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePoints.\n" );
        return -1;
      }
      fread( &(point->X), sizeof(double), 1, fptr );
      if ( fread( &(point->Y), sizeof(double), 1, fptr ) == 0 ) {
        Rprintf( "Error: Reading shapefile in C function parsePoints.\n" );
        return -1;
      }
      filePosition += 2*sizeof(double);

    /* a Null record was encountered in the shapefile, so return an error */ 
    } else {
      Rprintf( "Error: A shapefile containing a Null record was encountered in C function \nparsePoints.\n" );
      return -1;
    }

    ++(shape->numParts);

    /* add new record to shape struct */
    record.point = point;
    record.pointZ = NULL;
    record.pointM = NULL;
    record.poly = NULL;
    record.polyZ = NULL;
    record.polyM = NULL;
    if ( addRecord( &shape->records, &record, &lastRecord ) == - 1 ) {
      return -1;
    }
    ++(shape->numRecords);
  }

  return 1;
}


/**********************************************************
** Function:   parsePointsZ
**
** Purpose:    To parse a pointZ shapefile's records.
** Algorithm:  This function keeps reading in point data until it
**             reaches the end of the file, when filePosition 
**             becomes larger than the fileLength.  A complete record
**             is read from the file and stored in the record struct
**             and point struct.  This record is then added to the
**             linked list found in the shape struct before reading the
**             next record from the file.
** Notes:      In cases where there are multiple shapefiles this 
**             function will get called once for each file.  This is
**             why the lastRecord pointer needs to be set first in
**             casee the sent shape struct already contains some records.
** Arguments:  fptr,  file pointer to the shapefile
**             shape, shape struct that stores all the info
**                    and data found in the shapefile
** Return:     1,  on success
**             -1, on failure
***********************************************************/
int parsePointsZ( FILE * fptr, Shape * shape ) {

  int filePosition = 100;   /* start at 100 bytes, just past the header */
  Record record;            /* temp record struct forstoring data */
  PointZ * pointZ;            /* point struct for holding point data */
  Record * lastRecord;      /* pointer to last record in linked list */
  unsigned char buffer[4];  /* temp buffer for reading data from the file */
  Record * temp;            /* used for traversing linked list of records */

  /* if this isn't the first shapefile read in then we need to set up */
  /* the lastRecord pointer */
  temp = shape->records;
  if ( temp ) {
    while ( temp->next ) {
      temp = temp->next;
    }
    lastRecord = temp;
  }

  /* go though the rest of the file */ 
  while ( filePosition < (shape->fileLength*2) ) {

    /* read in record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in content length */
    fread( buffer, sizeof(char), 4, fptr );
    record.contentLength = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in shape type */
    fread( buffer, sizeof(char), 4, fptr ); 
    record.shapeType = readLittleEndian( buffer, 4 );
    filePosition += 4;

    /* read in pointZ data */
    if ( record.shapeType == POINTS_Z ) {
      if ( (pointZ = (PointZ *) malloc( sizeof(PointZ) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePointsZ.\n" );
        return -1;
      }
      fread( &(pointZ->X), sizeof(double), 1, fptr );
      if ( fread( &(pointZ->Y), sizeof(double), 1, fptr ) == 0 ) {
        Rprintf( "Error: Reading shapefile in C function parsePointsZ.\n" );
        return -1;
      }
      filePosition += 2*sizeof(double);

      fread( &(pointZ->Z), sizeof(double), 1, fptr );
      if ( fread( &(pointZ->M), sizeof(double), 1, fptr ) == 0 ) {
        Rprintf( "Error: Reading shapefile in C function parsePointsZ.\n" );
        return -1;
      }
      filePosition += 2*sizeof(double);

    /* a Null record was encountered in the shapefile, so return an error */ 
    } else {
      Rprintf( "Error: A shapefile containing a Null record was encountered in C function \nparsePointsZ.\n" );
      return -1;
    }

    ++(shape->numParts);

    /* add new record to shape struct */
    record.pointZ = pointZ;
    record.point = NULL;
    record.pointM = NULL;
    record.poly = NULL;
    record.polyZ = NULL;
    record.polyM = NULL;
    if ( addRecord( &shape->records, &record, &lastRecord ) == - 1 ) {
      return -1;
    }
    ++(shape->numRecords);
  }

  return 1;
}


/**********************************************************
** Function:   parsePointsM
**
** Purpose:    To parse a pointM shapefile's records.
** Algorithm:  This function keeps reading in point data until it
**             reaches the end of the file, when filePosition 
**             becomes larger than the fileLength.  A complete record
**             is read from the file and stored in the record struct
**             and point struct.  This record is then added to the
**             linked list found in the shape struct before reading the
**             next record from the file.
** Notes:      In cases where there are multiple shapefiles this 
**             function will get called once for each file.  This is
**             why the lastRecord pointer needs to be set first in
**             casee the sent shape struct already contains some records.
** Arguments:  fptr,  file pointer to the shapefile
**             shape, shape struct that stores all the info
**                    and data found in the shapefile
** Return:     1,  on success
**             -1, on failure
***********************************************************/
int parsePointsM( FILE * fptr, Shape * shape ) {

  int filePosition = 100;   /* start at 100 bytes, just past the header */
  Record record;            /* temp record struct forstoring data */
  PointM * pointM;            /* point struct for holding point data */
  Record * lastRecord;      /* pointer to last record in linked list */
  unsigned char buffer[4];  /* temp buffer for reading data from the file */
  Record * temp;            /* used for traversing linked list of records */

  /* if this isn't the first shapefile read in then we need to set up */
  /* the lastRecord pointer */
  temp = shape->records;
  if ( temp ) {
    while ( temp->next ) {
      temp = temp->next;
    }
    lastRecord = temp;
  }

  /* go though the rest of the file */ 
  while ( filePosition < (shape->fileLength*2) ) {

    /* read in record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in content length */
    fread( buffer, sizeof(char), 4, fptr );
    record.contentLength = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in shape type */
    fread( buffer, sizeof(char), 4, fptr ); 
    record.shapeType = readLittleEndian( buffer, 4 );
    filePosition += 4;

    /* read in pointM data */
    if ( record.shapeType == POINTS_M ) {
      if ( (pointM = (PointM *) malloc( sizeof(PointM) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePointsM.\n" );
        return -1;
      }
      fread( &(pointM->X), sizeof(double), 1, fptr );
      if ( fread( &(pointM->Y), sizeof(double), 1, fptr ) == 0 ) {
        Rprintf( "Error: Reading shapefile in C function parsePointsM.\n" );
        return -1;
      }
      filePosition += 2*sizeof(double);

      if ( fread( &(pointM->M), sizeof(double), 1, fptr ) == 0 ) {
        Rprintf( "Error: Reading shapefile in C function parsePointsM.\n" );
        return -1;
      }
      filePosition += sizeof(double);

    /* a Null record was encountered in the shapefile, so return an error */ 
    } else {
      Rprintf( "Error: A shapefile containing a Null record was encountered in C function \nparsePointsM.\n" );
      return -1;
    }

    ++(shape->numParts);

    /* add new record to shape struct */
    record.pointM = pointM;
    record.point = NULL;
    record.pointZ = NULL;
    record.poly = NULL;
    record.polyZ = NULL;
    record.polyM = NULL;
    if ( addRecord( &shape->records, &record, &lastRecord ) == - 1 ) {
      return -1;
    }
    ++(shape->numRecords);
  }

  return 1;
}


/**********************************************************
** Function:   parsePolygon
**
** Purpose:    To parse a polygon or polyline shapefile's records.
** Algorithm:  This function keeps reading in polygon/polyline data until it
**             reaches the end of the file, when filePosition 
**             becomes larger than the fileLength.  A complete record
**             is read from the file and stored in the record struct
**             and polygon struct.  This record is then added to the
**             linked list found in the shape struct before reading the
**             next record from the file.
** Notes:      As far as error checking goes we only check the last fread
**             since if one fails they all will after that so only need to
**             catch one.
**             The polygon struct works for polylines too.
**             In cases where there are multiple shapefiles this 
**             function will get called once for each file.  This is
**             why the lastRecord pointer needs to be set first in
**             case the sent shape struct already contains some records.
** Arguments:  fptr, file pointer to the shapefile
**             shape, shape struct that stores all the info
**                    and data found in the shapefile
** Return:     1,  on success
**             -1, on failure
***********************************************************/
int parsePolygon( FILE * fptr, Shape * shape ) {

  int i, j, k;               /* loop counters */
  int filePosition = 100;    /* start at 100 bytes, just past file header */
  unsigned char buffer[4];   /* temp buffer for reading data from the file */
  Record record;             /* temp record struct for storing data */
  Record * lastRecord;       /* pointer to last record in linked list */
  Polygon * poly;            /* ploygon struct for holding polygon data */
  Record * temp;             /* used for traversing the linked list of records*/
  Point * part = NULL;       /* used for determing ring direction for */
                             /* individual parts */
  int partSize;              /* number of points that make up a part */
  int pidx;                  /* index into part array */
  int partNumber = 0;        /* counts which part we are on for a given record*/

  /* if this isn't the first shapefile read in then we need to set up */
  /* the lastRecord pointer */
  temp = shape->records;
  if ( temp ) {
    while ( temp->next ) {
      temp = temp->next;
    }
    lastRecord = temp;
  }
  
  /* go through the rest of the file */
  while ( filePosition < (shape->fileLength*2) ) {

    /* read in record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in content length */
    fread( buffer, sizeof(char), 4, fptr );
    record.contentLength = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in shape type */
    fread( buffer, sizeof(char), 4, fptr );
    record.shapeType = readLittleEndian( buffer, 4 );
    filePosition += 4;

    /* read in polyline or polygon data */
    if ( record.shapeType == POLYLINE || record.shapeType == POLYGON ) {

      /* allocate a new plygon */
      if ( (poly = (Polygon *) malloc( sizeof(Polygon) )) == NULL ) {
        Rprintf( "Error: Allocating memory in in C function parsePolygon.\n" );
        return -1;
      }

      /* read in box data */
      for ( i=0; i < 4; ++i ) {
        fread( &(poly->box[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
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
      if ( (poly->parts = (int *) malloc( sizeof(int) * poly->numParts ))
                                                                == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygon.\n" );
        return -1;
      }
      if ( (poly->ringDirs = (int *) malloc( sizeof(int) * poly->numParts ))
		                                                         == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygon.\n" );
        return -1;
      } 
      for ( i=0; i < poly->numParts; ++i ) {
        fread( &(poly->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 
      shape->numParts += poly->numParts;

      /* read in points data */
      if ((poly->points = (Point *) malloc( sizeof(Point) * poly->numPoints ))
                                                               == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygon.\n" );
        return -1;
      }

      for ( i=0; i < poly->numPoints; ++i ) {
        fread( &(poly->points[i].X), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
        if ( fread( &(poly->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: Reading shapefile in C function parsePolygon.\n" );
          return -1;
        }
        filePosition += sizeof(double);
      } 

      /* we want to set the bounding box info based on the coordinates */
      poly->box[0] = poly->points[0].X;
      poly->box[1] = poly->points[0].Y;
      poly->box[2] = poly->points[0].X;
      poly->box[3] = poly->points[0].Y;
      for ( i=1; i < poly->numPoints; ++i ) {
        if ( poly->points[i].X < poly->box[0] ) {
          poly->box[0] = poly->points[i].X;
        }
        if ( poly->points[i].Y < poly->box[1] ) {
          poly->box[1] = poly->points[i].Y;
        }
        if ( poly->points[i].X > poly->box[2] ) {
          poly->box[2] = poly->points[i].X;
        }
        if ( poly->points[i].Y > poly->box[3] ) {
          poly->box[3] = poly->points[i].Y;
        }


        /* also update the bounding box info in the header */
        /* in case header info is incorrect */
        if ( poly->points[i].X < shape->Xmin ) {
          shape->Xmin = poly->points[i].X;
        }
        if ( poly->points[i].Y < shape->Ymin ) {
          shape->Ymin = poly->points[i].Y;
        }
        if ( poly->points[i].X > shape->Xmax ) {
          shape->Xmax = poly->points[i].X;
        }
        if ( poly->points[i].Y > shape->Ymax ) {
          shape->Ymax = poly->points[i].Y;
        }

      }

      /* determine ring directions of the parts */
      /* if there are more than one part we need to check them separately */
      if ( poly->numParts > 1 ) {
        partNumber = 0;
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
              Rprintf( "Error: Allocating memory in C function parsePolygon.\n" );
              return -1;
            }
            pidx = 0;
            for(j=poly->parts[k]; j < poly->numPoints; ++j){
              part[pidx] = poly->points[j];
              ++pidx; 
            }

          /* not the last part */
          } else {

            /* store part data into part array */
            partSize = poly->parts[k+1] - poly->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize ))
			                                         == NULL ) {
              Rprintf( "Error: Allocating memory in C function parsePolygon.\n" );
              return -1;
            }
            pidx = 0;
            for(j=poly->parts[k]; j < poly->parts[k+1]; ++j){
              part[pidx] = poly->points[j];
              ++pidx; 
            }

          }

	  /* determine ring direction for this part */
	  if ( polygonArea2( part, partSize ) < 0 ) {
            poly->ringDirs[partNumber] = -1;
          } else {
            poly->ringDirs[partNumber] = 1;
          }
          
	  ++partNumber;
        }  

	if ( part ) {
	  free( part );
          part = NULL;
	}

      /* only one part so according to the ESRI docs it must be positive area*/
      } else {
        poly->ringDirs[0] = 1;
      }         

    /* a Null record was encountered in the shapefile, so return an error */ 
    } else {
      Rprintf( "Error: A shapefile containing a Null record was encountered in C function \nparsePolygon.\n" );
      return -1;
    }

    /* add new record to shape struct */
    record.poly = poly;
    record.polyZ = NULL;
    record.polyM = NULL;
    record.point = NULL;
    record.pointZ = NULL;
    record.pointM = NULL;
    if ( addRecord( &shape->records, &record, &lastRecord ) == -1 ) {
      return -1;
    }
    ++(shape->numRecords);

  }

  return 1;
}


/**********************************************************
** Function:   parsePolygonZ
**
** Purpose:    To parse a polygonZ or polylineZ shapefile's records.
** Algorithm:  This function keeps reading in polygon/polyline data until it
**             reaches the end of the file, when filePosition 
**             becomes larger than the fileLength.  A complete record
**             is read from the file and stored in the record struct
**             and polygon struct.  This record is then added to the
**             linked list found in the shape struct before reading the
**             next record from the file.
** Notes:      As far as error checking goes we only check the last fread
**             since if one fails they all will after that so only need to
**             catch one.
**             The polygon struct works for polylines too.
**             In cases where there are multiple shapefiles this 
**             function will get called once for each file.  This is
**             why the lastRecord pointer needs to be set first in
**             case the sent shape struct already contains some records.
** Arguments:  fptr, file pointer to the shapefile
**             shape, shape struct that stores all the info
**                    and data found in the shapefile
** Return:     1,  on success
**             -1, on failure
***********************************************************/
int parsePolygonZ( FILE * fptr, Shape * shape ) {

  int i, j, k;               /* loop counters */
  int filePosition = 100;    /* start at 100 bytes, just past file header */
  unsigned char buffer[4];   /* temp buffer for reading data from the file */
  Record record;             /* temp record struct for storing data */
  Record * lastRecord;       /* pointer to last record in linked list */
  PolygonZ * polyZ;          /* ploygon struct for holding polygon data */
  Record * temp;             /* used for traversing the linked list of records*/
  Point * part = NULL;       /* used for determing ring direction for */
                             /* individual parts */
  int partSize;              /* number of points that make up a part */
  int pidx;                  /* index into part array */
  int partNumber = 0;        /* counts which part we are on for a given record*/

  /* if this isn't the first shapefile read in then we need to set up */
  /* the lastRecord pointer */
  temp = shape->records;
  if ( temp ) {
    while ( temp->next ) {
      temp = temp->next;
    }
    lastRecord = temp;
  }
  
  /* go through the rest of the file */
  while ( filePosition < (shape->fileLength*2) ) {

    /* read in record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in content length */
    fread( buffer, sizeof(char), 4, fptr );
    record.contentLength = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in shape type */
    fread( buffer, sizeof(char), 4, fptr );
    record.shapeType = readLittleEndian( buffer, 4 );
    filePosition += 4;

    /* read in polylineZ or polygonZ data */
    if ( record.shapeType == POLYLINE_Z || record.shapeType == POLYGON_Z ) {

      /* allocate a new plygon */
      if ( (polyZ = (PolygonZ *) malloc( sizeof(PolygonZ) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
        return -1;
      }

      /* read in box data */
      for ( i=0; i < 4; ++i ) {
        fread( &(polyZ->box[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
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
      if ( (polyZ->parts = (int *) malloc( sizeof(int) * polyZ->numParts ))
                                                                  == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
        return -1;
      }
      if ( (polyZ->ringDirs = (int *) malloc( sizeof(int) * polyZ->numParts ))
		                                                           == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
        return -1;
      } 
      for ( i=0; i < polyZ->numParts; ++i ) {
        fread( &(polyZ->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 
      shape->numParts += polyZ->numParts;

      /* read in points data */
      if ((polyZ->points = (Point *) malloc( sizeof(Point) * polyZ->numPoints ))
                                                                     == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
        return -1;
      }

      for ( i=0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->points[i].X), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
        if ( fread( &(polyZ->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: Reading shapefile in C function parsePolygonZ.\n" );
          return -1;
        }
        filePosition += sizeof(double);
      } 

      /* we want to set the bounding box info based on the coordinates */
      polyZ->box[0] = polyZ->points[0].X;
      polyZ->box[1] = polyZ->points[0].Y;
      polyZ->box[2] = polyZ->points[0].X;
      polyZ->box[3] = polyZ->points[0].Y;
      for ( i=1; i < polyZ->numPoints; ++i ) {
        if ( polyZ->points[i].X < polyZ->box[0] ) {
          polyZ->box[0] = polyZ->points[i].X;
        }
        if ( polyZ->points[i].Y < polyZ->box[1] ) {
          polyZ->box[1] = polyZ->points[i].Y;
        }
        if ( polyZ->points[i].X > polyZ->box[2] ) {
          polyZ->box[2] = polyZ->points[i].X;
        }
        if ( polyZ->points[i].Y > polyZ->box[3] ) {
          polyZ->box[3] = polyZ->points[i].Y;
        }

        /* also update the bounding box info in the header */
        /* in case header info is incorrect */
        if ( polyZ->points[i].X < shape->Xmin ) {
          shape->Xmin = polyZ->points[i].X;
        }
        if ( polyZ->points[i].Y < shape->Ymin ) {
          shape->Ymin = polyZ->points[i].Y;
        }
        if ( polyZ->points[i].X > shape->Xmax ) {
          shape->Xmax = polyZ->points[i].X;
        }
        if ( polyZ->points[i].Y > shape->Ymax ) {
          shape->Ymax = polyZ->points[i].Y;
        }

      }

      /* figure out ring directions of the parts */
      /* if there are more than one part we need to check them separately */
      if ( polyZ->numParts > 1 ) {
        partNumber = 0;
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
              Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
              return -1;
            }
            pidx = 0;
            for(j=polyZ->parts[k]; j < polyZ->numPoints; ++j){
              part[pidx] = polyZ->points[j];
              ++pidx; 
            }

          /* not the last part */
          } else {

            /* store part data into part array */
            partSize = polyZ->parts[k+1] - polyZ->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize ))
			                                                == NULL ) {
              Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
              return -1;
            }
            pidx = 0;
            for(j=polyZ->parts[k]; j < polyZ->parts[k+1]; ++j){
              part[pidx] = polyZ->points[j];
              ++pidx; 
            }

          }

	  /* determine ring direction for this part */
	  if ( polygonArea2( part, partSize ) < 0 ) {
            polyZ->ringDirs[partNumber] = -1;
          } else {
            polyZ->ringDirs[partNumber] = 1;
          }
          
	  ++partNumber;
        }  

	if ( part ) {
	  free( part );
          part = NULL;
	}

      /* only one part so according to the ESRI docs it must be positive area*/
      } else {
        polyZ->ringDirs[0] = 1;
      }         

      /* read in Z range */
      for ( i=0; i < 2; ++i ) {
        fread( &(polyZ->zRange[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
      }

      /* read in Z values */
      if ((polyZ->zArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
                                                                     == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
        return -1;
      }
      for ( i=0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->zArray[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
      } 

      /* read in M range */
      for ( i=0; i < 2; ++i ) {
        fread( &(polyZ->mRange[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
      }

      /* read in M values */
      if ((polyZ->mArray = (double *) malloc( sizeof(double)*polyZ->numPoints))
                                                                     == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
        return -1;
      }
      for ( i=0; i < polyZ->numPoints; ++i ) {
        fread( &(polyZ->mArray[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
      } 

    /* a Null record was encountered in the shapefile, so return an error */ 
    } else {
      Rprintf( "Error: A shapefile containing a Null record was encountered in C function \nparsePolygonZ.\n" );
      return -1;
    }

    /* add new record to shape struct */
    record.polyZ = polyZ;
    record.poly = NULL;
    record.polyM = NULL;
    record.point = NULL;
    record.pointZ = NULL;
    record.pointM = NULL;
    if ( addRecord( &shape->records, &record, &lastRecord ) == -1 ) {
      return -1;
    }
    ++(shape->numRecords);
  }

  return 1;
}


/**********************************************************
** Function:   parsePolygonM
**
** Purpose:    To parse a polygonM or polylineM shapefile's records.
** Algorithm:  This function keeps reading in polygon/polyline data until it
**             reaches the end of the file, when filePosition 
**             becomes larger than the fileLength.  A complete record
**             is read from the file and stored in the record struct
**             and polygon struct.  This record is then added to the
**             linked list found in the shape struct before reading the
**             next record from the file.
** Notes:      As far as error checking goes we only check the last fread
**             since if one fails they all will after that so only need to
**             catch one.
**             The polygon struct works for polylines too.
**             In cases where there are multiple shapefiles this 
**             function will get called once for each file.  This is
**             why the lastRecord pointer needs to be set first in
**             case the sent shape struct already contains some records.
** Arguments:  fptr, file pointer to the shapefile
**             shape, shape struct that stores all the info
**                    and data found in the shapefile
** Return:     1,  on success
**             -1, on failure
***********************************************************/
int parsePolygonM( FILE * fptr, Shape * shape ) {

  int i, j, k;               /* loop counters */
  int filePosition = 100;    /* start at 100 bytes, just past file header */
  unsigned char buffer[4];   /* temp buffer for reading data from the file */
  Record record;             /* temp record struct for storing data */
  Record * lastRecord;       /* pointer to last record in linked list */
  PolygonM * polyM;          /* ploygon struct for holding polygon data */
  Record * temp;             /* used for traversing the linked list of records*/
  Point * part = NULL;       /* used for determing ring direction for */
                             /* individual parts */
  int partSize;              /* number of points that make up a part */
  int pidx;                  /* index into part array */
  int partNumber = 0;        /* counts which part we are on for a given record*/

  /* if this isn't the first shapefile read in then we need to set up */
  /* the lastRecord pointer */
  temp = shape->records;
  if ( temp ) {
    while ( temp->next ) {
      temp = temp->next;
    }
    lastRecord = temp;
  }
  
  /* go through the rest of the file */
  while ( filePosition < (shape->fileLength*2) ) {

    /* read in record number */
    fread( buffer, sizeof(char), 4, fptr );
    record.number = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in content length */
    fread( buffer, sizeof(char), 4, fptr );
    record.contentLength = readBigEndian( buffer, 4 );
    filePosition += 4;

    /* read in shape type */
    fread( buffer, sizeof(char), 4, fptr );
    record.shapeType = readLittleEndian( buffer, 4 );
    filePosition += 4;

    /* read in polylineM or polygonM data */
    if ( record.shapeType == POLYLINE_M || record.shapeType == POLYGON_M ) {

      /* allocate a new plygon */
      if ( (polyM = (PolygonM *) malloc( sizeof(PolygonM) )) == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonM.\n" );
        return -1;
      }

      /* read in box data */
      for ( i=0; i < 4; ++i ) {
        fread( &(polyM->box[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
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
      if ( (polyM->parts = (int *) malloc( sizeof(int) * polyM->numParts ))
                                                                  == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonM.\n" );
        return -1;
      }
      if ( (polyM->ringDirs = (int *) malloc( sizeof(int) * polyM->numParts ))
		                                                           == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonM.\n" );
        return -1;
      } 
      for ( i=0; i < polyM->numParts; ++i ) {
        fread( &(polyM->parts[i]), sizeof(char), 4, fptr );
        filePosition += 4;
      } 
      shape->numParts += polyM->numParts;

      /* read in points data */
      if ((polyM->points = (Point *) malloc( sizeof(Point) * polyM->numPoints ))
                                                                     == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonM.\n" );
        return -1;
      }

      for ( i=0; i < polyM->numPoints; ++i ) {
        fread( &(polyM->points[i].X), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
        if ( fread( &(polyM->points[i].Y), sizeof(double), 1, fptr ) == 0 ) {
          Rprintf( "Error: Reading shapefile in C function parsePolygonM.\n" );
          return -1;
        }
        filePosition += sizeof(double);
      } 

      /* we want to set the bounding box info based on the coordinates */
      polyM->box[0] = polyM->points[0].X;
      polyM->box[1] = polyM->points[0].Y;
      polyM->box[2] = polyM->points[0].X;
      polyM->box[3] = polyM->points[0].Y;
      for ( i=1; i < polyM->numPoints; ++i ) {
        if ( polyM->points[i].X < polyM->box[0] ) {
          polyM->box[0] = polyM->points[i].X;
        }
        if ( polyM->points[i].Y < polyM->box[1] ) {
          polyM->box[1] = polyM->points[i].Y;
        }
        if ( polyM->points[i].X > polyM->box[2] ) {
          polyM->box[2] = polyM->points[i].X;
        }
        if ( polyM->points[i].Y > polyM->box[3] ) {
          polyM->box[3] = polyM->points[i].Y;
        }

        /* also update the bounding box info in the header */
        /* in case header info is incorrect */
        if ( polyM->points[i].X < shape->Xmin ) {
          shape->Xmin = polyM->points[i].X;
        }
        if ( polyM->points[i].Y < shape->Ymin ) {
          shape->Ymin = polyM->points[i].Y;
        }
        if ( polyM->points[i].X > shape->Xmax ) {
          shape->Xmax = polyM->points[i].X;
        }
        if ( polyM->points[i].Y > shape->Ymax ) {
          shape->Ymax = polyM->points[i].Y;
        }

      }

      /* figure out ring directions of the parts */
      /* if there are more than one part we need to check them separately */
      if ( polyM->numParts > 1 ) {
        partNumber = 0;
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
              Rprintf( "Error: Allocating memory in C function parsePolygonM.\n" );
              return -1;
            }
            pidx = 0;
            for(j=polyM->parts[k]; j < polyM->numPoints; ++j){
              part[pidx] = polyM->points[j];
              ++pidx; 
            }

          /* not the last part */
          } else {

            /* store part data into part array */
            partSize = polyM->parts[k+1] - polyM->parts[k];
            if ( (part = (Point *) malloc( sizeof( Point ) * partSize ))
			                                                == NULL ) {
              Rprintf( "Error: Allocating memory in C function parsePolygonM.\n" );
              return -1;
            }
            pidx = 0;
            for(j=polyM->parts[k]; j < polyM->parts[k+1]; ++j){
              part[pidx] = polyM->points[j];
              ++pidx; 
            }

          }

	  /* determine ring direction for this part */
	  if ( polygonArea2( part, partSize ) < 0 ) {
            polyM->ringDirs[partNumber] = -1;
          } else {
            polyM->ringDirs[partNumber] = 1;
          }
          
	  ++partNumber;
        }  

	if ( part ) {
	  free( part );
          part = NULL;
	}

      /* only one part so according to the ESRI docs it must be positive area*/
      } else {
        polyM->ringDirs[0] = 1;
      }         

      /* read in M range */
      for ( i=0; i < 2; ++i ) {
        fread( &(polyM->mRange[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
      }

      /* read in M values */
      if ((polyM->mArray = (double *) malloc( sizeof(double)*polyM->numPoints))
                                                                     == NULL ) {
        Rprintf( "Error: Allocating memory in C function parsePolygonZ.\n" );
        return -1;
      }
      for ( i=0; i < polyM->numPoints; ++i ) {
        fread( &(polyM->mArray[i]), sizeof(double), 1, fptr );
        filePosition += sizeof(double);
      } 

    /* a Null record was encountered in the shapefile, so return an error */ 
    } else {
      Rprintf( "Error: A shapefile containing a Null record was encountered in C function \nparsePolygonM.\n" );
      return -1;
    }

    /* add new record to shape struct */
    record.polyM = polyM;
    record.poly = NULL;
    record.polyZ = NULL;
    record.point = NULL;
    record.pointZ = NULL;
    record.pointM = NULL;
    if ( addRecord( &shape->records, &record, &lastRecord ) == -1 ) {
      return -1;
    }
    ++(shape->numRecords);
  }

  return 1;
}


/**********************************************************
** Function:   parseHeader
**
** Purpose:    To parse the main shapefile header information.
** Algorithm:  Reads through the first 100 bytes of the file in
**             4 byte increments.
** Notes:      As far as error checking goes we only check the last fread
**             since if one fails they all will after that so only need to
**             catch one.
** Arguments:  fptr,  file pointer to the shapefile
**             shape, pointer to shape struct that will store the
**                    file information and data
** Return:     1,  on success
**             -1, on failure
***********************************************************/
int parseHeader( FILE * fptr, Shape * shape ) {

  int position = 0;           /* byte position in the file */
  unsigned char buffer[4];    /* temp buffer for reading from the file */

  /* read in the main file header info, 36 bytes worth */
  while ( position < 36 ) {
    if ( fread( buffer, sizeof(char), 4, fptr ) == 0 ) {
      return -1;
    }

    switch( position ) {
      /* 0th byte for the file code */
      case 0:   shape->fileCode = readBigEndian( buffer, 4 );
                break;

      /* 24th byte for the file length */
      case 24:  shape->fileLength = readBigEndian( buffer, 4 );
                break;

      /* 28th byte for the file version */
      case 28:  shape->fileVersion = readLittleEndian( buffer, 4 );
                break;

      /* 32nd byte for the shape type */ 
      case 32:  shape->shapeType = readLittleEndian( buffer, 4 );
                break;
    }

    position += 4;
  }

  /* read in the bounding box data, 64 bytes */
  fread( &shape->Xmin, sizeof(double), 1, fptr );
  fread( &shape->Ymin, sizeof(double), 1, fptr );
  fread( &shape->Xmax, sizeof(double), 1, fptr );
  fread( &shape->Ymax, sizeof(double), 1, fptr );
  fread( &shape->Zmin, sizeof(double), 1, fptr );
  fread( &shape->Zmax, sizeof(double), 1, fptr );
  fread( &shape->Mmin, sizeof(double), 1, fptr );
  if ( fread( &shape->Mmax, sizeof(double), 1, fptr ) == 0 ) {
    Rprintf( "Error: Reading shapefile in C function parseHeader.\n" );
    return -1;
  }

  return 1;
}


/**********************************************************
** Function:   calcArea
**
** Purpose:    Calcualte the area of a part of a polygon.
** Notes:      Since polygons are often made up of multiple parts this
**             function calculates the area of one part.  The coordinates
**             for all the parts (the entire polygon) are sent in the
**             points array, but only the ones specified by the firstPoint
**             and secondPoint indexes are used, which are the ones that
**             make up the part we are interested in.
** Arguments:  points,  array of coordinates making up the polygon
**             firstPoint,  index into the array at where the polygon
**                          starts
**             secondPoint, index into the array at where the polygon
**                          ends
** Return:     area,  area of the part
***********************************************************/
double calcArea( Point * points, int firstPoint, int secondPoint ) {

  int i;               /* loop counter */
  int idx;             /* array index */
  double area = 0.0;   /* area of the part */
  double minY;         /* minimum y coordinate */
  double * dx;         /* array of changed in x */
  double * xp;         /* x coordinates */
  double * yp;         /* y coordinates */
  double * ym;
  int length = (secondPoint - firstPoint) + 1;  /* number of points that */
                                                /* make up the part */

  /* allocate the necessary memory */
  if ( (dx = (double *) malloc( sizeof(double) * length )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function calcArea.\n" );
    return 0.0;
  }
  if ( (xp = (double *) malloc( sizeof(double) * length )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function calcArea.\n" );
    return 0.0;
  }
  if ( (yp = (double *) malloc( sizeof(double) * length )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function calcArea.\n" );
    return 0.0;
  }
  if ( (ym = (double *) malloc( sizeof(double) * length )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function calcArea.\n" );
    return 0.0;
  }

  /* copy the neccessary coordinates */
  idx = 0;
  for ( i = firstPoint; i <= secondPoint; ++i ) {
    xp[idx] = points[i].X;
    yp[idx] = points[i].Y;
    ++idx;
  }
 
  /* find the min Y coordiante */ 
  minY = points[firstPoint].Y;
  for ( i = 0; i < length; ++i ) {
    if ( minY > yp[i] ) {
      minY = yp[i];
    }
  }

  /* subtract the min y from each y coordinate */
  for ( i = 0; i < length; ++i ) {
    yp[i] = yp[i] - minY;
  }

  /* calc dx and ym */
  for ( i = 0; i < length; ++i ) {
    if ( i < length - 1 ) {
      dx[i] = xp[i+1] - xp[i];
      ym[i] = (yp[i] + yp[i+1]) / 2;
    } else {
      dx[i] = xp[0] - xp[i];
      ym[i] = (yp[i] + yp[0]) / 2;
    }
  }

  /* sum up the area */
  for ( i = 0; i < length; ++i ) {
    area += dx[i] * ym[i];
  }

  /* clean up */
  free( dx );
  free( xp );
  free( yp );
  free( ym );

  return area;
}


/**********************************************************
** Function:   printShape  (ONLY USED FOR DEBUGGING CODE)
**
** Purpose:    Prints to stdout all the information and data
**             stored in the sent shape struct.
** Arguments:  shape,   pointer to shape struct that stores the
**                      shapefile info and data
** Return:     void
***********************************************************/
void printShape( Shape * shape ) {

  int i;
  Record * temp;
  
  /* main file header info */
  Rprintf( "File code: %d\n", shape->fileCode );
  Rprintf( "File length: %d\n", shape->fileLength );
  Rprintf( "File Version: %d\n", shape->fileVersion );
  Rprintf( "Shape type: %d\n", shape->shapeType );
  Rprintf( "Xmin: %f\n", shape->Xmin);
  Rprintf( "Ymin: %f\n", shape->Ymin);
  Rprintf( "Xmax: %f\n", shape->Xmax);
  Rprintf( "Ymax: %f\n", shape->Ymax);
  Rprintf( "Zmin: %f\n", shape->Zmin);
  Rprintf( "Zmax: %f\n", shape->Zmax);
  Rprintf( "Mmin: %f\n", shape->Mmin);
  Rprintf( "Mmax: %f\n", shape->Mmax);

  /* record info and data */
  temp = shape->records;
  while ( temp ) {
    Rprintf( "number: %d\n", temp->number );
    Rprintf( "contentLength: %d\n", temp->contentLength );
  
    /* see if it is a polygon */ 
    if ( temp->poly ) { 
      Rprintf( "PART: " ); 
      for ( i=0; i < temp->poly->numParts; ++i ) {
        Rprintf( "%d ", temp->poly->parts[i] );
      }
      Rprintf( "\n" );

      /* picking one of them to print out the data for */
      if ( temp->number == 162 ) {
        for ( i=0; i < temp->poly->numPoints; ++i ) {
          Rprintf( "X: %f\n", temp->poly->points[i].X );
          Rprintf( "Y: %f\n", temp->poly->points[i].Y );
        }    
      }

    /* see if we have a point */
    } else if ( temp->point ) {
      Rprintf( "X: %f\n", temp->point->X );
      Rprintf( "Y: %f\n", temp->point->Y );
    }

    temp = temp->next;
  }

  return;
}


/**********************************************************
** Function:   getRecordShapeSizes
**
** Purpose:    Returns a R vector of areas or lengths of all the records 
**             found in all the .shp files in the current working directory.
** Notes:      This function will return an error if one of the .shp files
**             is not a polygon shape type.  
** Arguments:  void
** Return:     data,  R vector of areas or lengths for each record in the 
**                    shapefiles
***********************************************************/
SEXP getRecordShapeSizes( SEXP fileNamePrefix ) {

  int i;             /* loop counter */
  int partIndx;      /* index into polyline parts array */
  FILE * fptr;       /* file pointer to shapefile */
  Shape shape;       /* struct to store all info and data found in shapefile */
  SEXP data = NULL;  /* R object to store data in for returning to R */
  int idx;           /* array index */
  Record * temp;     /* used to trverse list of records */
  int done = FALSE;  /* flag signalling all .shp files have been read */
  DIR * dirp = NULL;         /* used to open the current directory */
  struct dirent * fileShp;  /* used for reading file names */
  int ptrShp = 0;           /* ptr to .shp files in the current directory */
  char * restrict shpFileName = NULL;  /* stores full shapefile name */
  int singleFile = FALSE;  /* flag signalling when we are only looking for a */
                           /* single specified shapefile */
  int shapeType = -1;      /* shape type of the first .shp file we find */

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;
  shape.numParts = 0;


  /* see if a specific file was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full .shp file name */
    if ((shpFileName = (char * restrict)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
                                              + strlen(".shp") + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in C function getRecordShapeSizes.\n" );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return data;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix,0)));
    strcat( shpFileName, ".shp" );
    singleFile = TRUE;

  } else {

    /* open the current directory */
    if((dirp = opendir(".")) == NULL) {
      Rprintf( "Error: Opening the current directory in C function getRecordShapeSizes.\n" );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data;
    }

    /* get the name of the first .shp file */
    while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    	 if ( strlen(fileShp->d_name) > 4 ) {
    	   ptrShp = fileMatch( fileShp->d_name, ".shp" );
    	   if ( ptrShp == 1 ) {
    	     if ( (shpFileName = (char * restrict)malloc(strlen(fileShp->d_name)
                +  1)) == NULL ) {
            Rprintf( "Error: Allocating memory in C function getRecordShapeSizes.\n" );
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
      Rprintf( "Error: Occured in C function getRecordShapeSizes.\n");
      closedir( dirp );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT(1);
      return data;
    }
  }

  while ( done == FALSE ) {

    /* open the shapefile */
    fptr = fopen( shpFileName, "rb" );
    if ( fptr == NULL ) {
      Rprintf( "Error: Opening shapefile in C function getRecordShapeSizes.\n" );
      deallocateRecords( shape.records );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return data;
    }

    /* parse main file header */
    if ( parseHeader( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading main file header in C function getRecordShapeSizes.\n" );
        deallocateRecords( shape.records );
        PROTECT( data = allocVector( VECSXP, 1 ) );
        UNPROTECT(1);
        fclose ( fptr );
        return data;
    }

    /* initialize the shape type if necessary and make sure that if there */
    /* are multiple .shp files that they are of the same shape type */
    if ( shapeType == -1 ) {
      shapeType = shape.shapeType;
    } else if ( shapeType != shape.shapeType ) {
      Rprintf( "Error: Multiple shapefiles have different shape types.\n" );
      Rprintf( "Error: Occured in C function getRecordShapeSizes.\n" );
      deallocateRecords( shape.records );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return data;
    }

    /* if we get a points file then return an error */
    if ( shape.shapeType == POINTS || shape.shapeType == POINTS_Z || 
    	                                    shape.shapeType == POINTS_M ) {
      Rprintf( "Error: Invalid shape type found in file %s.\n", shpFileName );
      Rprintf( "Error: Shape type must be polygons or polylines,\n" );
      Rprintf( "Error: Occured in C function getRecordShapeSizes.\n" );
      deallocateRecords( shape.records );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return data; 

    /* read in polyline or polygon shape */
    } else if ( shape.shapeType == POLYGON || shape.shapeType == POLYLINE ) {

      if ( parsePolygon( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading Polygon or Polyline data from file %s \nin C function getRecordShapeSizes.\n", shpFileName );
        deallocateRecords( shape.records );
        PROTECT( data = allocVector( VECSXP, 1 ) );
        UNPROTECT(1);
        fclose ( fptr );
        return data; 
      }

    /* read in polylineZ or polygonZ shape */
    } else if ( shape.shapeType == POLYGON_Z || shape.shapeType == POLYLINE_Z) {

      if ( parsePolygonZ( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading PolygonZ or PolylineZ data from file %s \nin C function getRecordShapeSizes.\n", shpFileName );
        deallocateRecords( shape.records );
        PROTECT( data = allocVector( VECSXP, 1 ) );
        UNPROTECT(1);
        fclose ( fptr );
        return data; 
      }

    /* read in polylineM or polygonM shape */
    } else if ( shape.shapeType == POLYGON_M || shape.shapeType == POLYLINE_M) {

      if ( parsePolygonM( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading PolygonM or PolylineM data from file %s \nin C function getRecordShapeSizes.\n", shpFileName );
        deallocateRecords( shape.records );
        PROTECT( data = allocVector( VECSXP, 1 ) );
        UNPROTECT(1);
        fclose ( fptr );
        return data; 
      }

    /* got a shape number that we can't parse */
    } else {
      Rprintf( "Error: Unrecognized shape type in C function getRecordShapeSizes.\n" );
      deallocateRecords( shape.records );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      fclose ( fptr );
      return data; 
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
    	       if ( (shpFileName = (char * restrict)malloc(strlen(fileShp->d_name)
                  +  1)) == NULL ) {
              Rprintf( "Error: Allocating memory in C function getRecordShapeSizes.\n" );
              closedir( dirp );
              deallocateRecords( shape.records );
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

    fclose( fptr );

  }

  /* close the current directory */
  if ( singleFile == FALSE )  {
    closedir( dirp );
  }

  /* create the returning R object */
  PROTECT( data = allocVector( REALSXP, shape.numRecords ) );

  /* go though each record and calculate part areas */
  temp = shape.records;
  idx = 0;
  while ( temp ) {
    double size = 0.0;

    if ( shape.shapeType == POLYGON ) {

      /* calculate the areas of the different parts */
      for ( i = 0; i < temp->poly->numParts; ++i ) {
        int firstPoint = temp->poly->parts[i];
        int secondPoint;

        if ( temp->poly->numParts == 1 || i == temp->poly->numParts-1 ) {
          secondPoint = temp->poly->numPoints - 1;
        } else {
          secondPoint = temp->poly->parts[i+1] - 1;
        }

        size += calcArea(temp->poly->points, firstPoint, secondPoint); 
      }

    } else if ( shape.shapeType == POLYGON_Z ) {

      /* calculate the areas of the different parts */
      for ( i = 0; i < temp->polyZ->numParts; ++i ) {
        int firstPoint = temp->polyZ->parts[i];
        int secondPoint;

        if ( temp->polyZ->numParts == 1 || i == temp->polyZ->numParts-1 ) {
          secondPoint = temp->polyZ->numPoints - 1;
        } else {
          secondPoint = temp->polyZ->parts[i+1] - 1;
        }

        size += calcArea(temp->polyZ->points, firstPoint, secondPoint); 
      }

    } else if ( shape.shapeType == POLYGON_M ) {

      /* calculate the areas of the different parts */
      for ( i = 0; i < temp->polyM->numParts; ++i ) {
        int firstPoint = temp->polyM->parts[i];
        int secondPoint;

        if ( temp->polyM->numParts == 1 || i == temp->polyM->numParts-1 ) {
          secondPoint = temp->polyM->numPoints - 1;
        } else {
          secondPoint = temp->polyM->parts[i+1] - 1;
        }

        size += calcArea(temp->polyM->points, firstPoint, secondPoint); 
      }

    } else if (shape.shapeType == POLYLINE ) {

      /* calculate the length of the polyline for this record */
      partIndx = 1; 
      for ( i = 0; i < temp->poly->numPoints-1; ++i ) {
        double dx, dy;

        /* if there are multiple parts assume that the parts are not connected*/
        if ( temp->poly->numParts > 1 && partIndx < temp->poly->numParts ) {
          if ( (i + 1) == temp->poly->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        dx = temp->poly->points[i+1].X - temp->poly->points[i].X;
        dy = temp->poly->points[i+1].Y - temp->poly->points[i].Y;

        size += sqrt( dx*dx + dy*dy );
      }

    } else if (shape.shapeType == POLYLINE_Z ) {

      /* calculate the length of the polyline for this record */
      partIndx = 1; 
      for ( i = 0; i < temp->polyZ->numPoints-1; ++i ) {
        double dx, dy;

        /* if there are multiple parts assume that the parts are not connected*/
        if ( temp->polyZ->numParts > 1 && partIndx < temp->polyZ->numParts ) {
          if ( (i + 1) == temp->polyZ->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        dx = temp->polyZ->points[i+1].X - temp->polyZ->points[i].X;
        dy = temp->polyZ->points[i+1].Y - temp->polyZ->points[i].Y;

        size += sqrt( dx*dx + dy*dy );
      }

    } else if (shape.shapeType == POLYLINE_M ) {

      /* calculate the length of the polyline for this record */
      partIndx = 1; 
      for ( i = 0; i < temp->polyM->numPoints-1; ++i ) {
        double dx, dy;

        /* if there are multiple parts assume that the parts are not connected*/
        if ( temp->polyM->numParts > 1 && partIndx < temp->polyM->numParts ) {
          if ( (i + 1) == temp->polyM->parts[partIndx] ) {
            ++partIndx;
            continue;
          }
        }

        dx = temp->polyM->points[i+1].X - temp->polyM->points[i].X;
        dy = temp->polyM->points[i+1].Y - temp->polyM->points[i].Y;

        size += sqrt( dx*dx + dy*dy );
      }
    }

    REAL(data)[idx] = size;
    ++idx;

    temp = temp->next;
  }

  /* clean up */
  deallocateRecords( shape.records );
  UNPROTECT(1);

  return data;
}


/**********************************************************
** Function:   convertToR
**
** Purpose:    Converts the sent shape C struct to an R object.
**             The shape can be either a points, polylines, or polygons 
**             shape. The returning R object will look like the objects
**             generated by maptools.
** Notes:      The sent shape struct may hold info for several .shp files
**             while each node in the sent list of dbf structs represents
**             one .dbf file.
** Arguments:  shape,   pointer to shape struct that stores the
**                      shapefile info and data
**             dbf,  pointer to the head node of a list of dbf structs 
** Return:     data, R object containing all the shape data 
***********************************************************/
SEXP convertToR( Shape * shape, Dbf * dbf, SEXP fileNamePrefix ) {

  int i, row, col;   /* loop counters */
  Record * temp;     /* temp pointer for traversing list of records */
  SEXP data = NULL;  /* R object to store data in for returning to R */
  SEXP tempVec;      /* temp storage for data */
  SEXP colNamesVec;  /* stores the names of the columns in the R object */
  int recIndex;      /* index couunter for adding records to the shape vector*/

  /* R vector for building the returning R object */
  SEXP shapes, shapeVec, Pstart, verts, shpType, nVerts, nParts, bbox; 
  SEXP areas, ringDirs, length, attribs, class, attData;
  SEXP zValue, mValue, zRange, zArray, mRange, mArray;
  char str[20];  /* stores the number of records as a character string */
                 /* assumes num records is never more than a 20 digit number */
  unsigned int numDbfRecords = 0;  /* total number of records in the list */
                                   /* of dbf structs */
  Dbf * tempDbf = NULL;  /* used for traversing link list of dbf structs */
  int idx;               /* array index */
  SEXP sizesVec;


  /* object will have two vectors, Shapes and att.data */
  PROTECT( data = allocVector( VECSXP, 2 ) );

  /* shapes stores all the individual records */
  PROTECT( shapes = allocVector( VECSXP, shape->numRecords ) );

  /* attData stores the attributes matrix */
  if ( shape->shapeType == POINTS ||  shape->shapeType == POINTS_Z ) {
    PROTECT( attData = allocVector( VECSXP, dbf->numFields ) );
  } else {
    PROTECT( attData = allocVector( VECSXP, dbf->numFields + 1 ) );
  }

  /* Points */
  if ( shape->shapeType == POINTS ) {

    /* add each record to the R object */
    temp = shape->records;
    recIndex = 0;
    while( temp ) {
      PROTECT( shapeVec = allocVector( VECSXP, 6 ) );
   
      /* store the point coordinates */ 
      PROTECT( verts = allocMatrix( REALSXP, 1, 2 ) );
      REAL( verts )[0] = temp->point->X;
      REAL( verts )[1] = temp->point->Y;

      /* part marker */
      PROTECT( Pstart = R_NilValue );

      /* shape type */
      PROTECT( shpType = allocVector( INTSXP, 1 ) );
      INTEGER( shpType )[0] = temp->shapeType;

      /* number of points */
      PROTECT( nVerts = allocVector( INTSXP, 1 ) );
      INTEGER( nVerts )[0] = 1;

      /* number of parts */
      PROTECT( nParts = allocVector( INTSXP, 1 ) );
      INTEGER( nParts )[0] = 1;

      /* bounding box */
      PROTECT( bbox = allocVector( REALSXP, 4 ) );
      REAL( bbox )[0] = temp->point->X;
      REAL( bbox )[1] = temp->point->Y;
      REAL( bbox )[2] = temp->point->X;
      REAL( bbox )[3] = temp->point->Y;

      /* add vectors to shapeVec */
      SET_VECTOR_ELT( shapeVec, 0, Pstart );
      SET_VECTOR_ELT( shapeVec, 1, verts );
      SET_VECTOR_ELT( shapeVec, 2, shpType );
      SET_VECTOR_ELT( shapeVec, 3, nVerts );
      SET_VECTOR_ELT( shapeVec, 4, nParts );
      SET_VECTOR_ELT( shapeVec, 5, bbox );

      /* add names to shapeVec */
      PROTECT( colNamesVec = allocVector( STRSXP, 6 ));
      SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
      SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
      SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
      SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
      SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
      SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
      setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

      /* add attributes to shapeVec */
      setAttrib( shapeVec, install("nVerts"), nVerts );
      setAttrib( shapeVec, install("nParts"), nParts );
      setAttrib( shapeVec, install("shp.type"), shpType );
      setAttrib( shapeVec, install("bbox"), bbox );

      /* add this record to the shapes vector */
      SET_VECTOR_ELT( shapes, recIndex, shapeVec );

      ++recIndex;
      UNPROTECT( 8 );
      temp = temp->next;
    }

    /* copy dbf data to R object */

    /* figure out the total number of records in the dbf files */
    tempDbf = dbf;
    numDbfRecords = 0;
    while ( tempDbf ) {
      numDbfRecords += tempDbf->numRecords;
      tempDbf = tempDbf->next;
    }

    /* copy over all the fields in the dbf structs */
    for ( col = 0; col < dbf->numFields; ++col ) {
      idx = 0;

      /* determine if it is a numeric or string */
      if ( dbf->fields[col].type == 'F' ||
           dbf->fields[col].type == 'N' ) {
        PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
      } else {
        PROTECT( tempVec = allocVector( STRSXP, numDbfRecords ) );
      }
      tempDbf = dbf;
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
      SET_VECTOR_ELT( attData, col, tempVec );
      UNPROTECT( 1 );
    }

    /* add field names (column names) to the R object */
    PROTECT( colNamesVec = allocVector( STRSXP, dbf->numFields ) );
    for ( i = 0; i < dbf->numFields; ++i ) {
      SET_STRING_ELT( colNamesVec , i, mkChar( dbf->fields[i].name ) );
    }
    setAttrib( attData, R_NamesSymbol, colNamesVec );
    UNPROTECT( 1 );

    /* add the row names */
    PROTECT( attribs = allocVector( STRSXP, numDbfRecords ));
    for ( i = 0; i < numDbfRecords; ++i ) {
      sprintf( str, "%d", i+1 );
      SET_STRING_ELT( attribs, i, mkChar( str ) );
    }
    setAttrib( attData, install("row.names"), attribs );
    UNPROTECT( 1 );

    /* add the class type */
    PROTECT( class = allocVector( STRSXP, 1 ) );
    SET_STRING_ELT( class, 0, mkChar( "data.frame" ) );
    classgets( attData, class );
    UNPROTECT( 1 );

  /* PointsZ */
  } else if ( shape->shapeType == POINTS_Z ) {

    /* add each record to the R object */
    temp = shape->records;
    recIndex = 0;
    while( temp ) {
      PROTECT( shapeVec = allocVector( VECSXP, 8 ) );
   
      /* store the point coordinates */ 
      PROTECT( verts = allocMatrix( REALSXP, 1, 2 ) );
      REAL( verts )[0] = temp->pointZ->X;
      REAL( verts )[1] = temp->pointZ->Y;

      /* part marker */
      PROTECT( Pstart = R_NilValue );

      /* shape type */
      PROTECT( shpType = allocVector( INTSXP, 1 ) );
      INTEGER( shpType )[0] = temp->shapeType;

      /* number of points */
      PROTECT( nVerts = allocVector( INTSXP, 1 ) );
      INTEGER( nVerts )[0] = 1;

      /* number of parts */
      PROTECT( nParts = allocVector( INTSXP, 1 ) );
      INTEGER( nParts )[0] = 1;

      /* bounding box */
      PROTECT( bbox = allocVector( REALSXP, 4 ) );
      REAL( bbox )[0] = temp->pointZ->X;
      REAL( bbox )[1] = temp->pointZ->Y;
      REAL( bbox )[2] = temp->pointZ->X;
      REAL( bbox )[3] = temp->pointZ->Y;

      /* z value */ 
      PROTECT( zValue = allocVector( REALSXP, 1 ) );
      REAL( zValue )[0] = temp->pointZ->Z;

      /* m value */ 
      PROTECT( mValue = allocVector( REALSXP, 1 ) );
      REAL( mValue )[0] = temp->pointZ->M;

      /* add vectors to shapeVec */
      SET_VECTOR_ELT( shapeVec, 0, Pstart );
      SET_VECTOR_ELT( shapeVec, 1, verts );
      SET_VECTOR_ELT( shapeVec, 2, shpType );
      SET_VECTOR_ELT( shapeVec, 3, nVerts );
      SET_VECTOR_ELT( shapeVec, 4, nParts );
      SET_VECTOR_ELT( shapeVec, 5, bbox );
      SET_VECTOR_ELT( shapeVec, 6, zValue );
      SET_VECTOR_ELT( shapeVec, 7, mValue );

      /* add names to shapeVec */
      PROTECT( colNamesVec = allocVector( STRSXP, 8 ));
      SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
      SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
      SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
      SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
      SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
      SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
      SET_STRING_ELT( colNamesVec, 6, mkChar( "z" ));
      SET_STRING_ELT( colNamesVec, 7, mkChar( "m" ));
      setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

      /* add attributes to shapeVec */
      setAttrib( shapeVec, install("nVerts"), nVerts );
      setAttrib( shapeVec, install("nParts"), nParts );
      setAttrib( shapeVec, install("shp.type"), shpType );
      setAttrib( shapeVec, install("bbox"), bbox );

      /* add this record to the shapes vector */
      SET_VECTOR_ELT( shapes, recIndex, shapeVec );

      ++recIndex;
      UNPROTECT( 10 );
      temp = temp->next;
    }

    /* copy dbf data to R object */

    /* figure out the total number of records in the dbf files */
    tempDbf = dbf;
    numDbfRecords = 0;
    while ( tempDbf ) {
      numDbfRecords += tempDbf->numRecords;
      tempDbf = tempDbf->next;
    }

    /* copy over all the fields in the dbf structs */
    for ( col = 0; col < dbf->numFields; ++col ) {
      idx = 0;

      /* determine if it is a numeric or string */
      if ( dbf->fields[col].type == 'F' ||
           dbf->fields[col].type == 'N' ) {
        PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
      } else {
        PROTECT( tempVec = allocVector( STRSXP, numDbfRecords ) );
      }
      tempDbf = dbf;
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
      SET_VECTOR_ELT( attData, col, tempVec );
      UNPROTECT( 1 );
    }

    /* add field names (column names) to the R object */
    PROTECT( colNamesVec = allocVector( STRSXP, dbf->numFields ) );
    for ( i = 0; i < dbf->numFields; ++i ) {
      SET_STRING_ELT( colNamesVec , i, mkChar( dbf->fields[i].name ) );
    }
    setAttrib( attData, R_NamesSymbol, colNamesVec );
    UNPROTECT( 1 );

    /* add the row names */
    PROTECT( attribs = allocVector( STRSXP, numDbfRecords ));
    for ( i = 0; i < numDbfRecords; ++i ) {
      sprintf( str, "%d", i+1 );
      SET_STRING_ELT( attribs, i, mkChar( str ) );
    }
    setAttrib( attData, install("row.names"), attribs );
    UNPROTECT( 1 );

    /* add the class type */
    PROTECT( class = allocVector( STRSXP, 1 ) );
    SET_STRING_ELT( class, 0, mkChar( "data.frame" ) );
    classgets( attData, class );
    UNPROTECT( 1 );

  /* PointsM */
  } else if ( shape->shapeType == POINTS_M ) {

    /* add each record to the R object */
    temp = shape->records;
    recIndex = 0;
    while( temp ) {
      PROTECT( shapeVec = allocVector( VECSXP, 7 ) );
   
      /* store the point coordinates */ 
      PROTECT( verts = allocMatrix( REALSXP, 1, 2 ) );
      REAL( verts )[0] = temp->pointM->X;
      REAL( verts )[1] = temp->pointM->Y;

      /* part marker */
      PROTECT( Pstart = R_NilValue );

      /* shape type */
      PROTECT( shpType = allocVector( INTSXP, 1 ) );
      INTEGER( shpType )[0] = temp->shapeType;

      /* number of points */
      PROTECT( nVerts = allocVector( INTSXP, 1 ) );
      INTEGER( nVerts )[0] = 1;

      /* number of parts */
      PROTECT( nParts = allocVector( INTSXP, 1 ) );
      INTEGER( nParts )[0] = 1;

      /* bounding box */
      PROTECT( bbox = allocVector( REALSXP, 4 ) );
      REAL( bbox )[0] = temp->pointM->X;
      REAL( bbox )[1] = temp->pointM->Y;
      REAL( bbox )[2] = temp->pointM->X;
      REAL( bbox )[3] = temp->pointM->Y;

      /* m value */ 
      PROTECT( mValue = allocVector( REALSXP, 1 ) );
      REAL( mValue )[0] = temp->pointM->M;

      /* add vectors to shapeVec */
      SET_VECTOR_ELT( shapeVec, 0, Pstart );
      SET_VECTOR_ELT( shapeVec, 1, verts );
      SET_VECTOR_ELT( shapeVec, 2, shpType );
      SET_VECTOR_ELT( shapeVec, 3, nVerts );
      SET_VECTOR_ELT( shapeVec, 4, nParts );
      SET_VECTOR_ELT( shapeVec, 5, bbox );
      SET_VECTOR_ELT( shapeVec, 6, mValue );

      /* add names to shapeVec */
      PROTECT( colNamesVec = allocVector( STRSXP, 7 ));
      SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
      SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
      SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
      SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
      SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
      SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
      SET_STRING_ELT( colNamesVec, 6, mkChar( "m" ));
      setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

      /* add attributes to shapeVec */
      setAttrib( shapeVec, install("nVerts"), nVerts );
      setAttrib( shapeVec, install("nParts"), nParts );
      setAttrib( shapeVec, install("shp.type"), shpType );
      setAttrib( shapeVec, install("bbox"), bbox );

      /* add this record to the shapes vector */
      SET_VECTOR_ELT( shapes, recIndex, shapeVec );

      ++recIndex;
      UNPROTECT( 9 );
      temp = temp->next;
    }

    /* copy dbf data to R object */

    /* figure out the total number of records in the dbf files */
    tempDbf = dbf;
    numDbfRecords = 0;
    while ( tempDbf ) {
      numDbfRecords += tempDbf->numRecords;
      tempDbf = tempDbf->next;
    }

    /* copy over all the fields in the dbf structs */
    for ( col = 0; col < dbf->numFields; ++col ) {
      idx = 0;

      /* determine if it is a numeric or string */
      if ( dbf->fields[col].type == 'F' ||
           dbf->fields[col].type == 'N' ) {
        PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
      } else {
        PROTECT( tempVec = allocVector( STRSXP, numDbfRecords ) );
      }
      tempDbf = dbf;
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
      SET_VECTOR_ELT( attData, col, tempVec );
      UNPROTECT( 1 );
    }

    /* add field names (column names) to the R object */
    PROTECT( colNamesVec = allocVector( STRSXP, dbf->numFields ) );
    for ( i = 0; i < dbf->numFields; ++i ) {
      SET_STRING_ELT( colNamesVec , i, mkChar( dbf->fields[i].name ) );
    }
    setAttrib( attData, R_NamesSymbol, colNamesVec );
    UNPROTECT( 1 );

    /* add the row names */
    PROTECT( attribs = allocVector( STRSXP, numDbfRecords ));
    for ( i = 0; i < numDbfRecords; ++i ) {
      sprintf( str, "%d", i+1 );
      SET_STRING_ELT( attribs, i, mkChar( str ) );
    }
    setAttrib( attData, install("row.names"), attribs );
    UNPROTECT( 1 );

    /* add the class type */
    PROTECT( class = allocVector( STRSXP, 1 ) );
    SET_STRING_ELT( class, 0, mkChar( "data.frame" ) );
    classgets( attData, class );
    UNPROTECT( 1 );

  /* Polyline or Polygon */
  } else if ( shape->shapeType == POLYLINE || shape->shapeType == POLYGON ) {

    /* add each record to the R object */
    temp = shape->records;
    recIndex = 0;
    while( temp ) {
      if ( shape->shapeType == POLYGON ) {
        PROTECT( shapeVec = allocVector( VECSXP, 8 ) );
      } else {
        PROTECT( shapeVec = allocVector( VECSXP, 7 ) );
      }
   
      /* store the point coordinates */ 
      PROTECT( verts = allocMatrix( REALSXP, temp->poly->numPoints, 2 ) );
      for ( i = 0; i < temp->poly->numPoints; ++i ) {
        REAL( verts )[i] = temp->poly->points[i].X;
        REAL( verts )[i + temp->poly->numPoints] = temp->poly->points[i].Y;
      }

      /* part markers */
      PROTECT( Pstart = allocVector( INTSXP, temp->poly->numParts ) );
      for ( i = 0; i < temp->poly->numParts; ++i ) {
        INTEGER( Pstart )[i] = temp->poly->parts[i];
      }

      /* shape type */
      PROTECT( shpType = allocVector( INTSXP, 1 ) );
      INTEGER( shpType )[0] = temp->shapeType;

      /* number of points */
      PROTECT( nVerts = allocVector( INTSXP, 1 ) );
      INTEGER( nVerts )[0] = temp->poly->numPoints;

      /* number of parts */
      PROTECT( nParts = allocVector( INTSXP, 1 ) );
      INTEGER( nParts )[0] = temp->poly->numParts;

      /* bounding box */
      PROTECT( bbox = allocVector( REALSXP, 4 ) );
      for ( i = 0; i < 4; ++i ) {
        REAL( bbox )[i] = temp->poly->box[i];
      }

      /* Polygon */
      if ( shape->shapeType == POLYGON ) {

        /* ring directions */
        if ( shape->shapeType == POLYGON ) {
          PROTECT( ringDirs = allocVector( INTSXP, temp->poly->numParts ) );
          for ( i = 0; i < temp->poly->numParts; ++i ) {
            INTEGER( ringDirs )[i] = temp->poly->ringDirs[i];
          }
        }

        /* calculate areas of the different parts */
        PROTECT( areas = allocVector( REALSXP, temp->poly->numParts ) );
        for ( i = 0; i < temp->poly->numParts; ++i ) {
          int firstPoint = temp->poly->parts[i];
          int secondPoint;
          if ( temp->poly->numParts == 1 || i == temp->poly->numParts-1 ) {
            secondPoint = temp->poly->numPoints - 1;
          } else {
            secondPoint = temp->poly->parts[i+1] - 1;
          }
          REAL(areas)[i]=calcArea(temp->poly->points, firstPoint, secondPoint); 
        }

        /* add vectors to shapeVec */
        SET_VECTOR_ELT( shapeVec, 0, Pstart );
        SET_VECTOR_ELT( shapeVec, 1, verts );
        SET_VECTOR_ELT( shapeVec, 2, shpType );
        SET_VECTOR_ELT( shapeVec, 3, nVerts );
        SET_VECTOR_ELT( shapeVec, 4, nParts );
        SET_VECTOR_ELT( shapeVec, 5, bbox );
        SET_VECTOR_ELT( shapeVec, 6, ringDirs );
        SET_VECTOR_ELT( shapeVec, 7, areas );

        /* add names to shapeVec */
        PROTECT( colNamesVec = allocVector( STRSXP, 8 ));
        SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
        SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
        SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
        SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
        SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
        SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
        SET_STRING_ELT( colNamesVec, 6, mkChar( "RingDir" ));
        SET_STRING_ELT( colNamesVec, 7, mkChar( "areas" ));
        setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

        /* add attributes to shapeVec */
        setAttrib( shapeVec, install("nVerts"), nVerts );
        setAttrib( shapeVec, install("nParts"), nParts );
        setAttrib( shapeVec, install("shp.type"), shpType );
        setAttrib( shapeVec, install("bbox"), bbox );
        setAttrib( shapeVec, install("RingDir"), ringDirs );
        setAttrib( shapeVec, install("areas"), areas );

       /* Polyline */
      } else {
        double dx;
        double dy;
        double len = 0.0;
        PROTECT( length = allocVector( REALSXP, 1 ) );

        /* calculate total length of the segments for this record */
        for ( i = 0; i < temp->poly->numPoints-1; ++i ) {
          dx = temp->poly->points[i+1].X - temp->poly->points[i].X;
          dy = temp->poly->points[i+1].Y - temp->poly->points[i].Y;
          len += sqrt( dx*dx + dy*dy );
        }
        REAL( length )[0] = len;

        /* add vectors to shapeVec */
        SET_VECTOR_ELT( shapeVec, 0, Pstart );
        SET_VECTOR_ELT( shapeVec, 1, verts );
        SET_VECTOR_ELT( shapeVec, 2, shpType );
        SET_VECTOR_ELT( shapeVec, 3, nVerts );
        SET_VECTOR_ELT( shapeVec, 4, nParts );
        SET_VECTOR_ELT( shapeVec, 5, bbox );
        SET_VECTOR_ELT( shapeVec, 6, length );

        /* add names to shapeVec */
        PROTECT( colNamesVec = allocVector( STRSXP, 7 ));
        SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
        SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
        SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
        SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
        SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
        SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
        SET_STRING_ELT( colNamesVec, 6, mkChar( "length" ));
        setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

        /* add attributes to shapeVec */
        setAttrib( shapeVec, install("nVerts"), nVerts );
        setAttrib( shapeVec, install("nParts"), nParts );
        setAttrib( shapeVec, install("shp.type"), shpType );
        setAttrib( shapeVec, install("bbox"), bbox );
        setAttrib( shapeVec, install("length"), length );

      }

      /* add this record to the shapes vector */
      SET_VECTOR_ELT( shapes, recIndex, shapeVec );

      ++recIndex;
      if ( shape->shapeType == POLYGON ) {
        UNPROTECT( 10 );
      } else {
        UNPROTECT( 9 );
      }
      temp = temp->next;
    }

    /* copy dbf data to R object */

    /* figure out the total number of records in the dbf files */
    tempDbf = dbf;
    numDbfRecords = 0;
    while ( tempDbf ) {
      numDbfRecords += tempDbf->numRecords;
      tempDbf = tempDbf->next;
    }

    /*copy over the fields in the dbf structs */
    for ( col = 0; col < dbf->numFields; ++col ) {
      idx = 0;

      /* determine if it is a numeric or string */
      if ( dbf->fields[col].type == 'F' || 
           dbf->fields[col].type == 'N' ) {
        PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
      } else {
        PROTECT( tempVec = allocVector( STRSXP, numDbfRecords ) );
      }
      tempDbf = dbf;

      /* go through the list of dbf structs */
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
      SET_VECTOR_ELT( attData, col, tempVec );
      UNPROTECT( 1 );
    }

    /* add the area_mdm or length_mdm values */
    sizesVec = getRecordShapeSizes( fileNamePrefix );
    PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
    for ( row = 0; row < numDbfRecords; ++row ) {
      REAL( tempVec )[row] = REAL( sizesVec )[row];
    }
    SET_VECTOR_ELT( attData, col, tempVec );
    UNPROTECT( 1 );


    /* add field names(column names) to the R object */
    PROTECT( colNamesVec = allocVector( STRSXP, dbf->numFields+1 ) );
    for ( i = 0; i < dbf->numFields; ++i ) {
      SET_STRING_ELT( colNamesVec, i, mkChar( dbf->fields[i].name ) );
    }
    if ( shape->shapeType == POLYGON ) {
      SET_STRING_ELT( colNamesVec, i, mkChar( "area_mdm" ) );
    } else {
      SET_STRING_ELT( colNamesVec, i, mkChar( "length_mdm" ) );
    }
    setAttrib( attData, R_NamesSymbol, colNamesVec );
    UNPROTECT( 1 );

    /* add the row names */
    PROTECT( attribs = allocVector( STRSXP, numDbfRecords ));
    for ( i = 0; i < numDbfRecords; ++i ) {
      sprintf( str, "%d", i+1 );
      SET_STRING_ELT( attribs, i, mkChar( str ) );
    }
    setAttrib( attData, install("row.names"), attribs );
    UNPROTECT( 1 );

    /* add the class */
    PROTECT( class = allocVector( STRSXP, 1 ) );
    SET_STRING_ELT( class, 0, mkChar( "data.frame" ) );
    classgets( attData, class );
    UNPROTECT( 1 );

  /* PolylineZ or PolygonZ */
  } else if (shape->shapeType == POLYLINE_Z || shape->shapeType == POLYGON_Z) {

    /* add each record to the R object */
    temp = shape->records;
    recIndex = 0;
    while( temp ) {
      if ( shape->shapeType == POLYGON_Z ) {
        PROTECT( shapeVec = allocVector( VECSXP, 12 ) );
      } else {
        PROTECT( shapeVec = allocVector( VECSXP, 11 ) );
      }
   
      /* store the point coordinates */ 
      PROTECT( verts = allocMatrix( REALSXP, temp->polyZ->numPoints, 2 ) );
      for ( i = 0; i < temp->polyZ->numPoints; ++i ) {
        REAL( verts )[i] = temp->polyZ->points[i].X;
        REAL( verts )[i + temp->polyZ->numPoints] = temp->polyZ->points[i].Y;
      }

      /* part markers */
      PROTECT( Pstart = allocVector( INTSXP, temp->polyZ->numParts ) );
      for ( i = 0; i < temp->polyZ->numParts; ++i ) {
        INTEGER( Pstart )[i] = temp->polyZ->parts[i];
      }

      /* shape type */
      PROTECT( shpType = allocVector( INTSXP, 1 ) );
      INTEGER( shpType )[0] = temp->shapeType;

      /* number of points */
      PROTECT( nVerts = allocVector( INTSXP, 1 ) );
      INTEGER( nVerts )[0] = temp->polyZ->numPoints;

      /* number of parts */
      PROTECT( nParts = allocVector( INTSXP, 1 ) );
      INTEGER( nParts )[0] = temp->polyZ->numParts;

      /* bounding box */
      PROTECT( bbox = allocVector( REALSXP, 4 ) );
      for ( i = 0; i < 4; ++i ) {
        REAL( bbox )[i] = temp->polyZ->box[i];
      }

      /* ring directions */
      if ( shape->shapeType == POLYGON_Z ) {
        PROTECT( ringDirs = allocVector( INTSXP, temp->polyZ->numParts ) );
        for ( i = 0; i < temp->polyZ->numParts; ++i ) {
          INTEGER( ringDirs )[i] = temp->polyZ->ringDirs[i];
        }
      }

      /* PolygonZ */
      if ( shape->shapeType == POLYGON_Z ) {

        /* calculate areas of the different parts */
        PROTECT( areas = allocVector( REALSXP, temp->polyZ->numParts ) );
        for ( i = 0; i < temp->polyZ->numParts; ++i ) {
          int firstPoint = temp->polyZ->parts[i];
          int secondPoint;
          if ( temp->polyZ->numParts == 1 || i == temp->polyZ->numParts-1 ) {
            secondPoint = temp->polyZ->numPoints - 1;
          } else {
            secondPoint = temp->polyZ->parts[i+1] - 1;
          }
          REAL(areas)[i]=calcArea(temp->polyZ->points, firstPoint, secondPoint); 
        }

        /* zRange */
        PROTECT( zRange = allocVector( REALSXP, 2 ));
        for ( i = 0; i < 2; ++i ) {
          REAL(zRange)[i] = temp->polyZ->zRange[i];
        }

        /* zArray */
        PROTECT( zArray = allocVector( REALSXP, temp->polyZ->numPoints ) );

        /* calculate z value of the parts */
        for ( i = 0; i < temp->polyZ->numPoints; ++i ) {
          REAL(zArray)[i]=temp->polyZ->zArray[i];
        }

        /* mRange */
        PROTECT( mRange = allocVector( REALSXP, 2 ));
        for ( i = 0; i < 2; ++i ) {
          REAL(mRange)[i] = temp->polyZ->mRange[i];
        }

        /* mArray */
        PROTECT( mArray = allocVector( REALSXP, temp->polyZ->numPoints ) );

        /* calculate m value of the parts */
        for ( i = 0; i < temp->polyZ->numPoints; ++i ) {
          REAL(mArray)[i]=temp->polyZ->mArray[i];
        }

        /* add vectors to shapeVec */
        SET_VECTOR_ELT( shapeVec, 0, Pstart );
        SET_VECTOR_ELT( shapeVec, 1, verts );
        SET_VECTOR_ELT( shapeVec, 2, shpType );
        SET_VECTOR_ELT( shapeVec, 3, nVerts );
        SET_VECTOR_ELT( shapeVec, 4, nParts );
        SET_VECTOR_ELT( shapeVec, 5, bbox );
        SET_VECTOR_ELT( shapeVec, 6, ringDirs );
        SET_VECTOR_ELT( shapeVec, 7, areas );
        SET_VECTOR_ELT( shapeVec, 8, zRange);
        SET_VECTOR_ELT( shapeVec, 9, zArray);
        SET_VECTOR_ELT( shapeVec, 10, mRange);
        SET_VECTOR_ELT( shapeVec, 11, mArray);

        /* add names to shapeVec */
        PROTECT( colNamesVec = allocVector( STRSXP, 12 ));
        SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
        SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
        SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
        SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
        SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
        SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
        SET_STRING_ELT( colNamesVec, 6, mkChar( "RingDir" ));
        SET_STRING_ELT( colNamesVec, 7, mkChar( "areas" ));
        SET_STRING_ELT( colNamesVec, 8, mkChar( "zRange" ));
        SET_STRING_ELT( colNamesVec, 9, mkChar( "zArray" ));
        SET_STRING_ELT( colNamesVec, 10, mkChar( "mRange" ));
        SET_STRING_ELT( colNamesVec, 11, mkChar( "mArray" ));
        setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

        /* add attributes to shapeVec */
        setAttrib( shapeVec, install("nVerts"), nVerts );
        setAttrib( shapeVec, install("nParts"), nParts );
        setAttrib( shapeVec, install("shp.type"), shpType );
        setAttrib( shapeVec, install("bbox"), bbox );
        setAttrib( shapeVec, install("RingDir"), ringDirs );
        setAttrib( shapeVec, install("areas"), areas );
        setAttrib( shapeVec, install("zRange"), zRange);
        setAttrib( shapeVec, install("zArray"), zArray );
        setAttrib( shapeVec, install("mRange"), mRange);
        setAttrib( shapeVec, install("mArray"), mArray );

      /* PolylineZ */
      } else {
        double dx;
        double dy;
        double len = 0.0;
        PROTECT( length = allocVector( REALSXP, 1 ) );

        /* calculate total length of the segments for this record */
        for ( i = 0; i < temp->polyZ->numPoints-1; ++i ) {
          dx = temp->polyZ->points[i+1].X - temp->polyZ->points[i].X;
          dy = temp->polyZ->points[i+1].Y - temp->polyZ->points[i].Y;
          len += sqrt( dx*dx + dy*dy );
        }
        REAL( length )[0] = len;

        /* zRange */
        PROTECT( zRange = allocVector( REALSXP, 2 ));
        for ( i = 0; i < 2; ++i ) {
          REAL(zRange)[i] = temp->polyZ->zRange[i];
        }

        /* zArray */
        PROTECT( zArray = allocVector( REALSXP, temp->polyZ->numPoints ) );

        /* calculate z value of the parts */
        for ( i = 0; i < temp->polyZ->numPoints; ++i ) {
          REAL(zArray)[i]=temp->polyZ->zArray[i];
        }

        /* mRange */
        PROTECT( mRange = allocVector( REALSXP, 2 ));
        for ( i = 0; i < 2; ++i ) {
          REAL(mRange)[i] = temp->polyZ->mRange[i];
        }

        /* mArray */
        PROTECT( mArray = allocVector( REALSXP, temp->polyZ->numPoints ) );

        /* calculate m value of the parts */
        for ( i = 0; i < temp->polyZ->numPoints; ++i ) {
          REAL(mArray)[i]=temp->polyZ->mArray[i];
        }

        /* add vectors to shapeVec */
        SET_VECTOR_ELT( shapeVec, 0, Pstart );
        SET_VECTOR_ELT( shapeVec, 1, verts );
        SET_VECTOR_ELT( shapeVec, 2, shpType );
        SET_VECTOR_ELT( shapeVec, 3, nVerts );
        SET_VECTOR_ELT( shapeVec, 4, nParts );
        SET_VECTOR_ELT( shapeVec, 5, bbox );
        SET_VECTOR_ELT( shapeVec, 6, length );
        SET_VECTOR_ELT( shapeVec, 7, zRange);
        SET_VECTOR_ELT( shapeVec, 8, zArray);
        SET_VECTOR_ELT( shapeVec, 9, mRange);
        SET_VECTOR_ELT( shapeVec, 10, mArray);

        /* add names to shapeVec */
        PROTECT( colNamesVec = allocVector( STRSXP, 11 ));
        SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
        SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
        SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
        SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
        SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
        SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
        SET_STRING_ELT( colNamesVec, 6, mkChar( "length" ));
        SET_STRING_ELT( colNamesVec, 7, mkChar( "zRange" ));
        SET_STRING_ELT( colNamesVec, 8, mkChar( "zArray" ));
        SET_STRING_ELT( colNamesVec, 9, mkChar( "mRange" ));
        SET_STRING_ELT( colNamesVec, 10, mkChar( "mArray" ));
        setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

      /* add attributes to shapeVec */
        setAttrib( shapeVec, install("nVerts"), nVerts );
        setAttrib( shapeVec, install("nParts"), nParts );
        setAttrib( shapeVec, install("shp.type"), shpType );
        setAttrib( shapeVec, install("bbox"), bbox );
        setAttrib( shapeVec, install("length"), length );
        setAttrib( shapeVec, install("zRange"), zRange);
        setAttrib( shapeVec, install("zArray"), zArray );
        setAttrib( shapeVec, install("mRange"), mRange);
        setAttrib( shapeVec, install("mArray"), mArray );

      }

      /* add this record to the shapes vector */
      SET_VECTOR_ELT( shapes, recIndex, shapeVec );

      ++recIndex;
      if ( shape->shapeType == POLYGON_Z ) {
        UNPROTECT( 14 );
      } else {
        UNPROTECT( 13 );
      }
      temp = temp->next;
    }

    /* copy dbf data to R object */

    /* figure out the total number of records in the dbf files */
    tempDbf = dbf;
    numDbfRecords = 0;
    while ( tempDbf ) {
      numDbfRecords += tempDbf->numRecords;
      tempDbf = tempDbf->next;
    }

    /*copy over the fields in the dbf structs */
    for ( col = 0; col < dbf->numFields; ++col ) {
      idx = 0;

      /* determine if it is a numeric or string */
      if ( dbf->fields[col].type == 'F' || 
           dbf->fields[col].type == 'N' ) {
        PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
      } else {
        PROTECT( tempVec = allocVector( STRSXP, numDbfRecords ) );
      }
      tempDbf = dbf;

      /* go through the list of dbf structs */
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
      SET_VECTOR_ELT( attData, col, tempVec );
      UNPROTECT( 1 );
    }

    /* add the area_mdm or length_mdm values */
    sizesVec = getRecordShapeSizes( fileNamePrefix );
    PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
    for ( row = 0; row < numDbfRecords; ++row ) {
      REAL( tempVec )[row] = REAL( sizesVec )[row];
    }
    SET_VECTOR_ELT( attData, col, tempVec );
    UNPROTECT( 1 );


    /* add field names(column names) to the R object */
    PROTECT( colNamesVec = allocVector( STRSXP, dbf->numFields+1 ) );
    for ( i = 0; i < dbf->numFields; ++i ) {
      SET_STRING_ELT( colNamesVec, i, mkChar( dbf->fields[i].name ) );
    }
    if ( shape->shapeType == POLYGON_Z ) {
      SET_STRING_ELT( colNamesVec, i, mkChar( "area_mdm" ) );
    } else {
      SET_STRING_ELT( colNamesVec, i, mkChar( "length_mdm" ) );
    }
    setAttrib( attData, R_NamesSymbol, colNamesVec );
    UNPROTECT( 1 );

    /* add the row names */
    PROTECT( attribs = allocVector( STRSXP, numDbfRecords ));
    for ( i = 0; i < numDbfRecords; ++i ) {
      sprintf( str, "%d", i+1 );
      SET_STRING_ELT( attribs, i, mkChar( str ) );
    }
    setAttrib( attData, install("row.names"), attribs );
    UNPROTECT( 1 );

    /* add the class */
    PROTECT( class = allocVector( STRSXP, 1 ) );
    SET_STRING_ELT( class, 0, mkChar( "data.frame" ) );
    classgets( attData, class );
    UNPROTECT( 1 );

  /* PolylineM or PolygonM */
  } else if (shape->shapeType == POLYLINE_M || shape->shapeType == POLYGON_M) {

    /* add each record to the R object */
    temp = shape->records;
    recIndex = 0;
    while( temp ) {
      if ( shape->shapeType == POLYGON_M ) {
        PROTECT( shapeVec = allocVector( VECSXP, 10 ) );
      } else {
        PROTECT( shapeVec = allocVector( VECSXP, 9 ) );
      }
   
      /* store the point coordinates */ 
      PROTECT( verts = allocMatrix( REALSXP, temp->polyM->numPoints, 2 ) );
      for ( i = 0; i < temp->polyM->numPoints; ++i ) {
        REAL( verts )[i] = temp->polyM->points[i].X;
        REAL( verts )[i + temp->polyM->numPoints] = temp->polyM->points[i].Y;
      }

      /* part markers */
      PROTECT( Pstart = allocVector( INTSXP, temp->polyM->numParts ) );
      for ( i = 0; i < temp->polyM->numParts; ++i ) {
        INTEGER( Pstart )[i] = temp->polyM->parts[i];
      }

      /* shape type */
      PROTECT( shpType = allocVector( INTSXP, 1 ) );
      INTEGER( shpType )[0] = temp->shapeType;

      /* number of points */
      PROTECT( nVerts = allocVector( INTSXP, 1 ) );
      INTEGER( nVerts )[0] = temp->polyM->numPoints;

      /* number of parts */
      PROTECT( nParts = allocVector( INTSXP, 1 ) );
      INTEGER( nParts )[0] = temp->polyM->numParts;

      /* bounding box */
      PROTECT( bbox = allocVector( REALSXP, 4 ) );
      for ( i = 0; i < 4; ++i ) {
        REAL( bbox )[i] = temp->polyM->box[i];
      }

      /* ring directions */
      if ( shape->shapeType == POLYGON_M ) {
        PROTECT( ringDirs = allocVector( INTSXP, temp->polyM->numParts ) );
        for ( i = 0; i < temp->polyM->numParts; ++i ) {
          INTEGER( ringDirs )[i] = temp->polyM->ringDirs[i];
        }
      }

      /* PolygonM */
      if ( shape->shapeType == POLYGON_M ) {

        /* calculate areas of the different parts */
        PROTECT( areas = allocVector( REALSXP, temp->polyM->numParts ) );
        for ( i = 0; i < temp->polyM->numParts; ++i ) {
          int firstPoint = temp->polyM->parts[i];
          int secondPoint;
          if ( temp->polyM->numParts == 1 || i == temp->polyM->numParts-1 ) {
            secondPoint = temp->polyM->numPoints - 1;
          } else {
            secondPoint = temp->polyM->parts[i+1] - 1;
          }
          REAL(areas)[i]=calcArea(temp->polyM->points, firstPoint, secondPoint); 
        }

        /* mRange */
        PROTECT( mRange = allocVector( REALSXP, 2 ));
        for ( i = 0; i < 2; ++i ) {
          REAL(mRange)[i] = temp->polyM->mRange[i];
        }

        /* mArray */
        PROTECT( mArray = allocVector( REALSXP, temp->polyM->numPoints ) );

        /* calculate m value of the parts */
        for ( i = 0; i < temp->polyM->numPoints; ++i ) {
          REAL(mArray)[i]=temp->polyM->mArray[i];
        }

        /* add vectors to shapeVec */
        SET_VECTOR_ELT( shapeVec, 0, Pstart );
        SET_VECTOR_ELT( shapeVec, 1, verts );
        SET_VECTOR_ELT( shapeVec, 2, shpType );
        SET_VECTOR_ELT( shapeVec, 3, nVerts );
        SET_VECTOR_ELT( shapeVec, 4, nParts );
        SET_VECTOR_ELT( shapeVec, 5, bbox );
        SET_VECTOR_ELT( shapeVec, 6, ringDirs );
        SET_VECTOR_ELT( shapeVec, 7, areas );
        SET_VECTOR_ELT( shapeVec, 8, mRange);
        SET_VECTOR_ELT( shapeVec, 9, mArray);

        /* add names to shapeVec */
        PROTECT( colNamesVec = allocVector( STRSXP, 10 ));
        SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
        SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
        SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
        SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
        SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
        SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
        SET_STRING_ELT( colNamesVec, 6, mkChar( "RingDir" ));
        SET_STRING_ELT( colNamesVec, 7, mkChar( "areas" ));
        SET_STRING_ELT( colNamesVec, 8, mkChar( "mRange" ));
        SET_STRING_ELT( colNamesVec, 9, mkChar( "mArray" ));
        setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

        /* add attributes to shapeVec */
        setAttrib( shapeVec, install("nVerts"), nVerts );
        setAttrib( shapeVec, install("nParts"), nParts );
        setAttrib( shapeVec, install("shp.type"), shpType );
        setAttrib( shapeVec, install("bbox"), bbox );
        setAttrib( shapeVec, install("RingDir"), ringDirs );
        setAttrib( shapeVec, install("areas"), areas );
        setAttrib( shapeVec, install("mRange"), mRange);
        setAttrib( shapeVec, install("mArray"), mArray );

      /* PolylineM */
      } else {
        double dx;
        double dy;
        double len = 0.0;
        PROTECT( length = allocVector( REALSXP, 1 ) );

        /* calculate total length of the segments for this record */
        for ( i = 0; i < temp->polyM->numPoints-1; ++i ) {
          dx = temp->polyM->points[i+1].X - temp->polyM->points[i].X;
          dy = temp->polyM->points[i+1].Y - temp->polyM->points[i].Y;
          len += sqrt( dx*dx + dy*dy );
        }
        REAL( length )[0] = len;

        /* mRange */
        PROTECT( mRange = allocVector( REALSXP, 2 ));
        for ( i = 0; i < 2; ++i ) {
          REAL(mRange)[i] = temp->polyM->mRange[i];
        }

        /* mArray */
        PROTECT( mArray = allocVector( REALSXP, temp->polyM->numPoints ) );

        /* calculate m value of the parts */
        for ( i = 0; i < temp->polyM->numPoints; ++i ) {
          REAL(mArray)[i]=temp->polyM->mArray[i];
        }

        /* add vectors to shapeVec */
        SET_VECTOR_ELT( shapeVec, 0, Pstart );
        SET_VECTOR_ELT( shapeVec, 1, verts );
        SET_VECTOR_ELT( shapeVec, 2, shpType );
        SET_VECTOR_ELT( shapeVec, 3, nVerts );
        SET_VECTOR_ELT( shapeVec, 4, nParts );
        SET_VECTOR_ELT( shapeVec, 5, bbox );
        SET_VECTOR_ELT( shapeVec, 6, length );
        SET_VECTOR_ELT( shapeVec, 7, mRange);
        SET_VECTOR_ELT( shapeVec, 8, mArray);

        /* add names to shapeVec */
        PROTECT( colNamesVec = allocVector( STRSXP, 9 ));
        SET_STRING_ELT( colNamesVec, 0, mkChar( "Pstart" ));
        SET_STRING_ELT( colNamesVec, 1, mkChar( "verts" ));
        SET_STRING_ELT( colNamesVec, 2, mkChar( "shp.type" ));
        SET_STRING_ELT( colNamesVec, 3, mkChar( "nVerts" ));
        SET_STRING_ELT( colNamesVec, 4, mkChar( "nParts" ));
        SET_STRING_ELT( colNamesVec, 5, mkChar( "bbox" ));
        SET_STRING_ELT( colNamesVec, 6, mkChar( "length" ));
        SET_STRING_ELT( colNamesVec, 7, mkChar( "mRange" ));
        SET_STRING_ELT( colNamesVec, 8, mkChar( "mArray" ));
        setAttrib( shapeVec, R_NamesSymbol, colNamesVec );

        /* add attributes to shapeVec */
        setAttrib( shapeVec, install("nVerts"), nVerts );
        setAttrib( shapeVec, install("nParts"), nParts );
        setAttrib( shapeVec, install("shp.type"), shpType );
        setAttrib( shapeVec, install("bbox"), bbox );
        setAttrib( shapeVec, install("length"), length );
        setAttrib( shapeVec, install("mRange"), mRange);
        setAttrib( shapeVec, install("mArray"), mArray );

      }

      /* add this record to the shapes vector */
      SET_VECTOR_ELT( shapes, recIndex, shapeVec );

      ++recIndex;
      if ( shape->shapeType == POLYGON_M ) {
        UNPROTECT( 12 );
      } else {
        UNPROTECT( 11 );
      }
      temp = temp->next;
    }

    /* copy dbf data to R object */

    /* figure out the total number of records in the dbf files */
    tempDbf = dbf;
    numDbfRecords = 0;
    while ( tempDbf ) {
      numDbfRecords += tempDbf->numRecords;
      tempDbf = tempDbf->next;
    }

    /*copy over the fields in the dbf structs */
    for ( col = 0; col < dbf->numFields; ++col ) {
      idx = 0;

      /* determine if it is a numeric or string */
      if ( dbf->fields[col].type == 'F' || 
           dbf->fields[col].type == 'N' ) {
        PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
      } else {
        PROTECT( tempVec = allocVector( STRSXP, numDbfRecords ) );
      }
      tempDbf = dbf;

      /* go through the list of dbf structs */
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
      SET_VECTOR_ELT( attData, col, tempVec );
      UNPROTECT( 1 );
    }

    /* add the area_mdm or length_mdm values */
    sizesVec = getRecordShapeSizes( fileNamePrefix );
    PROTECT( tempVec = allocVector( REALSXP, numDbfRecords ) );
    for ( row = 0; row < numDbfRecords; ++row ) {
      REAL( tempVec )[row] = REAL( sizesVec )[row];
    }
    SET_VECTOR_ELT( attData, col, tempVec );
    UNPROTECT( 1 );


    /* add field names(column names) to the R object */
    PROTECT( colNamesVec = allocVector( STRSXP, dbf->numFields+1 ) );
    for ( i = 0; i < dbf->numFields; ++i ) {
      SET_STRING_ELT( colNamesVec, i, mkChar( dbf->fields[i].name ) );
    }
    if ( shape->shapeType == POLYGON_M ) {
      SET_STRING_ELT( colNamesVec, i, mkChar( "area_mdm" ) );
    } else {
      SET_STRING_ELT( colNamesVec, i, mkChar( "length_mdm" ) );
    }
    setAttrib( attData, R_NamesSymbol, colNamesVec );
    UNPROTECT( 1 );

    /* add the row names */
    PROTECT( attribs = allocVector( STRSXP, numDbfRecords ));
    for ( i = 0; i < numDbfRecords; ++i ) {
      sprintf( str, "%d", i+1 );
      SET_STRING_ELT( attribs, i, mkChar( str ) );
    }
    setAttrib( attData, install("row.names"), attribs );
    UNPROTECT( 1 );

    /* add the class */
    PROTECT( class = allocVector( STRSXP, 1 ) );
    SET_STRING_ELT( class, 0, mkChar( "data.frame" ) );
    classgets( attData, class );
    UNPROTECT( 1 );

  }

  /* add attributes to the shapes list */

  /* shape type */
  PROTECT( attribs = allocVector( STRSXP, 1 ));
  if ( shape->shapeType == POLYGON || shape->shapeType == POLYGON_Z ||
  	                                   shape->shapeType == POLYGON_M ) {
    SET_STRING_ELT( attribs, 0, mkChar( "poly" ) );
  } else if ( shape->shapeType == POLYLINE || shape->shapeType == POLYLINE_Z ||
  	                                         shape->shapeType == POLYLINE_M ) {
    SET_STRING_ELT( attribs, 0, mkChar( "arc" ) );
  } else if ( shape->shapeType == POINTS || shape->shapeType == POINTS_Z ||
  	                                         shape->shapeType == POINTS_M ) {
    SET_STRING_ELT( attribs, 0, mkChar( "point" ) );
  } else {
    SET_STRING_ELT( attribs, 0, mkChar( "not recognized" ) );
  }
  setAttrib( shapes, install("shp.type"), attribs );
  UNPROTECT( 1 );

  /* number of records */
  PROTECT( attribs = allocVector( INTSXP, 1 ));
  INTEGER( attribs )[0] = shape->numRecords;
  setAttrib( shapes, install("nshps"), attribs );
  UNPROTECT( 1 );

  /* minbb */
  PROTECT( attribs = allocVector( REALSXP, 4 ));
  REAL( attribs )[0] = shape->Xmin;
  REAL( attribs )[1] = shape->Ymin;
  REAL( attribs )[2] = 0.0;
  REAL( attribs )[3] = 0.0;
  setAttrib( shapes, install("minbb"), attribs );
  UNPROTECT( 1 );

  /* maxbb */
  PROTECT( attribs = allocVector( REALSXP, 4 ));
  REAL( attribs )[0] = shape->Xmax;
  REAL( attribs )[1] = shape->Ymax;
  REAL( attribs )[2] = 0.0;
  REAL( attribs )[3] = 0.0;
  setAttrib( shapes, install("maxbb"), attribs );
  UNPROTECT( 1 );

  /* class */
  PROTECT( attribs = allocVector( STRSXP, 1 ));
  SET_STRING_ELT( attribs, 0, mkChar( "ShapeList" ) );
  setAttrib( shapes, install("class"), attribs );
  UNPROTECT( 1 );

   /* put the data object together */
  SET_VECTOR_ELT( data, 0, shapes );
  SET_VECTOR_ELT( data, 1, attData );
  UNPROTECT( 2 );

  /* add names to the data object */
  PROTECT( colNamesVec = allocVector( STRSXP, 2 ));
  SET_STRING_ELT( colNamesVec, 0, mkChar( "Shapes" ));
  SET_STRING_ELT( colNamesVec, 1, mkChar( "att.data" )); 
  setAttrib( data, R_NamesSymbol, colNamesVec );
  UNPROTECT( 1 );

  /* add class to the data object */
  PROTECT( class = allocVector( STRSXP, 1 ) );
  SET_STRING_ELT( class, 0, mkChar( "Map" ) );
  classgets( data, class );
  UNPROTECT( 1 );

  UNPROTECT( 1 );
  return data;
}


/**********************************************************
** Function:   getPartAreas
**
** Purpose:    Returns a R vector of areas of all the parts found in
**             all the .shp files in the current working directory.
** Notes:      This function will return an error if one of the .shp files
**             is not a polygon shape type.  
**             Parts that are holes will have a negative area.
** Arguments:  void
** Return:     data,  R vector of areas for each part in the shapefiles
***********************************************************/
SEXP getPartAreas() {

  int i;             /* loop counter */
  FILE * fptr;       /* file pointer to shapefile */
  Shape shape;       /* struct to store all info and data found in shapefile */
  SEXP data = NULL;  /* R object to store data in for returning to R */
  int idx;           /* array index */
  Record * temp;     /* used to traverse list of records */
  int done = FALSE;  /* flag signalling all .shp files have been read */
  DIR * dirp = NULL;          /* used to open the current directory */
  struct dirent * fileShp;    /* used for reading file names */
  int ptrShp = 0;             /* ptr to .shp files in the current directory */
  char * restrict shpFileName = NULL;  /* stores the full shapefile name */

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;
  shape.numParts = 0;

  /* open the current directory */
  if((dirp = opendir(".")) == NULL) {
    Rprintf( "Error: Opening the current directory in C function getPartAreas.\n" );
    PROTECT( data = allocVector( VECSXP, 1 ) );
    UNPROTECT( 1 );
    return data;
  }

  /* get the name of the first .shp file */
  while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    if ( strlen(fileShp->d_name) > 4 ) {
      ptrShp = fileMatch( fileShp->d_name, ".shp" );
      if ( ptrShp == 1 ) {
        if ( (shpFileName = (char * restrict)malloc(strlen(fileShp->d_name)
              +  1)) == NULL ) {
          Rprintf( "Error: Allocating memory in C function getPartAreas.\n" );
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
    Rprintf( "Error: Couldn't find a .shp file in C function getPartAreas.\n" );
    closedir( dirp );
    PROTECT( data = allocVector( VECSXP, 2 ) );
    UNPROTECT(1);
    return data;
  }

while ( done == FALSE ) {
  
    /* open the shapefile */
    if ( (fptr = fopen( shpFileName, "rb" )) == NULL ) {
      Rprintf( "Error: Opening shapefile in C function getPartAreas.\n" );
      deallocateRecords( shape.records );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT(1);
      return data;
    }

    /* parse main file header */
    if ( parseHeader( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading main file header in C function getPartAreas.\n" );
        deallocateRecords( shape.records );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        fclose ( fptr );
        return data;
    }

    /* if we get a Point or Polyline file then return an error */
    if ( shape.shapeType == POINTS || shape.shapeType == POLYLINE || 
            shape.shapeType == POINTS_Z || shape.shapeType == POLYLINE_Z ||
              shape.shapeType == POINTS_M || shape.shapeType == POLYLINE_M ) {
        Rprintf( "Error: Invalid shape type found in file %s.\n", shpFileName );
        Rprintf( "Error: Shape type must be polygons.\n" );
        Rprintf( "Error: Occurred in C function getPartAreas.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data; 

    /* read Polygon shape */
    } else if ( shape.shapeType == POLYGON ) {

      if ( parsePolygon( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading Polygon data from file %s \nin C function getPartAreas.\n", shpFileName  );
        deallocateRecords( shape.records );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        fclose ( fptr );
        return data; 
      }
      
    /* read PolygonZ shape */
    } else if ( shape.shapeType == POLYGON_Z ) {

      if ( parsePolygonZ( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading PolygonZ data from file %s \nin C function getPartAreas.\n", shpFileName  );
        deallocateRecords( shape.records );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        fclose ( fptr );
        return data; 
      }

    /* read PolygonM shape */
    } else if ( shape.shapeType == POLYGON_M ) {

      if ( parsePolygonM( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading PolygonM data from file %s \nin C function getPartAreas.\n", shpFileName  );
        deallocateRecords( shape.records );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        fclose ( fptr );
        return data; 
      }

    /* got a shape number that we can't parse */
    } else {
      Rprintf( "Error: Unrecognized shape type in C function getPartArea.\n" );
      deallocateRecords( shape.records );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT(1);
      fclose ( fptr );
      return data; 
    }

    fclose( fptr );

    /* get the name of the next .shp file */
    ptrShp = 0;
    while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
      if ( strlen(fileShp->d_name) > 4 ) {
        ptrShp = fileMatch( fileShp->d_name, ".shp" );
        if ( ptrShp == 1 ) {
          if ( (shpFileName = (char * restrict)malloc(strlen(fileShp->d_name)
                +  1)) == NULL ) {
            Rprintf( "Error: Allocating memory in C function getPartAreas.\n" );
            closedir( dirp );
            deallocateRecords( shape.records );
            fclose ( fptr );
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

  /* close the current directory */
  closedir( dirp );

  /* create the returning R object */
  PROTECT( data = allocVector( REALSXP, shape.numParts ) );

  /* go though each record and calcualte part areas */
  temp = shape.records;
  idx = 0;
  while ( temp ) {

    if (shape.shapeType == POLYGON ) {

      /* calculate the areas of the different parts */
      for ( i = 0; i < temp->poly->numParts; ++i ) {
        int firstPoint = temp->poly->parts[i];
        int secondPoint;

        if ( temp->poly->numParts == 1 || i == temp->poly->numParts-1 ) {
          secondPoint = temp->poly->numPoints - 1;
        } else {
          secondPoint = temp->poly->parts[i+1] - 1;
        }
        REAL(data)[idx]=calcArea(temp->poly->points, firstPoint, secondPoint); 
        ++idx;
      }

    } else if (shape.shapeType == POLYGON_Z) {
      /* calculate the areas of the different parts */
      for ( i = 0; i < temp->polyZ->numParts; ++i ) {
        int firstPoint = temp->polyZ->parts[i];
        int secondPoint;

        if ( temp->polyZ->numParts == 1 || i == temp->polyZ->numParts-1 ) {
          secondPoint = temp->polyZ->numPoints - 1;
        } else {
          secondPoint = temp->polyZ->parts[i+1] - 1;
        }
        REAL(data)[idx]=calcArea(temp->polyZ->points, firstPoint, secondPoint); 
        ++idx;
      }

    } else if (shape.shapeType == POLYGON_M) {
      /* calculate the areas of the different parts */
      for ( i = 0; i < temp->polyM->numParts; ++i ) {
        int firstPoint = temp->polyM->parts[i];
        int secondPoint;

        if ( temp->polyM->numParts == 1 || i == temp->polyM->numParts-1 ) {
          secondPoint = temp->polyM->numPoints - 1;
        } else {
          secondPoint = temp->polyM->parts[i+1] - 1;
        }
        REAL(data)[idx]=calcArea(temp->polyM->points, firstPoint, secondPoint); 
        ++idx;
      }
    }

    temp = temp->next;
  }

  /* clean up */
  deallocateRecords( shape.records );
  UNPROTECT(1);

  return data;
}


/**********************************************************
** Function:   readShapeFile
**
** Purpose:    This function is used to read in and parse all the .shp
**             files and their corresponding .dbf files in the current
**             working directory unless a fileNamePrefix was sent(i.e. it's
**             not set to NULL).  If a name was sent then only that file
**             and it's corresponding .dbf file are read in. The data 
**             found is written to an R object in the same format that 
**             maptools writes the .shp R objects. 
** Algorithm:  This function grabs one .shp at a time, parses it and 
**             stores the data in a shape struct.  Then it looks for a 
**             .dbf file whose name's prefix matches the .shp file.  If 
**             none is found an error message is output.  This process
**             then continues for all the .shp files found in the current
**             working directory.  After all the data has been gathered it
**             is converted to an R object and returned to the calling fcn.
** Notes:      This function also checks to make sure that each shapefile
**             is of the same shape type and that each .dbf file has the
**             same number of fields and the same field names.  Error 
**             messages are ouput if these requirements aren't met.
** Arguments:  fileNamePrefix,  name of shapefile not including the 
**                              .shp extension  If this is sent as NULL
**                              then all the shapefiles in the current
**                              working directory are read in.
** Return:     data,  an R object containing all the shape data 
**                    If an error occurs this object gets returned empty.
***********************************************************/
SEXP readShapeFile( SEXP fileNamePrefix ) {

  int i;             /* loop counter */
  FILE * fptr;       /* file pointer to shapefile */
  Shape shape;       /* struct to store all info and data found in shapefile */
  SEXP data = NULL;  /* R object to store data in for returning to R */
  char * fileName = NULL; /* full name of dbf file */
  int done = FALSE;  /* flag signalling all .shp files have been read */
  DIR * dirp = NULL;        /* used to open the current directory */
  struct dirent * fileShp;  /* used for reading file names */
  int ptrShp = 0;           /* ptr to .shp files in the current directory */
  int shapeType = -1;  /* shape type of the first .shp file we find */
  int strLen;        /* string length */
  Dbf * headDbf = NULL;  /* head ptr to list of dbf structs */
  Dbf * tempDbf = NULL;  /* used for traversing list of dbf structs */
  Dbf * dbf;             /* temp dbf strcut */
  char * restrict shpFileName = NULL;  /* stores full shapefile name */
  int singleFile = FALSE;  /* flag signalling when we are only looking for a */
                           /* single specified shapefile */

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;
  shape.numParts = 0;

  /* see if a specific file was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full .shp file name */
    if ((shpFileName = (char * restrict)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
                                              + strlen(".shp") + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in C function readShapeFile.\n" );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return data;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix,0)));
    strcat( shpFileName, ".shp" );
    singleFile = TRUE;

  } else {

    /* open the current directory */
    if((dirp = opendir(".")) == NULL) {
      Rprintf( "Error: Opening the current directory in C function readShapeFile.\n" );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data;
    }

    /* get the name of the first .shp file */
    while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    	 if ( strlen(fileShp->d_name) > 4 ) {
    	   ptrShp = fileMatch( fileShp->d_name, ".shp" );
    	   if ( ptrShp == 1 ) {
    	     if ( (shpFileName = (char * restrict)malloc(strlen(fileShp->d_name)
                +  1)) == NULL ) {
            Rprintf( "Error: Allocating memory in C function readShapeFile.\n" );
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
      Rprintf( "Error: Occured in C function readShapeFile.\n");
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
      Rprintf( "Error: Opening shapefile in C function readShapeFile.\n" );
      deallocateRecords( shape.records );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT(1);
      return data;
    }

    /* parse main file header */
    if ( parseHeader( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading main file header in C function readShapeFile.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data;
    }

    /* initialize the shape type if necessary and make sure that if there */
    /* are multiple .shp files that they are of the same shape type */
    if ( shapeType == -1 ) {
      shapeType = shape.shapeType;
    } else if ( shapeType != shape.shapeType ) {
      Rprintf( "Error: Multiple shapefiles have different shape types.\n" );
      Rprintf( "Error: Occured in C function readShapeFile.\n" );
      deallocateRecords( shape.records );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT(1);
      return data;
    }

    /* read Point shape */
    if ( shape.shapeType == POINTS ) {
      if ( parsePoints( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading Point data from shapefile in C function readShapeFile.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data;
      }

    /* read PointsZ shape */
    } else if ( shape.shapeType == POINTS_Z ) {
      if ( parsePointsZ( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading PointZ data from shapefile in C function readShapeFile.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data;
      } 

    /* read PointsM shape */
    } else if ( shape.shapeType == POINTS_M ) {
      if ( parsePointsM( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading PointM data from shapefile in C function readShapeFile.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data;
      } 

    /* read Polyline or Polygon shape */
    } else if ( shape.shapeType == POLYLINE || shape.shapeType == POLYGON ) {
      if ( parsePolygon( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading Polygon or Polyline data from shapefile in C function readShapeFile.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data; 
      }

    /* read PolylineZ or PolygonZ shape */
    } else if ( shape.shapeType == POLYLINE_Z || shape.shapeType == POLYGON_Z) {
      if ( parsePolygonZ( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading PolygonZ or PolylineZ data from shapefile in C function readShapeFile.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data; 
      }

    /* read PolylineM or PolygonM shape */
    } else if ( shape.shapeType == POLYLINE_M || shape.shapeType == POLYGON_M) {
      if ( parsePolygonM( fptr, &shape ) == - 1 ) {
        Rprintf( "Error: Reading PolygonM or PolylineM data from shapefile in C function readShapeFile.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data; 
      }

    /* got a shape number that we can't parse */
    } else {
      Rprintf( "Error: Unrecognized shape type in C function readShapeFile.\n" );
      deallocateRecords( shape.records );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT(1);
      return data; 
    }

    /* close the shapefile */
    fclose( fptr );

    /* build the corresponding .dbf file name */
    strLen = strlen( shpFileName );
    if ( (fileName = (char *) malloc( sizeof( char ) * (strLen+1) ))==NULL) {
      Rprintf( "Error: Allocating memory in C function readShapeFile.\n" );
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
      Rprintf( "Error: Reading dbf file header in C function readShapeFile.\n" );
      deallocateRecords( shape.records );
      deallocateDbf( dbf );
      free( fileName );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return data;
    }

    if ( parseFields( fptr, dbf ) == -1 ) {

      /* an error has occured */
      Rprintf( "Error: Reading dbf fields in C function readShapeFile.\n" );
      deallocateRecords( shape.records );
      deallocateDbf( dbf );
      free( fileName );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return data;
    }

    /* make sure this .dbf file's columns match the previous one if there */
    /* was a previous one */
    if ( headDbf != NULL ) {

      /* make sure there is the same number of columns */
      if ( headDbf->numFields != dbf->numFields ) {
        Rprintf("Error: Multiple .dbf files have varying number of fields.\n" );
        Rprintf("Error: Occured in C function readShapeFile.\n" );
        deallocateRecords( shape.records );
        deallocateDbf( dbf );
        free( fileName );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 1 ) );
        UNPROTECT(1);
        return data;
      }

      /* make sure column names match */
      for ( i = 0; i < headDbf->numFields; ++i ) {
        if ( strcmp( headDbf->fields[i].name, dbf->fields[i].name ) != 0 ) {
          Rprintf("Error: Multiple .dbf files have varying field names.\n" );
          Rprintf("Error: Occured in C function readShapeFile.\n" );
          deallocateRecords( shape.records );
          deallocateDbf( dbf );
          free( fileName );
          fclose( fptr );
          PROTECT( data = allocVector( VECSXP, 1 ) );
          UNPROTECT(1);
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
    	       if ( (shpFileName = (char * restrict)malloc(strlen(fileShp->d_name)
                  +  1)) == NULL ) {
              Rprintf( "Error: Allocating memory in C function readShapeFile.\n" );
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
  PROTECT( data = convertToR( &shape, headDbf, fileNamePrefix ) );

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
** Function:   writeShapeFilePoint
**
** Purpose:    To write the sent vectors of point coordinates to
**             a new shapefile.  This includes creating a .shp, .shx,
**             .dbf, and .prj file all with the same name prefix.  The
**             .prj file is just a copy of the .prj file sent in the
**             prjFileNameVec.
** Notes:      You an send a NULL value for prjFileNameVec if you don't
**             want to make a copy of a .prj file.
** Arguments:  xVec,  vector of the x-coordinates for the points
**             yVec,  vector of the y-coordinates for the points
**             prjFilaNameVec, name prefix of the .prj file to make a copy of
**                             (no .prj extension)
**             dbfFieldNames,  name of the dbf fields
**             dbfFields,  field values for the dbf file
**             filePrefix,  prefix of the name of the files to be created
** Return:     NULL
***********************************************************/
SEXP writeShapeFilePoint( SEXP xVec, SEXP yVec, SEXP prjFileNameVec, 
                        SEXP dbfFieldNames , SEXP dbfFields, SEXP filePrefix ) {

  int i;                            /* loop counter */
  FILE * fptrShp;                   /* pointer to new shapefile */
  FILE * fptrShx;                   /* pointer to new index file */
  FILE * prjOld;                    /* ptr to original .prj file */
  FILE * prjNew;                    /* ptr to new .prj file */
  unsigned int vecSize = length( xVec );  /* number of points */
  unsigned char buffer[4];          /* temp storage used to write to file */
  unsigned int fileSize;            /* size of the new shapefile */
  unsigned char * ptr;              /* used to write data to file */
  int tempInt;                      /* temp integer storage */
  double tempDbl;                   /* temp double storage */
  double xmin, ymin, xmax, ymax;    /* bounding box coordiantes */
  char * fileName;                  /* full file name */
  char * prjFileName;               /* name of the .prj file */
  char * prefix;                    /* name prefix to use for each file */
  unsigned int offset;              /* byte offset counter */
  unsigned char byte;               /* bytes used for copying file */

  /* file extensions */
  char * shp = ".shp";
  char * shx = ".shx";
  char * prj = ".prj";

  /* copy the file name's prefix */
  if ( (prefix = (char *) malloc( strlen(CHAR(STRING_ELT(filePrefix,0)))+1))
                                                                   == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePoint.\n" );
    return R_NilValue;
  }
  strcpy( prefix, CHAR(STRING_ELT(filePrefix,0)) );

  /* create the .shp file name */
  if ( (fileName = (char *) malloc( strlen(prefix) + strlen(shp) + 1))
                                                             == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePoint.\n" );
    return R_NilValue;
  }
  strcpy( fileName, prefix );
  strcat( fileName, shp );

  /* create the shapefile */
  if ( (fptrShp = fopen( fileName, "wb" )) == NULL ) {
    Rprintf( "Error: Creating shapefile in C function writeShapeFilePoint.\n" );
    return R_NilValue;
  }
  free( fileName );


  /* create the .shx file name */
  if ( (fileName = (char *) malloc( strlen(prefix) + strlen(shx) + 1))
                                                             == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePoint.\n" );
    return R_NilValue;
  }
  strcpy( fileName, prefix );
  strcat( fileName, shx );

  /* create the index file */
  if ( (fptrShx = fopen( fileName, "wb" )) == NULL ) {
    Rprintf( "Error: Creating shapefile in C function writeShapeFilePoint.\n" );
    return R_NilValue;
  }
  free( fileName );

  /* write the shapefile header */
  /* big endian byte order */
  tempInt = 9994;
  ptr = (unsigned char *) &tempInt;
  buffer[0] = ptr[3];
  buffer[1] = ptr[2];
  buffer[2] = ptr[1];
  buffer[3] = ptr[0];
  fwrite( buffer, sizeof(char), 4, fptrShp );
  fwrite( buffer, sizeof(char), 4, fptrShx );

  /* write 0's */
  /* big endian byte order */
  tempInt = 0;
  for ( i = 0; i < 5; ++i ) {
    fwrite( &tempInt, sizeof(char), 4, fptrShp );
    fwrite( &tempInt, sizeof(char), 4, fptrShx );
  }

  /* write the file length for the shapefile */
  /* big endian byte order */
  fileSize = (100 + (vecSize * 28)) / 2;
  ptr = (unsigned char *) &fileSize;
  buffer[0] = ptr[3];
  buffer[1] = ptr[2];
  buffer[2] = ptr[1];
  buffer[3] = ptr[0];
  fwrite( buffer, sizeof(char), 4, fptrShp );

  /* write the file length for the index file */
  /* big endian byte order */
  fileSize = (100 + (vecSize * 8)) / 2;
  ptr = (unsigned char *) &fileSize;
  buffer[0] = ptr[3];
  buffer[1] = ptr[2];
  buffer[2] = ptr[1];
  buffer[3] = ptr[0];
  fwrite( buffer, sizeof(char), 4, fptrShx );

  /* write version */
  /* little endian byte order */
  tempInt = 1000;
  fwrite( &tempInt, sizeof(char), 4, fptrShp );
  fwrite( &tempInt, sizeof(char), 4, fptrShx );

  /* write the shapefile type, which is 1 for a Point shapefile */
  /* little endian byte order */
  tempInt = 1;
  fwrite( &tempInt, sizeof(char), 4, fptrShp ); 
  fwrite( &tempInt, sizeof(char), 4, fptrShx ); 

  /* determine the mins and maxs */
  xmin = xmax = REAL( xVec )[0];
  ymin = ymax = REAL( yVec )[0];
  for ( i = 1; i < vecSize; ++i ) {
    if ( xmin > REAL( xVec )[i] ) {
      xmin = REAL( xVec )[i];
    } 
    if ( xmax < REAL( xVec )[i] ) {
      xmax = REAL( xVec )[i];
    } 
    if ( ymin > REAL( yVec )[i] ) {
      ymin = REAL( yVec )[i];
    } 
    if ( ymax < REAL( yVec )[i] ) {
      ymax = REAL( yVec )[i];
    } 
  }
 
  /* write mins and maxs to file */
  /* little endian byte order */
  fwrite( &xmin, sizeof(double), 1, fptrShp );
  fwrite( &ymin, sizeof(double), 1, fptrShp );
  fwrite( &xmax, sizeof(double), 1, fptrShp );
  fwrite( &ymax, sizeof(double), 1, fptrShp );
  fwrite( &xmin, sizeof(double), 1, fptrShx );
  fwrite( &ymin, sizeof(double), 1, fptrShx );
  fwrite( &xmax, sizeof(double), 1, fptrShx );
  fwrite( &ymax, sizeof(double), 1, fptrShx );

  /* write Zmin, Zmax, Mmin, and Mmax as 0's */
  /* little endian byte order */
  tempInt = 0;
  for ( i = 0; i < 4; ++i ) {
    fwrite( &tempInt, sizeof(char), 8, fptrShp );
    fwrite( &tempInt, sizeof(char), 8, fptrShx );
  }

  /* initialize offset for index file to just past the main header */
  offset = 50;

  /* write the records and point values */
  for ( i = 1; i <= vecSize; ++i ) {

    /* write the record number */
    /* big endian byte order */
    ptr = (unsigned char *) &i;
    buffer[0] = ptr[3];
    buffer[1] = ptr[2];
    buffer[2] = ptr[1];
    buffer[3] = ptr[0];
    fwrite( buffer, sizeof(char), 4, fptrShp );

    /* write the record offset to the index file */
    /* big endian byte order */
    ptr = (unsigned char *) &offset;
    buffer[0] = ptr[3];
    buffer[1] = ptr[2];
    buffer[2] = ptr[1];
    buffer[3] = ptr[0];
    fwrite( buffer, sizeof(char), 4, fptrShx );
    offset += 28/2;

    /* write the record content length, which is 10 for a Point shapefile */
    /* big endian byte order */
    tempInt = 10;
    ptr = (unsigned char *) &tempInt;
    buffer[0] = ptr[3];
    buffer[1] = ptr[2];
    buffer[2] = ptr[1];
    buffer[3] = ptr[0];
    fwrite( buffer, sizeof(char), 4, fptrShp );
    fwrite( buffer, sizeof(char), 4, fptrShx );

    /* write the shapefile type */
    /* little endian byte order */
    tempInt = 1;
    fwrite( &tempInt, sizeof(char), 4, fptrShp );

    /* write the x-coordinate */
    /* little endian byte order */
    tempDbl = REAL( xVec )[i-1];
    fwrite( &tempDbl, sizeof(double), 1, fptrShp );
    
    /* write the y-coordinate */
    /* little endian byte order */
    tempDbl = REAL( yVec )[i-1];
    if ( fwrite( &tempDbl, sizeof(double), 1, fptrShp ) == 0 ) {
      Rprintf( "Error: Writing to shapefile in C function writeShapeFilePoint.\n" );
      return R_NilValue;
    }
  }
  fclose( fptrShp );
  fclose( fptrShx );

  /* see if a .prj file name was sent */
  if ( prjFileNameVec != R_NilValue ) {

    /* create the original .prj file name */
    if((prjFileName=(char *)malloc(strlen(CHAR(STRING_ELT( prjFileNameVec,0 )))
                                                 + strlen(prj) + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in C function writeShapeFilePoint.\n" );
      return R_NilValue;
    }
    strcpy( prjFileName, CHAR(STRING_ELT( prjFileNameVec, 0 )) );
    strcat( prjFileName, prj );

    /* create the new .prj file name */
    if ((fileName = (char *) malloc( strlen(prefix) + strlen(prj) + 1))
                                                         == NULL ) {
      Rprintf( "Error: Allocating memory in C function writeShapeFilePoint.\n" );
      return R_NilValue;
    }
    strcpy( fileName, prefix );
    strcat( fileName, prj );

    /* open existing projection file */
    if ((prjOld = fopen( prjFileName, "rb" )) == NULL ){
      Rprintf( "Error: Opening .prj file in C function writeShapeFilePoint.\n" );
      return R_NilValue;
    }

    /* create the new projection file */
    if ( (prjNew = fopen( fileName, "wb" )) == NULL ) {
      Rprintf("Error: Creating .prj file in C function writeShapeFilePoint.\n" );
      fclose( prjOld );
      return R_NilValue;
    }

    /* make copy of old projection file */
    while ( fread( &byte, sizeof(char), 1, prjOld ) > 0 ) {
      fwrite( &byte, sizeof(char), 1, prjNew );
    }

    free( fileName );
    free( prjFileName );
    fclose( prjNew );
    fclose( prjOld );
  }

  /* create the dbf file */
  writeDbfFile( dbfFieldNames, dbfFields, filePrefix );

  free( prefix );

  return R_NilValue;
}


/**********************************************************
** Function:   readShapeFilePts
**
** Purpose:    This function is used to read in and parse all the .shp
**             files and their corresponding .dbf files in the current
**             working directory unless a fileNamePrefix was sent(i.e. it's
**             not set to NULL).  If a name was sent then only that file
**             and it's corresponding .dbf file are read in. The data 
**             found is written to an R object in the same format that 
**             maptools writes the .shp R objects. 
** Algorithm:  This function grabs one .shp at a time, parses it and 
**             stores the data in a shape struct.  Then it looks for a 
**             .dbf file whose name's prefix matches the .shp file.  If 
**             none is found an error message is output.  This process
**             then continues for all the .shp files found in the current
**             working directory.  After all the data has been gathered it
**             is converted to an R object and returned to the calling fcn.
** Notes:      This function also checks to make sure that each shapefile
**             is of the same shape type and that each .dbf file has the
**             same number of fields and the same field names.  Error 
**             messages are ouput if these requirements aren't met.
** Arguments:  fileNamePrefix,  name of shapefile not including the 
**                              .shp extension  If this is sent as NULL
                                then all the shapefiles in the current
**                              working directory are read in.
** Return:     data,  an R object containing all the shape data 
**                    If an error occurs this object gets returned empty.
***********************************************************/
SEXP readShapeFilePts( SEXP fileNamePrefix ) {

  int i;             /* loop counter */
  FILE * fptr = NULL;       /* file pointer to shapefile */
  Shape shape;       /* struct to store all info and data found in shapefile */
  SEXP data = NULL;  /* R object to store data in for returning to R */
  char * fileName = NULL; /* full name of dbf file */
  int done = FALSE;  /* flag signalling all .shp files have been read */
  DIR * dirp = NULL;        /* used to open the current directory */
  struct dirent * fileShp;  /* used for reading file names */
  int ptrShp = 0;           /* ptr to .shp files in the current directory */
  int shapeType = -1;  /* shape type of the first .shp file we find */
  char * restrict shpFileName = NULL;  /* stores full shapefile name */
  int singleFile = FALSE;  /* flag signalling when we are only looking for a */
                           /* single specified shapefile */
  Record * tempRec;
  SEXP coordsX;
  SEXP coordsY;
  int foundPtsShp = FALSE;
  SEXP class, attribs;
  char str[20];

  /* initialize the shape struct */
  shape.records = NULL;
  shape.numRecords = 0;
  shape.numParts = 0;

  /* see if a specific file was sent */
  if ( fileNamePrefix != R_NilValue ) {

    /* create the full shp file name */
    if ((shpFileName = (char * restrict)malloc(strlen(CHAR(STRING_ELT(fileNamePrefix,0)))
                                              + strlen(".shp") + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in C function readShapeFilePts.\n" );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT(1);
      return data;
    }
    strcpy( shpFileName, CHAR(STRING_ELT(fileNamePrefix,0)));
    strcat( shpFileName, ".shp" );
    singleFile = TRUE;

  } else {

    /* open the current directory */
    if((dirp = opendir(".")) == NULL) {
      Rprintf( "Error: Opening the current directory in C function readShapeFilePts.\n" );
      PROTECT( data = allocVector( VECSXP, 1 ) );
      UNPROTECT( 1 );
      return data;
    }

    /* get the name of the first .shp file */
    while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    	 if ( strlen(fileShp->d_name) > 4 ) {
    	   ptrShp = fileMatch( fileShp->d_name, ".shp" );
    	   if ( ptrShp == 1 ) {
    	     if ( (shpFileName = (char * restrict)malloc(strlen(fileShp->d_name)
                +  1)) == NULL ) {
            Rprintf( "Error: Allocating memory in C function readShapeFilePts.\n" );
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
      Rprintf( "Error: Occured in C function readShapeFilePts.\n");
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
      Rprintf( "Error: Occured in C function readShapeFilePts.\n");
      deallocateRecords( shape.records );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT( 1 );
      return data;
    }

    /* parse main file header */
    if ( parseHeader( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading main file header in C function readShapeFilePts.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data;
    }

    /* initialize the shape type if necessary and make sure that if there */
    /* are multiple .shp files that they are of the same shape type */
    if ( shapeType == -1 ) {
      shapeType = shape.shapeType;
    } else if ( shapeType != shape.shapeType ) {
      Rprintf( "Error: Multiple shapefiles have different shape types in C function readShapeFilePts.\n" );
      deallocateRecords( shape.records );
      fclose( fptr );
      PROTECT( data = allocVector( VECSXP, 2 ) );
      UNPROTECT(1);
      return data;
    }

    /* read Point shape */
    if ( shape.shapeType == POINTS ) {
      foundPtsShp = TRUE;
      if ( parsePoints( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading point data from shapefile in C function readShapeFilePts.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data;
      }

    /* read PointZ shape */
    } else if ( shape.shapeType == POINTS_Z ) {
      foundPtsShp = TRUE;
      if ( parsePointsZ( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading PointZ data from shapefile in C function readShapeFilePts.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data;
      }

    /* read PointM shape */
    } else if ( shape.shapeType == POINTS_M ) {
      foundPtsShp = TRUE;
      if ( parsePointsM( fptr, &shape ) == -1 ) {
        Rprintf( "Error: Reading PointM data from shapefile in C function readShapeFilePts.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data;
      }

    /* got a shapefile that is not a points shape */
    } else {

      /* see if we have already found a point shapefile */
      if ( foundPtsShp == TRUE || singleFile == TRUE ) {
        Rprintf( "Error: Invalid shape type in C function readShapeFilePts.\n" );
        Rprintf( "Error: Function only works with Point shape types.\n" );
        deallocateRecords( shape.records );
        fclose( fptr );
        PROTECT( data = allocVector( VECSXP, 2 ) );
        UNPROTECT(1);
        return data; 
      }
    }

    /* close the shapefile */
    fclose( fptr );

    /* get the next .shp file */
    if ( singleFile == TRUE ) {
      done = TRUE;
    } else {
      ptrShp = 0;
      while ( ptrShp == 0 && (fileShp = readdir( dirp )) != NULL ) {
    	   if ( strlen(fileShp->d_name) > 4 ) {
    	     ptrShp = fileMatch( fileShp->d_name, ".shp" );
    	     if ( ptrShp == 1 ) {
    	       if ( (shpFileName = (char * restrict)malloc(strlen(fileShp->d_name)
                  +  1)) == NULL ) {
              Rprintf( "Error: Allocating memory in C function readShapeFilePts.\n" );
              closedir( dirp );
              deallocateRecords( shape.records );
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
  }

  /* close the current directory */
  if ( singleFile == FALSE )  {
    closedir( dirp );
  }

  /* if no point shape types were found return an error */
  if ( foundPtsShp == FALSE ) {
    Rprintf( "Error: No point shapefiles were found in the current working directory in C \nfunction readShapeFilePts.\n" );
    deallocateRecords( shape.records );
    if ( fptr != NULL ) {
      fclose( fptr );
    }
    PROTECT( data = allocVector( VECSXP, 2 ) );
    UNPROTECT(1);
    return data;
  }

  /* allocate R vectors */
  PROTECT( data = allocVector( VECSXP, 2 ) );
  PROTECT( coordsX = allocVector( REALSXP, shape.numRecords ) );
  PROTECT( coordsY = allocVector( REALSXP, shape.numRecords ) );

  /* go through all the points an write them to a coord vectors */
  tempRec = shape.records;
  i = 0;
  while ( tempRec ) {

    /* store the point coordinates */ 
    if ( shape.shapeType == POINTS ) {
      REAL( coordsX )[i] = tempRec->point->X;
      REAL( coordsY )[i] = tempRec->point->Y;
    } else {
      REAL( coordsX )[i] = tempRec->pointZ->X;
      REAL( coordsY )[i] = tempRec->pointZ->Y;
    }
    ++i;   
    tempRec = tempRec->next;
  }

  /* add the coordinate vectors to the data frame */ 
  SET_VECTOR_ELT( data, 0, coordsX );
  SET_VECTOR_ELT( data, 1, coordsY );

  /* add the row names */
  PROTECT( attribs = allocVector( STRSXP, shape.numRecords ));
  for ( i = 0; i < shape.numRecords; ++i ) {
    sprintf( str, "%d", i+1 );
    SET_STRING_ELT( attribs, i, mkChar( str ) );
  }
  setAttrib( data, install("row.names"), attribs );
  UNPROTECT(1);

  /* add field names(column names) to the R object */
  PROTECT( attribs = allocVector( STRSXP, 2 ) );
  SET_STRING_ELT( attribs, 0, mkChar( "x" ) );
  SET_STRING_ELT( attribs, 1, mkChar( "y" ) );
  setAttrib( data, R_NamesSymbol, attribs );
  UNPROTECT( 1 );

  /* add the class type */
  PROTECT( class = allocVector( STRSXP, 1 ) );
  SET_STRING_ELT( class, 0, mkChar( "data.frame" ) );
  classgets( data, class );
  UNPROTECT( 1 );

  /* clean up */
  deallocateRecords( shape.records );
  if ( singleFile == TRUE ) {
    free( shpFileName );
  }
  UNPROTECT(3);

  return data;
}


/**********************************************************
** Function:   writeShapeFilePolygon
**
** Purpose:    To write the sent arguments to a new Polyline or Polygon 
**             shapefile.  This includes creating .shp, .shx, .dbf, and .prj 
**             files all with the same name prefix.  The .prj file is just a 
**             copy of the .prj file sent in the prjFileNameVec.
** Notes:      Send a NULL value for prjFileNameVec if you don't want to make a 
**             copy of a .prj file.
** Arguments:  shapeTypeVal, the type of shapefile, where 3 is Polyline and 5 is
**                Polygon
**             fileLengthVal, the length of the shapefile
**             contentLenVec, vector of the content length for each record in
**                the shapefile
**             nPartsVec, vector of the number of parts for each record in the
**                shapefile
**             nPointsVec, vector of the number of points for each record in the
**                shapefile
**             partsVec, vector of the index value for the first point in each
**                part of a record
**             xVec,  vector of the x-coordinates for the points
**             yVec,  vector of the y-coordinates for the points
**             prjFileNameVec, name prefix of the .prj file (no .prj extension)
**             dbfFieldNames,  names of the dbf fields
**             dbfFields,  field values for the dbf file
**             filePrefix,  prefix of the name of the files to be created
** Return:     NULL
***********************************************************/
SEXP writeShapeFilePolygon( SEXP shapeTypeVal, SEXP fileLengthVal,
  SEXP contentLenVec, SEXP nPartsVec, SEXP nPointsVec, SEXP partsVec, SEXP xVec,
  SEXP yVec, SEXP prjFileNameVec, SEXP dbfFieldNames, SEXP dbfFields,
  SEXP filePrefix ) {

  /* C variables that store the sent R object's values */
  unsigned int * contentLen = NULL;
  unsigned int * nParts = NULL;
  unsigned int * nPoints = NULL;
  unsigned int * parts = NULL;
  unsigned int partsSize = length( partsVec );
  unsigned int fileLength;

  int i, j;                         /* loop counter */
  FILE * fptrShp;                   /* pointer to new shapefile */
  FILE * fptrShx;                   /* pointer to new index file */
  FILE * prjOld;                    /* ptr to original .prj file */
  FILE * prjNew;                    /* ptr to new .prj file */
  unsigned int vecSize = length( xVec );  /* number of points */
  unsigned char buffer[4];          /* temp storage used to write to file */
  unsigned int fileSize;            /* size of the new shapefile index file*/
  unsigned char * ptr;              /* used to write data to file */
  int * intPtr;                     /* temp pointer to convert from R to C*/
  int tempInt;                      /* temp integer storage */
  double tempDbl;                   /* temp double storage */
  double xmin, ymin, xmax, ymax;    /* bounding box values */
  char * fileName;                  /* full file name */
  char * prjFileName;               /* name of the .prj file */
  char * prefix;                    /* name prefix to use for each file */
  unsigned int offset;              /* byte offset counter */
  unsigned char byte;               /* bytes used for copying file */
  int nRec = length( nPointsVec );  /* number of records in the shapefile */
  int shapeType;                    /* shapefile type */
  int iStartPart, iStopPart;        /* parts vector index values */
  int iStartPoint, iStopPoint;      /* coordinate vector index values */

  /* file extensions */
  char * shp = ".shp";
  char * shx = ".shx";
  char * prj = ".prj";

  /* copy the prefix of the output shapefile */
  if ( (prefix = (char *) malloc( strlen(CHAR(STRING_ELT(filePrefix,0)))+1))
                                                                   == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  strcpy( prefix, CHAR(STRING_ELT(filePrefix,0)) );

  /* create the .shp (main) file name */
  if ( (fileName = (char *) malloc( strlen(prefix) + strlen(shp) + 1))
                                                             == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  strcpy( fileName, prefix );
  strcat( fileName, shp );

  /* create the main file */
  if ( (fptrShp = fopen( fileName, "wb" )) == NULL ) {
    Rprintf( "Error: Creating shapefile in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  free( fileName );


  /* create the .shx (index) file name */
  if ( (fileName = (char *) malloc( strlen(prefix) + strlen(shx) + 1))
                                                             == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  strcpy( fileName, prefix );
  strcat( fileName, shx );

  /* create the index file */
  if ( (fptrShx = fopen( fileName, "wb" )) == NULL ) {
    Rprintf( "Error: Creating shapefile in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  free( fileName );

  /* write the shapefile headers */

  /* write the file code */
  /* big endian byte order */
  tempInt = 9994;
  ptr = (unsigned char *) &tempInt;
  buffer[0] = ptr[3];
  buffer[1] = ptr[2];
  buffer[2] = ptr[1];
  buffer[3] = ptr[0];
  fwrite( buffer, sizeof(char), 4, fptrShp );
  fwrite( buffer, sizeof(char), 4, fptrShx );

  /* write 0's */
  /* big endian byte order */
  tempInt = 0;
  for ( i = 0; i < 5; ++i ) {
    fwrite( &tempInt, sizeof(char), 4, fptrShp );
    fwrite( &tempInt, sizeof(char), 4, fptrShx );
  }

  /* write the file length for the main file */
  /* big endian byte order */
  PROTECT( fileLengthVal = AS_INTEGER( fileLengthVal ) );
  intPtr = INTEGER_POINTER( fileLengthVal );
  fileLength = *intPtr;
  UNPROTECT(1);
  ptr = (unsigned char *) &fileLength;
  buffer[0] = ptr[3];
  buffer[1] = ptr[2];
  buffer[2] = ptr[1];
  buffer[3] = ptr[0];
  fwrite( buffer, sizeof(char), 4, fptrShp );

  /* write the file length for the index file */
  /* big endian byte order */
  fileSize = 50 + (nRec * 4);
  ptr = (unsigned char *) &fileSize;
  buffer[0] = ptr[3];
  buffer[1] = ptr[2];
  buffer[2] = ptr[1];
  buffer[3] = ptr[0];
  fwrite( buffer, sizeof(char), 4, fptrShx );

  /* write the version */
  /* little endian byte order */
  tempInt = 1000;
  fwrite( &tempInt, sizeof(char), 4, fptrShp );
  fwrite( &tempInt, sizeof(char), 4, fptrShx );

  /* write the shapefile type, which is 3 for a Polyline shapefile */
  /* and 5 for a Polygon shapefile */
  /* little endian byte order */
  PROTECT( shapeTypeVal = AS_INTEGER( shapeTypeVal ) );
  intPtr = INTEGER_POINTER( shapeTypeVal );
  shapeType = *intPtr;
  UNPROTECT(1);
  fwrite( &shapeType, sizeof(char), 4, fptrShp ); 
  fwrite( &shapeType, sizeof(char), 4, fptrShx ); 

  /* determine minimum and maximum coordinate values */
  xmin = xmax = REAL( xVec )[0];
  ymin = ymax = REAL( yVec )[0];
  for ( i = 1; i < vecSize; ++i ) {
    if ( xmin > REAL( xVec )[i] ) {
      xmin = REAL( xVec )[i];
    } 
    if ( xmax < REAL( xVec )[i] ) {
      xmax = REAL( xVec )[i];
    } 
    if ( ymin > REAL( yVec )[i] ) {
      ymin = REAL( yVec )[i];
    } 
    if ( ymax < REAL( yVec )[i] ) {
      ymax = REAL( yVec )[i];
    } 
  }
 
  /* write the bounding box */
  /* little endian byte order */
  fwrite( &xmin, sizeof(double), 1, fptrShp );
  fwrite( &ymin, sizeof(double), 1, fptrShp );
  fwrite( &xmax, sizeof(double), 1, fptrShp );
  fwrite( &ymax, sizeof(double), 1, fptrShp );
  fwrite( &xmin, sizeof(double), 1, fptrShx );
  fwrite( &ymin, sizeof(double), 1, fptrShx );
  fwrite( &xmax, sizeof(double), 1, fptrShx );
  fwrite( &ymax, sizeof(double), 1, fptrShx );

  /* write Zmin, Zmax, Mmin, and Mmax as 0's */
  /* little endian byte order */
  tempInt = 0;
  for ( i = 0; i < 4; ++i ) {
    fwrite( &tempInt, sizeof(char), 8, fptrShp );
    fwrite( &tempInt, sizeof(char), 8, fptrShx );
  }

  /* initialize offset for index file to just past the index header */
  offset = 50;

  /* copy the content length vector into a C array */
  if((contentLen = (unsigned int *) malloc( sizeof( unsigned int ) * nRec ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  for ( i = 0; i < nRec; ++i ) {
    contentLen[i] = INTEGER( contentLenVec )[i];
  }

  /* copy the number of parts vector into a C array */
  if((nParts = (unsigned int *) malloc( sizeof( unsigned int ) * nRec ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  for ( i = 0; i < nRec; ++i ) {
    nParts[i] = INTEGER( nPartsVec )[i];
  }

  /* copy the number of points vector into a C array */
  if((nPoints = (unsigned int *) malloc( sizeof( unsigned int ) * nRec ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  for ( i = 0; i < nRec; ++i ) {
    nPoints[i] = INTEGER( nPointsVec )[i];
  }

  /* copy the parts vector into a C array */
  if((parts = (unsigned int *) malloc( sizeof( unsigned int ) * partsSize ))
    == NULL ) {
    Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
    return R_NilValue;
  }
  for ( i = 0; i < partsSize; ++i ) {
    parts[i] = INTEGER( partsVec )[i];
  }

  /* write the records */
  iStartPart = 0;
  iStopPart = 0;
  iStartPoint = 0;
  iStopPoint = 0;
  for ( i = 1; i <= nRec; ++i ) {
    iStopPart += nParts[i-1];
    iStopPoint += nPoints[i-1];

    /* write the record number to the main file record header */
    /* big endian byte order */
    ptr = (unsigned char *) &i;
    buffer[0] = ptr[3];
    buffer[1] = ptr[2];
    buffer[2] = ptr[1];
    buffer[3] = ptr[0];
    fwrite( buffer, sizeof(char), 4, fptrShp );

    /* write the offset to the index file record */
    /* big endian byte order */
    ptr = (unsigned char *) &offset;
    buffer[0] = ptr[3];
    buffer[1] = ptr[2];
    buffer[2] = ptr[1];
    buffer[3] = ptr[0];
    fwrite( buffer, sizeof(char), 4, fptrShx );
    offset += contentLen[i-1] + 4;

    /* write the record content length to the main file record header and */
    /* the index file record*/
    /* big endian byte order */
    tempInt = contentLen[i-1];
    ptr = (unsigned char *) &tempInt;
    buffer[0] = ptr[3];
    buffer[1] = ptr[2];
    buffer[2] = ptr[1];
    buffer[3] = ptr[0];
    fwrite( buffer, sizeof(char), 4, fptrShp );
    fwrite( buffer, sizeof(char), 4, fptrShx );

    /* write the shapefile type to the main file record */
    /* little endian byte order */
    fwrite( &shapeType, sizeof(char), 4, fptrShp );

    /* determine minimum and maximum coordinate values for the record */
    xmin = xmax = REAL( xVec )[iStartPoint];
    ymin = ymax = REAL( yVec )[iStartPoint];
    for ( j = (iStartPoint+1); j < iStopPoint; ++j ) {
      if ( xmin > REAL( xVec )[j] ) {
        xmin = REAL( xVec )[j];
      } 
      if ( xmax < REAL( xVec )[j] ) {
        xmax = REAL( xVec )[j];
      } 
      if ( ymin > REAL( yVec )[j] ) {
        ymin = REAL( yVec )[j];
      } 
      if ( ymax < REAL( yVec )[j] ) {
        ymax = REAL( yVec )[j];
      } 
    }
 
    /* write the bounding box to the main file record */
    /* little endian byte order */
    fwrite( &xmin, sizeof(double), 1, fptrShp );
    fwrite( &ymin, sizeof(double), 1, fptrShp );
    fwrite( &xmax, sizeof(double), 1, fptrShp );
    fwrite( &ymax, sizeof(double), 1, fptrShp );
    fwrite( &xmin, sizeof(double), 1, fptrShx );
    fwrite( &ymin, sizeof(double), 1, fptrShx );
    fwrite( &xmax, sizeof(double), 1, fptrShx );
    fwrite( &ymax, sizeof(double), 1, fptrShx );

    /* write the number of parts to the main file record */
    /* little endian byte order */
    tempInt = nParts[i-1];
    fwrite( &tempInt, sizeof(char), 4, fptrShp );

    /* write the number of points to the main file record */
    /* little endian byte order */
    tempInt = nPoints[i-1];
    fwrite( &tempInt, sizeof(char), 4, fptrShp );

    /* write the parts to the main file record */
    /* little endian byte order */
    for ( j = iStartPart; j < iStopPart; ++j ) {
      tempInt = parts[j];
      fwrite( &tempInt, sizeof(char), 4, fptrShp );
    }

    /* write the x-coordinates and y-coordinates to the main file record */
    /* little endian byte order */
    for ( j = iStartPoint; j < iStopPoint; ++j ) {
      tempDbl = REAL( xVec )[j];
      fwrite( &tempDbl, sizeof(double), 1, fptrShp );
    
      tempDbl = REAL( yVec )[j];
      if ( fwrite( &tempDbl, sizeof(double), 1, fptrShp ) == 0 ) {
        Rprintf( "Error: Writing to shapefile in C function writeShapeFilePolygon.\n" );
        return R_NilValue;
      }
    }

    iStartPart += nParts[i-1];
    iStartPoint += nPoints[i-1];
  }
  fclose( fptrShp );
  fclose( fptrShx );

  /* see if a .prj (projection) file name was sent */
  if ( prjFileNameVec != R_NilValue ) {

    /* create the original projection file name */
    if((prjFileName=(char *)malloc(strlen(CHAR(STRING_ELT( prjFileNameVec,0 )))
                                                 + strlen(prj) + 1)) == NULL ) {
      Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
      return R_NilValue;
    }
    strcpy( prjFileName, CHAR(STRING_ELT( prjFileNameVec, 0 )) );
    strcat( prjFileName, prj );

    /* create the new projection file name */
    if ((fileName = (char *) malloc( strlen(prefix) + strlen(prj) + 1))
                                                         == NULL ) {
      Rprintf( "Error: Allocating memory in C function writeShapeFilePolygon.\n" );
      return R_NilValue;
    }
    strcpy( fileName, prefix );
    strcat( fileName, prj );

    /* open the existing projection file */
    if ((prjOld = fopen( prjFileName, "rb" )) == NULL ){
      Rprintf( "Error: Opening .prj file in C function writeShapeFilePolygon.\n" );
      return R_NilValue;
    }

    /* create the new projection file */
    if ( (prjNew = fopen( fileName, "wb" )) == NULL ) {
      Rprintf("Error: Creating .prj file in C function writeShapeFilePolygon.\n" );
      fclose( prjOld );
      return R_NilValue;
    }

    /* make a copy of the old projection file */
    while ( fread( &byte, sizeof(char), 1, prjOld ) > 0 ) {
      fwrite( &byte, sizeof(char), 1, prjNew );
    }

    free( fileName );
    free( prjFileName );
    fclose( prjNew );
    fclose( prjOld );
  }

  /* create the .dbf (dBASE) file */
  writeDbfFile( dbfFieldNames, dbfFields, filePrefix );

  free( prefix );

  return R_NilValue;
}
