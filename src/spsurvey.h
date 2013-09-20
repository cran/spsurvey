/****************************************************************************** 
**  File:        spsurvey.h   
**  Purpose:     This file contains definitions used for registering the C 
**               functions in the DLL of the spsurvey library.
**  Programmer:  Tom Kincaid
**  Created:     May 4, 2006
**  Revised:     February 11, 2010
******************************************************************************/

#ifndef R_SPSURVEY_H
#define R_SPSURVEY_H

#ifdef ENABLE_NLS
#include <libintl.h>
#define _(String) dgettext ("spsurvey", String)
#else
#define _(String) (String)
#endif

/* .C Methods */

void ranho(char ** adr, int * size);

/* .Call Methods */

SEXP readDbfFile(SEXP fileNamePrefix);
SEXP readShapeFile(SEXP fileNamePrefix);
SEXP readShapeFilePts(SEXP fileNamePrefix);
SEXP getRecordShapeSizes(SEXP fileNamePrefix);
SEXP writeDbfFile(SEXP fieldNames, SEXP fields, SEXP filePrefix);
void writeShapeFilePoint(SEXP xVec, SEXP yVec, SEXP prjFileNameVec,
   SEXP dbfFieldNames, SEXP dbfFields, SEXP filePrefix);
void writeShapeFilePolygon(SEXP shapeTypeVal, SEXP fileLengthVal,
  SEXP contentLenVec, SEXP nPartsVec, SEXP nPointsVec, SEXP partsVec, SEXP xVec,
  SEXP yVec, SEXP prjFileNameVec, SEXP dbfFieldNames, SEXP dbfFields,
  SEXP filePrefix);
SEXP pointInPolygonObj(SEXP ptXVec, SEXP ptYVec, SEXP polyXVec, SEXP polyYVec);
SEXP numLevels(SEXP fileNamePrefix, SEXP nsmpVec, SEXP shiftGridVec,
   SEXP startLevVec, SEXP maxLevVec, SEXP dsgnmdIDVec, SEXP dsgnmdVec);
SEXP constructAddr(SEXP xcVec, SEXP ycVec, SEXP dxVec, SEXP dyVec,
   SEXP nlevVec);
SEXP pickGridCells(SEXP samplesize, SEXP idxVec);
SEXP insideAreaGridCell(SEXP fileNamePrefix, SEXP dsgnmdIDVec, SEXP cellIDsVec,
     SEXP xcsVec, SEXP ycsVec, SEXP dxVal, SEXP dyVal);
SEXP insideLinearGridCell(SEXP fileNamePrefix, SEXP dsgnmdIDVec, SEXP cellIDsVec,
     SEXP xcsVec, SEXP ycsVec, SEXP dxVal, SEXP dyVal);
SEXP pointInPolygonFile(SEXP fileNamePrefix, SEXP xcsVec, SEXP ycsVec, 
   SEXP dsgnmdIDVec, SEXP dsgnmdVec);
SEXP pickAreaSamplePoints(SEXP fileNamePrefix, SEXP shpIDsVec, 
   SEXP recordIDsVec, SEXP xcVec, SEXP ycVec, SEXP dxVal, SEXP dyVal, 
   SEXP maxTryVal);
SEXP pickLinearSamplePoints(SEXP fileNamePrefix, SEXP shpIDsVec, 
   SEXP recordIDsVec, SEXP xcVec, SEXP ycVec, SEXP dxVal, SEXP dyVal);
SEXP linSample(SEXP fileNamePrefix, SEXP xcVec, SEXP ycVec, SEXP dxVec, 
   SEXP dyVec, SEXP dsgnmdIDVec, SEXP dsgnmdVec);
SEXP getRecordIDs(SEXP areaCumSumVec, SEXP sampPosVec, SEXP dsgnIDVec);
SEXP getShapeBox(SEXP fileNamePrefix, SEXP dsgnIDVec);
SEXP linSampleIRS(SEXP fileNamePrefix, SEXP lenCumSumVec, SEXP sampPosVec,
   SEXP dsgnIDVec, SEXP dsgnLenVec, SEXP dsgnMdmVec);
 
#endif
