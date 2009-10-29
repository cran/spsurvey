/****************************************************************************** 
**  File:        init.c   
**  Purpose:     This file contains the code used for registering the C 
**               functions in the DLL of the spsurvey library.
**  Programmer:  Tom Kincaid
**  Created:     May 4, 2006
**  Revised:     September 24, 2009
******************************************************************************/

#include <R.h>
#include <Rinternals.h>
#include "spsurvey.h"
#include <R_ext/Rdynload.h>

static R_NativePrimitiveArgType ranho_t[2] = {STRSXP, INTSXP};

static const R_CMethodDef cMethods[] = {
   {"ranho", (DL_FUNC) &ranho, 2, ranho_t},
   {NULL, NULL, 0}
};

static const R_CallMethodDef callMethods[] = {
   {"readDbfFile", (DL_FUNC) &readDbfFile, 1},
   {"readShapeFile", (DL_FUNC) &readShapeFile, 1},
   {"readShapeFilePts", (DL_FUNC) &readShapeFilePts, 1},
   {"getRecordShapeSizes", (DL_FUNC) &getRecordShapeSizes, 1},
   {"writeDbfFile", (DL_FUNC) &writeDbfFile, 3},
   {"writeShapeFilePoint", (DL_FUNC) &writeShapeFilePoint, 6},
   {"writeShapeFilePolygon", (DL_FUNC) &writeShapeFilePolygon, 12},
   {"pointInPolygonObj", (DL_FUNC) &pointInPolygonObj, 4},
   {"numLevels", (DL_FUNC) &numLevels, 7},
   {"constructAddr", (DL_FUNC) &constructAddr, 5},
   {"pickGridCells", (DL_FUNC) &pickGridCells, 2},
   {"pointInPolygonFile", (DL_FUNC) &pointInPolygonFile, 5},
   {"linSample", (DL_FUNC) &linSample, 7},
   {"getRecordIDs", (DL_FUNC) &getRecordIDs, 3},
   {"getShapeBox", (DL_FUNC) &getShapeBox, 2},
   {"linSampleIRS", (DL_FUNC) &linSampleIRS, 6},
   {NULL, NULL, 0}
};

void R_init_spsurvey(DllInfo *dll) {
   R_registerRoutines(dll, cMethods, callMethods, NULL, NULL);
   R_useDynamicSymbols(dll, FALSE);
}
