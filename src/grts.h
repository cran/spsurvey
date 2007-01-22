/****************************************************************************** 
**  File:       grts.h
**  
**  Purpose:    Contains defines for grts.c
**  Programmer: Christian Platt
**  Created:    11/17/2004
**  Revised:     December 22, 2006
******************************************************************************/

#ifndef GRTS_H 
#define GRTS_H

/* temporary shapefile name */
#define TEMP_SHP_FILE  "shapefile1021.temp"

/* struct for storing a cell's coordinates */
typedef struct cellStruct Cell;
struct cellStruct {
  double xMin;
  double yMin;
  double xMax;
  double yMax;
};

#endif
