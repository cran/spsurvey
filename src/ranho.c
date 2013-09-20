/****************************************************************************** 
**  File:        ranho.c
**
**  Purpose:     This file contains the C implementation of the ranho function
**               who's purpose is to create hierarchical random addresses from
**               4-fold hierarchical addresses.
**  Programmers: Christian Platt, Tom Kincaid
**  Algorithm:   The entry point from R is in the ranho function.  The 
**               algorithm behaves just like the R version in that this
**               entry function then calls the recursive permFcn which 
**               generates the hierarchical addresses.  The new addresses
**               are written over the sent addresses so R needs to retrieve
**               them from the original structure that holds the addresses.
**
**               To implement some of the build in R functions that were used
**               in the R version to linked lists are used.  One is used to
**               store temporary subset of pointers to the addresses and 
**               behaves like the 'b' variable in the R version of ranho.  
**               The other list is used to store columns values of the 
**               addresses at a specific column level. The nodes in this list
**               also store a pointer to the address so that it can be
**               referenced from the node.  
**
**               The reason that two different node types were used instead of
**               combining them into one was to save memory space.  Since the
**               number of addresses can get large storing an unecessary char
**               in each address node would of been a waste of memory.
**  Notes:       The perm array (in permFcn) can be changed to be only 4 
**               chars in length. It is at 5 so that the '\0' could be 
**               added so that they could be printed out using Rprintf.
**  Created:     August 11, 2004
**  Revised:     April 24, 2006
**  Revised:     January 27, 2012
******************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <R.h>
#include <Rmath.h>
#include <Rdefines.h>

/* node type for the linked list of addresses */
typedef struct AddrNode addrNode;
struct AddrNode {
  char * addr;                  /* address (i.e. "4311", "321324", etc. ) */
  struct AddrNode * next;       /* pointer to next node in the list */
};

/* node type for the linked list of column chars assoicated with addresses */
typedef struct ColCharNode colCharNode;
struct ColCharNode {
  char colChar;                 /* character found at column position(i.e. 4)*/
  char * addr;                  /* address (i.e. "4311", "321324", etc. ) */
  struct ColCharNode * next;    /* pointer to next node in the list */
};

/* all possible perms to be used with the genPerm function */
char * perms[24] = { "1234", "1243", "1324", "1342", "1423", "1432",
                     "2134", "2143", "2314", "2341", "2413", "2431",
                     "3124", "3142", "3214", "3241", "3412", "3421",
                     "4123", "4132", "4213", "4231", "4312", "4321" };


/**********************************************************
** Function:   genPerm
**
** Purpose:    Randomly generates a new perm.  This is done by
**             randomly choosing a string from the perms[] array.
** Arguments:  perm, character array to store the new perm in
** Return:     void
***********************************************************/
void genPerm( char * perm ) {

  /* pick and copy the perm into the sent char array */
  strncpy( perm, perms[(int) (24.0*runif( 0.0, RAND_MAX )/(RAND_MAX+1.0)) ] , 4 );

  return;
}


/**********************************************************
** Function:   addAddrNode
**
** Purpose:    Allocates and adds a new node to the front of
**             addrList that is pointed to by head.  This node's
**             addr pointer will then point to the sent addr. This
**             list is used to store a temporary subset of the addresses.
** Arguments:  head, pointer to pointer to beginning of the linked list
**             addr, pointer to array of chars that store address
** Return:     1,  on success
**             -1, on error 
***********************************************************/
int addAddrNode( addrNode ** head, char * addr ) {
  addrNode * temp;

  /* check to see if there is already a node in the list */
  if ( *head ) {
    if ( (temp = (addrNode *) malloc( sizeof(addrNode) )) == NULL ) {
      return -1;
    }
    temp->addr = addr;
    temp->next = *head;

    *head = temp;

  /* else list is empty so create the first node */
  } else {
    if ( (*head = (addrNode *) malloc( sizeof(addrNode) )) == NULL ) {
      return -1;
    }
    (*head)->addr = addr;
    (*head)->next = NULL;
  }

  return 1;
}


/**********************************************************
** Function:   addColCharNode
**
** Purpose:    Allocates and adds a new node to the front of
**             colCharList that is pointed to by head.  This node's
**             addr pointer will then point to the sent addr and it's
**             colCh will be set to the sent ch.  
**             This linked list is equivalent to the 'a' variable in the
**             ranho R version.  It stores all the column values, as a char,
**             at the specified column value (level).
** Algorithm:  The order in which the column values are added is important
**             so each new node needs to be added at the end of the list.
**             This is the reason for the sent last pointer which will
**             always point to the last node in the list.  This allows
**             the function to quickly add at the end.
** Arguments:  head, pointer to pointer to beginning of the linked list
**             ch, character found in the column of the current level
**             addr, pointer to array of chars that store address
** Return:     1,  on success
**             -1, on error
***********************************************************/
int addColCharNode( colCharNode ** head, char ch, char * addr,
                                                  colCharNode ** last ) {
  colCharNode * temp;

  /* check to see if there is already a node in the list */
  if ( *head ) {

    if ( (temp = (colCharNode *) malloc( sizeof(colCharNode) )) == NULL ) {
      return -1;
    }
    temp->colChar = ch;
    temp->addr = addr;
    temp->next = NULL;

    (*last)->next = temp;
    *last = temp;

  /* else list is empty so create the first node */
  } else {
    if ( (*head = (colCharNode *) malloc( sizeof(colCharNode) )) == NULL ) {
      return -1;
    }
    (*head)->colChar = ch;
    (*head)->addr = addr;
    (*head)->next = NULL;
    *last = *head;
  }

  return 1;
}


/**********************************************************
** Function:   deallocateAddrList
**
** Purpose:    Deallocates all memory used by the nodes for 
**             the addrList pointed to by head.
** Arguments:  head, pointer to beginning of the linked list
** Return:     void
***********************************************************/
void deallocateAddrList( addrNode * head ) {
  addrNode * temp;

  while( head ) {
    temp = head->next;
    free( head ); 
    head = temp;
  }

  return;
}


/**********************************************************
** Function:   deallocateColCharList
**
** Purpose:    Deallocates all memory used by the nodes for 
**             the colCharList pointed to by head.
** Arguments:  head, pointer to beginning of the linked list
** Return:     void
***********************************************************/
void deallocateColCharList( colCharNode * head ) {
  colCharNode * temp;

  while( head ) {
    temp = head->next;
    free( head ); 
    head = temp;
  }

  return;
}


/**********************************************************
** Function:   constructAddr
**
** Purpose:    To construct the hierarchical addresses.
** Arguments:  xcVec,   vector of x coordinates for the cells
**             ycVec,   vector of y coordinates for the cells
**             dxVec,   x offset 
**             dyVec,   y offset
**             nlevVec, number of levels
** Return:     results, vector of strings representing the hierarchical address 
***********************************************************/
SEXP constructAddr( SEXP xcVec, SEXP ycVec, SEXP dxVec, SEXP dyVec, 
                                                              SEXP nlevVec ) {
  int i, j;                          /* loop counters */
  int vecSize = length( xcVec );     /* size of incoming xc and yc vcectors */
  int x;                             /* temp x addr */
  int y;                             /* temp y addr */
  int nlev = INTEGER( nlevVec )[0];  /* number of levels */
  int * addr;                        /* temp array of addresses as integers */
  char * addrStr;                    /* char string representation of the addr*/
  SEXP results = NULL;               /* returing R object */
 

  /* allocate the necessary memory */
  if ( (addr = (int *) malloc( sizeof(int) * nlev )) == NULL ) {
    Rprintf( "Error: Allocating memory in C function constructAddr.\n" );
    PROTECT( results = allocVector(VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }
  if ((addrStr = (char *) malloc( sizeof(char) * ((nlev) + 1)))==NULL){
    Rprintf( "Error: Allocating memory in C function constructAddr.\n" );
    PROTECT( results = allocVector(VECSXP, 1 ) );
    UNPROTECT( 1 );
    return results;
  }

  /* create the R object for results */
  PROTECT( results = allocVector( STRSXP, vecSize ) );

  /* go through each cell */
  for ( i = 0; i < vecSize; ++i ) {
    x = (int) ceil( REAL( xcVec )[i] / REAL( dxVec )[0] );
    y = (int) ceil( REAL( ycVec )[i] / REAL( dyVec )[0] );

    /* build the address */
    for ( j = nlev-1; j >= 0; --j ) {
      addr[j] = 2 * abs( x % 2 ) + abs( y % 2 ) + 1;

      if ( (x % 2) == -1 ) {
        x = (x / 2) - 1;
      } else {
        x = x / 2;
      }
      if ( (y % 2) == -1 ) {
        y = (y / 2) - 1;
      } else {
        y = y / 2;
      }

      /* convert the address to a char */ 
      switch( addr[j] ) {
        case 1: addrStr[j] = '1';
                break;
        case 2: addrStr[j] = '2';
                break;
        case 3: addrStr[j] = '3';
                break;
        case 4: addrStr[j] = '4';
                break;
        default: Rprintf("Error: The C function constructAddr produced an invalid value: %d\n", addr[j]);
                 break;
      }
    }

    /* add the terminating char */
    addrStr[nlev] = '\0';
    SET_STRING_ELT( results, i, mkChar( addrStr ) );
  }

  /* clean up */
  free( addr );
  free( addrStr );
  UNPROTECT(1);

  return results;
}


/**********************************************************
** Function:   permFcn 
**
** Purpose:    To generate random hierarchical addresses.
** Algorithm:  This recursive function uses the same algorithm that 
**             the R version uses.
** Arguments:  level, current columns position in the address
**             fin,   max number of columns in the address
**             prevAddrList, pointer to linked list of addresses generated
**                           from the previous function call
** Return:     void
***********************************************************/
void permFcn( int level, int fin, addrNode * prevAddrList ) {
  char perm[5];                      /* perm for this level */
  addrNode * tempAddrNode;           /* temp node pointer */
  addrNode * currAddrList = NULL;    /* linked list of addresses */
  colCharNode * tempColCharNode;     /* temp node pointer */
  colCharNode * colCharList = NULL;  /* linked list of column values */
  colCharNode * lastPtr = NULL;      /* used for adding nodes to the */
                                     /* colChar list */

  /* check level */
  if( level >= fin )  return;

  /* generate new perm for this level */
  genPerm( perm );

  /* create the list of the characters for this column level from the */
  /* sent addr list */
  tempAddrNode = prevAddrList;
  while( tempAddrNode ) {
    if ( addColCharNode( &colCharList, tempAddrNode->addr[level]
                                   , tempAddrNode->addr, &lastPtr ) == -1 ) {
      Rprintf( "Error: Allocating memory in ranho.c\n" );
      return;
    }
    tempAddrNode = tempAddrNode->next;
  }


  /* create list of all the addresses that have a '1' at the current */
  /* column level */
  tempColCharNode = colCharList;
  while( tempColCharNode ) {
    if ( tempColCharNode->colChar == '1' ) {
      if ( addAddrNode( &currAddrList, tempColCharNode->addr ) == -1 ) {
        Rprintf( "Error: Allocating memory in ranho.c\n" );
        return;
      }
    }
    tempColCharNode = tempColCharNode->next;
  } 

  /* if list is not empty call permFcn again and then modify the addresses */
  /* based on the current perm */
  if( currAddrList != NULL ) {
    permFcn( level+1, fin, currAddrList );
    tempAddrNode = currAddrList;
    while( tempAddrNode ) {
      tempAddrNode->addr[level] = perm[0];
      tempAddrNode = tempAddrNode->next;
    }
  }
 
  /* clear the addr list */
  deallocateAddrList( currAddrList ); 
  currAddrList = NULL;


  /* create list of all the addresses that have a '2' at the current */
  /* column level */
  tempColCharNode = colCharList;
  while( tempColCharNode ) {
    if ( tempColCharNode->colChar == '2' ) {
      if ( addAddrNode( &currAddrList, tempColCharNode->addr ) == -1 ) {
        Rprintf( "Error: Allocating memory in ranho.c\n" );
        return;
      }
    }
    tempColCharNode = tempColCharNode->next;
  } 

  /* if list is not empty call permFcn again and then modify the addresses */
  /* based on the current perm */
  if( currAddrList != NULL ) {
    permFcn( level+1, fin, currAddrList );
    tempAddrNode = currAddrList;
    while( tempAddrNode ) {
      tempAddrNode->addr[level] = perm[1];
      tempAddrNode = tempAddrNode->next;
    }
  }  

  /* clear the addr list */
  deallocateAddrList( currAddrList ); 
  currAddrList = NULL;


  /* create list of all the addresses that have a '3' at the current */
  /* column level */
  tempColCharNode = colCharList;
  while( tempColCharNode ) {
    if ( tempColCharNode->colChar == '3' ) {
      if ( addAddrNode( &currAddrList, tempColCharNode->addr ) == -1 ) {
        Rprintf( "Error: Allocating memory in ranho.c\n" );
        return;
      }
    }
    tempColCharNode = tempColCharNode->next;
  } 

  /* if list is not empty call permFcn again and then modify the addresses */
  /* based on the current perm */
  if( currAddrList != NULL ) {
    permFcn( level+1, fin, currAddrList );
    tempAddrNode = currAddrList;
    while( tempAddrNode ) {
      tempAddrNode->addr[level] = perm[2];
      tempAddrNode = tempAddrNode->next;
    }
  }  

  /* clear the addr list */
  deallocateAddrList( currAddrList );
  currAddrList = NULL;


  /* create list of all the addresses that have a '4' at the current */
  /* column level */
  tempColCharNode = colCharList;
  while( tempColCharNode ) {
    if ( tempColCharNode->colChar == '4' ) {
      if ( addAddrNode( &currAddrList, tempColCharNode->addr ) == -1 ) {
        Rprintf( "Error: Allocating memory in ranho.c\n" );
        return;
      }
    }
    tempColCharNode = tempColCharNode->next;
  } 

  /* if list is not empty call permFcn again and then modify the addresses */
  /* based on the current perm */
  if( currAddrList != NULL ) {
    permFcn( level+1, fin, currAddrList );
    tempAddrNode = currAddrList;
    while( tempAddrNode ) {
      tempAddrNode->addr[level] = perm[3];
      tempAddrNode = tempAddrNode->next;
    }
  }  


  /* free up memory */
  deallocateAddrList( currAddrList );
  deallocateColCharList( colCharList );

  return;
}


/**********************************************************
** Function:   ranho
**
** Purpose:    This is the entry point from R.  This function is sent
**             the array of addresses and the number of addresses in
**             the set.  It starts off the algorithm by calling the 
**             recursive function permFcnand the addresses in adr are
**             modified as the alogirithm works.  R will retrieve the
**             new addresses from the same sent adr structure.
** Arguments:  adr,  array of strings representing all the addresses
**             size, pointer to integer representing the number of 
**                   addresses that are in adr
** Return:     void
***********************************************************/
void ranho( char ** adr, int * size ) {
  int i;
  addrNode * head = NULL;  /* pointer to linked list of addresses */

  /* obtain the R random number generator type and seed */
  GetRNGstate();

  /* create the initial linked list of all the original addresses */
  for ( i=0; i < *size; ++i ) {
    if ( addAddrNode( &head, adr[i] ) == -1 ) {
      Rprintf( "Error: Allocating memory in ranho.c\n" );
    }
  }

  /* start the algorithm */
  permFcn( 0, strlen(adr[0]), head );
 
  /* write out the R random number generator type and seed */
  PutRNGstate();

  /* free up memory used */ 
  deallocateAddrList(head);

  return;
}
