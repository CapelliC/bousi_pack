/***********************************************************************
 * Module for handling dynamic arrays
 */

#include <stdio.h>
#include <stdlib.h>
#include "array.h"

/***********************************************************************/



/***********************************************************************
 * Declarations
 ***********************************************************************/

/* Private function prototypes */
static void check_size(array *pmArray, int nRow, int nColumn);
static void initialize_value(array *pmArray, int nRow, int nColumn);



/***********************************************************************
 * Internal functions
 ***********************************************************************/


/* Creates a new dynamic array with the specified initial size, and fills
   it with zeros or NULL values, depending on the array type. Note that
   each of the items of the array must be no bigger in size than
   ARRAY_TYPE, which is the internal type used by the dynamic array.      */
void create_array(array *pmArray, int nRows, int nColumns, int nType) {
	int i, j;

	/* allocates memory for the array */
	pmArray->uBase.pAny = malloc(sizeof(ARRAY_TYPE) * nRows * nColumns);

	/* saves the properties of the array */
	pmArray->nRows = nRows;
	pmArray->nColumns = nColumns;
	pmArray->nType = nType;

	/* fills the new array with zeros or NULLs */
	for(i = 0; i < nRows; i++) {
		for(j = 0; j < nColumns; j++) {
			initialize_value(pmArray, i, j);
		}
	}
}


/* Creates a copy of the array pointed by pmArray1 into pmArray2. This function
 is an adaptation of the create_array() function*/

void create_copy_array(array *pmArray1, array *pmArray2) {
    int i, j;
    
    /* allocates memory for the array */
    pmArray2->uBase.pAny = malloc(sizeof(ARRAY_TYPE) * pmArray1->nRows * pmArray1->nColumns);
    
    /* saves the properties of the array */
    pmArray2->nRows = pmArray1->nRows;
    pmArray2->nColumns = pmArray1->nColumns;
    pmArray2->nType = pmArray1->nType;
    
    /* fills the second new array with a copy of the first*/
    for(i = 0; i < pmArray1->nRows; i++) {
        for(j = 0; j < pmArray1->nColumns; j++) {
            switch(pmArray1->nType) {
                case INTEGER_ARRAY:
                    array_set_int(pmArray2, i, j, array_get_int(pmArray1, i, j));
                    break;
                case DOUBLE_ARRAY:
                    array_set_double(pmArray2, i, j, array_get_double(pmArray1, i, j));
                    break;
                case STRING_ARRAY:
                    array_set_string(pmArray2, i, j, array_get_string(pmArray1, i, j));
                    break;
                default: /* POINTER_ARRAY */
                    array_set(pmArray2, i, j, array_get(pmArray1, i, j));
                    break;
            }
        }
    }
}



/* Shrinks or enlarges a dynamic array, keeping all the items of the old
   array when possible. New items of the array will be initialized with
   zeros or NULL values, depending on the array type.                    */
void resize_array(array *pmArray, int nNewRows, int nNewColumns) {
	ARRAY_TYPE *pNewBase;
	int nOldRows, nOldColumns;
	int i, j;

	/* allocates memory for the new array */
	pNewBase = malloc(sizeof(ARRAY_TYPE) * nNewRows * nNewColumns);

	/* copies the items of the old array to the new array */
	for(i = 0; i < MIN(pmArray->nRows, nNewRows); i++) {
		for(j = 0; j < MIN(pmArray->nColumns, nNewColumns); j++) {
			pNewBase[i * nNewColumns + j] = pmArray->uBase.pAny[i * pmArray->nColumns + j];
		}
	}

	/* frees memory used by the old array */
	free(pmArray->uBase.pAny);

	/* saves the new properties of the array */
	nOldRows = pmArray->nRows;
	nOldColumns = pmArray->nColumns;
	pmArray->nRows = nNewRows;
	pmArray->nColumns = nNewColumns;
	pmArray->uBase.pAny = pNewBase;

	/* fills the new items of the array with zeros or NULLs */
	for(i = nOldRows; i < nNewRows; i++) {
		for(j = 0; j < nNewColumns; j++) {
			initialize_value(pmArray, i, j);
		}
	}
	for(j = nOldColumns; j < nNewColumns; j++) {
		for(i = 0; i < nNewRows; i++) {
			initialize_value(pmArray, i, j);
		}
	}
}


/* Destroys an existing dynamic array. This will NOT delete the array
   contents.                                                          */
void destroy_array(array *pmArray) {
	free(pmArray->uBase.pAny);
}


/* Destroys an existing dynamic array and all its contents (calling
   'free' for each not-NULL item).                                  */
void destroy_array_and_contents(array *pmArray) {
	void* pItem;
	int i, j;

	for(i = 0; i < pmArray->nRows; i++) {
		for(j = 0; j < pmArray->nColumns; j++) {
			pItem = array_get(pmArray, i, j);
			if(pItem != NULL) {
				free(pItem);
			}
		}
	}
	free(pmArray->uBase.pAny);
}


/* Returns an item of a dynamic array as a pointer. */
void* array_get(array *pmArray, int nRow, int nColumn) {
	check_size(pmArray, nRow, nColumn);
	return (void*)pmArray->uBase.pAny[nRow * pmArray->nColumns + nColumn];
}


/* Returns an item of a dynamic array as a string. */
char* array_get_string(array *pmArray, int nRow, int nColumn) {
	check_size(pmArray, nRow, nColumn);
	return pmArray->uBase.pStr[nRow * pmArray->nColumns + nColumn];
}


/* Returns an item of a dynamic array as an integer number. */
int array_get_int(array *pmArray, int nRow, int nColumn) {
	check_size(pmArray, nRow, nColumn);
	return pmArray->uBase.pInt[(nRow * pmArray->nColumns + nColumn)
	                           * sizeof(ARRAY_TYPE) / sizeof(int)];
}


/* Returns an item of a dynamic array as a double floating point number. */
double array_get_double(array *pmArray, int nRow, int nColumn) {
	check_size(pmArray, nRow, nColumn);
	return pmArray->uBase.pDbl[(nRow * pmArray->nColumns + nColumn)
	                           * sizeof(ARRAY_TYPE) / sizeof(double)];
}


/* Stores a pointer in a dynamic array. */
void array_set(array *pmArray, int nRow, int nColumn, void* pValue) {
	check_size(pmArray, nRow, nColumn);
	pmArray->uBase.pAny[nRow * pmArray->nColumns + nColumn] = (ARRAY_TYPE)pValue;
}


/* Stores a string in a dynamic array. */
void array_set_string(array *pmArray, int nRow, int nColumn, char* sValue) {
	check_size(pmArray, nRow, nColumn);
	pmArray->uBase.pStr[nRow * pmArray->nColumns + nColumn] = sValue;
}


/* Stores an integer number in a dynamic array. */
void array_set_int(array *pmArray, int nRow, int nColumn, int nValue) {
	check_size(pmArray, nRow, nColumn);
	pmArray->uBase.pInt[(nRow * pmArray->nColumns + nColumn)
	                    * sizeof(ARRAY_TYPE) / sizeof(int)] = nValue;
}


/* Stores a double floating point number in a dynamic array. */
void array_set_double(array *pmArray, int nRow, int nColumn, double dValue) {
	check_size(pmArray, nRow, nColumn);
	pmArray->uBase.pDbl[(nRow * pmArray->nColumns + nColumn)
	                    * sizeof(ARRAY_TYPE) / sizeof(double)] = dValue;
}



/***********************************************************************
 * Private functions
 ***********************************************************************/


/* Checks if the specified row and column indices are valid for a certain
   dynamic array. If any of the indices are out of bounds, dynamic array
   will be automatically resized.                                         */
static void check_size(array *pmArray, int nRow, int nColumn) {
	while(nRow >= pmArray->nRows) {
		resize_array(pmArray, pmArray->nRows * 2, pmArray->nColumns);
	}
	while(nColumn >= pmArray->nColumns) {
		resize_array(pmArray, pmArray->nRows, pmArray->nColumns * 2);
	}
}


/* Initializes an item of a dynamic array with the default value, which
   is 0 if array type is INTEGER_ARRAY or DOUBLE_ARRAY, or NULL if array
   type is POINTER_ARRAY or STRING_ARRAY.                                */
static void initialize_value(array *pmArray, int nRow, int nColumn) {
	switch(pmArray->nType) {
	case INTEGER_ARRAY:
		array_set_int(pmArray, nRow, nColumn, 0);
		break;
	case DOUBLE_ARRAY:
		array_set_double(pmArray, nRow, nColumn, 0.0);
		break;
	case STRING_ARRAY:
		array_set_string(pmArray, nRow, nColumn, NULL);
		break;
	default: /* POINTER_ARRAY */
		array_set(pmArray, nRow, nColumn, NULL);
		break;
	}
}

