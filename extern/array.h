#ifndef ARRAY_H
#define ARRAY_H

/* Macro that computes the minimum of two numbers */
#define MIN(a, b) (((a) < (b)) ? (a) : (b))

/* Internal type of the items of a dynamic array; this type allows us to
   store pointers, integers and floating point numbers in dynamics arrays,
   as "long long" is the largest type in C (excluding "long double")       */
#define ARRAY_TYPE long long

/* Dynamic array types */
#define POINTER_ARRAY 0
#define STRING_ARRAY  1
#define INTEGER_ARRAY 2
#define DOUBLE_ARRAY  3

/* Dynamic array definition */
typedef struct _array {
	union _uBase {
		ARRAY_TYPE *pAny;
		double *pDbl;
		char **pStr;
		int *pInt;
	} uBase;
	int nRows;
	int nColumns;
	int nType;
} array;

/* Internal function prototypes */
void create_array(array *pmArray, int nRows, int nColumns, int nType);
void create_copy_array(array *pmArray1, array *pmArray2);
void destroy_array(array *pmArray);
void destroy_array_and_contents(array *pmArray);
void resize_array(array *pmArray, int nNewRows, int nNewColumns);
void* array_get(array *pmArray, int nRow, int nColumn);
char* array_get_string(array *pmArray, int nRow, int nColumn);
int array_get_int(array *pmArray, int nRow, int nColumn);
double array_get_double(array *pmArray, int nRow, int nColumn);
void array_set(array *pmArray, int nRow, int nColumn, void *pValue);
void array_set_string(array *pmArray, int nRow, int nColumn, char *sValue);
void array_set_int(array *pmArray, int nRow, int nColumn, int nValue);
void array_set_double(array *pmArray, int nRow, int nColumn, double dValue);

#endif

