#ifndef FUZZYSETS_H
#define FUZZYSETS_H

#include <SWI-Prolog.h>

/* Macros for computing the minimum and the maximum of two numbers */
#define MAX(a, b) (((a) > (b)) ? (a) : (b))
#define MIN(a, b) (((a) < (b)) ? (a) : (b))

/* Macro for computing the value of 'a' raised to the power 'b' */
#define POW(a, b) (((b) == 1.0) ? (a) : (pow(a, b)))

/* Initial size for adjacency matrix and lists of subsets */
#define INI_ADJ_MATRIX_SIZE  5
#define INI_SUBSET_LIST_SIZE 5

/* Subset types */
#define SUBSET_UNKNOWN              0
#define SUBSET_TRAPEZOIDAL          1
#define SUBSET_TRIANGULAR           2
#define SUBSET_TRAPEZOIDAL_MODIFIER 3
#define SUBSET_TRIANGULAR_MODIFIER  4
#define SUBSET_POINT                5

/* Structure that represents the domain of a fuzzy set */
typedef struct _domain {
	double dMinimum;
	double dMaximum;
} domain;

/* Structure used to define a trapezoidal fuzzy subset */
typedef struct _trapezoidalSubset {
	double dValueA;
	double dValueB;
	double dValueC;
	double dValueD;
} trapezoidalSubset;

/* Structure used to define a triangular fuzzy subset */
typedef struct _triangularSubset {
	double dValueA;
	double dValueB;
	double dValueC;
} triangularSubset;

/* Structure used to define a trapezoidal fuzzy subset with a modifier */
typedef struct _trapezoidalModifierSubset {
	double dValueA;
	double dValueB;
	double dValueC;
	double dValueD;
	double dExponent;
} trapezoidalModifierSubset;

/* Structure used to define a triangular fuzzy subset with a modifier */
typedef struct _triangularModifierSubset {
	double dValueA;
	double dValueB;
	double dValueC;
	double dExponent;
} triangularModifierSubset;

/* Structure used to define a domain point */
typedef struct _pointSubset {
	int nValue;
} pointSubset;

/* Generic structure that can represent any of the previous fuzzy subsets */
typedef struct _subset {
	term_t tName;
	int nType;
	union _uSubset {
		trapezoidalSubset oTrapezoidal;
		triangularSubset oTriangular;
		trapezoidalModifierSubset oTrapezoidalMod;
		triangularModifierSubset oTriangularMod;
		pointSubset oPoint;
	} uDefinition;
} subset;

#endif

