#ifndef CLOSURE_H
#define CLOSURE_H

/* Macro that computes the maximum of two numbers */
#define MAX(a, b)     (((a) > (b)) ? (a) : (b))

/* Macros with the t-norm definitions */
#define MIN(a, b)     (((a) < (b)) ? (a) : (b))
#define PRODUCT(a, b) ((a) * (b))
#define LUKA(a, b)    (MAX(0, ((a) + (b) - 1)))

/* Initial size for adjacency matrix */
#define INI_ADJ_MATRIX_SIZE 5

/* Initial size for a matrix representing a SET*/
#define INI_SET_MATRIX_SIZE 5


/* Closure flags */
#define CLS_REFLEXIVE  1
#define CLS_SYMMETRIC  2
#define CLS_TRANSITIVE 4

/* T-Norm identifiers */
#define TNM_MINIMUM     1
#define TNM_PRODUCT     2
#define TNM_LUKASIEWICZ 3
#define TNM_NOT_USED -1

/* Internal function prototypes */

void fill_adjacency_matrix(term_t tEqList, array *pmAdjMatrix,
                           array *pmListTerms, int *pnSize);
void build_closure(array *pmAdjMatrix, int nSize, int nClosureId,
                   int nTNormId);


#endif

