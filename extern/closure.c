/***********************************************************************
 * Computation of the closure of a fuzzy relation
 */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <stdio.h>
#include "array.h"
#include "bool.h"
#include "closure.h"

/***********************************************************************/



/***********************************************************************
 * Declarations
 ***********************************************************************/

/* Private function prototypes */
static void create_equations(array *pmAdjMatrix, array *pmListTerms, int nSize,
                             atom_t aPrefix, double dLambdaCut, term_t tEqList, int nClosureId);

static int search_term(term_t tTerm, array *pmListTerms, int nSize);

static void print_adjacency_matrix(array *pmAdjMatrix, int *pnSize); /* FOR DEBUGGING */

/***********************************************************************
 * Public functions
 ***********************************************************************/


/** ext_closure(+InputEquations, +Closure, +TNorm, +RelationName, +LambdaCut, -OutputEquations)
 *
 *    Computes the reflexive, symmetric and/or transitive closure of
 *    the fuzzy relation defined by a set of equations and returns the
 *    list of equations of the resulting fuzzy relation. Input
 *    equations must be like "rel(a, b, 0.5)", where "rel" can be any
 *    functor; output equations will be similar but replacing "rel"
 *    with RelationName atom.
 *    Closure must be a combination of one or more of these flags:
 *     * 1: Reflexive
 *     * 2: Symmetric
 *     * 4: Transitive
 *    TNorm must be one of these values:
 *     * 1: Minimum
 *     * 2: Product
 *     * 3: Lukasiewicz
 */
foreign_t pl_closure(term_t tInputEquations, term_t tClosure, term_t tTNorm,
                     term_t tRelationName, term_t tLambdaCut, term_t tOutputEquations) {
	array mAdjMatrix, mListTerms;
	term_t tEquations;
	atom_t aPrefix;
	int nClosureId, nTNormId;
	int nSize;
	double dLambdaCut;

	/* checks that arguments have the expected types */
	if(!PL_is_atom(tRelationName) || !PL_is_list(tInputEquations)
      || !PL_is_number(tClosure) || !PL_is_number(tLambdaCut) || !PL_is_number(tTNorm)) {
		PL_fail;
	};

	/* creates adjacency matrix and list of terms */
	nSize = 0;
	create_array(&mListTerms, 1, INI_ADJ_MATRIX_SIZE, POINTER_ARRAY);
	create_array(&mAdjMatrix, INI_ADJ_MATRIX_SIZE, INI_ADJ_MATRIX_SIZE, DOUBLE_ARRAY);
	fill_adjacency_matrix(tInputEquations, &mAdjMatrix, &mListTerms, &nSize);

	/* builds reflexive, symmetric and/or transitive closure */
  if( !PL_get_integer_ex(tClosure, &nClosureId) ) PL_fail;
  if( !PL_get_integer_ex(tTNorm, &nTNormId) ) PL_fail;
  if( !PL_get_float_ex(tLambdaCut, &dLambdaCut) ) PL_fail;
	build_closure(&mAdjMatrix, nSize, nClosureId, nTNormId);

	/* creates the list of equations from the adjacency matrix */
	tEquations = PL_new_term_ref();
    if ( !PL_get_atom_ex(tRelationName, &aPrefix) )  PL_fail;
	create_equations(&mAdjMatrix, &mListTerms, nSize, aPrefix, dLambdaCut,
	                 tEquations, nClosureId);

	/* frees memory used by dynamic arrays */
	destroy_array(&mListTerms);
	destroy_array(&mAdjMatrix);

	/* returns list of equations */
	if (!PL_unify(tEquations, tOutputEquations)) {
		PL_fail;
	};

	PL_succeed;
}

/***********************************************************************
 THE FOLLOWING FUNCTIONS ALSO ARE USED FOR COMPUTING THE SET OF PROXIMITY
 BLOCKS. Then, they are made visible for other modules.
 ***********************************************************************/

/* Populates an adjacency matrix and a list of terms using the specified
 list of equations. Both arrays must be initialized when calling to
 this function.                                                        */
void fill_adjacency_matrix(term_t tEqList, array *pmAdjMatrix,
                                  array *pmListTerms, int *pnSize) {
    int nTermIndex[2];
    term_t tTail, tHead;
    term_t tItem;
    double dDegree;
    int i;
    
    tTail = PL_copy_term_ref(tEqList);
    tHead = PL_new_term_ref();
    tItem = PL_new_term_ref();
    
    /* reads original list of equations (each equation must be a term
     with three arguments: two symbols and a similarity degree)     */
    while(PL_get_list(tTail, tHead, tTail)) {
        for(i = 0; i < 2; i++) {
            /* gets the position of the first/second term of the equation
             in the list of terms used by the adjacency matrix          */
            if ( !PL_get_arg(i + 1, tHead, tItem) ) {
                printf("ERROR (closure module:fill_adjacency_matrix): line 122, getting the \n");
                printf("position of the first/second term of the equation in the list of terms \n");
                printf("used by the adjacency matrix. \n");
            };
            nTermIndex[i] = search_term(tItem, pmListTerms, *pnSize);
            if(nTermIndex[i] == -1) {
                /* adds new term to list of terms */
                array_set(pmListTerms, 0, *pnSize, (void*)PL_copy_term_ref(tItem));
                nTermIndex[i] = *pnSize;
                (*pnSize)++;
            }
        }
        /* stores the equation in the adjacency matrix */
        if ( !PL_get_arg(3, tHead, tItem) ) {
            printf("ERROR (closure module:fill_adjacency_matrix): line 136, storing the equation \n");
            printf("in the adjacency matrix. \n");
        };
        if ( !PL_get_float_ex(tItem, &dDegree) ) {
            printf("ERROR (closure module:fill_adjacency_matrix): line 140, storing the equation \n");
            printf("in the adjacency matrix. \n");
        };
        array_set_double(pmAdjMatrix, nTermIndex[0], nTermIndex[1], dDegree);
    }
}

/* Builds the reflexive, symmetric, and/or transitive closure of the
 relation defined by an adjacency matrix and a list of terms.      */
void build_closure(array *pmAdjMatrix, int nSize, int nClosureId,
                          int nTNormId) {
    double dDegree, dTNormResult;
    bool bTransitive, bSymmetric, bReflexive;
    int i, j, k;
    
    /* extracts the closure properties from the closure id */
    bTransitive = FALSE;
    bSymmetric = FALSE;
    bReflexive = FALSE;
    if((nClosureId & CLS_TRANSITIVE) != 0) {
        bTransitive = TRUE;
    }
    if((nClosureId & CLS_SYMMETRIC) != 0) {
        bSymmetric = TRUE;
    }
    if((nClosureId & CLS_REFLEXIVE) != 0) {
        bReflexive = TRUE;
    }
    
    /* builds the reflexive closure (if needed) */
    if(bReflexive) {
        for(i = 0; i < nSize; i++) {
            array_set_double(pmAdjMatrix, i, i, 1.0);
        }
    }
    
    /* builds the symmetric closure (if needed) */
    if(bSymmetric) {
        for(i = 0; i < nSize; i++) {
            for(j = 0; j < nSize; j++) {
                if(i != j && array_get_double(pmAdjMatrix, i, j) > 0.0) {
                    array_set_double(pmAdjMatrix, j, i,
                                     array_get_double(pmAdjMatrix, i, j));
                }
            }
        }
    }
    
    /* applies a Warshall-like algorithm to build the transitive
     closure (if needed)                                       */
    if(bTransitive) {
        for(k = 0; k < nSize; k++) {
            for(i = 0; i < nSize; i++) {
                for(j = 0; j < nSize; j++) {
                    /* selects the t-norm to be used */
                    switch(nTNormId) {
                        case TNM_MINIMUM:
                            dTNormResult = MIN(array_get_double(pmAdjMatrix, i, k),
                                               array_get_double(pmAdjMatrix, k, j));
                            break;
                        case TNM_PRODUCT:
                            dTNormResult = PRODUCT(array_get_double(pmAdjMatrix, i, k),
                                                   array_get_double(pmAdjMatrix, k, j));
                            break;
                        case TNM_LUKASIEWICZ:
                            dTNormResult = LUKA(array_get_double(pmAdjMatrix, i, k),
                                                array_get_double(pmAdjMatrix, k, j));
                            break;
                    }
                    dDegree = MAX(array_get_double(pmAdjMatrix, i, j),
                                  dTNormResult);
                    array_set_double(pmAdjMatrix, i, j, dDegree);
                } /* for(j) */
            } /* for(i) */
        } /* for(k) */
    }
}


/***********************************************************************
 * Private functions
 ***********************************************************************/

/* Creates and returns the list of equations that define a fuzzy relation
   from its adjacency matrix and list of terms. All the equations will
   have the atom "aPrefix" as the main functor.                           */
static void create_equations(array *pmAdjMatrix, array *pmListTerms, int nSize,
                             atom_t aPrefix, double dLambdaCut, term_t tEqList, int nClosureId) {
	term_t tHead;
	term_t tSymbol1, tSymbol2, tDegree;
	functor_t fRel;
	bool bReflexive;
	int i, j;
	double dThreshold;

	/* initializes list of equations */
	fRel = PL_new_functor(aPrefix, 3);
	tHead = PL_new_term_ref();
	PL_put_nil(tEqList);

	/* checks if the relation must be reflexive */
	bReflexive = ((nClosureId & CLS_REFLEXIVE) != 0); 

// 	/* sets the threshold: dLambdaCut for sim, and 0.0 for the rest */
// 	dThreshold = !strcmp(PL_atom_chars(aPrefix),"sim") ? dLambdaCut : 0.0;
  /* sets the threshold: dLambdaCut */
  dThreshold = dLambdaCut;
	
	/* scans adjacency matrix, ignoring zeros and diagonal positions
	   (only if the relation must be reflexive, because the reflexive
	   equation is added later)                                       */
	for(i = 0; i < nSize; i++) {
		for(j = 0; j < nSize; j++) {
			if((i != j || !bReflexive) 
//			   && array_get_double(pmAdjMatrix, i, j) > 0.0) {
			   && array_get_double(pmAdjMatrix, i, j) > dThreshold) {
				/* builds the equation and adds it to list */
				tSymbol1 = (term_t)array_get(pmListTerms, 0, i);
				tSymbol2 = (term_t)array_get(pmListTerms, 0, j);
				tDegree = PL_new_term_ref();
        if (!PL_put_float(tDegree, array_get_double(pmAdjMatrix, i, j)) ) {
            printf("ERROR (closure module:create_equations): line 259, scanning adjacency matrix \n");
        };
        if (!PL_cons_functor(tHead, fRel, tSymbol1, tSymbol2, tDegree) ) {
            printf("ERROR (closure module:create_equations): line 262, scanning adjacency matrix \n");
        };
        if (!PL_cons_list(tEqList, tHead, tEqList) ) {
            printf("ERROR (closure module:create_equations): line 265, scanning adjacency matrix \n");
        };
			}
		}
	}
	
	/* if fuzzy relation must be reflexive, adds the generic equation
	   "rel(X, X, 1)" (where X is a free variable) instead of adding a
	   different equation for each symbol                              */
	if(bReflexive) {
		tSymbol1 = PL_new_term_ref();
		tDegree = PL_new_term_ref();
		PL_put_variable(tSymbol1);
        if ( !PL_put_float(tDegree, 1.0) ) {
            printf("ERROR (closure module:create_equations): line 279, adding the generic reflexive equation \n");
        };
        if ( !PL_cons_functor(tHead, fRel, tSymbol1, tSymbol1, tDegree) ) {
            printf("ERROR (closure module:create_equations): line 282, adding the generic reflexive equation \n");
        };
        if ( !PL_cons_list(tEqList, tHead, tEqList) ) {
            printf("ERROR (closure module:create_equations): line 285, adding the generic reflexive equation \n");
        };
	}
}


/* Looks for a specific term in a list of terms and returns its 0-based
   position in the list. If the term it's not found, -1 is returned.    */
static int search_term(term_t tTerm, array *pmListTerms, int nSize) {
	int nIndex;
	int i;

	nIndex = -1;
	for(i = 0; nIndex == -1 && i < nSize; i++) {
		if(PL_unify((term_t)array_get(pmListTerms, 0, i), tTerm)) {
			nIndex = i;
		}
	}

	return nIndex;
}


/* DEBUGGING */
static void print_adjacency_matrix(array *pmAdjMatrix, int *pnSize) {
    int i, j;
    double dItem;
    printf("Rows pmAdjMatrix: %d\n", *pnSize);
    for (i = 0; i < (*pnSize); i++) {
        printf("Row: %d\n", i);
        for(j = 0; j < pmAdjMatrix->nColumns; j++) {
            dItem = array_get_double(pmAdjMatrix, i, j);
            printf("%f ", dItem);
        }
        printf("\n");
    }
}
