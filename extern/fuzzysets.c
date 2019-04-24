/***********************************************************************
 * Computation of the similarity degree between fuzzy sets
 */

#include <SWI-Prolog.h>
#include <SWI-Stream.h>
#include <string.h>
#include <math.h>
#include <stdio.h>
#include "fuzzysets.h"
#include "array.h"
#include "bool.h"

/***********************************************************************/



/***********************************************************************
 * Declarations
 ***********************************************************************/

/* Private function prototypes */
static subset create_subset(domain *poDomain, term_t tSubset, term_t tSubsetList);
static double modifier_factor(char *sModifier);
static subset search_subset(domain *poDomain, char *sName, term_t tSubsetList);
static int search_subset_in_array(subset *oSubset, array *mArray, int nTotalSubsets);
static double membership(subset *poSubconj, double dValor);
static double similarity(domain *poDomain, subset *poSubset1, subset *poSubset2);
static double necessity(domain *poDomain, subset *poSubset1, subset *poSubset2);
static double possibility(domain *poDomain, subset *poSubset1, subset *poSubset2);
static double possibility_compl(domain *poDomain, subset *poSubset1, subset *poSubset2);



/***********************************************************************
 * Public functions
 ***********************************************************************/


/** ext_translate_fuzzysets(+Domain, +Subsets, +NewSubsets, +RelationName, -Equations)
 *
 *    Computes the similarity degree between the fuzzy subsets of the
 *    NewSubsets list and the fuzzy subsets of the Subsets list, and
 *    returns a list of equations that represent a fuzzy relation with
 *    the subsets in NewSubsets.
 *    Domain must be a list with four items: [Name, Min, Max,
 *    MeasureUnit]. Output equations will be like "rel(a, b, 0.5)",
 *    where "rel" is RelationName atom, and "a"/"b" are subsets' names.
 *    Finally, these are the syntax of the valid fuzzy subsets:
 *     * name(A, B, C, D): trapezoidal subset.
 *     * name(A, B, C): triangular subset.
 *     * name(point(X)): domain point.
 *     * name(about(X)): fuzzy domain point.
 *     * name(between(X, Y)): domain range.
 *     * name(about(X, Y)): fuzzy domain range.
 *     * name(modifier(Subset)): modifier subset (valid modifiers are
 *       "very", "extremely", "more_or_less", and "somewhat").
 */
foreign_t pl_translate_fuzzysets(term_t tDomain, term_t tSubsets,
	                             term_t tNewSubsets, term_t tRelationName,
	                             term_t tEquations) {
	array mAdjMatrix, mSubsets, mSubsetIsNew;
	domain oDomain;
	subset *poSubset, *poNewSubset;
	subset *poFirstSubset, *poSecondSubset;
	term_t tTail, tHead;
	term_t tEqList;
	term_t tDegree;
	atom_t aRelationName;
	functor_t fRel;
	double dValue;
	int nIndex, nTotalSubsets;
	int i, j;

	/* checks that arguments have the expected types */
	if(!PL_is_list(tDomain) || !PL_is_list(tSubsets)
	  || !PL_is_list(tNewSubsets) || !PL_is_atom(tRelationName)) {
		PL_fail;
	}

	/* creates adjacency matrix and lists of subsets */
	nTotalSubsets = 0;
	create_array(&mAdjMatrix, INI_ADJ_MATRIX_SIZE, INI_ADJ_MATRIX_SIZE, DOUBLE_ARRAY);
	create_array(&mSubsets, 1, INI_SUBSET_LIST_SIZE, POINTER_ARRAY);
	create_array(&mSubsetIsNew, 1, INI_SUBSET_LIST_SIZE, INTEGER_ARRAY);

	/* extracts the minimum and maximum values of the domain (these
	   values are stored in the 2nd and 3rd items of the list)      */
	tTail = PL_copy_term_ref(tDomain);
	tHead = PL_new_term_ref();
    if ( !PL_get_list_ex(tTail, tHead, tTail) ) {
        printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 90. \n");
        PL_fail;
    };
    if ( !PL_get_list_ex(tTail, tHead, tTail) ) {
        printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 94. \n");
        PL_fail;
    };
    if ( !PL_get_float_ex(tHead, &dValue) ) {
        printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 98. \n");
        PL_fail;
    };
	oDomain.dMinimum = dValue;
    if ( !PL_get_list_ex(tTail, tHead, tTail) ) {
        printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 103. \n");
        PL_fail;
    };
    if ( !PL_get_float_ex(tHead, &dValue) ) {
        printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 107. \n");
        PL_fail;
    };
	oDomain.dMaximum = dValue;

	/* reads list of subsets */
	tTail = PL_copy_term_ref(tSubsets);
	tHead = PL_new_term_ref();
	while(PL_get_list(tTail, tHead, tTail)) {
		/* creates and adds a new 'subset' structure to the
		   list of subsets                                  */
		poSubset = malloc(sizeof(subset));
		*poSubset = create_subset(&oDomain, tHead, tSubsets);
		array_set(&mSubsets, 0, nTotalSubsets, poSubset);
		nTotalSubsets++;
	}

	/* reads list of new subsets */
	tTail = PL_copy_term_ref(tNewSubsets);
	tHead = PL_new_term_ref();
	while(PL_get_list(tTail, tHead, tTail)) {
		/* looks for this subset in the full list of subsets
		   and sets its 'new' mark (if subset is found)      */
		poNewSubset = malloc(sizeof(subset));
		*poNewSubset = create_subset(&oDomain, tHead, tNewSubsets);
		nIndex = search_subset_in_array(poNewSubset, &mSubsets, nTotalSubsets);
		if(nIndex != -1) {
			array_set_int(&mSubsetIsNew, 0, nIndex, TRUE);
		}
	}

	/* fills the adjacency matrix with the similarity degree between
	   each of the 'new' fuzzy subsets and the rest of the subsets   */
	for(i = 0; i < nTotalSubsets; i++) {
		for(j = 0; j < nTotalSubsets; j++) {
			if(array_get_int(&mSubsetIsNew, 0, i) || array_get_int(&mSubsetIsNew, 0, j)) {
				poFirstSubset = (subset*)array_get(&mSubsets, 0, i);
				poSecondSubset = (subset*)array_get(&mSubsets, 0, j);
				array_set_double(&mAdjMatrix, i, j,
				                 similarity(&oDomain, poFirstSubset, poSecondSubset));
			}
		}
	}

	/* initializes list of equations */
	tEqList = PL_new_term_ref();
	tHead = PL_new_term_ref();
	PL_put_nil(tEqList);
    if ( !PL_get_atom_ex(tRelationName, &aRelationName) ) {
        printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 156. \n");
        PL_fail;
    };
	fRel = PL_new_functor(aRelationName, 3);

	/* scans adjacency matrix, ignoring diagonal positions, zeros
	   and cells that weren't filled in the previous step         */
	for(i = 0; i < nTotalSubsets; i++) {
		for(j = 0; j < nTotalSubsets; j++) {
			if((array_get_int(&mSubsetIsNew, 0, i) || array_get_int(&mSubsetIsNew, 0, j))
			 && i != j && array_get_double(&mAdjMatrix, i, j) > 0.0) {
				/* builds the equation and adds it to list */
				poFirstSubset = (subset*)array_get(&mSubsets, 0, i);
				poSecondSubset = (subset*)array_get(&mSubsets, 0, j);
				tDegree = PL_new_term_ref();
                if ( !PL_put_float(tDegree, array_get_double(&mAdjMatrix, i, j)) ) {
                    printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 172. \n");
                    PL_fail;
                };
				if ( !PL_cons_functor(tHead, fRel, poFirstSubset->tName, poSecondSubset->tName, tDegree) ) {
                    printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 176. \n");
                    PL_fail;
                };
                if ( !PL_cons_list(tEqList, tHead, tEqList) ) {
                    printf("ERROR (module fuzzysets: pl_translate_fuzzysets): line 180. \n");
                    PL_fail;
                };
			}
		}
	}

	/* frees memory used by dynamic arrays */
	destroy_array(&mAdjMatrix);
	destroy_array_and_contents(&mSubsets);
	destroy_array(&mSubsetIsNew);

	/* returns list of equations */
	if(!PL_unify(tEqList, tEquations)) {
		PL_fail;
	}

	PL_succeed;
}



/***********************************************************************
 * Private functions
 ***********************************************************************/


/* Creates a new 'subset' structure that represents the same fuzzy subset
   as a Prolog term. This term must have the following syntax:
   "<name>(<definition>)", where <name> is the name of the subset and
   <definition> is another term that defines the subset.                  */
static subset create_subset(domain *poDomain, term_t tSubset, term_t tSubsetList) {
	subset oNewSubset, oExistingSubset;
	atom_t aSubsetName, aModifier;
	term_t tSubsetName, tSubsetModifier, tExistingSubsetName, tValue;
	char *sModifier, *sExistingSubsetName;
	double dValue, dFactor;
	int nIntValue, nNValues, nNValuesMod;

	tSubsetName = PL_new_term_ref();
	tSubsetModifier = PL_new_term_ref();
	tExistingSubsetName = PL_new_term_ref();
	tValue = PL_new_term_ref();

	/* saves the name of the subset */
    if ( !PL_get_name_arity(tSubset, &aSubsetName, &nNValues) ) {
        printf("ERROR (module fuzzysets: create_subset): line 226. \n");
    };
	PL_put_atom(tSubsetName, aSubsetName);
	oNewSubset.tName = tSubsetName;

	switch(nNValues) {

	/* if subset is defined with 4 terms, it's a trapezoidal fuzzy subset */
	case 4:
		oNewSubset.nType = SUBSET_TRAPEZOIDAL;
        if ( !PL_get_arg(1, tSubset, tValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 237. \n");
        };
        if ( !PL_get_float_ex(tValue, &dValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 240. \n");
        };
		oNewSubset.uDefinition.oTrapezoidal.dValueA = dValue;
        if ( !PL_get_arg(2, tSubset, tValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 244. \n");
        };
        if ( !PL_get_float_ex(tValue, &dValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 247. \n");
        };
		oNewSubset.uDefinition.oTrapezoidal.dValueB = dValue;
        if ( !PL_get_arg(3, tSubset, tValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 252. \n");
        };
        if ( !PL_get_float_ex(tValue, &dValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 254. \n");
        };
		oNewSubset.uDefinition.oTrapezoidal.dValueC = dValue;
        if ( !PL_get_arg(4, tSubset, tValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 258. \n");
        };
        if ( !PL_get_float_ex(tValue, &dValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 261. \n");
        };
		oNewSubset.uDefinition.oTrapezoidal.dValueD = dValue;
		break;

	/* if subset is defined with 3 terms, it's a triangular fuzzy subset */
	case 3:
		oNewSubset.nType = SUBSET_TRIANGULAR;
        if ( !PL_get_arg(1, tSubset, tValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 270. \n");
        };
        if ( !PL_get_float_ex(tValue, &dValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 273. \n");
        };
		oNewSubset.uDefinition.oTriangular.dValueA = dValue;
        if ( !PL_get_arg(2, tSubset, tValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 277. \n");
        };
        if ( !PL_get_float_ex(tValue, &dValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 280. \n");
        };
		oNewSubset.uDefinition.oTriangular.dValueB = dValue;
        if ( !PL_get_arg(3, tSubset, tValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 284. \n");
        };
        if ( !PL_get_float_ex(tValue, &dValue) ) {
            printf("ERROR (module fuzzysets: create_subset): line 287. \n");
        };
		oNewSubset.uDefinition.oTriangular.dValueC = dValue;
		break;

	/* if subset is defined with just one term, subset is a special
	   modifier subset, such as 'very(young)' or 'between(20, 50)'  */
	case 1:

		/* retrieves the name of the modifier */
        if ( !PL_get_arg(1, tSubset, tSubsetModifier) ) {
                printf("ERROR (module fuzzysets: create_subset): line 297. \n");
        };
        if ( !PL_get_name_arity(tSubsetModifier, &aModifier, &nNValuesMod) ) {
                printf("ERROR (module fuzzysets: create_subset): line 301. \n");
        };
		sModifier = (char*)PL_atom_chars(aModifier);

		/* creates the fuzzy subset according to the modifier found */
		if(strcmp(sModifier, "point") == 0 && nNValuesMod == 1) {
			/* subset is a domain point */
			oNewSubset.nType = SUBSET_POINT;
            if (!PL_get_arg(1, tSubsetModifier, tValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 310. \n");
            };
            if ( !PL_get_integer_ex(tValue, &nIntValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 313. \n");
            };
			oNewSubset.uDefinition.oPoint.nValue = nIntValue;

		} else if(strcmp(sModifier, "about") == 0 && nNValuesMod == 1) {
			/* subset is a fuzzy domain point; these subsets are
			   stored as triangular fuzzy subsets                */
			oNewSubset.nType = SUBSET_TRIANGULAR;
			dFactor = ((poDomain->dMaximum - poDomain->dMinimum) * 2.5) / 100;
            if ( !PL_get_arg(1, tSubsetModifier, tValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 323. \n");
            };
            if ( !PL_get_integer_ex(tValue, &nIntValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 326. \n");
            };
			oNewSubset.uDefinition.oTriangular.dValueA =
				MAX(poDomain->dMinimum, nIntValue - dFactor);
			oNewSubset.uDefinition.oTriangular.dValueB =
				nIntValue;
			oNewSubset.uDefinition.oTriangular.dValueC =
				MIN(poDomain->dMaximum, nIntValue + dFactor);

		} else if(strcmp(sModifier, "between") == 0 && nNValuesMod == 2) {
			/* subset is a domain range; these subsets are stored
			   as trapezoidal fuzzy subsets                       */
			oNewSubset.nType = SUBSET_TRAPEZOIDAL;
            if ( !PL_get_arg(1, tSubsetModifier, tValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 340. \n");
            };
            if ( !PL_get_integer_ex(tValue, &nIntValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 343. \n");
            };
			oNewSubset.uDefinition.oTrapezoidal.dValueA = nIntValue;
			oNewSubset.uDefinition.oTrapezoidal.dValueB = nIntValue;
            if ( !PL_get_arg(2, tSubsetModifier, tValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 248. \n");
            };
            if ( !PL_get_integer_ex(tValue, &nIntValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 351. \n");
            };
			oNewSubset.uDefinition.oTrapezoidal.dValueC = nIntValue;
			oNewSubset.uDefinition.oTrapezoidal.dValueD = nIntValue;

		} else if(strcmp(sModifier, "about") == 0 && nNValuesMod == 2) {
			/* subset is a fuzzy domain range; these subsets are
			   stored as trapezoidal fuzzy subsets               */
			oNewSubset.nType = SUBSET_TRAPEZOIDAL;
			dFactor = ((poDomain->dMaximum - poDomain->dMinimum) * 2.5) / 100;
            if ( !PL_get_arg(1, tSubsetModifier, tValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 362. \n");
            };
            if ( !PL_get_integer_ex(tValue, &nIntValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 365. \n");
            };
			oNewSubset.uDefinition.oTrapezoidal.dValueA =
				MAX(poDomain->dMinimum, nIntValue - dFactor);
			oNewSubset.uDefinition.oTrapezoidal.dValueB = nIntValue;
            if ( !PL_get_arg(2, tSubsetModifier, tValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 371. \n");
            };
            if ( !PL_get_integer_ex(tValue, &nIntValue) ) {
                printf("ERROR (module fuzzysets: create_subset): line 374. \n");
            };
			oNewSubset.uDefinition.oTrapezoidal.dValueC = nIntValue;
			oNewSubset.uDefinition.oTrapezoidal.dValueD =
				MIN(poDomain->dMaximum, nIntValue + dFactor);

		} else if(modifier_factor(sModifier) != 0.0 && nNValuesMod == 1) {
			/* subset is defined using a modifier and an existing subset */
            if ( !PL_get_arg(1, tSubsetModifier, tExistingSubsetName) ) {
                printf("ERROR (module fuzzysets: create_subset): line 383. \n");
            };
            if ( !PL_get_atom_chars(tExistingSubsetName, &sExistingSubsetName) ) {
                printf("ERROR (module fuzzysets: create_subset): line 386. \n");
            };
			oExistingSubset = search_subset(poDomain, sExistingSubsetName, tSubsetList);
			if(oExistingSubset.nType == SUBSET_TRAPEZOIDAL) {
				/* copies the trapezoidal subset and adds the exponent */
				oNewSubset.nType = SUBSET_TRAPEZOIDAL_MODIFIER;
				oNewSubset.uDefinition.oTrapezoidalMod.dExponent =
					modifier_factor(sModifier);
				oNewSubset.uDefinition.oTrapezoidalMod.dValueA =
					oExistingSubset.uDefinition.oTrapezoidal.dValueA;
				oNewSubset.uDefinition.oTrapezoidalMod.dValueB =
					oExistingSubset.uDefinition.oTrapezoidal.dValueB;
				oNewSubset.uDefinition.oTrapezoidalMod.dValueC =
					oExistingSubset.uDefinition.oTrapezoidal.dValueC;
				oNewSubset.uDefinition.oTrapezoidalMod.dValueD =
					oExistingSubset.uDefinition.oTrapezoidal.dValueD;
			} else if(oExistingSubset.nType == SUBSET_TRIANGULAR) {
				/* copies the triangular subset and adds the exponent */
				oNewSubset.nType = SUBSET_TRIANGULAR_MODIFIER;
				oNewSubset.uDefinition.oTriangularMod.dExponent =
					modifier_factor(sModifier);
				oNewSubset.uDefinition.oTriangularMod.dValueA =
					oExistingSubset.uDefinition.oTriangular.dValueA;
				oNewSubset.uDefinition.oTriangularMod.dValueB =
					oExistingSubset.uDefinition.oTriangular.dValueB;
				oNewSubset.uDefinition.oTriangularMod.dValueC =
					oExistingSubset.uDefinition.oTriangular.dValueC;
			} else {
				oNewSubset.nType = SUBSET_UNKNOWN;
			}

		} else {
			/* unknown subset */
			oNewSubset.nType = SUBSET_UNKNOWN;

		}
		break;

	/* unknown subset */
	default:
		oNewSubset.nType = SUBSET_UNKNOWN;
		break;

	}

	return oNewSubset;
}


/* Returns the factor or exponent associated with a subset modifier. */
static double modifier_factor(char *sModifier) {
	double dFactor;

	dFactor = 0.0;
	if(strcmp(sModifier, "very") == 0) {
		dFactor = 2.0;
	} else if(strcmp(sModifier, "extremely") == 0) {
		dFactor = 3.0;
	} else if(strcmp(sModifier, "more_or_less") == 0) {
		dFactor = 1.0 / 2;
	} else if(strcmp(sModifier, "somewhat") == 0) {
		dFactor = 1.0 / 3;
	}

	return dFactor;
}


/* Looks for a subset with a certain name in a list of subsets and, if
   it exists, returns its full definition. If subset isn't found, a
   new subset with a SUBSET_UNKNOWN is returned.                       */
static subset search_subset(domain *poDomain, char *sName, term_t tSubsetList) {
	subset oSubset;
	term_t tHead, tTail;
	atom_t aName;
	char *sSubsetName;
	int nArity;
	bool bFound;

	tTail = PL_copy_term_ref(tSubsetList);
	tHead = PL_new_term_ref();

	/* an unknown subset will be returned if the subset isn't found */
	oSubset.nType = SUBSET_UNKNOWN;

	/* traverses list of subsets */
	bFound = FALSE;
	while(!bFound && PL_get_list(tTail, tHead, tTail)) {
		/* gets subset's name and compares it with the name passed as argument */
        if ( !PL_get_name_arity(tHead, &aName, &nArity) ) {
            printf("ERROR (module fuzzysets: search_subset): line 476. \n");
        };
		sSubsetName = (char*)PL_atom_chars(aName);
		if(strcmp(sSubsetName, sName) == 0) {
			/* returns a copy of the subset */
			oSubset = create_subset(poDomain, tHead, tSubsetList);
			bFound = TRUE;
		}
	}

	return oSubset;
}


/* Looks for a subset in an array of subsets (comparing just their
   names) and returns the index where the subset it's located. If
   subset isn't found, -1 is returned.                             */
static int search_subset_in_array(subset *oSubset, array *mArray, int nTotalSubsets) {
	subset *oCompSubset;
	char *sSubsetName, *sCompSubsetName;
	int nIndex;
	int i;

	/* traverses array of subsets */
	nIndex = -1;
	for(i = 0; nIndex == -1 && i < nTotalSubsets; i++) {
		oCompSubset = (subset*)array_get(mArray, 0, i);
        if ( !PL_get_atom_chars(oSubset->tName, &sSubsetName) ) {
            printf("ERROR (module fuzzysets: create_subset): line 504. \n");
        };
        if ( !PL_get_atom_chars(oCompSubset->tName, &sCompSubsetName) ) {
            printf("ERROR (module fuzzysets: create_subset): line 507. \n");
        };
		if(strcmp(sSubsetName, sCompSubsetName) == 0) {
			/* found subset */
			nIndex = i;
		}
	}
	
	return nIndex;
}


/* Returns the membership degree of a certain point to a fuzzy subset.
   This function checks the type of the subset in order to use the most
   appropiate function to compute the membership degree.                */
static double membership(subset *oSubset, double dValueX) {
	/* double dValueA, dValueB, dValueC, dValueD, dExponent; */
    double dValueA = 0.0, dValueB, dValueC, dValueD, dExponent;
	double dDegree;

	if(oSubset->nType == SUBSET_POINT) {

		/* if subset is a domain point, membership degree will be 1 only
		   if the specified point is exactly the same as the domain point */
		if(dValueX == oSubset->uDefinition.oPoint.nValue) {
			dDegree = 1.0;
		} else {
			dDegree = 0.0;
		}

	} else if(oSubset->nType == SUBSET_UNKNOWN) {

		/* unknown subsets should never be used in this function */
		dDegree = 0.0;

	} else {

		/* retrieves the values that define the fuzzy subset, and converts
		   them into a generic trapezoidal subset with a modifier factor,
		   which is the easiest way to handle any kind of subset           */
		if(oSubset->nType == SUBSET_TRAPEZOIDAL) {
			dValueA = oSubset->uDefinition.oTrapezoidal.dValueA;
			dValueB = oSubset->uDefinition.oTrapezoidal.dValueB;
			dValueC = oSubset->uDefinition.oTrapezoidal.dValueC;
			dValueD = oSubset->uDefinition.oTrapezoidal.dValueD;
			dExponent = 1.0;
		} else if(oSubset->nType == SUBSET_TRAPEZOIDAL_MODIFIER) {
			dValueA = oSubset->uDefinition.oTrapezoidalMod.dValueA;
			dValueB = oSubset->uDefinition.oTrapezoidalMod.dValueB;
			dValueC = oSubset->uDefinition.oTrapezoidalMod.dValueC;
			dValueD = oSubset->uDefinition.oTrapezoidalMod.dValueD;
			dExponent = oSubset->uDefinition.oTrapezoidalMod.dExponent;
		} else if(oSubset->nType == SUBSET_TRIANGULAR) {
			dValueA = oSubset->uDefinition.oTriangular.dValueA;
			dValueB = oSubset->uDefinition.oTriangular.dValueB;
			dValueC = oSubset->uDefinition.oTriangular.dValueB;
			dValueD = oSubset->uDefinition.oTriangular.dValueC;
			dExponent = 1.0;
		} else if(oSubset->nType == SUBSET_TRIANGULAR_MODIFIER) {
			dValueA = oSubset->uDefinition.oTriangularMod.dValueA;
			dValueB = oSubset->uDefinition.oTriangularMod.dValueB;
			dValueC = oSubset->uDefinition.oTriangularMod.dValueB;
			dValueD = oSubset->uDefinition.oTriangularMod.dValueC;
			dExponent = oSubset->uDefinition.oTriangularMod.dExponent;
		}

		/* computes the membership degree */
		if(dValueX <= dValueA) {
			if(dValueX == dValueA && dValueA == dValueB) {
				/* value is the initial point of the trapezoidal function
				   (in this case, first and second values are the same)   */
				dDegree = 1.0;
			} else {
				/* value is outside (to the left of) the trapezoidal function */
				dDegree = 0.0;
			}
		} else if(dValueX > dValueA && dValueX <= dValueB) {
			/* value is on the ascending side of the trapezoidal function */
			dDegree = POW((dValueX - dValueA) / (dValueB - dValueA), dExponent);
		} else if(dValueX > dValueB && dValueX <= dValueC) {
			/* value is inside the trapezoidal function */
			dDegree = 1.0;
		} else if(dValueX > dValueC && dValueX <= dValueD) {
			if(dValueC == dValueD) {
				/* value is the ending point of the trapezoidal function
				   (in this case, third and fourth values are the same)  */
				dDegree = 1.0;
			} else {
				/* value is on the descending side of the trapezoial function */
				dDegree = POW((dValueD - dValueX) / (dValueD - dValueC), dExponent);
			}
		} else {
			/* value is outside (to the right of) the trapezoidal function */
			dDegree = 0.0;
		}

	}

	return dDegree;
}


/* Returns the similarity degree between two fuzzy subsets that belong
   to the same domain/universe.                                        */
static double similarity(domain *poDomain, subset *poSubset1, subset *poSubset2) {
	double dDegree;

	if(necessity(poDomain, poSubset1, poSubset2) > 0.5) {
		dDegree = possibility(poDomain, poSubset1, poSubset2);
	} else {
		dDegree = (necessity(poDomain, poSubset1, poSubset2) + 0.5)
		          * possibility(poDomain, poSubset1, poSubset2);
	}

	return dDegree;
}


/* Returns a measure of the necessity between two fuzzy subsets. */
static double necessity(domain *poDomain, subset *poSubset1, subset *poSubset2) {
	return 1.0 - possibility_compl(poDomain, poSubset1, poSubset2);
}


/* Returns a measure of the possibility between two fuzzy subsets. */
static double possibility(domain *poDomain, subset *poSubset1, subset *poSubset2) {
	double dFirst, dLast;
	double dMaximum, dMinimum;
	double x;

	/* computes initial values */
	dFirst = poDomain->dMinimum;
	dLast = poDomain->dMaximum;
	dMaximum = MIN(membership(poSubset1, dFirst), membership(poSubset2, dFirst));
	dMinimum = dMaximum;

	/* gets the possibility */
	for(x = dFirst + 1.0; x <= dLast; x++) {
		dMinimum = MIN(membership(poSubset1, x), membership(poSubset2, x));
		if(dMaximum < dMinimum) {
			dMaximum = dMinimum;
		}
	}

	return dMaximum;
}


/* Returns a measure of the possibility between the complement of a
   fuzzy subset and another fuzzy subset.                           */
static double possibility_compl(domain *poDomain, subset *poSubset1, subset *poSubset2) {
	double dFirst, dLast;
	double dMaximum, dMinimum;
	double x;

	/* computes initial values */
	dFirst = poDomain->dMinimum;
	dLast = poDomain->dMaximum;
	dMaximum = MIN(1.0 - membership(poSubset1, dFirst), membership(poSubset2, dFirst));
	dMinimum = dMaximum;

	/* gets the possibility */
	for(x = dFirst + 1.0; x <= dLast; x++) {
		dMinimum = MIN(1.0 - membership(poSubset1, x), membership(poSubset2, x));
		if(dMaximum < dMinimum) {
			dMaximum = dMinimum;
		}
	}

	return dMaximum;
}

