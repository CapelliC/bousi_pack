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

/* Private function prototypes: maximal cliques */

static void maxCliques(double dLambdaCut, array *pmAdjMatrix, array *pmClique,
                       array *pmPosCandidate, array *pmXdiscarded, array *pmProxBlocks, int *pnRows);

static int choose_pivot(double dLambdaCut, array *pmAdjMatrix,
                        array *pmPosCandidate, array *pmXdiscarded);

static void neighbours(double dLambdaCut, int nVertex, array *pmAdjMatrix, array *pmLneighbours);


/* Private function prototypes for manipulating sets*/

static void fill_set(array *pmLItems, int nVal);

static void set_intersect(array *pmLItems1, array *pmLItems2, array *pmResult);

static void set_union(array *pmLItems1, array *pmLItems2, array *pmResult);

static void set_diff(array *pmLItems1, array *pmLItems2, array *pmResult);

static void set_comp(array *pmLItems1, array *pmResult);

static int set_is_empty(array *pmLItems);

static int set_Card(array *pmLItems);

static void print_matrix_ints(array *pmLItems);  /* FOR DEBUGING */

/** Private functions for managing the Proximity Blocks Matrix   *****/

static void fill_block_matrix(array *pmSetB, array *pmProxBlocks, int *pnRows);

static bool new_clique(array *pmClique, array *pmProxBlocks,  int *pnRows);

static bool equal_clique_block(array *pmClique, array *pmProxBlocks, int nRow);

static void print_block_matrix(array *pmProxBlocks, int *pnRows);  /* FOR DEBUGING */


/**  Private function for creating the extended fuzzy relation  *******/

static void create_equations_blocks(array *pmProxBlocks, array *pmAdjMatrix,
                                    array *pmListTerms, int nRows, atom_t aPrefix, term_t tEqList);

/**  Private function for creating the list of proximity blocks (maximal cliques)  *******/

static void create_block_sets(array *pmProxBlocks, array *pmListTerms, int nRows,
                              atom_t aPrefix, term_t tBlockList);




/***********************************************************************
 * Public functions
 ***********************************************************************/

/** ext_block_equs(+LambdaCut, +InputEquations, +RelationName, -OutputEquations)
 *
 *    First, computes the reflexive, symmetric closure of the fuzzy relation defined
 *    by a set of input equations.
 *
 *    Then, computes the bolcks matrix, which stores the
 *    sets of proximity blocks. Finally, it returns the list of extended equations of
 *    the resulting fuzzy relation .
 *
 *    Input equations must be like "rel(a, b, 0.5)", where "rel" can be any
 *    functor; output equations will be similar but replacing "rel"
 *    with RelationName atom and extending the arity with a new argument storing
 *    the information on the block constrain of the first two arguments.
 *
 */
foreign_t pl_block_equs(term_t tLambdaCut, term_t tInputEquations,
                    term_t tRelationName, term_t tOutputEquations) {
    array mAdjMatrix, /* Adjacency matrix representing the input fuzzy relation */
          mListTerms; /* vector relating the symbols in the domain fuzzy relation with and index */
    int nSize; /* Size of the matrix mAdjMatrix (nSize x nSize) and length of the vector mListTerms */
    double dLambdaCut;
    
    array mProxBlocks; /* Proximity Blocks matrix storing the maximal cliques of the input fuzzy relation */
    int nRowsPBM = 0; /* Row number index of the matrix mProxBlocks */
    array mClique, mPosCandidate, mXdiscarded;
    
    term_t tEquations;
    atom_t aPrefix;

    
    /* checks whether the arguments have the expected types */
    if(!PL_is_atom(tRelationName) || !PL_is_list(tInputEquations) || !PL_is_number(tLambdaCut)) {
        PL_fail;
    }
    
    /* creates adjacency matrix and list of terms */
    nSize = 0;
    create_array(&mListTerms, 1, INI_ADJ_MATRIX_SIZE, POINTER_ARRAY);
    create_array(&mAdjMatrix, INI_ADJ_MATRIX_SIZE, INI_ADJ_MATRIX_SIZE, DOUBLE_ARRAY);
    fill_adjacency_matrix(tInputEquations, &mAdjMatrix, &mListTerms, &nSize);
    
   
    
    /**********    BRON-KERBOSCH ALGORITHM     **********/
    
    /* Inicialitation:  creates the matrices which will be used for computing maximal cliques
     and fill then with NULLs (POINTER_ARRAY) or zeros (INTEGER_ARRAY) */
    create_array(&mClique, 1, nSize, INTEGER_ARRAY);
    create_array(&mPosCandidate, 1, nSize, INTEGER_ARRAY);
    create_array(&mXdiscarded, 1, nSize, INTEGER_ARRAY);
    create_array(&mProxBlocks, INI_ADJ_MATRIX_SIZE, INI_ADJ_MATRIX_SIZE, POINTER_ARRAY);
    
    /* Inicialitation:  mPosCandidate is equal to the set of 'vertices' of the fuzzy relation*/
    fill_set(&mPosCandidate, 1);
    
    if (!PL_get_float_ex(tLambdaCut, &dLambdaCut) ) PL_fail;
    if (dLambdaCut == 0) dLambdaCut = dLambdaCut + 0.000001;
    /* The last instruction is a trick to avoid a repeated verification of
     the dLambdaCut value */
    
    maxCliques(dLambdaCut, &mAdjMatrix, &mClique, &mPosCandidate, &mXdiscarded, &mProxBlocks, &nRowsPBM);
    
    /*******************************************************/
    
    /* creates the list of extended (proximity) equations from the Proximity Blocks matrix
     (using the adjacency matrix) */
    tEquations = PL_new_term_ref();
    if ( !PL_get_atom_ex(tRelationName, &aPrefix) ) PL_fail;
    create_equations_blocks(&mProxBlocks, &mAdjMatrix, &mListTerms, nRowsPBM, aPrefix,
                     tEquations);
    
    /* frees memory used by dynamic arrays */
    destroy_array(&mListTerms);
    destroy_array(&mAdjMatrix);
    destroy_array(&mProxBlocks);
    destroy_array(&mClique);
    destroy_array(&mPosCandidate);
    destroy_array(&mXdiscarded);
    
    /* returns list of equations */
    if(!PL_unify(tEquations, tOutputEquations)) {
        PL_fail;
    }
    
    PL_succeed;
}



/** ext_block_sets(+LambdaCut, +InputEquations, +RelationName, -OutputBlockSets)
 *
 *    Works like 'ext_blocks' except because it returns the set of labeled
 *    proximity blocks (maximal cliques) through the parameter 'OutputBlockSets'.
 *
 *    The structure of 'OutputBlockSets' is as follows:
 *           [block(0, ListElementsB0), ... , block(n, ListElementsBn)]
 *
 */
foreign_t pl_block_sets(term_t tLambdaCut, term_t tInputEquations,
                    term_t tRelationName, term_t OutputBlockSets) {
    array mAdjMatrix, /* Adjacency matrix representing the input fuzzy relation */
    mListTerms; /* vector relating the symbols in the domain fuzzy relation with and index */
    int nSize; /* Size of the matrix mAdjMatrix (nSize x nSize) and length of the vector mListTerms */
    double dLambdaCut;
    
    array mProxBlocks; /* Proximity Blocks matrix storing the maximal cliques of the input fuzzy relation */
    int nRowsPBM = 0; /* Rows of the matrix mProxBlocks */
    array mClique, mPosCandidate, mXdiscarded;
    
    term_t tBlockSets;
    atom_t aPrefix;
    
    
    /* checks that arguments have the expected types */
    if(!PL_is_atom(tRelationName) || !PL_is_list(tInputEquations) || !PL_is_number(tLambdaCut)) {
        PL_fail;
    }
    
    /* creates adjacency matrix and list of terms */
    nSize = 0;
    create_array(&mListTerms, 1, INI_ADJ_MATRIX_SIZE, POINTER_ARRAY);
    create_array(&mAdjMatrix, INI_ADJ_MATRIX_SIZE, INI_ADJ_MATRIX_SIZE, DOUBLE_ARRAY);
    fill_adjacency_matrix(tInputEquations, &mAdjMatrix, &mListTerms, &nSize);
    
    /* Builds the reflexive, symmetric closure. The closure type is fixed by the value of the
     *     third argument, which must be a combination of one or more of these flags:
     *     * 1: Reflexive
     *     * 2: Symmetric
     *     * 4: Transitive (NOT USED HERE)
     *    TNorm must be one of these values:
     *     * 1: Minimum
     *     * 2: Product
     *     * 3: Lukasiewicz
     *     *-1: TNM_NOT_USED
     */
    build_closure(&mAdjMatrix, nSize, CLS_SYMMETRIC, TNM_NOT_USED);
    
    
    /**********    BRON-KERBOSCH ALGORITHM     **********/
    
    /* Inicialitation:  creates the matrices which will be used for computing maximal cliques
     and fill then with NULLs (POINTER_ARRAY) or zeros (INTEGER_ARRAY) */
    create_array(&mClique, 1, nSize, INTEGER_ARRAY);
    create_array(&mPosCandidate, 1, nSize, INTEGER_ARRAY);
    create_array(&mXdiscarded, 1, nSize, INTEGER_ARRAY);
    create_array(&mProxBlocks, INI_ADJ_MATRIX_SIZE, INI_ADJ_MATRIX_SIZE, POINTER_ARRAY);
    
    /* Inicialitation:  mPosCandidate is equal to the set of 'vertices' of the fuzzy relation*/
    fill_set(&mPosCandidate, 1);
    
    if ( !PL_get_float_ex(tLambdaCut, &dLambdaCut) ) PL_fail;
    if (dLambdaCut == 0) dLambdaCut = dLambdaCut + 0.000001;
    /* The last instruction is a trick to avoid a repeated verification of
     the dLambdaCut value */

    maxCliques(dLambdaCut, &mAdjMatrix, &mClique, &mPosCandidate, &mXdiscarded, &mProxBlocks, &nRowsPBM);
    
    /*******************************************************/
    
    /* creates the list of labeled blocks from the Proximity Blocks matrix
     (using the adjacency matrix) */
    tBlockSets = PL_new_term_ref();
    if ( !PL_get_atom_ex(tRelationName, &aPrefix) ) PL_fail;
    create_block_sets(&mProxBlocks, &mListTerms, nRowsPBM, aPrefix, tBlockSets);
    
    /* frees memory used by dynamic arrays */
    destroy_array(&mListTerms);
    destroy_array(&mAdjMatrix);
    destroy_array(&mProxBlocks);
    destroy_array(&mClique);
    destroy_array(&mPosCandidate);
    destroy_array(&mXdiscarded);
    
    /* returns list of equations */
    if(!PL_unify(tBlockSets, OutputBlockSets)) {
        PL_fail;
    }
    
    PL_succeed;
}


/***********************************************************************
 * Private functions
 ***********************************************************************/


/***********************************************************************
 * A variant of the Bron-Kerbosch algorithm with pivoting
 ***********************************************************************
 
 Procedure maxCliques(C, P, X);
    if P=∅andX=∅ then
        Report C as a maximal clique (λ-block) and label it as B_i;
        B ← B ∪ {(B_i, C)}; i ← i + 1;
    endif;
    Choose a pivot u ∈ P ∪ X in order to maximize |P ∩ Γλ(u)|; (Tomita et al.)
    for each vertex v ∈ P \ Γλ(u) do
        P ← P \ {v}; <<<< must be done before launching maxCliques, not after <<<
                     <<<< otherwise we will compute redundant proximity blocks
        maxCliques(C ∪ {v}, P ∩ Γλ(v), X ∩ Γλ(v));
        X ← X ∪ {v};
    endfor
 end
 ***********************************************************************/
 static void maxCliques(double dLambdaCut, array *pmAdjMatrix, array *pmClique,
                array *pmPosCandidate, array *pmXdiscarded, array *pmProxBlocks, int *pnRows) {
     array mC, mP, mX;
     create_copy_array(pmClique, &mC);
     create_copy_array(pmPosCandidate, &mP);
     create_copy_array(pmXdiscarded, &mX);
     
     int nSize = pmPosCandidate->nColumns;
     int v;

     if (set_is_empty(&mP) && set_is_empty(&mX)) {
         /* Report *pmClique as a maximal clique (Lambda-block) and label it as *pnRows.
          The clique is added to the proximity blocks matrix *pmProxBlocks */
         fill_block_matrix(&mC, pmProxBlocks, pnRows);
     } else {
         int nPivot;
         array mLneighbours, mDifference;
         create_array(&mLneighbours, 1, nSize, INTEGER_ARRAY);
         create_array(&mDifference, 1, nSize, INTEGER_ARRAY);
         
         /* Choose a pivot u ∈ P ∪ X in order to maximize |P ∩ Γλ(u)|; (Tomita et al.) */
         nPivot = choose_pivot(dLambdaCut, pmAdjMatrix, &mP, &mX);
         neighbours(dLambdaCut, nPivot, pmAdjMatrix, &mLneighbours); /* Γλ(u) */

         /* for each vertex v ∈ P \ Γλ(u) do */
         set_diff(pmPosCandidate, &mLneighbours, &mDifference);
         
         for (v=0; v< (mDifference.nColumns); v++) {
             /* v ∈ P \ Γλ(u) if the content of the array mDifference is set to 1 */
             if (array_get_int(&mDifference, 0, v)) {
                 array mLneighbours_v, mCv, mPN, mXN;
                 create_array(&mLneighbours_v, 1, nSize, INTEGER_ARRAY);
                 create_array(&mCv, 1, nSize, INTEGER_ARRAY);
                 create_array(&mPN, 1, nSize, INTEGER_ARRAY);
                 create_array(&mXN, 1, nSize, INTEGER_ARRAY);

                 array_set_int(&mP, 0, v, 0); /* P ← P \ {v}; */

                 array_set_int(&mCv, 0, v, 1); set_union(&mC, &mCv, &mCv); /* C ∪ {v} */
                 neighbours(dLambdaCut, v, pmAdjMatrix, &mLneighbours_v); /* Γλ(v) */
                 /* set_intersect(pmPosCandidate, &mLneighbours_v, &mPN); */ /* P ∩ Γλ(v) */
                 /* set_intersect(pmXdiscarded, &mLneighbours_v, &mXN); */ /* X ∩ Γλ(v) */
                 set_intersect(&mP, &mLneighbours_v, &mPN); /* P ∩ Γλ(v) */
                 set_intersect(&mX, &mLneighbours_v, &mXN); /* X ∩ Γλ(v) */

                 /* maxCliques(C ∪ {v}, P ∩ Γλ(v), X ∩ Γλ(v)); */
                 maxCliques(dLambdaCut, pmAdjMatrix, &mCv, &mPN, &mXN, pmProxBlocks, pnRows);
             
                 /* array_set_int(&mP, 0, v, 0); */  /* P ← P \ {v}; */
                 array_set_int(&mX, 0, v, 1); /* X ← X ∪ {v} */
             
                 destroy_array(&mCv);
                 destroy_array(&mPN);
                 destroy_array(&mXN);
             }
         }
         destroy_array(&mLneighbours);
         destroy_array(&mDifference);
     }
     destroy_array(&mC);
     destroy_array(&mP);
     destroy_array(&mX);
 }

/* Choose a pivot u ∈ P ∪ X in order to maximize |P ∩ Γλ(u)|; (Tomita et al.) */
static int choose_pivot(double dLambdaCut, array *pmAdjMatrix,
                        array *pmPosCandidate, array *pmXdiscarded) {
    int u, nPivot = 0;
    int nCardPNu, nMaxCard = 0;
    array mPX, mPNu, mLneighbours_u; /** mPNu,  **/
    int nSize = pmPosCandidate->nColumns;
    create_array(&mPX, 1, nSize, INTEGER_ARRAY);
    create_array(&mPNu, 1, nSize, INTEGER_ARRAY);
    create_array(&mLneighbours_u, 1, nSize, INTEGER_ARRAY);
    
    /* for each vertex u ∈ P ∪ X do */
    set_union(pmPosCandidate, pmXdiscarded, &mPX);
    for (u=0; u < (mPX.nColumns); u++) {
        if (array_get_int(&mPX, 0, u)) {
            neighbours(dLambdaCut, u, pmAdjMatrix, &mLneighbours_u); /* Γλ(u) */
            set_intersect(pmPosCandidate, &mLneighbours_u, &mPNu); /* P ∩ Γλ(u) */
            if ((nCardPNu=set_Card(&mPNu)) >= nMaxCard) { /* if |P ∩ Γλ(u)| > nMaxCard */
                nMaxCard = nCardPNu;
                nPivot = u;
            }
        }
    }
    destroy_array(&mLneighbours_u);
    destroy_array(&mPNu);
    destroy_array(&mPX);

    return nPivot;
}

static void neighbours(double dLambdaCut, int nVertex, array *pmAdjMatrix, array *pmLneighbours) {
    int i;
    int nSize = pmLneighbours->nColumns;
    
    /* reads pmAdjMatrix and sets pmLneighbours*/
    for (i = 0; i < nSize; i++) {
     if (array_get_double(pmAdjMatrix, nVertex, i) >= dLambdaCut && i != nVertex) {
            array_set_int(pmLneighbours, 0, i, 1);
     } else {
            array_set_int(pmLneighbours, 0, i, 0);
     }
    }
}
    

/***********************************************************************
 * Private functions for managing sets
 ***********************************************************************
 
 Procedures that implement operations on sets.
 The sets are represented as boolean vectors (arrays / unidimensional matrices)
 which store the characteristic function that defines a set. Each index
 corresponds to one of the elements of the universal set with which we work.
 If the element does not belong to the set, a 0 is stored, if it belongs, we
 store 1.
 ***********************************************************************/

/* Initiallizes a set (an unidemensional array) using the specified integer
 value provided by the second argument. The array must be created when
 calling to this function.              */

static void fill_set(array *pmLItems, int nVal) {
    int i;
    
    for (i = 0; i < pmLItems->nColumns; i++) {
        array_set_int(pmLItems, 0, i, nVal);
    }
    
}

static void set_intersect(array *pmLItems1, array *pmLItems2, array *pmResult) {
    int i, nVal1, nVal2;
    
    /* reads the original list of terms */
    for (i = 0; i < pmLItems1->nColumns; i++) {
        nVal1 = array_get_int(pmLItems1, 0, i);
        nVal2 = array_get_int(pmLItems2, 0, i);
        array_set_int(pmResult, 0, i, nVal1 && nVal2);
    }
    
}


static void set_union(array *pmLItems1, array *pmLItems2, array *pmResult) {
    int i, nVal1, nVal2;
    
    /* reads the original list of terms */
    for (i = 0; i < pmLItems1->nColumns; i++) {
        nVal1 = array_get_int(pmLItems1, 0, i);
        nVal2 = array_get_int(pmLItems2, 0, i);
        array_set_int(pmResult, 0, i, nVal1 || nVal2);
    }
    
}


static void set_diff(array *pmLItems1, array *pmLItems2, array *pmResult) {
    int i, nVal1, nVal2;
    
    /* reads the original list of terms */
    for (i = 0; i < pmLItems1->nColumns; i++) {
        nVal1 = array_get_int(pmLItems1, 0, i);
        nVal2 = array_get_int(pmLItems2, 0, i);
        array_set_int(pmResult, 0, i, nVal1 && !nVal2);
    }
    
}


static void set_comp(array *pmLItems1, array *pmResult) {
    int i, nVal1;
    
    /* reads the original list of terms */
    for (i = 0; i < pmLItems1->nColumns; i++) {
        nVal1 = array_get_int(pmLItems1, 0, i);
        array_set_int(pmResult, 0, i, !nVal1);
    }
    
}

static int set_is_empty(array *pmLItems) {
    int i, nVal;
    
    /* reads the original list of terms */
    for (i = 0; i < pmLItems->nColumns; i++) {
        nVal = array_get_int(pmLItems, 0, i);
        if (nVal != 0) return 0;
    }
    return 1;
}


static int set_Card(array *pmLItems) {
    int i, nCard = 0;
        
    /* reads the original list of terms */
    for (i = 0; i < pmLItems->nColumns; i++) {
            nCard = nCard + array_get_int(pmLItems, 0, i);
    }
    return nCard;
}


/***********************************************************************
 * Private functions for managing the Proximity Blocks Matrix
 ***********************************************************************
 The row indices represent proximity block numbers. One row is
 composed of the set of elements that belong to the proximity block.
 
 The input sets are represented as boolean vectors (arrays / unidemensional matrices)
 storing the characteristic function that defines the set. Each index
 corresponds to one of the elements of the universal set with which we work.
 If the element does not belong to the set, a 0 is stored, if it belongs to the
 set, 1 is stored.
 
 Finally, what we will store in the proximity blocks matrix will be the set of indices
 of the elements that belong to the proximity block and that will later allow us to redo
 the extended proximity equations (with the block number).
 ***********************************************************************/

/* Arrays must be initialized when calling to this function.
 ALMACENA SOLAMENTE los índices REPRESENTAN LOS elementos que forman los BLOQUES DE PROXIMIDAD
 (EN CADA FILA DE LA MATRIZ mProxBlocks)                                                        */
static void fill_block_matrix(array *pmSetB, array *pmProxBlocks, int *pnRows) {
    int nItem;
    int i=0, index;
    
    /* gets the indexes of the elements in pmSetB and stores them in the mProxBlocks matrix  */
    for (index = 0; index < pmSetB->nColumns; index++) {
        nItem = array_get_int(pmSetB, 0, index);
        if (nItem==1) {
            /* adds the new element (represented by index) to the block *pnRows */
            array_set_int(pmProxBlocks, *pnRows, i, index);
            i++;
        }
    }
    /* adds a pivot to signal out the termination of the file (array) */
    array_set_int(pmProxBlocks, *pnRows, i, -1);
    (*pnRows)++;
}

/* NOT USED ***************************************************************/
static bool new_clique(array *pmClique, array *pmProxBlocks, int *pnRows) {
    int i;
    
    i=0;
    while (i < *pnRows) {
        /* compares new_clique and block i */
        if (equal_clique_block(pmClique, pmProxBlocks, i)) return FALSE;
        i++;
    }
    return TRUE;
}

static bool equal_clique_block(array *pmClique, array *pmProxBlocks, int nRow) {
    int nItemC, nItemB;
    int i, j;
    
    i=0; j=0;
    /* compares new_clique and block nRow*/
    /* while there is elements in the block to compare (pivot -1 does not reached) */
    while ((nItemB = array_get_int(pmProxBlocks, nRow, i)) != -1) {
        /* skip elements not in pmClique */
        while (j < pmClique->nColumns && (nItemC = array_get_int(pmClique, 0, j)) != 1) j++;
        /* compare the index of the element in pmClique and the curent element of the mProxBlocks matrix  */
        if (j != nItemB || j == pmClique->nColumns) return FALSE;
        i++; j++;
    }
    return TRUE;
}

/*****************************************************************/

/* DEBUG */
static void print_block_matrix(array *pmProxBlocks, int *pnRows) {
    int i, j;
    int nItem;
    printf("Rows pmProxBlocks: %d\n", *pnRows);
    for (i = 0; i < (*pnRows); i++) {
        printf("Row: %d\n", i);
        for(j = 0; j < pmProxBlocks->nColumns; j++) {
            nItem = array_get_int(pmProxBlocks, i, j);
            printf("%d ", nItem);
        }
        printf("\n");
    }
}

static void print_matrix_ints(array *pmLItems) {
    int j;
    int nItem;
    
    for(j = 0; j < pmLItems->nColumns; j++) {
        nItem = array_get_int(pmLItems, 0, j);
        printf("%d ", nItem);
    }
    printf("\n");
}


    /***********************************************************************
     * Private function for creating the extended fuzzy relation using the
     * Proximity Blocks Matrix and the adjacency matrix
     ***********************************************************************/

    /* Creates and returns the list of equations that define the proximity relation
     from their proximity blocks matrix and using the adjacency matrix. It also uses
     the list of terms, pointed by pmListTerms, to establish the correspondence between
     the indexes (used internally) and the symbols of the domain of the fuzzy relation.
     
     REMARKS: All the equations will have the atom "aPrefix" as the main functor.
     They are extended (proximity) equations of arity four. This function gives the
     option to conditionally generate the reflexive inputs, setting the nClosureId
     parameter to the CLS_REFLEXIVE value.*/
    
    static void create_equations_blocks(array *pmProxBlocks, array *pmAdjMatrix,
        array *pmListTerms, int nRows, atom_t aPrefix, term_t tEqList) {
        term_t tHead;
        int nIndex1, nIndex2;
        double dDegree1, dDegree2;
        term_t tSymbol1, tSymbol2, tBlock, tDegree1, tDegree2;
        functor_t fRel;
        bool bReflexive;
        int i, j, k;
        
        /* initializes list of equations */
        fRel = PL_new_functor(aPrefix, 4);
        tHead = PL_new_term_ref();
        PL_put_nil(tEqList);
        
        
        /* scans proximity blocks matrix and generates the symmetric entries
         (If the relation must be reflexive, the reflexive equation is added later)  */
        for(i = 0; i < nRows; i++) { /* i is a block number */
            j = 0;
            while((nIndex1 = array_get_int(pmProxBlocks, i, j)) != -1) {
                k=j+1;
                while((nIndex2 = array_get_int(pmProxBlocks, i, k)) != -1) {
                    /* gets the elements of the equation */
                    tSymbol1 = (term_t)array_get(pmListTerms, 0, nIndex1);
                    tSymbol2 = (term_t)array_get(pmListTerms, 0, nIndex2);
                    tBlock = PL_new_term_ref();
                    if ( !PL_put_integer(tBlock, i) ) {
                        printf("ERROR (module blocks: create_equations_blocks): line 641. \n");
                        return;
                    };
                    dDegree1 = array_get_double(pmAdjMatrix, nIndex1, nIndex2);
                    /* builds the equation and adds it to list */
                    tDegree1 = PL_new_term_ref();
                    if ( !PL_put_float(tDegree1, dDegree1) ) {
                        printf("ERROR (module blocks: create_equations_blocks): line 648. \n");
                    };
                    if ( !PL_cons_functor(tHead, fRel, tSymbol1, tSymbol2, tBlock, tDegree1) ) {
                        printf("ERROR (module blocks: create_equations_blocks): line 651. \n");
                    };
                    if ( !PL_cons_list(tEqList, tHead, tEqList)  ) {
                        printf("ERROR (module blocks: create_equations_blocks): line 654. \n");
                    };
                    /* builds the symmetric equation and adds it to list */
                    dDegree2 = array_get_double(pmAdjMatrix, nIndex2, nIndex1);
                    /* builds the equation and adds it to list */
                    tDegree2 = PL_new_term_ref();
                    if ( !PL_put_float(tDegree2, dDegree2) ) {
                        printf("ERROR (module blocks: create_equations_blocks): line 648. \n");
                    };
                    if ( !PL_cons_functor(tHead, fRel, tSymbol2, tSymbol1, tBlock, tDegree2) ) {
                        printf("ERROR (module blocks: create_equations_blocks): line 658. \n");
                    };
                    if ( !PL_cons_list(tEqList, tHead, tEqList) ) {
                        printf("ERROR (module blocks: create_equations_blocks): line 661. \n");
                    };
                    k++;
                } /* end_while */
                j++;
            } /* end_while */
        } /* end_for */
    }


/***********************************************************************
 * Private function for creating the labeled proximity block sets using the
 * Proximity Blocks Matrix
 ***********************************************************************/

static void create_block_sets(array *pmProxBlocks, array *pmListTerms, int nRows,
                              atom_t aPrefix, term_t tLabeledBlockList) {
    int i, j, nIndex1;
    term_t tLabeledBlockHead;
    functor_t fRel;
    
    /* initializes the list of Labeled Blocks */
    fRel = PL_new_functor(aPrefix, 2);
    tLabeledBlockHead = PL_new_term_ref();
    PL_put_nil(tLabeledBlockList);
    
    /* scans proximity blocks matrix and generates the labeled list of blocks  */
    for(i = 0; i < nRows; i++) { /* i is a block number */
        j = 0;
        /* initializes list of elements of a Block */
        term_t tSymbol1, tnBlock, tElementsList;
        tElementsList = PL_new_term_ref();
        PL_put_nil(tElementsList);
        tnBlock = PL_new_term_ref();
        if ( !PL_put_integer(tnBlock, i) ) {
            printf("ERROR (module blocks: create_block_sets): line 716. \n");
        };
        /* generates elements of block i */
        while((nIndex1 = array_get_int(pmProxBlocks, i, j)) != -1) {
            /* gets the element of the block */
            tSymbol1 = (term_t)array_get(pmListTerms, 0, nIndex1);
            /* adds the element to the list */
            if ( !PL_cons_list(tElementsList, tSymbol1, tElementsList) ) {
                printf("ERROR (module blocks: create_block_sets): line 724. \n");
            };
            j++;
        } /* end_while */
        if ( !PL_cons_functor(tLabeledBlockHead, fRel, tnBlock, tElementsList) ) {
            printf("ERROR (module blocks: create_block_sets): line 729. \n");
        };
        if ( !PL_cons_list(tLabeledBlockList, tLabeledBlockHead, tLabeledBlockList) ) {
            printf("ERROR (module blocks: create_block_sets): line 732. \n");
        };
    } /* end_for */

}

