/***********************************************************************
 * Foreign library installer
 */

#include <SWI-Prolog.h>

/***********************************************************************/



/***********************************************************************
 * Declarations
 ***********************************************************************/

/* Foreign predicate definitions */
extern foreign_t pl_closure(term_t tInputEquations, term_t tClosure,
                            term_t tTNorm, term_t tRelationName, term_t tLambdaCut,
                            term_t tOutputEquations);

extern foreign_t pl_block_equs(term_t tLambdaCut, term_t tInputEquations,
                               term_t tRelationName, term_t tOutputEquations);

extern foreign_t pl_block_sets(term_t tLambdaCut, term_t tInputEquations,
                               term_t tRelationName, term_t OutputBlockSets);

extern foreign_t pl_translate_fuzzysets(term_t tDomain, term_t Subsets,
                                        term_t tNewSubsets, term_t RelationName,
                                        term_t tEquations);
extern foreign_t pl_tokenize(term_t tStringAtom, term_t tTokens);
extern foreign_t pl_read_shell_line(term_t tPrompt, term_t tString,
                                    term_t tArguments);
// extern foreign_t pl_load_shell_history(term_t tFile);
// extern foreign_t pl_save_shell_history(term_t tFile, term_t tMaxCommands);
extern foreign_t pl_set_system_predicate_list(term_t tList);
extern foreign_t pl_set_program_predicate_list(term_t tList);



/***********************************************************************
 * Public functions
 ***********************************************************************/


/** install
 *
 *    Registers foreign predicates in SWI-Prolog system. This function
 *    is automatically called when load_foreign_library/1 is used.
 */
install_t install() {
	PL_register_foreign("ext_closure", 6, pl_closure, 0);
  PL_register_foreign("ext_block_equs", 4, pl_block_equs, 0);
  PL_register_foreign("ext_block_sets", 4, pl_block_sets, 0);
	PL_register_foreign("ext_translate_fuzzysets", 5, pl_translate_fuzzysets, 0);
	PL_register_foreign("ext_tokenize", 2, pl_tokenize, 0);
	PL_register_foreign("ext_read_shell_line", 3, pl_read_shell_line, 0);
// PL_register_foreign("ext_load_shell_history", 1, pl_load_shell_history, 0);
//	PL_register_foreign("ext_save_shell_history", 2, pl_save_shell_history, 0);
	PL_register_foreign("ext_set_system_predicate_list", 1, pl_set_system_predicate_list, 0);
	PL_register_foreign("ext_set_program_predicate_list", 1, pl_set_program_predicate_list, 0);
};

