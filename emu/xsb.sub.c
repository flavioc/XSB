/*=========================================================================*/

/*
 *		 Subsumptive Answer Check/Insert Operation
 *		 =========================================
 */


/*
 * Create an Empty Answer Set, represented as a Time-Stamped Trie.
 * Note that the root of the TST is labelled with a ret/n symbol,
 * where `n' is the number of terms in an answer.
 */

#ifdef SUBSUMPTION_XSB
inline static  void *newAnswerSet(CTXTdeclc int n, TSTNptr Parent)
#else
void *newTSTAnswerSet(void)
#endif /* SUBSUMPTION_XSB */
{
  TSTNptr root;
  
#ifdef SUBSUMPTION_XSB
  Cell symbol;
  if ( n > 0 )
    symbol = EncodeTriePSC(get_ret_psc(n));
  else
    symbol = EncodeTrieConstant(makestring(get_ret_string()));
  New_TSTN(root, TS_ANSWER_TRIE_TT, TRIE_ROOT_NT, symbol, Parent, NULL );
#else  
  New_TSTN(root, TST_TRIE_NT, TRIE_ROOT_NT, 0, NULL, NULL);
#endif /* SUBSUMPTION_XSB */
  
  TSTN_TimeStamp(root) = EMPTY_TST_TIMESTAMP;
  
  return root;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/*
 * Update statistical info, allocate an Answer Set object to house the
 * answer if the set was originally empty, conditionally insert the
 * answer into the set, and indicate to the caller whether it is new.
 */

inline
#ifdef SUBSUMPTION_XSB
static
#endif
TSTNptr subsumptive_answer_search(CTXTdeclc SubProdSF sf, int nTerms,
				  CPtr answerVector, xsbBool *isNew) {

  TSTNptr root, tstn;

#ifdef SUBSUMPTION_XSB
#ifndef MULTI_THREAD
  NumSubOps_AnswerCheckInsert++;
#else
#ifdef NON_OPT_COMPILE
  NumSubOps_AnswerCheckInsert++;
#endif
#endif
#endif /* SUBSUMPTION_XSB */

  AnsVarCtr = 0;
#ifdef SUBSUMPTION_XSB
  if ( IsNULL(subg_ans_root_ptr(sf)) )
    subg_ans_root_ptr(sf) = newAnswerSet(CTXTc nTerms, (TSTNptr) sf);
#endif /* SUBSUMPTION_XSB */
  root = (TSTNptr)subg_ans_root_ptr(sf);
  tstn = subsumptive_tst_search( CTXTc root, nTerms, answerVector, 
       (xsbBool)ProducerSubsumesSubgoals(sf), isNew );
       
#ifdef SUBSUMPTION_YAP
  Trail_Unwind_All;
#endif /* SUBSUMPTION_YAP */

#ifdef SUBSUMPTION_XSB
#ifndef MULTI_THREAD
  if ( *isNew )
    NumSubOps_AnswerInsert++;
#else
#ifdef NON_OPT_COMPILE
  if ( *isNew )
    NumSubOps_AnswerInsert++;
#endif
#endif
#endif /* SUBSUMPTION_XSB */
  return tstn;
}