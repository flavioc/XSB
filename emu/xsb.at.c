
/*-------------------------------------------------------------------------*/

/*
 * Answer Template Creation
 * ------------------------
 * There are three ways that the answer template may be created during a
 * Subsumptive Call Check/Insert Operation:
 * (1) If a producing table entry is found during the lookup of a call,
 *     then the template appears in the first several entries of the
 *     TrieVarBindings[] array.
 * (2) If a subsuming, but non-producing, entry is found during call
 *     lookup, then the template for the call must be reconstructed
 *     based upon the producer associated with the found entry.
 * (3) If no subsuming entry is found, then the call is inserted into
 *     the trie, and the template exists on the (trie-specific) Trail.
 * See the file slginsts_xsb_i.h for the layout of the answer template.
 */

#if (defined(SUBSUMPTION_YAP) && defined(TABLING_CALL_SUBSUMPTION)) || defined(SUBSUMPTION_XSB)
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

inline static  CPtr extract_template_from_lookup(CTXTdeclc CPtr ans_tmplt) {

  int i;

  i = 0;
  while ( TrieVarBindings[i] != (Cell) (& TrieVarBindings[i]) )
    *ans_tmplt-- = TrieVarBindings[i++];
  *ans_tmplt = makeint(i);
  return ans_tmplt;
}

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

inline static
CPtr reconstruct_template_for_producer(CTXTdeclc TabledCallInfo *call_info,
				       SubProdSF subsumer, CPtr ans_tmplt) {

  int sizeAnsTmplt;
  Cell subterm, symbol;

  /*
   * Store the symbols along the path of the more general call.
   */
  SymbolStack_ResetTOS;
  SymbolStack_PushPath(subg_leaf_ptr(subsumer));

  /*
   * Push the arguments of the subsumed call.
   */
  TermStack_ResetTOS;
#ifdef SUBSUMPTION_YAP
  TermStack_PushLowToHighVector(CALL_ARGUMENTS(), CODE_ARITY(call_info));
#else
  TermStack_PushLowToHighVector(CallInfo_Arguments(*call_info),
			        CallInfo_CallArity(*call_info))
#endif /* SUBSUMPTION_YAP */

  /*
   * Create the answer template while we process.  Since we know we have a
   * more general subsuming call, we can greatly simplify the "matching"
   * process: we know we either have exact matches of non-variable symbols
   * or a variable paired with some subterm of the current call.
   */
  sizeAnsTmplt = 0;
  while ( ! TermStack_IsEmpty ) {
    TermStack_Pop(subterm);
    XSB_Deref(subterm);
    SymbolStack_Pop(symbol);
    if ( IsTrieVar(symbol) && IsNewTrieVar(symbol) ) {
      *ans_tmplt-- = subterm;
      sizeAnsTmplt++;
    }
    else if ( IsTrieFunctor(symbol) )
      TermStack_PushFunctorArgs(subterm)
    else if ( IsTrieList(symbol) )
      TermStack_PushListArgs(subterm)
  }
  *ans_tmplt = makeint(sizeAnsTmplt);
  return ans_tmplt;
}
#endif /* (SUBSUMPTION_YAP && TABLING_CALL_SUBSUMPTION) || SUBSUMPTION_XSB */

/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

inline static  CPtr extract_template_from_insertion(CTXTdeclc CPtr ans_tmplt) {

  int i;

  i = 0;
  while ( i < (int) Trail_NumBindings )
    *ans_tmplt-- = (Cell)Trail_Base[i++];
  *ans_tmplt = makeint(i);
  return ans_tmplt;
}