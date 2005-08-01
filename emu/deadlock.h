#ifdef MULTI_THREAD
                                                                                
int would_deadlock( th_context *t1, th_context *t2 ) ;
void reset_leader( th_context *th ) ;
void reset_other_threads( th_context *th, th_context *ctxt, VariantSF sgf ) ;

#endif

