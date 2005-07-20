#include "context.h"
#include "deadlock.h"

#ifdef MULTI_THREAD
                                                                                
int would_deadlock( th_context *t1, th_context *t2 )
{
        th_context * t = t1 ;
                                                                                
        while( t != NULL )
                if( t == t2 )
                        return 1 ;
                else
                        t = t->waiting_for_thread;
                                                                                
        return 0 ;
}

#endif
