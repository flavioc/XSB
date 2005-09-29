#include "xsb_config.h"

#ifdef CONC_COMPL

#include <pthread.h>

#include "context.h"
#include "basicdefs.h"
#include "macro_xsb.h"
#include "cell_xsb.h"
#include "choice.h"
#include "conc_compl.h"
#include "tables.h"
#include "tr_utils.h"
#include "thread_xsb.h"
#include "error_xsb.h"

#define GetDepSubgoal(d)	((d)->Subgoal)
#define GetDepCompleting(d)	((d)->completing)
#define GetDepTid(d)		(subg_tid((d)->Subgoal))

static ThreadDep * find_tid( ThreadDepList *TDL, int tid )
{
	int i ;
	i = 0 ;
	while ( i < TDL->NumDeps )
		if ( GetDepTid(&TDL->Deps[i]) == tid )
			return &TDL->Deps[i] ;
		else
			i++ ;
	return NULL ;
}

static int check_ins_subg( ThreadDepList *TDL, VariantSF subg )
{	int i ;
	i = 0 ;
	while ( i < TDL->NumDeps )
		if ( GetDepTid(&TDL->Deps[i]) == subg_tid(subg) )
		{	if( subg_compl_stack_ptr(subg) > 
			    subg_compl_stack_ptr(GetDepSubgoal(&TDL->Deps[i])) )
			{	GetDepSubgoal(&TDL->Deps[i]) = subg ;
				return TRUE ;
			}
			else
				return FALSE ;
		}
		else
			i++ ;
	if( TDL->NumDeps == MAX_TDEP_LIST - 1 )
		xsb_abort( "Too many inter-thread dependencies" );

	GetDepSubgoal(&TDL->Deps[TDL->NumDeps++]) = subg ;
	return TRUE ;
}

int EmptyThreadDepList( ThreadDepList *TDL )
{
	return TDL->NumDeps == 0 ;
}

void InitThreadDepList( ThreadDepList *TDL )
{
	TDL->NumDeps = 0 ;
}

ThreadDep *GetInitDep( ThreadDepList *TDL )
{
	if( TDL->NumDeps == 0 )
		return NULL ;
	else
		return &TDL->Deps[0] ;
}

ThreadDep *GetNextDep( ThreadDepList *TDL, ThreadDep *dep )
{
	int i = dep - TDL->Deps ;
	i++ ;
	if( TDL->NumDeps <= i )
		return NULL ;
	else
		return &TDL->Deps[i] ;

}

static void PropagateDeps( th_context *th, th_context *dep_th, 
		    	ThreadDepList *NewTDL, CPtr *leader, int *new_deps )
{
    ThreadDep *dep ;
    VariantSF sgf ;

    dep = GetInitDep(&dep_th->TDL); 
    while( dep != NULL )
    {   sgf = GetDepSubgoal(dep) ;
	if( subg_tid(sgf) == th->tid )
	{
	    if( subg_compl_stack_ptr(sgf) > *leader )
		*leader = subg_compl_stack_ptr(sgf) ;
	}
	else
		*new_deps = *new_deps || check_ins_subg(NewTDL, sgf) ;
	dep = GetNextDep(&dep_th->TDL, dep); 
    }
}


void UpdateDeps(th_context *th, int *busy, CPtr *leader)
{
    ThreadDepList NewTDL;
    int new_deps ;
    VariantSF sgf;
    ThreadDep *dep, *dep1 ;
    th_context * dep_th ;

    InitThreadDepList( &NewTDL ) ;
    *busy = FALSE ;

    do
    {
	new_deps = FALSE ;
	dep = GetInitDep(&th->TDL); 
	while( dep != NULL )
	{   sgf = GetDepSubgoal(dep) ;
	    if( !is_completed(sgf) )
	    {
		check_ins_subg(&NewTDL, sgf) ;
		dep1 = find_tid(&NewTDL,subg_tid(sgf));
		dep_th = find_context(subg_tid(sgf)) ;
		GetDepCompleting(dep1) = dep_th->completing ;
		if( dep_th->completing )
		    PropagateDeps( th, dep_th, &NewTDL, leader, &new_deps ) ;
		else
		    *busy = TRUE ;
	    }
	    dep = GetNextDep(&th->TDL, dep); 
	}
    	th->TDL = NewTDL ;
    }
    while( new_deps ) ;
}

int MayHaveAnswers( th_context * th )
{
    th_context * dep_th ;
    ThreadDep *dep, *dep1 ;
    int tid ;

    dep = GetInitDep(&th->TDL); 
    while( dep != NULL )
    {   tid = GetDepTid(dep) ;
	dep_th = find_context(tid) ;
    	dep1 = GetInitDep(&dep_th->TDL); 
    	while( dep1 != NULL )
    	{   if( GetDepTid(dep1) != th->tid && !GetDepCompleting(dep1) )
		return TRUE ;
	    dep1 = GetNextDep(&dep_th->TDL, dep1); 
	}
	dep = GetNextDep(&th->TDL, dep); 
    }
    return FALSE ;
}

int CheckForSCC( th_context * th )
{
    th_context * dep_th ;
    ThreadDep *dep, *dep1 ;
    int tid, tid1 ;
    VariantSF sgf;

    dep = GetInitDep(&th->TDL); 
    while( dep != NULL )
    {   tid = GetDepTid(dep) ;
        sgf = GetDepSubgoal(dep) ;
	dep_th = find_context(tid) ;
        if(dep_th->cc_leader < subg_compl_stack_ptr(sgf))
		return FALSE ;
    	dep1 = GetInitDep(&dep_th->TDL); 
    	while( dep1 != NULL )
    	{   tid1 = GetDepTid(dep1) ;
	    if( tid1 != th->tid && !find_tid(&th->TDL,tid1) )
		return FALSE ;
	    dep1 = GetNextDep(&dep_th->TDL, dep1); 
	}
    	dep1 = GetInitDep(&th->TDL); 
    	{   tid1 = GetDepTid(dep1) ;
	    if( tid1 != dep_th->tid && !find_tid(&dep_th->TDL,tid1) )
		return FALSE ;
	    dep1 = GetNextDep(&th->TDL, dep1); 
	}
	dep = GetNextDep(&th->TDL, dep); 
    }
    return TRUE ;
}

void CompleteOtherThreads( th_context * th )
{
    th_context * dep_th ;
    ThreadDep *dep ;
    VariantSF sgf ;

    dep = GetInitDep(&th->TDL); 
    while( dep != NULL )
    {   sgf = GetDepSubgoal(dep) ;
	dep_th = find_context(subg_tid(sgf)) ;
	CompleteTop(dep_th, dep_th->cc_leader) ;
	dep_th->completed = TRUE ;
	pthread_cond_signal(&dep_th->cond_var) ;
	dep = GetNextDep(&th->TDL, dep); 
    }
}

void WakeOtherThreads( th_context * th )
{
    th_context * dep_th ;
    ThreadDep *dep ;
    int tid;

    dep = GetInitDep(&th->TDL); 
    while( dep != NULL )
    {   tid = GetDepTid(dep) ;
	dep_th = find_context(tid) ;
	pthread_cond_signal(&dep_th->cond_var) ;
	dep = GetNextDep(&th->TDL, dep); 
    }
}

void WakeDependentThreads( th_context * th, VariantSF subg ) 
{
    CPtr Cons ;
    th_context *cons_th ;
	
    Cons = subg_asf_list_ptr(subg) ;
    while(Cons)
    {
	if( int_val(nlcp_tid(Cons)) != th->tid )
	{   cons_th = find_context(int_val(nlcp_tid(Cons))) ;
	    pthread_cond_signal(&cons_th->cond_var) ;
	}
	Cons = nlcp_prevlookup(Cons) ;
    }
}

void CompleteTop( th_context * th, CPtr leader )
{
  VariantSF compl_subg;
  CPtr ComplStkFrame = leader;
                                                                                
  /* mark all SCC as completed and do simplification also, reclaim
     space for all but the leader */
                                                                                
  while (ComplStkFrame >= openreg) {
    compl_subg = compl_subgoal_ptr(ComplStkFrame);
    if( !is_completed(compl_subg) )
    {
    	mark_as_completed(compl_subg);
    	WakeDependentThreads(th, compl_subg);
    }
    reclaim_incomplete_table_structs(compl_subg);
    ComplStkFrame = next_compl_frame(ComplStkFrame);
  } /* while */
  openreg = prev_compl_frame(leader);
  reclaim_stacks(breg);
  breg = tcp_prevbreg(breg);
}

CPtr sched_external( th_context *th, CPtr ExtCons )
{
    CPtr sched_cons = NULL ;
    VariantSF ExtProd ;

    if ( ALN_Next(nlcp_trie_return(ExtCons)) )
	sched_cons = ExtCons ;

    ExtProd = (VariantSF)nlcp_subgoal_ptr(ExtCons) ;
    if( !is_completed(ExtProd) )
	check_ins_subg(&th->TDL, ExtProd) ;

    return sched_cons ;
}

#endif
