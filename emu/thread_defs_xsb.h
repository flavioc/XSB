/* TLS: temporary for testing.. */
#define CTXT
#define CTXTc			
 
/* THREAD PRIMITIVES */

#define XSB_THREAD_CREATE	 1
#define XSB_THREAD_EXIT		 2
#define XSB_THREAD_JOIN		 3
#define XSB_THREAD_DETACH	 4
#define XSB_THREAD_SELF		 5

#define XSB_MUTEX_INIT		 6
#define XSB_MUTEX_LOCK		 7
#define XSB_MUTEX_TRYLOCK	 8
#define XSB_MUTEX_UNLOCK 	 9
#define XSB_MUTEX_DESTROY       10

#define XSB_SYS_MUTEX_LOCK	11
#define XSB_SYS_MUTEX_UNLOCK	12

#define XSB_ENSURE_ONE_THREAD	13

#define MAX_THREADS		1024


/* MUTEX KINDS (under LINUX) */

#define XSB_FAST_MUTEX		1
#define XSB_RECURSIVE_MUTEX	2
#define XSB_ERRORCHECK_MUTEX	3


/* first mutexes are recursive */

#define LAST_REC_MUTEX		8

/* Mutexes to protect execution of critical system stuff */

#define MUTEX_DYNAMIC		0
#define MUTEX_IO		1	/* Must be recursive */
#define MUTEX_TABLE		2
#define MUTEX_TRIE		3
#define MUTEX_SYMBOL		4
#define MUTEX_FLAGS		5
#define MUTEX_LOAD_UNDEF	6	/* Must be recursive */
#define MUTEX_STRING		7
#define MUTEX_ATOM_BUF		8
#define MUTEX_STACKS		8

#define MAX_SYS_MUTEXES		20

/* Some mutexes available to users */

#define MUTEX_CONSOLE		10
#define MUTEX_USER1		11
#define MUTEX_USER2		12
#define MUTEX_USER3		13
#define MUTEX_USER4		14
#define MUTEX_USER5		15
#define MUTEX_USER6		16
#define MUTEX_USER7		17
#define MUTEX_USER8		18
#define MUTEX_USER9		19

/* Used for random number generation in testing modules */

#define INIT_MT_RANDOM          0
#define MT_RANDOM               1
#define MT_RANDOM_INTERVAL      2
