
#define FPREFIX(X)     '_$_$_flora_' ## X /* this prefix changes dynamically */

#define DYN_FPREFIX(X)    '_$_$_flora_dyn_' ## X
#define STD_FPREFIX(X)    '_$_$_flora_' ## X
/* Use:
   FPREFIX('isa'(X,Y)).
   FPREFIX('fd'(O,M,X)).
   SYN_FPREFIX('isa'/2).
*/

/* Note: things like '_$_$_flora_tag'(T)
   are to be written as '_$_$_flora_tag'##(T)
   and '_$_$_flora_tag'/1 as '_$_$_flora_tag'##/1
*/
#define FLORA_TAG    	          '_$_$_flora_tag'

#define FLORA_PRINT_ALL           '_$_$_flora_print_all'
#define FLORA_PRINT_ONE           '_$_$_flora_print_one'
#define FLORA_PRINT_ANSWERS       '_$_$_flora_print_answers'

#define FLORA_SHELL    	       	   '_$_$_flora_shell'
#define FLORA_SHELL_LOOP    	   '_$_$_flora_shell_loop'
#define FLORA_LOOP_GUARD    	   '_$_$_flora_loop_guard' 
#define FLORA_WELCOME_MSG    	   '_$_$_flora_welcome_msg'
#define FLORA_WRITE_SHELL_PROMPT   '_$_$_flora_write_shell_prompt'

#define FLORA_ASSERT	       	   '_$_$_flora_assert'
#define FLORA_RETRACT	       	   '_$_$_flora_retract'
#define FLORA_RETRACTALL       	   '_$_$_flora_retractall'
#define FLORA_ERASE       	   '_$_$_flora_erase'

#define FLORA_MIN    	     	   '_$_$_flora_min'
#define FLORA_MAX    	     	   '_$_$_flora_max'
#define FLORA_AVG    	     	   '_$_$_flora_avg'
#define FLORA_SUM    	     	   '_$_$_flora_sum'
#define FLORA_COLLECTSET     	   '_$_$_flora_collectset'
#define FLORA_COLLECTBAG     	   '_$_$_flora_collectbag'

#define FLORA_CHECK_LIBRARY 	   '_$_$_flora_check_library'

#define FLORA_DEBUGGER_DATA_LOADED '_$_$_flora_debugger_data_loaded'
#define FLORA_TRACE    	       	   '_$_$_flora_trace'
#define FLORA_NOTRACE	     	   '_$_$_flora_notrace'
#define FLORA_SWITCH	           '_$_$_flora_switch'

#define FLORA_DEBUGGER_DATA_LOADED '_$_$_flora_debugger_data_loaded'.
#define FLORA_DEBUG_GET_CONTEXT	   '_$_$__flora_debug_get_context'.
