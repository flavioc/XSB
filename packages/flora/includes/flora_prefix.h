
/* Variable Flora prefix: this prefix changes dynamically */
#define VAR_FPREFIX(X)    '_$_$_flora_' ## X
/* Variable Flora prefix for dynamically loaded files: changes dynamically */
#define DYN_FPREFIX(X)    '_$_$_flora_dyn_' ## X
/* Static Flora prefix, for standard or kitchen-sink predicates */
#define STD_FPREFIX(X)    '_$_$_flora_' ## X
/* Use:
   VAR_FPREFIX('isa'(X,Y)).
   DYN_FPREFIX('fd'(O,M,X)).
   SYN_FPREFIX('isa'/2).
*/

