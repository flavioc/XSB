
/* Variable Flora prefix changes dynamically 
   Use:
       #define VAR_FPREFIX_BASE ...
   
   Examples:
       VAR_FPREFIX(isa)(X,Y)
       STD_FPREFIX(isa)/2.
*/

#mode standard Prolog

#define STD_FPREFIX_BASE  _$_$_flora_

#if !defined(VAR_FPREFIX_BASE)
#define VAR_FPREFIX_BASE  STD_FPREFIX_BASE
#endif

/* Tell GPP to stop interpreting ' as a string while evaluating these macros
   The result is that these macros will be substituting inside the '...' */
#mode save
#mode nostring "\!#'"
#define VAR_FPREFIX(X)    'VAR_FPREFIX_BASE''X'
#define STD_FPREFIX(X)    'STD_FPREFIX_BASE''X'
#mode restore


:- op(1050, xfy, STD_FPREFIX(then)).
:- op(1100, xfy, STD_FPREFIX(else)).


