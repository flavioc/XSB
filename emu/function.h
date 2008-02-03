typedef struct {
  int fitype;  // 1 if int, 0 if float
  union {
    Float valflt;
    Integer valint;
  } fival;
} FltInt;

#define set_int_val(fivar,ival) do { 		\
    fivar->fitype = 1;				\
    fivar->fival.valint = ival;			\
  } while (0)
#define set_flt_val(fivar,fval) do { 		\
    fivar->fitype = 0;				\
    fivar->fival.valflt = fval;			\
  } while (0)

#define fiint_val(firec) (Integer)firec.fival.valint

#define fiflt_val(firec) (Float)firec.fival.valflt

#define isfiint(firec) firec.fitype

#define isfiflt(firec) firec.fitype == 0

int xsb_eval(CTXTdeclc Cell exp, FltInt *value);
