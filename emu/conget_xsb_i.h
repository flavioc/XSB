

static inline Integer conget(Cell string)
{
  int value;
  Psc psc, str_psc;
  Pair sym;

  if (!isstring(string))
    xsb_abort("[CONGET] Non-string in the first argument");

  psc = (Psc)flags[CURRENT_MODULE];
  sym = insert(string_val(string), 0, psc, &value);
  str_psc = pair_psc(sym);
  return (Integer)get_data(str_psc);
}


static inline xsbBool conset(Cell string, Integer newval)
{
  int value;
  Psc psc, str_psc;
  Pair sym;

  if (!isstring(string))
    xsb_abort("[CONSET] Non-string in the first argument");

  psc = (Psc)flags[CURRENT_MODULE];
  sym = insert(string_val(string), 0, psc, &value);
  str_psc = pair_psc(sym);

  set_data(str_psc, (Psc) newval);
  return TRUE;
}
