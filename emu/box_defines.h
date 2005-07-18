#ifndef BOX_DEFINES_H
#define BOX_DEFINES_H

#include "xsb_config.h"


/*16 to give space for the most significant 16 bits of 64bit numbers when using boxed representation.*/
#define BOX_ID_OFFSET 16 

/**********************************
*   BOX ID's, to be placed in the 24th through 17th least significant
*   bits of the first integer in a boxed value.
***********************************/
#define ID_BOXED_INT 1
#define ID_BOXED_FLOAT 2

//TODO: define different shift constants in the BIT extracting macros below and alternate
//depending on architecture implementation of ints, doubles, floats, etc... 
#define LOW_24_BITS_MASK 0xffffff
#define LOW_16_BITS_MASK 0xffff

#define INT_LOW_24_BITS(value) (((unsigned)(value)) >> 24)


#ifdef PRECISE_FLOATS
//The below macros expect "float" to be a double float variable local to the thread it is envoked in. 
// Any caller must ensure that it isn't something such as a macro that produces a function call, 
// a constant, an expression, or a static/global variable (for thread-safe safety)
#define FLOAT_HIGH_16_BITS(float) ((((UInteger)(*((UInteger *)((void *)(& float)))))>>16) & LOW_16_BITS_MASK)
#define FLOAT_MIDDLE_24_BITS(float) ((((UInteger)(*(((UInteger *)((void *)(& float)))+1)))>>24)|(  (UInteger)(*((UInteger *)((void *)&(float)))) & LOW_16_BITS_MASK)<<8 )
#define FLOAT_LOW_24_BITS(float) (((UInteger)(*(((UInteger *)((void *)(& float)))+1))) & LOW_24_BITS_MASK)

 #endif
 
 #endif
