#version 100

// non-line continuation comment #error good error



float foo;  // same as 'float foo;'




void main()    {                     gl_Position = vec4(foo); }

vec4 foo2(vec4 a)
{                                
  vec4 b = a;         return b;                   
}

// aoeuntheo unatehutna \ antaehnathe 
// anteonuth $ natohe " '
// anteonuth     natohe
/*@*/
/* *@/*/
//@


const highp int a1 = \ 4;  // ERROR
const highp int a2 = @ 3;  // ERROR
const highp int a3 = $4;   // ERROR
const highp int a4 = a2\;  // ERROR

int q1 = \ 1;
int q2 = \1;
int q3 =  1;
int q4 =  1;


