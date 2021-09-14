#version 300 es

// this file contains no errors other than the #error which are there to see if line numbering for errors is correct


float foo;  // same as 'float foo;'




void main()    {                     gl_Position = vec4(foo); }

vec4 foo2(vec4 a)
{                                
  vec4 b = a;         return b;                   
}

// aoeuntheo unatehutna \ antaehnathe 
// anteonuth $ natohe " '
// anteonuth     natohe


int /* */ goodDecl;


const highp int a1 = \ 4;  // ERROR
const highp int a2 = @ 3;  // ERROR
const highp int a3 = $4;   // ERROR
const highp int a4 = a2\;  // ERROR

int q1 = \ 1;
int q2 = \1;
int q3 =  1;
int q4 =  1;



// anoetuh nonaetu \\\\\\
still in comment

const int abdece = 10;
const int aoeuntaoehu = abdece;

float funkyf = .123e+17;int funkyh=0xf4;
int funkyo =042;
int c = 11;
int d = 12;


int bar103 = 17;

// ERROR
3)
int bar104 = 19;

// ERROR
2,3)
int bar105 = 19;

int bar106 = 5 + 7;
int bar107 = 2 + 3
    ;

void foo203209409()
{
    bar107 += 37;
    bar107 *= 38;
    bar107 /=39;
    bar107 +41;
}


void foo230920394()
{
    // syntax error
    bar107 + = 42;
}
