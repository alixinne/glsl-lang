#version 450

// side test verifies multiple rounds of argument expansion
int SecondExpansion;                    // mmmB -> bear, and then in mmmA(), bear -> SecondExpansion

// pasting skips the first round of expansion
int PostPasteExpansion;             // mmcat/mmdog not expanded, mmcatmmdog -> PostPasteExpansion

// multi-token pre
float foo27;          // should declare "float foo27;"

// multi-token post
uniform float foo155; // should declare "uniform float foo155;"

// non-first argument
float foo719;          // should declare "float foo719;"
uniform float barfoo ;   // should declare "uniform float barfoo;"

// no args
float argless;

// bad location
 dc1;
 dc2;

// multiple ##
uniform float foo875;

// too long
// 1020 + 5 characters
float ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF0123456789ABCDEF01234512345;

// non-identifiers
int a = 1112;

// should work in #if as well

uniform float seahorse_var;
uniform float sealion_var;

// operators
const int aop = 10;
const int bop = 4;
int cop = aop << bop;
bool dop = aop != bop;


void ShouldntExpandToThis()
{
    int e = 16;
    e >>= 2;

    // recovery from bad op
    bool f = e  5;
}

// arguments: should make 'uniform int argPaste2;'
uniform int argPaste2;
// should make 'uniform int argPaste20suff;'
uniform int argPaste20suff;



(
