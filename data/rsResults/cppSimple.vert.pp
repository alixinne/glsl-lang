#version 400


float sum = 0.0;

void main()
{

//yes
sum += 1.0;


//yes
sum += 300.0;



//yes
sum += 600000.0;

//yes
sum += 7000000.0;

//yes
sum += 80000000.0;

//yes
sum += 900000000.0;

sum += 0.05;

// sum should be 987600301.7
    gl_Position = vec4(sum);
}














int linenumber = 123;
int filenumber = 0;
int version = 400;

float twoPi = (2.0 * (3.14));

//#define PASTE(a,b) a ## b
//float PASTE(tod, ay) = 17;

"boo" // ERROR
int a = length("aoenatuh");  // ERROR
'int';  // ERROR
// ERROR: all the following are reserved




// okay

// ERROR

// ERROR

// ERROR

// ERROR (whitespace)


// ERROR

// ERROR

int n = 0xf;

double f = .08e-2Lf;


##
####
####ff
#########ff fg 0x25
#pragma
#pragma(aoent)
	#	pragma
#pragma STDGL
#pragma	 optimize(	on)
#pragma  optimize(off)
#pragma debug( on)
#pragma debug(off	)
#pragma	 optimize(	on)
#pragma debugoff	)
#pragma aontheu natoeh uantheo uasotea noeahuonea uonethau onethuanoeth aunotehau noeth anthoeua  anoethuantoeh uantoehu natoehu naoteh unotaehu noethua onetuh aou







void foo234()
{
    gl_Position = vec4((((2.0 / 1.0) * 2.0) + (2.0 / 1.0)));
}

// more whitespace recording tests


// space in middle is an error




// ERRORS...

// recursion (okay)
int RECURSE;
int R1 = RECURSE;

int aoeua = FOOOM;


// ERROR for macro expansion to yield 'defined'

