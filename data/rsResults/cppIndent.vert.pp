#version 110


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
sum += 80000000.0;

//yes
sum += 900000000.0;

// sum should be 980600301.0
    gl_Position = vec4(sum);
}

// needs to be last test in file due to syntax error
void foo986(){	FUNC( (((2)))), 4); }  // ERROR, too few arguments )
