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

                //yes
                sum += 7000000.0;




// sum should be 987600301.0
    gl_Position = vec4(sum);
}

    int selected4 = 4;

            int selected2 = 2;

            int selected3 = 3;

// ERROR cases...

int;

int;


void foo985(){	(((2)))+((3),4); }
// needs to be last test in file
void foo987(){	FUNC(((); }  // ERROR, EOF in argument
