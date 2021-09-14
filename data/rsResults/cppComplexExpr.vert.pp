#version 300 es
float sum = 0.0;

void main()
{
//yes
    sum += 1.0;

//yes
    sum += 20.0;



//yes
    sum += 300.0;


// sum should be 321.0
    gl_Position = vec4(sum);
}


float foo()
{
    return gl_Position.xyxwx + 3.0 + ((gl_Position.xyxwx) + ((3.0)));  // ERROR, should be this line number
    return gl_Position.y + 3.0 + ((gl_Position.y) + ((3.0)));
}


// identical

// ERROR, one character different





;
;
float c = 1.1 + 2.2;
)















    



