#version 310 es



float fn ( float x )   {    return x + 4.0;   }

int main() {
  gl_Position = vec4(1);
  gl_Position = clamp(1, 2, 3);
  gl_Position = vec4(1);
  gl_Position = vec4(1, 2);
  gl_Position = vec4(fn(3));
  [] . ++ --
  + - * % / - ! ~
  << >> < > <= >=
  == !=
  & ^ | && ^^ || ? :
  += -= *= /= %= <<= >>= &= |= ^=
  1.2 2E10 5u -5lf
}

struct S {
    int member1;
    float member2;
    vec4 member3;
};



void foo()
{
    S s;
    s.member2 + s.member1;
    s.member3.zyx;
    s.member2.xxyz;
    s.member2.yyz;
    s.member2.xxyz();
    s.member2.yzy;
    vec3 a = vec3(0); 	vec3 b = a.zxyz;  	vec3 b = a.xxyz;   	vec3 b = a.yyz;   	vec3 b = a.xxyz();   	vec3 b = a.yzy;   	vec3 b = a.z;
    yyz;
    yzy


}
