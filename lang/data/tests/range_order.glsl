#define M_PI 3.14
#define M_2PI (2. * M_PI)
#define M_NPI(x) (x * M_PI)
#define M_TAU M_NPI(2.)

void main() {
    gl_FragColor = vec3(M_2PI, M_NPI(5.), M_TAU);
}
