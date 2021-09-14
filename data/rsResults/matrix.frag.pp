#version 130

//#define TEST_POST_110

uniform mat3 colorTransform;
varying vec3 Color;
uniform mat4 m, n;

uniform mat4 um43;
uniform mat4 un34;

varying vec4 v;

varying vec4 u;

void main()
{
    gl_FragColor = vec4(un34[1]);
    gl_FragColor += vec4(Color * colorTransform, 1.0);

    if (m != n)
        gl_FragColor += v;
   else {
        gl_FragColor += m * v;
        gl_FragColor += v * (m - n);
   }

    mat4 m34 = mat4(v.x*u.x, v.x*u.y, v.x*u.z, v.x*u.w,
                    v.y*u.x, v.y*u.y, v.y*u.z, v.y*u.w,
                    v.z*u.x, v.z*u.y, v.z*u.z, v.z*u.w,
                    v.w*u.x, v.w*u.y, v.w*u.z, v.w*u.w);
    m34 += mat4(v.x);
    m34 += mat4(u, u.x, u, u.x, u, u.x, u.x);


    if (m34 == un34)
        gl_FragColor += m34 * u;
    else
        gl_FragColor += (un34 * um43) * v;

    mat4x2 m42 = mat4x2(42);
    if (m42 == mat4x2(42, 0, 0, 42, 0, 0, 0, 0)) {
        gl_FragColor += v;
    }
}
