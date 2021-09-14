#version 450

#extension GL_KHR_shader_subgroup_shuffle_relative: enable

layout (local_size_x = 8, local_size_y = 8, local_size_z = 1) in;

layout(binding = 0) buffer Buffers
{
    vec4  f4;
    ivec4 i4;
    uvec4 u4;
    dvec4 d4;
} data[4];

void main()
{
    uint invocation = (gl_SubgroupInvocationID + gl_SubgroupSize) % 4;

    data[invocation].f4.x   = subgroupShuffleUp(data[0].f4.x,    invocation);
    data[invocation].f4.xy  = subgroupShuffleUp(data[1].f4.xy,   invocation);
    data[invocation].f4.xyz = subgroupShuffleUp(data[2].f4.xyz,  invocation);
    data[invocation].f4     = subgroupShuffleUp(data[3].f4,      invocation);

    data[invocation].i4.x   = subgroupShuffleUp(data[0].i4.x,    invocation);
    data[invocation].i4.xy  = subgroupShuffleUp(data[1].i4.xy,   invocation);
    data[invocation].i4.xyz = subgroupShuffleUp(data[2].i4.xyz,  invocation);
    data[invocation].i4     = subgroupShuffleUp(data[3].i4,      invocation);

    data[invocation].u4.x   = subgroupShuffleUp(data[0].u4.x,    invocation);
    data[invocation].u4.xy  = subgroupShuffleUp(data[1].u4.xy,   invocation);
    data[invocation].u4.xyz = subgroupShuffleUp(data[2].u4.xyz,  invocation);
    data[invocation].u4     = subgroupShuffleUp(data[3].u4,      invocation);

    data[invocation].d4.x   = subgroupShuffleUp(data[0].d4.x,    invocation);
    data[invocation].d4.xy  = subgroupShuffleUp(data[1].d4.xy,   invocation);
    data[invocation].d4.xyz = subgroupShuffleUp(data[2].d4.xyz,  invocation);
    data[invocation].d4     = subgroupShuffleUp(data[3].d4,      invocation);

    data[invocation].i4.x   =   int(subgroupShuffleUp(data[0].i4.x < 0,                   invocation));
    data[invocation].i4.xy  = ivec2(subgroupShuffleUp(lessThan(data[1].i4.xy, ivec2(0)),  invocation));
    data[invocation].i4.xyz = ivec3(subgroupShuffleUp(lessThan(data[1].i4.xyz, ivec3(0)), invocation));
    data[invocation].i4     = ivec4(subgroupShuffleUp(lessThan(data[1].i4, ivec4(0)),     invocation));

    data[invocation].f4.x   = subgroupShuffleDown(data[0].f4.x,    invocation);
    data[invocation].f4.xy  = subgroupShuffleDown(data[1].f4.xy,   invocation);
    data[invocation].f4.xyz = subgroupShuffleDown(data[2].f4.xyz,  invocation);
    data[invocation].f4     = subgroupShuffleDown(data[3].f4,      invocation);

    data[invocation].i4.x   = subgroupShuffleDown(data[0].i4.x,    invocation);
    data[invocation].i4.xy  = subgroupShuffleDown(data[1].i4.xy,   invocation);
    data[invocation].i4.xyz = subgroupShuffleDown(data[2].i4.xyz,  invocation);
    data[invocation].i4     = subgroupShuffleDown(data[3].i4,      invocation);

    data[invocation].u4.x   = subgroupShuffleDown(data[0].u4.x,    invocation);
    data[invocation].u4.xy  = subgroupShuffleDown(data[1].u4.xy,   invocation);
    data[invocation].u4.xyz = subgroupShuffleDown(data[2].u4.xyz,  invocation);
    data[invocation].u4     = subgroupShuffleDown(data[3].u4,      invocation);

    data[invocation].d4.x   = subgroupShuffleDown(data[0].d4.x,    invocation);
    data[invocation].d4.xy  = subgroupShuffleDown(data[1].d4.xy,   invocation);
    data[invocation].d4.xyz = subgroupShuffleDown(data[2].d4.xyz,  invocation);
    data[invocation].d4     = subgroupShuffleDown(data[3].d4,      invocation);

    data[invocation].i4.x   =   int(subgroupShuffleDown(data[0].i4.x < 0,                   invocation));
    data[invocation].i4.xy  = ivec2(subgroupShuffleDown(lessThan(data[1].i4.xy, ivec2(0)),  invocation));
    data[invocation].i4.xyz = ivec3(subgroupShuffleDown(lessThan(data[1].i4.xyz, ivec3(0)), invocation));
    data[invocation].i4     = ivec4(subgroupShuffleDown(lessThan(data[1].i4, ivec4(0)),     invocation));
}
