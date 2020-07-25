
#version 450

layout(location = 1) out vec2 frag_tex_coord;

void main()
{
    // generate a triangle covering the entire screen along with the
    // appropriate texture coordinates using just gl_VertexIndex
    frag_tex_coord = vec2((gl_VertexIndex << 1) & 2, gl_VertexIndex & 2);
    gl_Position = vec4(frag_tex_coord * 2.0f + -1.0f, 0.0f, 1.0f);
}
