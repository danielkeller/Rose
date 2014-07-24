#version 330

in vec3 position;

uniform mat4x4 modelView;

void main()
{
    gl_Position = modelView * vec4(position, 1);
}