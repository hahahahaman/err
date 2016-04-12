#version 330 core

layout (location = 0) in vec2 vertex;

uniform mat4 model;
uniform mat4 projection;

uniform mat4 surfaceMatrix;
varying vec2 surfacePosition;

void main (){
  surfacePosition = (vec4(vertex, 1.0, 1.0) * surfaceMatrix).xy;
  gl_Position = projection * model * vec4(vertex, 1.0, 1.0);
}
