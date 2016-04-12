#version 330 core

#define PI 3.1415926535897932384626433832795

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

const float position = 0.0;
const float scale = 1.0;
const float intensity = 1.0;

varying vec2 surfacePosition;

float band(vec2 pos, float amplitude, float frequency) {
	float wave = scale * amplitude * sin(2.0 * PI * frequency * pos.x + time) / 2.05;
	float light = clamp(amplitude * frequency * 0.002, 0.001 + 0.001 / scale, 5.0) * scale / abs(wave - pos.y);
	return light;
}

void main( void ) {
  float fill1 = mouse.x;
  float fill2 = resolution.x;
	vec2 surfacePosition = vec2(surfacePosition.x * -2. + fill1 - fill1 + fill2 - fill2, surfacePosition.y);
	surfacePosition.x -= 1.7;
	surfacePosition.y *= 1.333;
	vec3 color = vec3(0.5, 0.5, 1.0);
	color = color == vec3(0.0)? vec3(0.5, 0.5, 1.0) : color;
	vec2 pos = surfacePosition;
	float spectrum = 0.0;
	const float lim = 200.;
#define time 1.+time*0.037 + pos.x*3.1415
	for(float i = 0.; i < lim; i++){
		spectrum += band(pos, 1.0*sin(time*0.1), 1.0*sin(time*i/lim))/pow(lim, 0.420 + 0.133*cos(time*3e3+time));
	}
	gl_FragColor = vec4(color * spectrum, 1.);
}
