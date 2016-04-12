#version 330 core


#define PI 3.1415926535897932384626433832795

uniform float time;
uniform vec2 mouse;
uniform vec2 resolution;

const float prec = 0.1;
const float axis = 0.000;

float func(float x, float y) {
	float r = x;
	float i = y;
	float nextR = 0.0;
	float nextI = 0.0;

	for (int num = 0; num < 100; num++) {
		if (float(num) >= floor(mod(time * 50.0, 100.0)) + 3.0)
			return 0.0;

		if (r * r + i * i > 4.0)
			return 1.0;
		nextR = r*r - i*i + x;
		nextI = 2.0*r*i + y;
		r = nextR;
		i = nextI;
	}

	return 0.0;
}

void main(void) {

  vec2 uv = -1.0 + 2.0 * gl_FragCoord.xy / resolution.xy;
	uv *= 1.5 * mouse.y;
	uv.x -= mouse.x;
  uv.x *=  resolution.x / resolution.y;

  vec3 color = vec3(1.0 - func(uv.x, uv.y),0.2,0.4);

	if (abs(func(uv.x, uv.y)) < prec)
		color = vec3(1.0);

	if (abs(uv.y) < axis || abs(uv.x) < axis)
		color = vec3(0.0);

  gl_FragColor = vec4(color,1.0);
}
