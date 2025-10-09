// Simple Raymarching Demo

const float FOV = 1.7; // Field of view radians
const float CAMDIST = 5.; // Distance to center
const int STEPS = 120; // Maximum number of raymarching steps
const float MAXDIST = 120.0; // Maximum distance for raymarching
const float MINDIST = 0.01; // Minimum distance for surface detection
const vec3 SKYCOLOR = vec3(0.27, 0.51, 0.71); // sky color
const vec3 HORIZONCOLOR = vec3(0.8, 0.87, 0.95); // horizon color
const vec3 SUNDIR = normalize(vec3(0.5, 1.7, 1.0));
const vec3 SUNCOLOR = vec3(1.0, 0.8, 0.3);
const float PI = 3.1415926535;

const int SHADOWMAXSTEPS = 30; // shadow march steps
const float SHADOWMAXDIST = 20.; // Maximum shadow distance
const float SHADOWSOFTNESS = 32.0; // Softness factor: higher = sharper

// Object class - objects passed into map()
struct Object {
    int type; // 0 = sphere, 1 = box
    vec3 pos;
    vec3 size; // radius for sphere, half-extents for box
    float rot; // optional rotation for boxes
    vec3 col; // object color
    float shininess;
    float specStrength;
};

// Surface class - returned from a hit point
struct Surface {
    float dist; // signed distance value
    vec3 col; // color
    int id; // object identifier
    float shininess;
    float specStrength;
};

// ### Helper functions
mat2 rot2D(float angle) {
    float s = sin(angle);
    float c = cos(angle);
    return mat2(c, -s, s, c);
}
float sdBox(vec3 p, vec3 b) {
    vec3 q = abs(p) - b;
    return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}
float sdSphere(vec3 p, float s) {
    return length(p)-s;
}
float sMin(float a, float b, float k) {
    // get a "smooth" minimum - makes objs look like they're blended toether
    float h = max(k-abs(a-b), 0.0)/k;
    return min(a,b)-h*h*h*k*(1.0/6.0);
}

vec3 skyColor(vec3 dir) {
    float t = clamp(dir.y * 0.5 + 0.5, 0.0, 1.0); // map -1,1 -> 0, 1 idek if this works

    float d = dot(dir, SUNDIR);
    float sunSize = 0.997; // closer to 1 = smaller disc
    
    // Sun disc with sharp edge
    float sun = smoothstep(sunSize, sunSize + 0.005, d);
    
    // Glow effect with exponential falloff
    float glow = 0.5 * pow(max(0.0, d - 0.98) / (1.0 - 0.98), 3.0);
    glow = clamp(glow, 0.0, 1.0); // Adjust glow intensity
    
    // Combine sky, sun, and glow
    vec3 sky = mix(HORIZONCOLOR, SKYCOLOR, t);
    vec3 color = mix(sky, SUNCOLOR, sun); // Add sun disc
    color += SUNCOLOR * glow; // Add glow effect
    
    return color;
}


Surface map(vec3 p) {
    // ### uses SDF's to find nearest object
    
    // set the ground
    Surface result;
    result.dist = p.y + 3.;
    result.id = 0;
    result.shininess = 8.0;
    result.specStrength = 0.3;

    // define the other objects then calculate - may need to move this somewhere else lol
    Object objects[3];
    objects[0] = Object(0, vec3(sin(iTime*1.5)*3.0,0.0,0.0), vec3(1.0), 0.0, vec3(1.0,0.3,0.3), 32., 0.5);
    objects[1] = Object(1, vec3(0.0,0.0,0.0), vec3(1.0,0.75,0.75), iTime, vec3(0.3,0.3,1.0), 32., 0.5);
    objects[2] = Object(1, vec3(0.0,0.0,2.5), vec3(1.0), 0.0, vec3(0.3,1.0,0.3), 100., 0.8);

    for (int i = 0; i < 3; i++) {
        float d;
        if (objects[i].type == 0) {
            d = sdSphere(p - objects[i].pos, objects[i].size.x);
        } else {
            vec3 q = p - objects[i].pos;
            q.xy *= rot2D(objects[i].rot);
            d = sdBox(q, objects[i].size);
        }

        if (d < result.dist) {
            // a closer object? well then that's our new result!
            result.dist = d;
            result.id = i + 1;  // add 1 because id 0 is reserved for ground
            result.col = objects[i].col;
            result.shininess = objects[i].shininess;
            result.specStrength = objects[i].specStrength;
        }
    }
    
    if (result.id == 0)
        // no closer obj, just set color to the ground color
        result.col = vec3(0.5,0.5,0.5)*(1.0 + 0.7*mod(floor(p.x)+floor(p.z),2.0));

    return result;
}

vec3 calcNormal(vec3 p) {
    vec2 e = vec2(0.001, 0.0); // epsilon for normal calculation
    return normalize(vec3(
        map(p + e.xyy).dist - map(p - e.xyy).dist,
        map(p + e.yxy).dist - map(p - e.yxy).dist,
        map(p + e.yyx).dist - map(p - e.yyx).dist
    ));
}

float softShadow(vec3 ro, vec3 rd) {
    // ### Soft shadow utility - takes in ray origin and ray distance
    // partially gpted
    float res = 1.0;  // 1 -> bright
    float dist = MINDIST * 2.0;  // small offset to prevent self-shadow

    for (int i = 0; i < SHADOWMAXSTEPS; i++) { // 40 steps is enough
        if (dist > SHADOWMAXDIST) break;       // stop after max distance

        float travel = map(ro + rd * dist).dist;  // distance
        res = min(res, SHADOWSOFTNESS * travel / dist);
        if (res <= 0.0) return 0.0;  // fully blocked

        dist += max(travel, 0.02);         // step size grows with distance to surface
    }
    return clamp(res, 0.0, 1.0);
}


Surface rayMarch(vec3 rayOrigin, vec3 rayDir) {
    // ### Raymarch
    Surface obj;
    obj.col = vec3(0.0);
    obj.id = -1;
    obj.dist = 0.0;
    obj.shininess = 8.0;
    obj.specStrength = 0.0;

    float dist = 0.0;
    for (int i = 0; i < STEPS; i++) {
        vec3 pos = rayOrigin + rayDir * dist;
        Surface scene = map(pos);
        float travel = scene.dist;

        if (travel < MINDIST) {
            // we hit something?
            obj.id = scene.id;
            obj.dist = dist;  // set dist
            obj.col = scene.col;
            obj.shininess = scene.shininess;
            obj.specStrength = scene.specStrength;
            return obj; // early out on hit (clean)
        }

        dist += travel;
        if (dist > MAXDIST) break; // too far, give up
    }

    // if we reach here, we did NOT hit any surface ->
    // exceeded maxdist or passed steps
    obj.id = -1;  // set it to background / no hits
    return obj;
}

vec3 phongLighting(vec3 rayO, vec3 rayD, Surface surface) {
    // ### Adds phong lighting
    vec3 hitPoint = rayO + rayD * surface.dist;
    vec3 normal = calcNormal(hitPoint);

    // push point slightly onto surface to avoid thin rim halos
    hitPoint -= normal * 0.0008;

    // shadow factor (0 = fully shadowed, 1 = lit)
    float shadow = softShadow(hitPoint + normal * 0.01, SUNDIR);
    float diffuse = clamp(dot(normal, SUNDIR), 0.0, 1.0) * shadow;

    // specular (attenuate by shadow as well)
    vec3 viewDirection = normalize(rayO - hitPoint);
    vec3 halfwayDir = normalize(SUNDIR + viewDirection);
    float spec = pow(max(dot(normal, halfwayDir), 0.0), surface.shininess);
    vec3 specular = surface.specStrength * spec * SUNCOLOR * shadow;

    // ambient lighting + backlighting (gpt helped with backlighting)
    vec3 ambient = SKYCOLOR * 0.05;
    float ndotl = max(dot(normal, SUNDIR), 0.0);  // get angle
    float backLight = 0.3 * (1.0 - ndotl); // illuminate the shadowed side
    return diffuse * surface.col + backLight * surface.col + ambient + specular;
}

void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // ### Normalize pixel coords and mouse stuff
    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.y;
    vec2 m = 2. * (iMouse.xy / iResolution.xy - 0.5) * vec2(PI, PI/2.);  // Mouse -> angles
    // Orbit origin around the origin w/ help from chatgpt
    vec3 rayOrigin = vec3(
        CAMDIST * cos(-m.y) * sin(m.x),
        CAMDIST * sin(-m.y),
        CAMDIST * cos(-m.y) * cos(m.x)
    );
    vec3 forward = normalize(-rayOrigin);
    vec3 right = normalize(cross(vec3(0,1,0), forward));
    vec3 up = cross(forward, right);

    vec3 rayDir = normalize(forward + uv.x * right * FOV + uv.y * up * FOV);  // calc final ray dir

    // ### Perform raymarching and return dist, col, and id
    Surface surface = rayMarch(rayOrigin, rayDir);
   
    vec3 col;
    if (surface.id != -1) { // Actual hit on obj - (not bg)
        // ### Adds phong lighting
        col = phongLighting(rayOrigin, rayDir, surface);
    } else {
        // ### Adds sun and nice sky blend - todo: add clouds
        col = skyColor(rayDir);
    }

    // Output to screen
    fragColor = vec4(col, 1.0);
}
