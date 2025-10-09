// Nighttime Alleyway
// Raymarched in 3d
// Slow asf

// Rendering constants
const float FOV = 1.7;              // Field of view
const int STEPS = 120;              // Max raymarching steps
const float MAXDIST = 100.0;        // Max render distance
const float MINDIST = 0.01;         // Min distance for hit detection
const float PI = 3.1415926535;

// Scene dimensions
const float ALLEY_WIDTH = 3.;       // Width of the alley
const float WALL_HEIGHT = 4.0;      // Height of walls
const float SEGMENT_LENGTH = 6.0;   // Length of alley segments

// Shadow settings
const int SHADOWMAXSTEPS = 70;      // Max shadow ray steps
const float SHADOWMAXDIST = 30.0;   // Max shadow distance
const float SHADOWSOFTNESS = 32.0;  // Shadow softness factor

// Atmosphere settings
const float FOGDENSITY = 0.025;     // Fog density
const vec3 FOGCOLOR = vec3(0.02, 0.025, 0.035);  // Fog color (dark blue)
const vec3 AMBIENT = vec3(0.06, 0.07, 0.09);      // Ambient light color

// Lamppost settings
const float LIGHTSTRENGTH = 0.4;    // Overall light intensity for lamps
const float LAMPSPACING = 8.5;   // Distance between lamps TODO: make random
const float LAMPTOWALL = 0.7;  // Distance between lamps and walls (btw has a bit of randomness)
const float LAMPGLOWINTENSITY = 0.3;  // Extra lamp glow - not lighting (idk it looks better)
const float LIGHTBULBRADIUS = 0.1;  // lightbulb radius

// Star settings
const float STARBRIGHTNESS = 1.0;  // Brightness of the stars (in the sky - change ambient for actual light)
const float STARTHRESH = 0.93;  // Amount of stars in the sky (higher = lower) - change with care
const float STARHORIZON = 0.5;  // Height which stars begin to show

// Movement settings
const float CAMHEIGHT = 1.6;  // camera's height above ground
const float BOB_SPEED_X = 3.0;      // bobbing speed for X axis (hz-ish)
const float BOB_SPEED_Y = 4.0;      // bobbing speed for Y axis
const float BOB_AMOUNT_X = 0.05;    // bobbing amplitude for X axis (meters)
const float BOB_AMOUNT_Y = 0.07;    // bobbing amplitude for Y axis (meters)
const float SPEED = 1.8;            // Camera movement speed (walk speed if you will)

// Properties of surfaces (after hitting)
struct Surface {
    float dist;         // Distance to surface
    vec3 col;          // Surface color
    int id;            // Surface type ID
    float shininess;   // Specular shininess
    float specStrength; // Specular strength
};

float hash11(float n) {  // rng
    return fract(sin(n*127.1)*43758.5453);
}

// Basic SDF's
float sdBox(vec3 p, vec3 b) {
    vec3 q = abs(p) - b;
    return length(max(q,0.0)) + min(max(q.x,max(q.y,q.z)),0.0);
}
float sdCylinder(vec3 p, float h, float r) {
    vec2 d = abs(vec2(length(p.xz),p.y)) - vec2(r,h);
    return min(max(d.x,d.y),0.0) + length(max(d,0.0));
}
float sdSphere(vec3 p, float s) {
  return length(p)-s;
}

// Create ground surface with concrete texture
Surface mapGround(vec3 p) {
    Surface s;
    s.dist = p.y;
    s.id = 0;      // Ground ID
    
    // Create concrete color using noise
    float octave1 = texture(iChannel1, p.xz * 0.02).r;   // also scale it a bit
    float octave2 = texture(iChannel1, (p.xz + vec2(69.69, 67.67)) * 0.067).r;
    float noise = octave1 * 0.3 + octave2 * 0.08;
    s.col = vec3(0.65, 0.65, 0.63) * (0.85 + noise);
    // TODO: add some cracks puddles smth
    
    
    // materials
    s.shininess = 2.0 + noise * 18.0;
    s.specStrength = 0.02 + noise * 0.3;
    
    return s;
}

// Create wall surfaces with brick texture
Surface mapWalls(vec3 p) {
    Surface s;
    s.id = 1;  // Wall ID
    
    // walls are js massive boxes i did the math if you wait 176 years you can reach the end
    vec3 boxSize = vec3(.3, WALL_HEIGHT, 1e10);
    float distToLeft = sdBox(p - vec3(-ALLEY_WIDTH, WALL_HEIGHT * 0.5, 0.0), boxSize);
    float distToRight = sdBox(p - vec3(ALLEY_WIDTH, WALL_HEIGHT * 0.5, 0.0), boxSize);
    
    s.dist = min(distToLeft, distToRight);  // Distance to nearest wall
    
    // texture
    vec2 uv = vec2(p.z * 0.8, p.y * 0.8);
    float wallSide = step(distToLeft, distToRight);  // Which wall are we on
    uv.x += wallSide * 67.67; // Offset texture per wall
    
    vec3 brickTex = texture(iChannel0, uv).rgb;
    
    // noise
    vec2 noiseUV = vec2(p.zy * 0.1);
    noiseUV.x += wallSide * 123.45;
    float noiseVal = texture(iChannel1, noiseUV).r;
    brickTex *= 0.6 + noiseVal * 0.4;
    
    s.col = brickTex;
    
    // materials
    s.shininess = 6.0 * (0.5 + noiseVal * 0.5);
    s.specStrength = 0.1 * (0.3 + noiseVal * 0.7);
    
    return s;
}


// Create street lamps - flickering lights
Surface mapLamps(vec3 p, float time) {
    Surface s;
    s.dist = 1000.0;
    s.id = 2;         // Lamp ID
    
    float lampZ = floor(p.z / LAMPSPACING);
    
    // get close lamps
    for (int i = -1; i <= 1; i++) {
        float z = (lampZ + float(i)) * LAMPSPACING;
        float seed = z * 13.579;
        
        // skip some lamps 15% chance
        if (hash11(seed * 0.7) < 0.15) continue;
        
        // random offset on opposite sides
        float side = mod(float(int(z/LAMPSPACING)), 2.0) * 2.0 - 1.0;
        side += (hash11(seed * 1.3) - 0.5) * 0.3;
        
        float xPos = side * (ALLEY_WIDTH - LAMPTOWALL - hash11(seed));
        vec3 lampBase = vec3(xPos, 0.0, z);
        vec3 localP = p - lampBase;
        
        // lamp pole
        float poleHeight = 4.5 + hash11(seed * 2.1) * 1.0;
        float poleDist = sdCylinder(localP - vec3(0, poleHeight*0.5, 0), poleHeight*0.5, 0.06);
        
        // lightbulb
        vec3 bulbP = localP - vec3(0, poleHeight, 0);
        float bulbDist = sdSphere(bulbP, LIGHTBULBRADIUS);
        
        float lampDist = min(poleDist, bulbDist);
        
        // if closest lamp, do this - probably can be optimized like merged into 1 function
        if (lampDist < s.dist) {
            s.dist = lampDist;
            
            // calc flicker
            float flickerSeed = seed * 11.3 + time * (1.5 + hash11(seed * 3.7) * 3.0);
            float flickerType = hash11(seed * 4.9);
            float flicker = 1.0;
            vec3 bulbColor = vec3(0.9, 0.95, 1.0);  // Default cold white
            
            // Check flicker typs
            if (flickerType < 0.25) {
                // Subtle smooth flicker
                flicker = 0.92 + 0.08 * sin(flickerSeed * 4.0);
            } else if (flickerType < 0.4) {
                // bigger flicker + color chg
                float flashFreq = 1.5 + hash11(seed * 5.2) * 2.0;
                flicker = 0.7 + 0.3 * smoothstep(0.3, 0.7, fract(sin(time * flashFreq + seed) * 43758.5453));
                bulbColor = mix(vec3(1.0, 0.9, 0.8), vec3(0.8, 0.9, 1.0), fract(time * flashFreq));
            } else {
                // Constant light
                flicker = 0.96 + 0.04 * sin(time * 0.5 + seed);
            }
            
            // 20% lamp is dead
            if (hash11(seed * 6.1) < 0.2) flicker = 0.0;
            
            // check pole or bulb
            if (bulbDist < poleDist) {
                // set actual bulb color
                if (flicker > 0.0) {
                    s.col = bulbColor * (0.5 + flicker * 2.5);
                } else {
                    s.col = vec3(0.02, 0.02, 0.02); // dark
                }
            } else {
                // always black pole
                s.col = vec3(0.15, 0.15, 0.16);  // eh might add variation here
            }
            
            // material
            s.shininess = 18.0;
            s.specStrength = 0.3;
        }
    }
    
    return s;
}

// Create trash cans scattered in alley
Surface mapTrash(vec3 p) {
    Surface s;
    s.dist = 1000.0;
    s.id = 3;         // Trash ID
    
    float segZ = floor(p.z / SEGMENT_LENGTH);
    
    for (int i = -1; i <= 1; i++) {
        float z = (segZ + float(i)) * SEGMENT_LENGTH;
        float seed = z * 7.123;  // seed
        
        // skip some segments (no trash cans)
        if (hash11(seed) > 0.4) continue;
        
        // Random position for trash can - this is bad
        float side = hash11(seed * 1.7) > 0.5 ? 1.0 : -1.0;
        float xPos = side * (ALLEY_WIDTH - 1. - hash11(seed));
        float zOffset = (hash11(seed * 2.3) - 0.5) * 2.0;
        
        vec3 canPos = vec3(xPos, 0.0, z + zOffset);
        vec3 localP = p - canPos;
        
        // Random can dimensions i had gpt with this lol
        float h = 0.9 + hash11(seed * 3.1) * 0.3;
        float r = 0.3 + hash11(seed * 4.2) * 0.1;
        
        float canDist = sdCylinder(localP - vec3(0, h*0.5, 0), h*0.5, r);
        
        // if closest we reupdate
        if (canDist < s.dist) {
            s.dist = canDist;
            
            // gray color + noise
            vec3 baseGray = vec3(0.5, 0.52, 0.53);
            vec2 canUV = localP.xz * 0.2;
            float noiseVal = texture(iChannel1, canUV).r;
            s.col = baseGray * (0.85 + noiseVal * 0.15);
            
            // material
            s.shininess = 10.0 + noiseVal * 3.0; 
            s.specStrength = 0.3 + noiseVal * 0.05;
        }
    }
    
    return s;
}

// Combine all scene objects
Surface map(vec3 p, float time) {
    // TODO: don't calc texture until we know it's the closest
    // probably will help perf a lot
    Surface ground = mapGround(p);
    Surface walls = mapWalls(p);
    Surface trash = mapTrash(p);
    Surface lamps = mapLamps(p, time); // TODO: Add other things like street signs
    
    // find closest then return the hit point as a Surface
    Surface result = ground;
    
    if(walls.dist < result.dist) result = walls;
    if(trash.dist < result.dist) result = trash;
    if(lamps.dist < result.dist) result = lamps;
    
    return result;
}

// Calculate surface normal
vec3 calcNormal(vec3 p, float time) {
    vec2 e = vec2(0.001, 0.0);  // small offset like elipson
    return normalize(vec3(
        map(p + e.xyy, time).dist - map(p - e.xyy, time).dist,
        map(p + e.yxy, time).dist - map(p - e.yxy, time).dist,
        map(p + e.yyx, time).dist - map(p - e.yyx, time).dist
    ));
}

// Calculate soft shadows
float softShadow(vec3 ro, vec3 rd, float time, float lightDist) {
    float res = 1.0;  // Start fully lit
    float dist = MINDIST * 2.;  // small start dist
    
    for (int i = 0; i < SHADOWMAXSTEPS; i++) {
        if (dist > min(SHADOWMAXDIST, lightDist)) break;
        
        Surface s = map(ro + rd * dist, time);
        
        // we skip lamps when casting shadows
        // ngl ts is chopped
        // todo: less chopped
        if (s.id == 2 && (lightDist - dist < 0.5)) {
            dist += 0.2;  // just increment i suppose
            continue;
        }
        
        res = min(res, SHADOWSOFTNESS * s.dist / dist);
        if (res <= 0.001) return 0.0;  // Fully shadowed
        
        dist += max(s.dist, 0.02);
    }
    return clamp(res, 0.0, 1.0);
}

// Get info about lamps for lighting
void getLampInfo(vec3 p, float time, out vec3 lampPos[7], out float lampIntensity[7], out vec3 lampColors[7], out int numLamps) {
    float lampZ = floor(p.z / LAMPSPACING);
    
    numLamps = 0;
    
    // Check lamps
    for (int i = -3; i <= 3; i++) {
        if (numLamps >= 7) break;  // 7 lamps
        
        float z = (lampZ + float(i)) * LAMPSPACING;
        float seed = z * 13.579;
        
        // skip missing lamps 15%
        if (hash11(seed * 0.7) < 0.15) continue;
        
        // lamp position (same logic as mapLamps)
        float side = mod(float(int(z/LAMPSPACING)), 2.0) * 2.0 - 1.0;
        side += (hash11(seed * 1.3) - 0.5) * 0.3;
        
        float lampOffset = hash11(seed);
        float xPos = side * (ALLEY_WIDTH - LAMPTOWALL - lampOffset);
        
        float poleHeight = 4.5 + hash11(seed * 2.1) * 1.0;
        
        // flicker and color (same as mapLamps)
        float flickerSeed = seed * 11.3 + time * (1.5 + hash11(seed * 3.7) * 3.0);
        float flickerType = hash11(seed * 4.9);
        float flicker = 1.0;
        vec3 bulbColor = vec3(0.85, 0.9, 1.0);
        
        if (flickerType < 0.25) {
            flicker = 0.92 + 0.08 * sin(flickerSeed * 4.0);
        } else if (flickerType < 0.4) {
            float flashFreq = 1.5 + hash11(seed * 5.2) * 2.0;
            flicker = 0.7 + 0.3 * smoothstep(0.3, 0.7, fract(sin(time * flashFreq + seed) * 43758.5453));
            bulbColor = mix(vec3(1.0, 0.9, 0.8), vec3(0.8, 0.9, 1.0), fract(time * flashFreq));
        } else {
            flicker = 0.96 + 0.04 * sin(time * 0.5 + seed);
        }
        
        // Dead lamps
        if (hash11(seed * 6.1) < 0.2) flicker = 0.0;
        
        // Store lamp info
        lampPos[numLamps] = vec3(xPos, poleHeight - 0.35, z);
        lampIntensity[numLamps] = flicker;
        lampColors[numLamps] = bulbColor;
        numLamps++;
    }
}

// Main raymarching function
Surface rayMarch(vec3 ro, vec3 rd, float time) {
    Surface obj;
    obj.col = vec3(0.0);
    obj.id = -1;      // -1 means no hit
    obj.dist = 0.0;
    obj.shininess = 8.0;
    obj.specStrength = 0.0;
    
    float dist = 0.0;
    for (int i = 0; i < STEPS; i++) {
        vec3 pos = ro + rd * dist;
        Surface scene = map(pos, time);
        
        // super close probably a hit
        if (scene.dist < MINDIST) {
            obj = scene;
            obj.dist = dist;
            return obj;
        }
        
        // move
        dist += scene.dist;
        if (dist > MAXDIST) break;  // too far give up
    }
    
    obj.id = -1;  // nothing, set bg
    return obj;
}

// Render starfield for sky
vec3 renderStars(vec3 rd) {
    // Fade near horizon
    float horizonFade = smoothstep(0.0, STARHORIZON, abs(rd.y));  // aprox 30deg out of horizon - gpt carry

    // turn ray into random vals
    vec2 st = rd.xz / max(abs(rd.y), 0.001);
    float starRand = hash11(dot(st, vec2(12.9898, 78.233)));
    float starRand2 = hash11(st.x * st.y * rd.z);

    if (starRand > STARTHRESH && starRand2 > STARTHRESH) {  // random points
        vec2 starCenter = (st + 0.5) / 150.0;
        vec2 delta = st - starCenter;
        
        float newRand = hash11(starRand * starRand2); // new random from 0 - 1

        // set star brightness
        float brightness = newRand;

        // colors
        vec3 color = mix(vec3(1.0, 0.9, 0.8), vec3(0.8, 0.9, 1.0), hash11(newRand));

        return color * brightness * horizonFade;
    }

    return vec3(0.);
}


vec3 phongLighting(vec3 rayO, vec3 rayD, Surface surface, float time) {
    vec3 hitPoint = rayO + rayD * surface.dist;
    vec3 normal = calcNormal(hitPoint, time);
    
    // Push point slightly onto surface to avoid artifacts
    hitPoint += normal * 0.01;

    // Get lamp info
    vec3 lampPos[7];
    float lampIntensity[7];
    vec3 lampColors[7];
    int numLamps;
    getLampInfo(hitPoint, time, lampPos, lampIntensity, lampColors, numLamps);

    vec3 lighting = vec3(0.0);
    
    // Iterate over all lamps
    for (int i = 0; i < 7; i++) {
        if (i >= numLamps) break;

        // Light direction and distance
        vec3 toLight = lampPos[i] - hitPoint;
        float dist = length(toLight);
        vec3 lightDir = toLight / dist;

        // Light attenuation gpt carry
        float atten = 1.0 / (1.0 + 0.09 * dist + 0.015 * dist * dist);
        atten *= lampIntensity[i]; // Apply flicker

        // Shadow
        float shadow = softShadow(hitPoint + normal * 0.05, lightDir, time, dist);

        // Diffuse
        float diffuse = max(dot(normal, lightDir), 0.0) * shadow;

        // Specular
        vec3 viewDirection = normalize(rayO - hitPoint);
        vec3 halfwayDir = normalize(lightDir + viewDirection);
        float spec = pow(max(dot(normal, halfwayDir), 0.0), surface.shininess);
        vec3 specular = surface.specStrength * spec * lampColors[i] * shadow;

        // Combine for point
        lighting += (diffuse * surface.col + specular) * lampColors[i] * atten * LIGHTSTRENGTH;
    }

    // Add ambient lighting
    lighting += AMBIENT * surface.col;

    return lighting;
}


void mainImage(out vec4 fragColor, in vec2 fragCoord) {
    // CAMERA SETUP todo: add looking around
    vec2 uv = (fragCoord * 2.0 - iResolution.xy) / iResolution.y;
    
    float camZ = iTime * SPEED;  // Move forward over time
    vec3 rayOrigin = vec3(0.0, CAMHEIGHT, camZ);  // Eye height
    
    // cam bobbing
    rayOrigin += vec3(
        sin(iTime * BOB_SPEED_X) * BOB_AMOUNT_X,
        sin(iTime * BOB_SPEED_Y) * BOB_AMOUNT_Y,
        0.0
    );

    vec3 forward = vec3(0, 0, 1);
    vec3 right = vec3(1, 0, 0);
    vec3 up = vec3(0, 1, 0);
    
    float focalLength = 1.0 / tan(FOV * 0.5);
    vec3 rayDir = normalize(uv.x * right + uv.y * up + forward * focalLength);
    
    // RAYMARCHING
    Surface surface = rayMarch(rayOrigin, rayDir, iTime);
    
    vec3 col;
    if(surface.id != -1) {
        // We hit something (not bg)
        vec3 hitPoint = rayOrigin + rayDir * surface.dist;
        vec3 normal = calcNormal(hitPoint, iTime);
        
        // LIGHTING
        col = phongLighting(rayOrigin, rayDir, surface, iTime);
        
        // now we gotta make them glow (the lights, i mean)
        if (surface.id == 2) {  // TODO: bloom would be super cool
            float brightness = dot(surface.col, vec3(0.333));
            if (brightness > 1.0)  // is on = add glow
                col += surface.col * LAMPGLOWINTENSITY;
        }
        
        // apply fog
        float fogFactor = exp(-FOGDENSITY * surface.dist);
        col = col * fogFactor + FOGCOLOR * (1.0 - fogFactor);
    } else {
        // BG - set sky + stars
        col = renderStars(rayDir);
    }

    // vignette effect
    col *= 1.0 - 0.4 * length(uv * 0.4);
    
    // change colors
    col = pow(col, vec3(1.05));  // Slight gamma adjustment
    col *= vec3(0.9, 1.0, 1.15);  // make more cool - ts is tuff
    
    fragColor = vec4(col, 1.0);
}

