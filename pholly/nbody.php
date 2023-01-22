<?php // @pholyglot

class Body
{
    public float $x;
    public float $y;
    public float $z;
    public float $vx;
    public float $vy;
    public float $vz;
    public float $mass;
    public function offsetMomentum(float $px, float $py, float $pz): void
    {
        $pi = 3.141592653589793;   
        $solarmass = 4. * $pi * $pi;
        $this->vx = (0. - $px) / $solarmass;
        $this->vy = (0. - $py) / $solarmass;
        $this->vz = (0. - $pz) / $solarmass;
    }                             
}

/**
 * @param array<Body> $bodies
 * @param float $dt
 */
function advance(array &$bodies, float $dt): void
{
    foreach ($bodies as $i => $body) {
        $slice = array_slice($bodies, $i + 1);
        foreach ($slice as $body2) {
            $dx = $body->x - $body2->x;
            $dy = $body->y - $body2->y;
            $dz = $body->z - $body2->z;

            $distance = sqrt($dx * $dx + $dy * $dy + $dz * $dz);               
            $mag = $dt / ($distance * $distance * $distance);

            $body->vx = $body->vx - $dx * $body2->mass * $mag;
            $body->vy = $body->vy - $dy * $body2->mass * $mag;
            $body->vz = $body->vz - $dz * $body2->mass * $mag;

            $body2->vx = $body2->vx + $dx * $body->mass * $mag;
            $body2->vy = $body2->vy + $dy * $body->mass * $mag;
            $body2->vz = $body2->vz + $dz * $body->mass * $mag;
        }
    }      

    foreach ($bodies as $body3) {
        $body3->x = $body3->x + $dt * $body3->vx;
        $body3->y = $body3->y + $dt * $body3->vy;
        $body3->z = $body3->z + $dt * $body3->vz;
    }      
}

/**
 * @param array<Body> $bodies
 */
function energy(array &$bodies): float
{
    $e = 0.0;
    foreach ($bodies as $i => $body) {
        $tmp2 =
              $body->vx * $body->vx 
            + $body->vy * $body->vy 
            + $body->vz * $body->vz;
        $e += 0.5 * $body->mass * $tmp2;

        $slice = array_slice($bodies, $i + 1);
        foreach ($slice as $body2) {
            $dx = $body->x - $body2->x;
            $dy = $body->y - $body2->y;
            $dz = $body->z - $body2->z;
            $distance = sqrt($dx*$dx + $dy*$dy + $dz*$dz);
			$e -= ($body->mass * $body2->mass) / $distance;
        }
    }
    return $e;
}

function main(): int
{
    $daysperyear = 365.24;
    $pi = 3.141592653589793;   
    $solarmass = 4. * $pi * $pi;

    $jupiter = new Body();
    $jupiter->x = 4.84143144246472090e+00;
    $jupiter->y = -1.16032004402742839e+00;
    $jupiter->z = -1.03622044471123109e-01;
    $jupiter->vx = 1.66007664274403694e-03 * $daysperyear;
    $jupiter->vy = 7.69901118419740425e-03 * $daysperyear;
    $jupiter->vz = -6.90460016972063023e-05 * $daysperyear;
    $jupiter->mass = 9.54791938424326609e-04 * $solarmass;

    $saturn = new Body();
    $saturn->x = 8.34336671824457987e+00;
    $saturn->y = 4.12479856412430479e+00;
    $saturn->z = -4.03523417114321381e-01;
    $saturn->vx = -2.76742510726862411e-03 * $daysperyear;
    $saturn->vy = 4.99852801234917238e-03 * $daysperyear;
    $saturn->vz = 2.30417297573763929e-05 * $daysperyear;
    $saturn->mass = 2.85885980666130812e-04 * $solarmass;

    $uranus = new Body();
    $uranus->x = 1.28943695621391310e+01;
    $uranus->y = -1.51111514016986312e+01;
    $uranus->z = -2.23307578892655734e-01;
    $uranus->vx = 2.96460137564761618e-03 * $daysperyear;
    $uranus->vy = 2.37847173959480950e-03 * $daysperyear;
    $uranus->vz = -2.96589568540237556e-05 * $daysperyear;
    $uranus->mass = 4.36624404335156298e-05 * $solarmass;

    $neptune = new Body();
    $neptune->x = 1.53796971148509165e+01;
    $neptune->y = -2.59193146099879641e+01;
    $neptune->z = 1.79258772950371181e-01;
    $neptune->vx = 2.68067772490389322e-03 * $daysperyear;
    $neptune->vy = 1.62824170038242295e-03 * $daysperyear;
    $neptune->vz = -9.51592254519715870e-05 * $daysperyear;
    $neptune->mass = 5.15138902046611451e-05 * $solarmass;

    $sun = new Body();
    $sun->x = 0.;
    $sun->y = 0.;
    $sun->z = 0.;
    $sun->vx = 0.;
    $sun->vy = 0.;
    $sun->vz = 0.;
    $sun->mass = $solarmass;

    $bodies = [$sun, $jupiter, $saturn, $uranus, $neptune];

    $px = 0.0;
    $py = 0.0;   
    $pz = 0.0;            
    foreach ($bodies as $body2) {
        $px += $body2->vx * $body2->mass;
        $py += $body2->vy * $body2->mass;      
        $pz += $body2->vz * $body2->mass;            
    }      
    $b = $bodies[0];
    $b->offsetMomentum($px, $py, $pz);

    $e = energy($bodies);
    printf("%f\n", $e);
    $k = 0;
    do {
        advance($bodies, 0.01);
        $k++;
    } while ($k < 50000000);

    $e2 = energy($bodies);
    printf("%f\n", $e2);

    return 0;
}
