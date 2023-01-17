<?php

class Body
{
    public float $x = 0;
    public float $y = 0;
    public float $z = 0;
    public float $vx = 0;
    public float $vy = 0;
    public float $vz = 0;
    public float $mass = 0;
}

function advance_broken(array &$bodies, float $dt): void
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

function advance(array &$bodies, float $dt)
{
    foreach ($bodies as $i => $body) {
        $slice = array_slice($bodies, $i + 1);
        foreach ($slice as $j => $body2) {
            $dx = $body->x - $body2->x;
            $dy = $body->y - $body2->y;
            $dz = $body->z - $body2->z;

            $distance = sqrt($dx*$dx + $dy*$dy + $dz*$dz);               
            $mag = $dt / ($distance * $distance * $distance);

            $body->vx = $body->vx - $dx * $body2->mass * $mag;
            $body->vy = $body->vy - $dy * $body2->mass * $mag;
            $body->vz = $body->vz - $dz * $body2->mass * $mag;

            $body2->vx = $body2->vx + $dx * $body->mass * $mag;
            $body2->vy = $body2->vy + $dy * $body->mass * $mag;
            $body2->vz = $body2->vz + $dz * $body->mass * $mag;
        }
    }      

    foreach ($bodies as $body) {
        $body->x = $body->x + $dt * $body->vx;
        $body->y = $body->y + $dt * $body->vy;
        $body->z = $body->z + $dt * $body->vz;
    }      
}         
function advance2(array &$bodies, float $dt)
{
    for($i=0; $i < count($bodies); $i++) {
        for($j=$i+1; $j < count($bodies); $j++) {   
            $dx = $bodies[$i]->x - $bodies[$j]->x;
            $dy = $bodies[$i]->y - $bodies[$j]->y;
            $dz = $bodies[$i]->z - $bodies[$j]->z;

            $distance = sqrt($dx*$dx + $dy*$dy + $dz*$dz);               
            $mag = $dt / ($distance * $distance * $distance);

            $bodies[$i]->vx -= $dx * $bodies[$j]->mass * $mag;
            $bodies[$i]->vy -= $dy * $bodies[$j]->mass * $mag;
            $bodies[$i]->vz -= $dz * $bodies[$j]->mass * $mag;

            $bodies[$j]->vx += $dx * $bodies[$i]->mass * $mag;
            $bodies[$j]->vy += $dy * $bodies[$i]->mass * $mag;
            $bodies[$j]->vz += $dz * $bodies[$i]->mass * $mag;
        }
    }      

    for($i=0; $i < count($bodies); $i++) {
        $bodies[$i]->x += $dt * $bodies[$i]->vx;
        $bodies[$i]->y += $dt * $bodies[$i]->vy;
        $bodies[$i]->z += $dt * $bodies[$i]->vz;
    }      
}         

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

$bodies = [clone $jupiter, clone $saturn];
advance($bodies, 0.01);
$bodies2 = [clone $jupiter, clone $saturn];
advance2($bodies2, 0.01);
var_dump($bodies[0]->vz == $bodies2[0]->vz);
var_dump($bodies[1]->vz == $bodies2[1]->vz);
echo "\n";
