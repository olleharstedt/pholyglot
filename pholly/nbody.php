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
}

function main(): int
{
    $daysperyear = 365.24;
    $pi = 3.141592653589793;   
    $solarmass = 4 * $pi * $pi;

	$jupiter = new Body();
	$jupiter->x = 4.84143144246472090;
	$jupiter->y = -1.16032004402742839;
	$jupiter->z = -1.03622044471123109e-01;
	$jupiter->vx = 1.66007664274403694e-03 * $daysperyear;
	$jupiter->vy = 7.69901118419740425e-03 * $daysperyear;
	$jupiter->vz = -6.90460016972063023e-05 * $daysperyear;
	$jupiter->mass = 9.54791938424326609e-04 * $solarmass;                  
	$bodies = [$jupiter];
	printf("%f", $bodies[0]->x);
	return 0;
}
