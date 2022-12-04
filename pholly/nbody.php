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
    $jupiter = new Body();
    $jupiter->x = 10.0;
    $bodies = [$jupiter];
    //printf("%f", $bodies[0]->x);
    return 0;
}
