<?php ob_start();

function main()
{
    //?>
    this is silent
    //<?php
    $stdout = fopen('php://stdout', 'w');
    fwrite($stdout, "This is shown");
    //?>
    this is also silent
    <?php
}

//?>
//<?php main(); ob_end_clean();
