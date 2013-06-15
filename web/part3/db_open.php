<?php
$mysqli = new mysqli('localhost', 'root', 'ilovemysql', 'web');

if ($mysqli->connect_error) {
        die('Connect Error (' . $mysqli->connect_errno . ') '
                        . $mysqli->connect_error);
}

$mysqli->query("SET NAMES 'utf8'");
