<?php
session_start();

require_once("functions.php");
require_once("db_open.php");

$page = isset($_REQUEST["page"]) ? $_REQUEST["page"] : null;
include("input-handler.php");

if (empty($page)) {
    $page = "home";
}

if (!file_exists("./pages/".$page.".php")) {
    $page = 404;
}

include("header.php");

if (!empty($notification)) {
?>
<article id="notification">
    <h3><?php echo $notification;?></h3>
</article>    
<?php
}

include("./pages/".$page.".php");

include("footer.php");

require_once("db_close.php");
