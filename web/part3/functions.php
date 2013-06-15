<?php
function get_day($day) {
    $days = array("ראשון", "שני", "שלישי", "רביעי", "חמישי", "שישי", "שבת");
    return "יום ".$days[$day-1];
}
