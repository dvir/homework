<?php
$id = $_REQUEST["id"];
$result = $mysqli->query("SELECT class.*, CONCAT(user.first_name, ' ', user.last_name) AS teacher_name 
                            FROM class JOIN user ON class.teacher_id=user.id 
                            WHERE class.id='$id'
                            ") or die($mysqli->error);

$class = $result->fetch_array();

$result = $mysqli->query("SELECT user.* FROM
                            class
                                JOIN user_class ON class.id=user_class.class_id
                                JOIN user ON user.id=user_class.user_id
                                ") or die($mysqli->error);
$participants = array();
while ($participant = $result->fetch_array()) {
    $pariticpants[] = $participant;
}
?>
<aside>
    <?php
    include("announcements.php");
    ?>
</aside>
<article id="class">
    <span class="close"><a href="index.php?page=<?php echo $page;?>&update-class&id==<?php echo $class["id"];?>">עדכן</a>&nbsp;<a href="index.php?page=<?php echo $page;?>&delete-class&id=<?php echo $class["id"];?>">מחק</a></span>
    <h3><?php echo $class["name"];?></h3>
    <p>בהנחיית <?php echo $class["teacher_name"];?>, <?php echo get_day($class["day"]);?> <?php echo $class["hour_start"].":00-".$class["hour_end"].":00";?></p> 
    <p><?php echo $class["description"];?></p>

    <h3>משתתפים: <?php echo count($participants);?></h3>
    <?php if (count($participants) > 0) { ?>
        <ul>
            <?php foreach ($participants as $idx => $participant) { ?>
            <li><?php echo $idx;?>. <?php echo $participant["first_name"]." ".$participant["last_name"];?></li>
            <?php } ?>
        </ul>
    <?php } ?>
</article>
