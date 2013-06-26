<?php
$result = $mysqli->query("SELECT message.*,user.first_name FROM message JOIN user ON message.user_id = user.id ORDER BY message.date_created DESC") or die($mysqli->error);
$messages = array();
while ($message = $result->fetch_array()) {
    $messages[] = $message;
}
?>
<section id="announcements">
  <h2>הודעות</h2>
  <ul>
    <?php foreach ($messages as $idx => $message) { ?>
    <li class="message <?php echo ($idx >= 3) ? "hidden" : "unhidden";?>">
        <?php if (isset($_SESSION["user"]) && ($_SESSION["user"]["id"] == $message["user_id"] || $_SESSION["user"]["type"] == 2)) { ?>
        <span class="close"><a onclick="return confirm('האם אתה בטוח שברצונך למחוק את ההודעה הזו?');" href="index.php?page=<?php echo $page;?>&delete-announcement&id=<?php echo $message["id"];?>">מחק</a></span>
        <?php } ?>
        <span class="title"><?php echo $message["title"];?></span>
        <p>
            <?php echo $message["description"];?><br />
            - <?php echo $message["first_name"];?>, <?php echo date("d/m H:m", strtotime($message["date_created"]));?>
        </p>
    </li>
    <?php } ?>
    <?php if (isset($_SESSION["user"]) && $_SESSION["user"]["type"] != 0) { ?>
    <li>
        <a href="javascript:void(0);" class="post_message">הוסף הודעה</a>
        <form id="post_message" action="index.php?page=<?php echo $page;?>" method="POST" style="display: none;">
            <ul>
                <li><input type="text" name="title" placeholder="הקלד נושא הודעה" required="true" /></li>
                <li><textarea name="description" placeholder="הקלד הודעה לפרסום" required="true" ></textarea></li>
                <li><input type="submit" name="post_message" value="פרסם הודעה" /></li>
            </ul>
        </form>
    </li>
    <?php } ?>
  </ul>
</section>
