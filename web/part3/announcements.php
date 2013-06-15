<?php
$result = $mysqli->query("SELECT message.*,user.first_name FROM message JOIN user ON message.user_id = user.id ORDER BY message.date_created DESC LIMIT 5") or die($mysqli->error);
?>
<section id="announcements">
  <a href="javascript:void(0);"><h2>הודעות</h2></a>
  <ul>
    <?php while ($message = $result->fetch_array()) { ?>
    <li>
        <?php if (isset($_SESSION["user"]) && $_SESSION["user"]["id"] == $message["user_id"]) { ?>
        <span class="close"><a href="index.php?page=<?php echo $page;?>&delete-announcement&id=<?php echo $message["id"];?>">מחק</a></span>
        <?php } ?>
        <span class="title"><?php echo $message["title"];?></span>
        <p>
            <?php echo $message["description"];?><br />
            - <?php echo $message["first_name"];?>, <?php echo date("d/m H:m", strtotime($message["date_created"]));?>
        </p>
    </li>
    <?php } ?>
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
  </ul>
</section>
