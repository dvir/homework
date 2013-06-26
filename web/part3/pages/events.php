<?php
$result = $mysqli->query("
SELECT 
    event.*,
    CONCAT(user.first_name, ' ', user.last_name) AS organizer_name 
FROM 
    event 
    JOIN user ON event.organizer_id=user.id
ORDER BY `when` ASC") or die($mysqli->error);
$events = array();
while ($event = $result->fetch_array()) {
    $events[] = $event; 
}
?>
<aside>
    <?php if (isset($_SESSION["user"]) && $_SESSION["user"]["type"] != 0) { ?>
    <section>
      <a href="javascript:void(0);" onclick="javascript:unhide('create_event');"><h2>יצירת אירוע</h2></a>
    </section>
    <?php } ?>

    <section>
      <a href="javascript:void(0);" onclick="javascript:unhide('events_main');"><h2>ארועים</h2></a>
      <ul>
        <?php foreach ($events as $idx => $event) { ?>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('event-<?php echo $idx;?>');"><?php echo $event["title"];?></a></li>
        <?php } ?>
      </ul>
    </section>

    <?php
    include("announcements.php");
    ?>
</aside>

<article id="events">
  <div id="events_main" class="unhidden">
    <img src="./img/events.jpg" />
  </div>
<?php foreach ($events as $idx => $event) { ?>
  <div id="event-<?php echo $idx;?>" class="hidden">
    <?php if (isset($_SESSION["user"]) && ($_SESSION["user"]["id"] == $event["organizer_id"] || $_SESSION["user"]["type"] == 2)) { ?>
    <span class="close"><a onclick="return confirm('האם אתה בטוח שברצונך למחוק את האירוע?');" href="index.php?page=<?php echo $page;?>&delete-event&id=<?php echo $event["id"];?>">מחק</a></span>
    <?php } ?>
    <h3><?php echo $event["title"];?></h3>
    <p><?php echo $event["description"];?></p>
    <p><b>איפה?</b> <?php echo $event["location"];?></p>
    <p><b>מתי?</b> <?php echo $event["when"];?></p>
    <p><b>מי מארגן?</b> <?php echo $event["organizer_name"];?></p>
    <p><b>כמה אנשים יכולים להשתתף?</b> <?php echo $event["capacity"];?></p>
  </div>
<?php } ?>

    <div id="create_event" class="<?php if ($section == "create_event") { echo "unhidden"; } else { echo "hidden"; }?>">
        <h3>פתיחת אירוע בקאנטרי</h3>
        <form action="index.php?page=events" method="POST">
            <ul>    
                <li><label>כותרת האירוע<font color="red" size="2">*</font>: <input type="text" required="true" name="title" placeholder="כותרת האירוע" /></label></li>
                <li><label>תיאור האירוע<font color="red" size="2">*</font>: <textarea required="true" name="description" placeholder="תאר את האירוע בקצרה"></textarea></label></li>
                <li><label>מיקום<font color="red" size="2">*</font>: <input type="text" required="true" name="location" placeholder="מיקום האירוע" /></label></li>
                <li>
                    <label>כמות משתתפים<font color="red" size="2">*</font>:
                        <input type="number" min="1" required="true" name="capacity" placeholder="כמות משתתפים מקסימלית" />
                    </label>
                </li>
                <li>
                    <label>תאריך ושעה<font color="red" size="2">*</font>:
                        <input type="datetime-local" required="true" name="when" />
                    </label>
                </li>
                <li>
                    <input type="submit" name="create_event" value="צור אירוע" />
                </li>
            </ul>
        </form>
    </div>
</article>
