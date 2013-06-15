<?php
$result = $mysqli->query("SELECT * FROM event ORDER BY `when` ASC") or die($mysqli->error);
$events = array();
while ($event = $result->fetch_array()) {
    $events[] = $event; 
}
?>
<aside>
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
    <h3><?php echo $event["title"];?></h3>
    <p><?php echo $event["description"];?></p>
  </div>
<?php } ?>

  <div id="event-1" class="hidden">
    <h3>חג המים</h3>

    <p>חג המים הוא חג מסורתי שחוגגים במושב מאז הקמת המושב.<br />
    חג המים מצויין ביום שבת הראשון של ספטמבר כל שנה.<br />
    האירוע עשיר בהפעלות לכל המשפחה, מתקנים מתנפחים, תחרויות שונות כגון שחייה, שש בש ועוד.</p>
  </div>
  <div id="event-2" class="hidden">
    <h3>מסיבת שבועות</h3>

    <p>בכל שנה, ערב חג שבועות, מציין הנוער במושב את חג השבועות במסיבה אשר אליה מגיעים בני נוער מכל האזור על מנת לקחת חלק בחוויה ייחודית.</p>
	<p>מסיבת בריכה ענקית לרגל חג השבועות</p>
  </div>
</article>
