<?php
    $showId = null;
    $section = "classes_main";
    if ($success && isset($_POST["create_class"])) {
        $section = "class";
        $showId = $newClassId;
    } else if ($success && isset($_POST["search_classes"])) {
        $section = "search_classes";
    } else if ($success && isset($_REQUEST["delete-class"])) {
    } else if (isset($_GET["id"])) {
        $section = "class";
        $showId = $_GET["id"];
    }

    $result = $mysqli->query("SELECT 
                                class.*, 
                                CONCAT(user.first_name, ' ', user.last_name) AS teacher_name 
                              FROM 
                                class JOIN user ON class.teacher_id=user.id 
                                ") or die($mysqli->error);
    $classes = array();
    while ($class = $result->fetch_array(MYSQLI_ASSOC)) {
        $result2 = $mysqli->query("SELECT user.* FROM
                                    class
                                        JOIN user_class ON class.id=user_class.class_id
                                        JOIN user ON user.id=user_class.user_id
                                        WHERE class.id='".$class["id"]."'
                                        ") or die($mysqli->error);
        $participants = array();
        while ($participant = $result2->fetch_array(MYSQLI_ASSOC)) {
            $participants[] = $participant;
        }
        $class["participants"] = $participants;

        $classes[] = $class;
    }
?>
<aside>
    <section>
      <a href="javascript:void(0);" onclick="javascript:unhide('search_classes');"><h2>חיפוש חוגים</h2></a>
    </section>

    <?php if (isset($_SESSION["user"]) && $_SESSION["user"]["type"] != 0) { ?>
    <section>
      <a href="javascript:void(0);" onclick="javascript:unhide('create_class');"><h2>פתיחת חוג</h2></a>
    </section>
    <?php } ?>
    
    <section>
      <a href="javascript:void(0);" onclick="javascript:unhide('classes_main');"><h2>חוגים</h2></a>
      <ul>
        <?php foreach ($classes as $class) { ?>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('class-<?php echo $class["id"];?>');"><?php echo $class["name"];?></a></li>
        <?php } ?>
      </ul>
    </section>

    <?php
    include("announcements.php");
    ?>
</aside>

<article id="classes">
<div id="search_classes" class="<?php if ($section == "search_classes") { echo "unhidden"; } else { echo "hidden"; }?>">
    <h3>חיפוש חוגים בקאנטרי</h3>
        <form action="index.php?page=classes" method="POST">
            <ul>    
            <li><label>שם החוג: <input type="text" id="class_name" name="name" placeholder="שם מלא או חלק משם החוג" <?php if (isset($_POST["name"])) { echo 'value="'.$_POST["name"].'"'; } ?> /></label></li>
                <li>
                    <label>יום בשבוע:
                        <?php $day = isset($_POST["day"]) ? $_POST["day"] : null; ?>
                        <select name="day" id="class_day">
                            <option value="0" <?php if ($day == 0 || empty($day)) echo 'selected="selected"';?>>הכל</option>
                            <option value="1" <?php if ($day == 1) echo 'selected="selected"';?>>יום ראשון</option>
                            <option value="2" <?php if ($day == 2) echo 'selected="selected"';?>>יום שני</option>
                            <option value="3" <?php if ($day == 3) echo 'selected="selected"';?>>יום שלישי</option>
                            <option value="4" <?php if ($day == 4) echo 'selected="selected"';?>>יום רביעי</option>
                            <option value="5" <?php if ($day == 5) echo 'selected="selected"';?>>יום חמישי</option>
                            <option value="6" <?php if ($day == 6) echo 'selected="selected"';?>>יום שישי</option>
                            <option value="7" <?php if ($day == 7) echo 'selected="selected"';?>>יום שבת</option>
                        </select> 
                    </label>
                </li>
                <li>
                    <label>שעת התחלה:
                        <?php $hour_start = isset($_POST["hour_start"]) ? $_POST["hour_start"] : -1; ?>
                        <select name="hour_start" id="class_hour_start">
                            <option value="-1" <?php if ($hour_start == -1) echo 'selected="selected"';?>>הכל</option>
                            <?php for ($i = 0; $i < 24; ++$i) { ?>
                            <option value="<?php echo $i;?>" <?php if ($hour_start == $i) echo 'selected="selected"';?>><?php echo str_pad($i, 2, "0", STR_PAD_LEFT);?>:00</option>
                            <?php } ?>
                        </select>       
                    </label>
                </li>
                <li>
                    <label>שעת סיום:
                        <?php $hour_end = isset($_POST["hour_end"]) ? $_POST["hour_end"] : -1; ?>
                        <select name="hour_end" id="class_hour_end">
                            <option value="-1" <?php if ($hour_end == -1) echo 'selected="selected"';?>>הכל</option>
                            <?php for ($i = 0; $i < 24; ++$i) { ?>
                            <option value="<?php echo $i;?>" <?php if ($hour_end == $i) echo 'selected="selected"';?>><?php echo str_pad($i, 2, "0", STR_PAD_LEFT);?>:00</option>
                            <?php } ?>
                        </select>       
                    </label>
                </li>
                <li>
                    <label>מחיר מקסימלי:
                    <input type="number" min="0" name="price" placeholder="מחיר מקסימלי לשיעור" <?php if (isset($_POST["price"])) { echo 'value="'.$_POST["price"].'"'; } ?> />
                    </label>
                </li>
                <li>
                    <input type="submit" name="search_classes" value="חפש חוגים" />
                </li>
            </ul>
        </form>
        <?php if (isset($_POST["search_classes"])) { ?>
        <h3>נמצאו <?php echo count($found_classes);?> תוצאות.</h3>
            <?php if (count($found_classes) > 0) { ?>
            <ul>
                <?php foreach ($found_classes as $class) { ?>
                <li><a href="index.php?page=classes&id=<?php echo $class["id"];?>"><?php echo $class["name"];?></a> בהנחיית <?php echo $class["teacher_name"];?>, <?php echo get_day($class["day"]);?> <?php echo $class["hour_start"].":00-".$class["hour_end"].":00";?></li> 
                <?php } ?>
        </ul>
            <?php } ?>
        <?php } ?>
    </div>

    <div id="create_class" class="<?php if ($section == "create_class") { echo "unhidden"; } else { echo "hidden"; }?>">
        <h3>פתיחת חוג בקאנטרי</h3>
        <form action="index.php?page=classes" method="POST">
            <ul>    
                <li><label>שם החוג<font color="red" size="2">*</font>: <input type="text" required="true" name="name" placeholder="שם החוג" /></label></li>
                <li><label>תיאור החוג<font color="red" size="2">*</font>: <textarea required="true" name="description"></textarea></label></li>
                <li>
                    <label>יום בשבוע<font color="red" size="2">*</font>:
                        <select name="day" id="class_day">
                            <option value="1" selected="selected">יום ראשון</option>
                            <option value="2">יום שני</option>
                            <option value="3">יום שלישי</option>
                            <option value="4">יום רביעי</option>
                            <option value="5">יום חמישי</option>
                            <option value="6">יום שישי</option>
                            <option value="7">יום שבת</option>
                        </select> 
                    </label>
                </li>
                <li>
                    <label>שעת התחלה<font color="red" size="2">*</font>:
                        <select name="hour_start" id="class_hour_start">
                            <?php for ($i = 0; $i < 24; ++$i) { ?>
                            <option value="<?php echo $i;?>" <?php if ($i == 16) echo 'selected="selected"';?>><?php echo str_pad($i, 2, "0", STR_PAD_LEFT);?>:00</option>
                            <?php } ?>
                        </select>       
                    </label>
                </li>
                <li>
                    <label>שעת סיום<font color="red" size="2">*</font>:
                        <select name="hour_end" id="class_hour_end">
                            <?php for ($i = 0; $i < 24; ++$i) { ?>
                            <option value="<?php echo $i;?>" <?php if ($i == 18) echo 'selected="selected"';?>><?php echo str_pad($i, 2, "0", STR_PAD_LEFT);?>:00</option>
                            <?php } ?>
                        </select>       
                    </label>
                </li>
                <li>
                    <label>כמות משתתפים<font color="red" size="2">*</font>:
                        <input type="number" min="1" required="true" name="capacity" placeholder="כמות משתתפים מקסימלית" />
                    </label>
                </li>
                <li>
                    <label>מחיר<font color="red" size="2">*</font>:
                        <input type="number" min="0" required="true" name="price" placeholder="מחיר מקסימלי לשיעור" />
                    </label>
                </li>
                <li>
                    <input type="submit" name="create_class" value="צור חוג" />
                </li>
            </ul>
        </form>
    </div>

  <div id="classes_main" class="<?php if ($section == "classes_main") { echo "unhidden"; } else { echo "hidden"; }?>">
	<h3>חוגים בקאנטרי בית יצחק</h3>
    <p>הקאנטרי מציע לחבריו תוכנית חוגים מגוונת ביותר.<br />
    תוכנית החוגים כוללת חוגי ילדים וחוגי מבוגרים, חוגי בוקר וחוגי ערב וזאת 7 ימים בשבוע.<br />
    בתוכנית החוגים מבחר עצום של חוגי ספורט ותנועה, חוגי מחול וחוגי אומנויות לחימה.<br />
    החוגים מתקיימים באולמות מרווחים, ממוזגים ומאובזרים בציוד החדיש ביותר.</p>
	<img src="./img/classes/hugim.jpg" />
  </div>

  <?php foreach ($classes as $class) { ?>
  <div>
  <div id="class-<?php echo $class["id"];?>" class="class <?php if ($section = "class" && $showId == $class["id"]) echo "un";?>hidden">
    <?php if (isset($_SESSION["user"]) && ($_SESSION["user"]["id"] == $class["teacher_id"] || $_SESSION["user"]["type"] == 2)) { ?>
    <span class="close"><a href="javascript:void(0);" class="edit">עדכן</a>&nbsp;<a onclick="return confirm('האם אתה בטוח שברצונך למחוק את החוג?');" href="index.php?page=<?php echo $page;?>&delete-class&id=<?php echo $class["id"];?>">מחק</a></span>
    <?php } ?>

    <div class="view">
        <h3><?php echo $class["name"];?></h3>
        <p>בהנחיית <?php echo $class["teacher_name"];?>, <?php echo get_day($class["day"]);?> <?php echo $class["hour_start"].":00-".$class["hour_end"].":00";?></p> 
        <p><?php echo $class["description"];?></p>

        <h3>משתתפים: <?php echo count($class["participants"])."/".$class["capacity"];?></h3>
        <?php if (count($class["participants"]) > 0) { ?>
            <ul>
                <?php foreach ($class["participants"] as $p_idx => $participant) { ?>
                <li><?php echo $p_idx+1;?>. <?php echo $participant["first_name"]." ".$participant["last_name"];?></li>
                <?php } ?>
            </ul>
        <?php } ?>
        <?php 
            if (isset($_SESSION["user"])) { 
                $registered = false;
                foreach ($class["participants"] as $participant) {
                    if ($participant["id"] == $_SESSION["user"]["id"]) {
                        // already registered to class!
                        $registered = true;
                    }
                }

                ?>
                <a href="javascript:void(0);" class="class-unregister" <?php if (!$registered) { echo 'style="display: none;"'; } ?>>בטל הרשמה</a>
                <a href="javascript:void(0);" class="class-register" <?php if ($registered || count($class["participants"]) >= $class["capacity"]) { echo 'style="display: none;"'; } ?>>הרשם לחוג</a>
                <?php
            }
      ?>
  </div>
  <div class="edit" style="display: none;">
    <h3>עדכון פרטי חוג '<?php echo $class["name"];?>'</h3>
    <form action="index.php?page=classes&id=<?php echo $class["id"];?>" method="POST">
    <input type="hidden" name="id" value="<?php echo $class["id"];?>" />
        <ul>    
        <li><label>שם החוג<font color="red" size="2">*</font>: <input type="text" required="true" name="name" placeholder="שם החוג" value="<?php echo $class["name"];?>" /></label></li>
        <li><label>תיאור החוג<font color="red" size="2">*</font>: <textarea required="true" name="description"><?php echo $class["description"];?></textarea></label></li>
            <li>
                <label>יום בשבוע<font color="red" size="2">*</font>:
                    <?php $day = $class["day"]; ?>
                    <select name="day" id="class_day">
                        <option value="1" <?php if ($day == 1) echo 'selected="selected"';?>>יום ראשון</option>
                        <option value="2" <?php if ($day == 2) echo 'selected="selected"';?>>יום שני</option>
                        <option value="3" <?php if ($day == 3) echo 'selected="selected"';?>>יום שלישי</option>
                        <option value="4" <?php if ($day == 4) echo 'selected="selected"';?>>יום רביעי</option>
                        <option value="5" <?php if ($day == 5) echo 'selected="selected"';?>>יום חמישי</option>
                        <option value="6" <?php if ($day == 6) echo 'selected="selected"';?>>יום שישי</option>
                        <option value="7" <?php if ($day == 7) echo 'selected="selected"';?>>יום שבת</option>
                    </select> 
                </label>
            </li>
            <li>
                <label>שעת התחלה:
                    <?php $hour_start = $class["hour_start"]; ?>
                    <select name="hour_start" id="class_hour_start">
                        <?php for ($i = 0; $i < 24; ++$i) { ?>
                        <option value="<?php echo $i;?>" <?php if (($hour_start == null && $i == 16) || $hour_start == $i) echo 'selected="selected"';?>><?php echo str_pad($i, 2, "0", STR_PAD_LEFT);?>:00</option>
                        <?php } ?>
                    </select>       
                </label>
            </li>
            <li>
                <label>שעת סיום:
                    <?php $hour_end = $class["hour_end"]; ?>
                    <select name="hour_end" id="class_hour_end">
                        <?php for ($i = 0; $i < 24; ++$i) { ?>
                        <option value="<?php echo $i;?>" <?php if (($hour_end == null && $i == 18) || $hour_end == $i) echo 'selected="selected"';?>><?php echo str_pad($i, 2, "0", STR_PAD_LEFT);?>:00</option>
                        <?php } ?>
                    </select>       
                </label>
            </li>
            <li>
                <label>כמות משתתפים<font color="red" size="2">*</font>:
                <input type="number" min="1" required="true" name="capacity" placeholder="כמות משתתפים מקסימלית" value="<?php echo $class["capacity"];?>" />
                </label>
            </li>
            <li>
                <label>מחיר<font color="red" size="2">*</font>:
                <input type="number" min="0" required="true" name="price" placeholder="מחיר מקסימלי לשיעור" value="<?php echo $class["price"];?>" />
                </label>
            </li>
            <li>
                <input type="submit" name="update_class" value="עדכן פרטי חוג" />
            </li>
        </ul>
    </form>
  </div>
  </div>
  </div>
  <?php } ?>
</article>
