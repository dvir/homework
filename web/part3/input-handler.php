<?php
$success = false; 
$notification = isset($_REQUEST["notification"]) ? $_REQUEST["notification"] : "";

if ($page == "logout") {
    unset($_SESSION["user"]);
    $notification = "ההתנתקות מהאתר בוצעה בהצלחה. להתראות!";
    $page = "login";
} else if (isset($_REQUEST["unique_email"])) {
    $email = $_REQUEST["email"];
    $result = $mysqli->query("SELECT id FROM user WHERE email='$email' LIMIT 1") or die($mysqli->error);
    die($result->num_rows > 0 ? "false" : "true");
} else if (isset($_REQUEST["delete-announcement"])) {
    $id = $_REQUEST["id"];
    $result = $mysqli->query("SELECT * FROM message WHERE id='$id'") or die($mysqli->error);
    if ($result->num_rows > 0) {
        $mysqli->query("DELETE FROM message WHERE id='$id' LIMIT 1") or die($mysqli->error);
        $message = $result->fetch_array();
        $notification = "הודעה '{$message["title"]}' נמחקה בהצלחה!";
    }
} else if (isset($_REQUEST["delete-contacts"])) {
    $idsString = $_REQUEST["ids"];
    $mysqli->query("DELETE FROM contact WHERE id IN ($idsString)") or die($mysqli->error);
    die("true");
} else if (isset($_REQUEST["delete-users"])) {
    $idsString = $_REQUEST["ids"];
    $mysqli->query("DELETE FROM 
                        user_class 
                    WHERE 
                        user_id IN (SELECT user.id FROM user WHERE user.id IN ($idsString))
                        OR class_id IN (SELECT class.id FROM class JOIN user ON class.teacher_id=user.id WHERE user.id IN ($idsString))
                    ") or die($mysqli->error);
    $mysqli->query("DELETE FROM 
                        class 
                    WHERE 
                        teacher_id IN ($idsString)
                    ") or die($mysqli->error);
    $mysqli->query("DELETE FROM user WHERE id IN ($idsString)") or die($mysqli->error);
    die("true");
} else if (isset($_REQUEST["delete-class"])) {
    $id = $_REQUEST["id"];
    $result = $mysqli->query("SELECT * FROM class WHERE id='$id'") or die($mysqli->error);
    if ($result->num_rows > 0) {
        $mysqli->query("DELETE FROM user_class WHERE class_id='$id'") or die($mysqli->error);
        $mysqli->query("DELETE FROM class WHERE id='$id' LIMIT 1") or die($mysqli->error);
        $class = $result->fetch_array();
        $notification = "החוג '{$class["name"]}' נמחק בהצלחה!";
        $success = true;
        $page = "classes";
    }
} else if (isset($_REQUEST["delete-event"])) {
    $id = $_REQUEST["id"];
    $result = $mysqli->query("SELECT * FROM event WHERE id='$id'") or die($mysqli->error);
    if ($result->num_rows > 0) {
        $mysqli->query("DELETE FROM event WHERE id='$id' LIMIT 1") or die($mysqli->error);
        $event = $result->fetch_array();
        $notification = "האירוע '{$event["title"]}' נמחק בהצלחה!";
        $page = "events";
    }
} else if (isset($_POST["contact"])) {
    $subject = $_POST["subject"];
    $full_name = $_POST["full_name"];
    $phone = $_POST["phone"];
    $email = $_POST["email"];
    $comment = $_POST["comment"];

    if ($subject == "other") {
        $subject = "other-".$_POST["subject_other"];
    }

    $mysqli->query("INSERT INTO contact (subject, full_name, phone, email, comment, date_created) VALUES
        ('$subject', '$full_name', '$phone', '$email', '$comment', CURRENT_TIMESTAMP)") or die($mysqli->error);

    $notification = "בקשה התקבלה. נחזור אליך בהקדם!";
    $success = true;
} else if (isset($_POST["register"])) {
    $first_name = $_POST["first_name"];
    $last_name = $_POST["last_name"];
    $phone = $_POST["phone"];
    $city = $_POST["city"];
    $address = $_POST["address"];
    $email = $_POST["email"];
    $password = $_POST["password"];

    $mysqli->query("INSERT INTO user (type, 
                                      first_name, 
                                      last_name, 
                                      phone, 
                                      city, 
                                      address, 
                                      email, 
                                      password, 
                                      date_created) 
                                VALUES
                                    (0, 
                                    '$first_name', 
                                    '$last_name', 
                                    '$phone', 
                                    '$city', 
                                    '$address', 
                                    '$email', 
                                    '$password', 
                                    CURRENT_TIMESTAMP)") or die($mysqli->error);
    $result = $mysqli->query("SELECT * FROM user WHERE id='".$mysqli->insert_id."'") or die($mysqli->error);
    if ($result->num_rows > 0) {
        $_SESSION["user"] = $result->fetch_array();
        $page = "profile";
        $notification = "הרישום לקאנטרי בוצע בהצלחה. ברוך הבא!";
    } else {
        $page = "login";
        $notification = "הרישום לקאנטרי בוצע בהצלחה, אך לא הצלחנו לחבר אותך אוטומטית לאתר. אנא התחבר";
    }
    $success = true;
} else if (isset($_POST["update_info"])) {
    $first_name = $_POST["first_name"];
    $last_name = $_POST["last_name"];
    $phone = $_POST["phone"];
    $city = $_POST["city"];
    $address = $_POST["address"];
    $email = $_POST["email"];
    $password = $_POST["password"];

    $mysqli->query("UPDATE user SET
                        first_name='$first_name',
                        last_name='$last_name',
                        phone='$phone',
                        city='$city',
                        address='$address',
                        email='$email',
                        password='$password',
                        date_updated=CURRENT_TIMESTAMP
                    WHERE
                        id='".$_SESSION["user"]["id"]."'
                   ") or die($mysqli->error);

    $result = $mysqli->query("SELECT * FROM user WHERE id='".$_SESSION["user"]["id"]."'") or die($mysqli->error);
    $_SESSION["user"] = $result->fetch_array();

    $success = true;
    $notification = "פרטי משתמש עודכנו בהצלחה!";
} else if (isset($_POST["login"])) {
    $email = $_POST["email"];
    $password = $_POST["password"];

    $result = $mysqli->query("SELECT * FROM user WHERE email='$email' AND password='$password'") or die($mysqli->error);
    if ($result->num_rows > 0) {
        $_SESSION["user"] = $result->fetch_array();
        die("true");
    }
    die("false");
} else if (isset($_POST["post_message"])) {
    $class_id = null;
    $title = $_POST["title"];
    $description = $_POST["description"];

    $mysqli->query("INSERT INTO message (
                                            title,
                                            description,
                                            user_id,
                                            ".(empty($class_id) ? "" : "class_id,")."
                                            date_created
                                           )
                                           VALUES
                                           (
                                            '$title',
                                            '$description',
                                            '".$_SESSION["user"]["id"]."',
                                            ".(empty($class_id) ? "" : "'$class_id',")."
                                            CURRENT_TIMESTAMP
                                        )") or die($mysqli->error);
    //die("true");
} else if (isset($_POST["class_register"])) {
    $class_id = $_POST["id"];

    $mysqli->query("INSERT INTO user_class (
                                            user_id,
                                            class_id,
                                            has_arrived,
                                            date_created
                                           )
                                           VALUES
                                           (
                                            '".$_SESSION["user"]["id"]."',
                                            '$class_id',
                                            0,
                                            CURRENT_TIMESTAMP
                                        )") or die($mysqli->error);
    die("true");
} else if (isset($_POST["class_unregister"])) {
    $class_id = $_POST["id"];

    $mysqli->query("DELETE FROM user_class WHERE
                                            user_id='".$_SESSION["user"]["id"]."'
                                            AND class_id='$class_id'
                                            LIMIT 1") or die($mysqli->error);
    die("true");
} else if (isset($_POST["create_class"])) {
    $name = $_POST["name"];
    $description = $_POST["description"];
    $day = $_POST["day"];
    $hour_start = $_POST["hour_start"];
    $hour_end = $_POST["hour_end"];
    $capacity = $_POST["capacity"];
    $price = $_POST["price"];
    $teacher_id = $_SESSION["user"]["id"];

    $mysqli->query("INSERT INTO class (name, 
                                      description, 
                                      day, 
                                      hour_start, 
                                      hour_end, 
                                      capacity, 
                                      price,
                                      teacher_id, 
                                      date_created) 
                                VALUES
                                    ('".addslashes($name)."', 
                                    '".addslashes($description)."', 
                                    '$day', 
                                    '$hour_start', 
                                    '$hour_end', 
                                    '$capacity', 
                                    '$price',
                                    '$teacher_id', 
                                    CURRENT_TIMESTAMP)") or die($mysqli->error);
    $newClassId = $mysqli->insert_id;
    $notification = "פתיחת חוג חדש בוצעה בהצלחה!";
    $success = true;
} else if (isset($_POST["update_class"])) {
    $id = $_POST["id"];
    $name = $_POST["name"];
    $description = $_POST["description"];
    $day = $_POST["day"];
    $hour_start = $_POST["hour_start"];
    $hour_end = $_POST["hour_end"];
    $capacity = $_POST["capacity"];
    $price = $_POST["price"];

    $mysqli->query("UPDATE class SET
                          name='$name',
                          description='$description',
                          day='$day',
                          hour_start='$hour_start', 
                          hour_end='$hour_end', 
                          capacity='$capacity', 
                          price='$price',
                          date_updated=CURRENT_TIMESTAMP
                    WHERE id='$id'") or die($mysqli->error);

    $notification = "עדכון חוג בוצע בהצלחה!";
    $success = true;
} else if (isset($_POST["search_classes"])) {
    $name = $_POST["name"];
    $day = $_POST["day"];
    $hour_start = $_POST["hour_start"];
    $hour_end = $_POST["hour_end"];
    $price = empty($_POST["price"]) ? 0 : $_POST["price"];

    $query = "SELECT 
                class.*, 
                CONCAT(user.first_name, ' ', user.last_name) AS teacher_name 
            FROM 
                class JOIN user ON class.teacher_id=user.id 
            WHERE
                class.name LIKE '%$name%'
    ";
    if ($day != 0) {
        $query .= " AND class.day='$day' ";
    }
    if ($hour_start != -1) {
        $query .= " AND class.hour_start >= $hour_start ";
    }
    if ($hour_end != -1) {
        $query .= " AND class.hour_end <= $hour_end ";
    }
    if ($price != null) {
        $query .= " AND class.price <= $price ";
    }

    $result = $mysqli->query($query) or die($mysqli->error);
    $found_classes = array();
    while ($class = $result->fetch_array()) {
        $found_classes[] = $class;
    }

    $success = true;
} else if (isset($_POST["create_event"])) {
    $title = $_POST["title"];
    $description = $_POST["description"];
    $location = $_POST["location"];
    $when = $_POST["when"];
    $capacity = $_POST["capacity"];
    $organizer_id = $_SESSION["user"]["id"];

    $mysqli->query("INSERT INTO event (title, 
                                      description, 
                                      location, 
                                      capacity, 
                                      `when`,
                                      organizer_id, 
                                      date_created) 
                                VALUES
                                    ('$title', 
                                    '$description', 
                                    '$location', 
                                    '$capacity', 
                                    '$when',
                                    '$organizer_id', 
                                    CURRENT_TIMESTAMP)") or die($mysqli->error);

    $notification = "יצירת האירוע בוצעה בהצלחה!";
    $success = true;
} else if (isset($_REQUEST["autocomplete_classname"])) {
    $q = $_REQUEST["q"];
    $result = $mysqli->query("SELECT id,name FROM class WHERE name LIKE '%$q%'") or die($mysqli->error);
    $classnames = array();
    while ($entry = $result->fetch_array(MYSQLI_ASSOC)) {
        $classnames[] = $entry["name"];
    }

    die(json_encode($classnames));
}
