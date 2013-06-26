<aside>
    <section>
      <a href="javascript:void(0);" onclick="javascript:unhide('profile_main');"><h2>פרופיל</h2></a>
    </section>
    <section>
      <a href="javascript:void(0);" onclick="javascript:unhide('update-info');"><h2>עדכון פרטים</h2></a>
    </section>

    <?php if (isset($_SESSION["user"]) && $_SESSION["user"]["type"] == 2) { ?>
    <section>
      <h2>אזור מנהלים</h2>
      <ul>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('admin_customers');">פרטי לקוחות</a></li>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('admin_contact');">פניות</a></li>
      </ul>
    </section>
    <?php } ?>

    <?php
    include("announcements.php");
    ?>
</aside>
<article id="profile">
    <div id="profile_main" class="unhidden">
        <h2>פרופיל משתמש</h2>
        <ul>
            <li><span class="field">שם פרטי</span> <?php echo $_SESSION["user"]["first_name"];?></li>
            <li><span class="field">שם משפחה</span> <?php echo $_SESSION["user"]["last_name"];?></li>
            <li><span class="field">סלולרי</span> <?php echo $_SESSION["user"]["phone"];?></li>
            <li><span class="field">עיר</span> <?php echo $_SESSION["user"]["city"];?></li>
            <li><span class="field">כתובת</span> <?php echo $_SESSION["user"]["address"];?></li>
            <li><span class="field">אימייל</span> <?php echo $_SESSION["user"]["email"];?></li>
        </ul>
    </div>
    
    <div id="update-info" class="hidden">
        <h2>עדכון פרטים</h2>
        <form action="index.php?page=profile" method="POST">
            <ul>
            <li><label>שם פרטי<font color="red" size="2">*</font>: <input type="text" name="first_name" placeholder="שם פרטי" required="required" value="<?php echo $_SESSION["user"]["first_name"];?>" /></label></li>
            <li><label>שם משפחה<font color="red" size="2">*</font>: <input type="text" name="last_name" placeholder="שם משפחה" required="required" value="<?php echo $_SESSION["user"]["last_name"];?>"/></label></li>
            <li><label>סלולרי<font color="red" size="2">*</font>: <input type="tel" name="phone" placeholder="מספר טלפון כולל קידומת" required="required" value="<?php echo $_SESSION["user"]["phone"];?>" /></label></li>
            <li><label>עיר<font color="red" size="2">*</font>: <input type="text" name="city" placeholder="עיר מגורים" required="required" value="<?php echo $_SESSION["user"]["city"];?>" /></label></li>
            <li><label>כתובת<font color="red" size="2">*</font>: <input type="text" name="address" placeholder="כתובת מגורים" required="required" value="<?php echo $_SESSION["user"]["address"];?>" /></label></li>
            <li><label>אימייל<font color="red" size="2">*</font>: <input type="email" name="email" placeholder="כתובת דואר אלקטרוני" required="required" value="<?php echo $_SESSION["user"]["email"];?>" /></label></li>
            <li><label>סיסמא<font color="red" size="2">*</font>: <input type="password" name="password" placeholder="סיסמא" required="required" value="<?php echo $_SESSION["user"]["password"];?>" /></label></li>
            <li><label>וידוא סיסמא<font color="red" size="2">*</font>: <input type="password" name="verify_password" id="verify_password" placeholder="הקלד סיסמא שוב" required="required" value="<?php echo $_SESSION["user"]["password"];?>" /></label></li>
                <li><input type="submit" name="update_info" value="עדכן פרטים" /></li>
            </ul>
        </form>
    </div>

    <div id="admin" class="hidden">
    </div>
    <div id="admin_customers" class="hidden">
    <?php
    $result = $mysqli->query("SELECT * FROM user");
    ?>
        <table>
            <thead>
                <tr>
                    <td width="3%"><input type="checkbox" id="select_all_users" name="select_all_users" /></td>
                    <td width="3%">#</td>
                    <td width="5%">שם פרטי</td>
                    <td width="5%">שם משפחה</td>
                    <td width="10%">עיר</td>
                    <td width="10%">כתובת</td>
                    <td width="5%">טלפון</td>
                    <td width="10%">אימייל</td>
                    <td width="5%">סיסמא</td>
                    <td width="5%">סוג</td>
                    <td width="10%">תאריך יצירה</td>
                </tr>
            </thead> 
            <tfoot>
                <tr>
                    <td><b>פעולות:</b>&nbsp;</td>
                    <td><a href="javascript:void(0);" class="delete_selected">מחק</a></td>
                </td>
            </tfoot>
            <tbody>
            <?php while ($user = $result->fetch_array()) { ?>
                <tr class="user_<?php echo $user["id"];?>">
                    <td><input type="checkbox" name="user_ids[]" value="<?php echo $user["id"];?>" /></td>
                    <td><?php echo $user["id"];?></td>
                    <td><?php echo $user["first_name"];?></td>
                    <td><?php echo $user["last_name"];?></td>
                    <td><?php echo $user["city"];?></td>
                    <td><?php echo $user["address"];?></td>
                    <td><?php echo $user["phone"];?></td>
                    <td><?php echo $user["email"];?></td>
                    <td><?php echo $user["password"];?></td>
                    <td><?php echo $user["type"] == 2 ? "מנהל" : ($user["type"] == 1 ? "עובד" : "לקוח");?></td>
                    <td><?php echo date("d-m-Y", strtotime($user["date_created"]));?></td>
                </tr>
            <?php } ?>
            </tbody>
        </table>
    </div>
    <div id="admin_contact" class="hidden">
    <?php
    $result = $mysqli->query("SELECT * FROM contact ORDER BY subject");
    ?>
        <table>
            <thead>
                <tr>
                    <td width="3%"><input type="checkbox" id="select_all_contacts" name="select_all_contacts" /></td>
                    <td width="3%">#</td>
                    <td width="10%">שם מלא</td>
                    <td width="15%">טלפון</td>
                    <td width="10%">אימייל</td>
                    <td width="10%">נושא</td>
                    <td width="20%">הערות</td>
                    <td width="10%">תאריך</td>
                </tr> 
            </thead>
            <tfoot>
                <tr>
                    <td><b>פעולות:</b>&nbsp;</td>
                    <td><a href="javascript:void(0);" class="delete_selected">מחק</a></td>
                </td>
            </tfoot>
            <tbody>
            <?php while ($contact = $result->fetch_array()) { ?>
                <tr class="contact_<?php echo $contact["id"];?>">
                    <td><input type="checkbox" name="contact_ids[]" value="<?php echo $contact["id"];?>" /></td>
                    <td><?php echo $contact["id"];?></td>
                    <td><?php echo $contact["full_name"];?></td>
                    <td><?php echo $contact["phone"];?></td>
                    <td><?php echo $contact["email"];?></td>
                    <td><?php echo $contact["subject"];?></td>
                    <td><?php echo $contact["comment"];?></td>
                    <td><?php echo date("d-m-Y", strtotime($contact["date_created"]));?></td>
                </tr>
            <?php } ?>
            </tbody>
        </table>
    </div>
</article>

