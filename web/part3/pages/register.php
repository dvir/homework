<aside>
    <?php
    include("announcements.php");
    ?>
</aside>
<article id="register">
    <?php if ($success) { ?>
        <h2>הרישום לקאנטרי בוצע בהצלחה. ברוך הבא!</h2>
    <?php } else { ?>
    <div id="register_main" class="unhidden">
        <h2>הרשמה לקאנטרי</h2>
        <form action="index.php?page=register" method="POST">
            <ul>
                <li><label>שם פרטי<font color="red" size="2">*</font>: <input type="text" name="first_name" placeholder="שם פרטי" required="required" /></label></li>
                <li><label>שם משפחה<font color="red" size="2">*</font>: <input type="text" name="last_name" placeholder="שם משפחה" required="required" /></label></li>
                <li><label>סלולרי<font color="red" size="2">*</font>: <input type="tel" name="phone" placeholder="מספר טלפון כולל קידומת" required="required" /></label></li>
                <li><label>עיר<font color="red" size="2">*</font>: <input type="text" name="city" placeholder="עיר מגורים" required="required" /></label></li>
                <li><label>כתובת<font color="red" size="2">*</font>: <input type="text" name="address" placeholder="כתובת מגורים" required="required" /></label></li>
                <li><label>אימייל<font color="red" size="2">*</font>: <input type="email" name="email" placeholder="כתובת דואר אלקטרוני" required="required" /></label></li>
                <li><label>סיסמא<font color="red" size="2">*</font>: <input type="password" name="password" placeholder="סיסמא" required="required" /></label></li>
                <li><label>וידוא סיסמא<font color="red" size="2">*</font>: <input type="password" name="verify_password" id="verify_password" placeholder="הקלד סיסמא שוב" required="required" /></label></li>
                <li><input type="submit" name="register" value="הרשמה" /></li>
            </ul>
        </form>
    </div>
    <?php } ?>
</article>
