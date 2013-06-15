<aside>
    <section>
      <a href="javascript:void(0);" onclick="javascript:unhide('profile_main');"><h2>פרופיל</h2></a>
      <a href="javascript:void(0);" onclick="javascript:unhide('update-info');"><h2>עדכון פרטים</h2></a>
    </section>

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
</article>

