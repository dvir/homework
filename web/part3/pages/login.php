<aside>
    <?php
    include("announcements.php");
    ?>
</aside>
<article id="login">
    <div id="login_main" class="unhidden">
        <h2>התחברות לאתר</h2>
        <form action="index.php?page=login" method="POST">
            <ul>
                <li><label>אימייל<font color="red" size="2">*</font>:
                           <input type="email" name="email" placeholder="הקלד כתובת דואר אלקטרוני" required="required" /></label></li>
                <li><label>סיסמא<font color="red" size="2">*</font>: 
                           <input type="password" name="password" placeholder="הקלד סיסמא" required="required" /></label></li>
                <li><input type="submit" name="login" value="התחבר" /></li>
            </ul>
        </form>
    </div>
</article>
