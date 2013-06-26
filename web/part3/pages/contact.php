<?php
$subject = isset($contactSubject) ? $contactSubject : (isset($_REQUEST["subject"]) ? $_REQUEST["subject"] : null);
?>
<aside>                
    <?php                                  
    include("announcements.php");                                                                 
    ?>                                                                   
</aside>  
<article id="contact">
  <div id="contact_main" class="unhidden">
    <h2>טופס יצירת קשר</h2>
    <form action="index.php?page=contact" method="POST" class="contact">
      <ul>
        <li>
        <label>נושא<font color="red" size="2">*</font>: 
          <select name="subject" id="select-subject">
            <option value="gym" <?php if ($subject == "gym") { ?>selected="selected"<?php } ?>>הרשמה לחדר כושר</option>
            <option value="swimming" <?php if ($subject == "swimming") { ?>selected="selected"<?php } ?>>הרשמה לביה"ס לשחייה</option>
            <option value="diet">דיאטנית</option>
            <option value="pool">בריכה</option>
            <option value="classes">חוגים</option>
            <option value="account">חשבונות</option>
            <option value="request">בקשות</option>
            <option value="complaints">תלונות</option>
            <option value="other">אחר</option>
          </select>
        </label>
        </li>
        <li id="other-subject" class="hide"><label>אחר: <input type="text" name="subject_other" placeholder="נא לציין את נושא הפנייה" /></label></li>
        <li><label>שם מלא<font color="red" size="2">*</font>: <input type="text" name="full_name" placeholder="שם פרטי ושם משפחה" required="required" /></label></li>
        <li><label>סלולרי<font color="red" size="2">*</font>: <input type="tel" name="phone" placeholder="מספר טלפון כולל קידומת" required="required" /></label></li>
        <li><label>אימייל<font color="red" size="2">*</font>: <input type="email" name="email" placeholder="כתובת דואר אלקטרוני למענה" required="required" /></label></li>
        <li><label>הערות: <textarea name="comment"></textarea></label></li>
        <li><input type="submit" name="contact" value="השאר פרטים ונחזור אליך בהקדם!" /></li>
      </ul>
    </div>
</article>
