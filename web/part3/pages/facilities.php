<aside>
    <section>
      <a href="javascript:void(0);"><h2>בריכות</h2></a>
      <ul>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('summerpool');">בריכה חיצונית</a></li>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('indoorpool');">בריכה מקורה</a></li>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('babypool');">בריכת פעוטות</a></li>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('sauna');">סאונה</a></li>
      </ul>
      <a href="javascript:void(0);"><h2>שירותים נוספים</h2></a>
      <ul>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('kiosk');">מזנון</a></li>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('coaches');">מדריכי כושר</a></li>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('proffesions');">אנשי מקצוע</a></li>
      </ul>
    </section>

    <?php
    include("announcements.php");
    ?>
</aside>

<article id="facilities">
  <div id="facility" class="hidden">
	<h3>מתקנים</h3>
    <p>המתקנים בבית יצחק הם מהחדשים ביותר בשוק</p> 
    <p>מבין המתקנים אפשר למצוא מכון כושר חדש, בריכת שחיה סגורה, בריכת שחיה פתוחה ועוד מתקנים רבים</p>
  </div>
  <div id="summerpool" class="unhidden">
  <h3>בריכה חיצונית</h3>
    <img src="img/outdoorpool.jpg" />
	<p> הבריכה החיצונית בקאנרטי פתוחה כל ימות השנה</p>
	<p>מי הבריכה מחוממים לטמפרטורה של 30 מעלות בימי החורף, ובכך מאפשרים כניסה לבריכה גם בעונות הקרות</p>
	<p>הבריכה יכולה לאכלס עד 250 אנשים בו זמנית ובכך הופכת אידיאלית לאירועים</p>
	<p>הבריכה מוקפת משטח דשא גדול ורחב שבחלקו קיימים מתקני כושר, ומתקני שעשועים לילדים</p>
	<p>הבריכה נותנת מענה לקייטנות, אורעי חברה, ימי הולדת, בר מצוות ועוד</p>
  </div>
  <div id="indoorpool" class="hidden">
	<h3>בריכה מקורה</h3>
    <img src="img/indoorpool.jpg" />
	<p>הבריכה המקורה פועלת כל ימות השנה עם טמפ' מים אידיאלית לעונה</p>
	<p>הבריכה נותנת מענה למנויים במהלך שעות הבוקר והערב</p>
	<p>בשעות הצהריים: 15:00-18:00 הבריכה סגורה למנויים ומתקיימים בה חוגי שחייה של הקאנרטי</p>
	<p>הבריכה באורך 25 מטר, ובעלת 6 מסלולי שחייה המסווגים למהירויות שחייה שונות</p>
  </div>
  <div id="babypool" class="hidden">
	<h3>בריכת פעוטות</h3>
	<p>בריכת הפעוטות פועלת רק בחודשי הקיץ</p>
	<p>הילדים בבריכה חייבים להיות בהשגחת הוריהם</p>
	<img src="img/babypool.jpg" />
  </div>
  <div id="sauna" class="hidden">
	<h3>סאונה</h3>
    <img src="img/sauna.jpg" />
  </div>
  <div id="kiosk" class="hidden">
	<h3>מזנון</h3>
    <p>בקאנטרי מזנון המציע תפריט רחב של גלידות, ארוחות חמות קפה ועוד.</p>
    <img src="img/kiosk.jpg" />
  </div>
  <div id="coaches" class="hidden">
  <h3>מאמנים אישיים</h3>
  <p>בקאנטרי מדריכי כושר רבים, כולם בעלי ניסיון מוכח בתחום. המדריכים שלנו שמים דגש מיוחד על ערכי האנושיות והמקצועיות כנותני שירות בקאנטרי.</p>
    <img src="img/coaches.jpg" />
  </div>
  <div id="proffesions" class="hidden">
	<h3>אנשי מקצוע</h3>
	<p><u><b>מעסה</b></u></p>
    <p>בקאנטרי בבית יצחק יש מעסה מקצועי שפועל בתחום שנים רבות.<br>יש לקבוע טיפולים בתיאום מראש.</p>
	<p><u><b>דיאטנית</b></u></p>
	<p>בקאנטרי דיאטנית מקצועית. אימון כושר אמיתי חייב להיות מותאם לתזונה בריאה ונכונה, לכן אנו מספקים שירותי ייעוץ של תזונאית מוכשרת ומנוסה.</p>
	<p><u><b>רופא ספורט</b></u></p>
	<p>בקאנטרי יש רופא ספורט, ניתן לבצע בדיקות תקופתיות, בדיקות מאמץ ועוד בתיאום מראש.</br>הרופא מאשר לשחקני הקבוצות השונות להתאמן בליגות האזוריות.</p>
  </div>
</article>
