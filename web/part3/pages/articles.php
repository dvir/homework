<aside>
    <section>
      <a href="javascript:void(0);"><h2>ספורט</h2></a>
      <ul>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('sports-1');">טיפים להגברת פעילות גופנית בחיי היום יום</a></li>
      </ul>
      <a href="javascript:void(0);"><h2>תזונה</h2></a>
      <ul>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('nutrition-1');">טיפים לשמירה על תזונה נבונה</a></li>
        <li><a href="javascript:void(0);" onclick="javascript:unhide('nutrition-2');">היתרונות של ניהול אורח חיים בריא</a></li>
      </ul>
    </section>

    <?php
    include("announcements.php");
    ?>
</aside>

<article id="articles">
  <div id="articles_main" class="hidden">
    <p>הכתבות הכי מעניינות בתחום!</p>
  </div>
  <div id="sports" class="hidden">
  </div>
  <div id="sports-1" class="unhidden">
    <img src="./img/sports-1.jpg" />
  </div>
  <div id="nutrition" class="hidden">
  </div>
  <div id="nutrition-1" class="hidden">
    <img src="./img/nutrition-1.jpg" />
  </div>
  <div id="nutrition-2" class="hidden">
    <img src="./img/nutrition-2.jpg" />
  </div>
</article>
