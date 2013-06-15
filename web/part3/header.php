<!DOCTYPE html>
<html lang="he">
<head>
<meta charset="utf-8">
<title>קאנטרי בית יצחק</title>
<script type="text/javascript" src="js/jquery-1.9.1.js"></script>	
<script type="text/javascript" src="js/script.js"></script>	
<link rel="stylesheet" href="css/style.css" />
</head>

<body dir="rtl">
<div class="overlay">
</div>
<img class="overlay" />
<header id="header">
  <h1>קאנטרי בית יצחק</h1>

  <nav>
    <ul>
      <?php if (!isset($_SESSION["user"])) { ?>
      <li><a href="index.php?page=register">הרשמה</a></li> 
      <li><a href="index.php?page=login">התחברות</a></li> 
      <?php } else { ?>
      <li><a href="index.php?page=profile">היי <?php echo $_SESSION["user"]["first_name"];?></a></li> 
      <li><a href="index.php?page=logout">התנתק</a></li> 
      <?php } ?>
      <li><a href="index.php?page=classes">חוגים</a></li> 
      <li><a href="index.php?page=facilities">מתקנים</a></li>
      <li><a href="index.php?page=swimming">בית ספר לשחיה</a></li>
      <li><a href="index.php?page=gym">חדר כושר</a></li>
      <li><a href="index.php?page=articles">כתבות</a></li>
      <li><a href="index.php?page=events">ארועים</a></li>
      <li><a href="index.php?page=gallery">גלריה</a></li>
      <li><a href="index.php?page=about">אודות</a></li>
      <li><a href="index.php?page=contact">צור קשר</a></li>
    </ul>
  </nav>
</header>
