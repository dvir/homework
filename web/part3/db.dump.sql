-- MySQL dump 10.13  Distrib 5.5.31, for debian-linux-gnu (x86_64)
--
-- Host: localhost    Database: web
-- ------------------------------------------------------
-- Server version	5.5.31-0ubuntu0.13.04.1

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `class`
--

DROP TABLE IF EXISTS `class`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `class` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `capacity` smallint(6) NOT NULL,
  `price` smallint(6) NOT NULL,
  `day` tinyint(4) NOT NULL,
  `hour_start` tinyint(4) NOT NULL,
  `hour_end` tinyint(4) NOT NULL,
  `description` text,
  `teacher_id` int(11) NOT NULL,
  `date_created` datetime DEFAULT NULL,
  `date_updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `teacher` (`teacher_id`),
  CONSTRAINT `teacher` FOREIGN KEY (`teacher_id`) REFERENCES `user` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=14 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `class`
--

LOCK TABLES `class` WRITE;
/*!40000 ALTER TABLE `class` DISABLE KEYS */;
INSERT INTO `class` VALUES (6,'כדורגל',2,0,1,16,18,'  <img src=\"./img/classes/soccer1.jpg\">\r\n  <p>החוג מתאים לשחקנים מתחילים ומתקדמים כאחד.<br>המטרה העיקרית של החוג היא פיתוח יכולת חברתית, ולהיות חלק מקבוצת כדורגל מקצועית.</p>\r\n  <p>במסגרת החוג נבצע תריגילים בסיסיים של שליטה על כדור, מסירה, יכולת תנועה עם כדור, לימוד תנועה ומשחק קבוצתי.</p>\r\n  <p>ייחודו של החוג מתבטא בגישה חברתית המאפשרת ללמוד להשתלב בפעילות קבוצתית, גם לבעלי יכולות פיזיות סבירות או לחילופין \"בעיות חברתיות\" הידועות להורים עוד לפני הגעת הילד לחוג.החוג עצמו הוא יותר אירוע או מפגש של ילדים האוהבים את אותו ענף ספורט אך אינם מונעים על ידי הוריהם להצליח או להיות טובים יותר, אלא להנות ולרכוש חברים, לדעת איך להתנהג במצבים חדשים וגם ללמוד כדורגל.</p>',1,'2013-06-14 13:26:23',NULL),(7,'כדורמים',10,100,2,12,16,'	<img src=\"img/waterball.jpg\">\r\n    <p><b>מועדון הכדורמים הפועל בבית יצחק</b> נוסד בסוף שנות השישים של המאה הקודמת. מייסדי הקבוצה היו יצחק פיטרברג וקורט אליאס, שחברו יחדיו לניהול ואימון הקבוצה. פעילות המועדון ערכה מספר שנים עד לאמצע שנות השבעים. בשנים אלו השתתף המועדון בליגות הנוער לגילאים השונים. הקבוצה הגיעה להישגים יפים ביניהם העפלה לגמר גביע המדינה והיוותה גורם משמעותי בליגות הנוער.<br>\r\n    הפסקת פעילות המועדון נבעה מכך שמרבית השחקנים התגייסו לצה\"ל.<br></p>\r\n    <p>בחודש דצמבר 2004 פנתה ועדת הספורט של בית יצחק עם מספר שחקני עבר לאמיר ויינברג, שוער נבחרת ישראל בעבר, בניסיון להחזיר את המועדון לפעילות. ואכן בתחילת שנת 2005 החל מועדון הכדורמים לפעול בשנית, לאחר הפסקה של 40 שנים.</p>\r\n    <p>בחודשים הראשונים התאמנו כחמישה עשר נערים והתמקדו בלימוד בסיס הכדורמים. בקיץ 2005 הצטרפו נערים נוספים והקבוצה החלה בפעילות אינטנסיבית. בעונת המשחקים 2005-2006 החל המועדון לקחת חלק בליגות הנוער.</p>\r\n    <p>בכל עונה התוצאות השתפרו והקבוצות השונות התקדמו בדירוג כאשר כבר בעונת 2007-2008 לאחר 3 שנים בלבד של פעילות, עלתה קבוצת גיל 16 לגמר גביע המדינה.</p>\r\n    <p>בעונת 2008-2009 החלה פעילות קבוצת הבוגרים. הקבוצה נוטלת חלק בליגה הלאומית זו העונה השלישית.</p>\r\n    <p>עד לסיום עונת 2009-2010 ולאחר 5 שנים של פעילות בליגות השונות, הגיע המועדון שלוש פעמים לגמר גביע המדינה ושלוש פעמים לגמר הליגה ולהתמודדות על אליפות המדינה.</p>\r\n    <p>כיום, מחלקת הנוער היא אחת מהבולטות בארץ ומייצרת ספורטאים המשתייכים לנבחרת ישראל.</p>',1,'2013-06-14 19:34:58','2013-06-15 22:08:36'),(8,'ספינינג',15,120,1,12,14,'	<img src=\"./img/classes/spinning1.jpg\">\r\n	<p>ספינינג הוא תחום אימון גופני אירובי בו רוכבים על אופני כושר באולם. האימון מתבצע באופן אישי ע\"י מדריך או עם קבוצה גדולה שמגיעה ל-30 ואף יותר רוכבים בחדר הכושר.השיטה פותחה בשביל רוכבי אופניים שאינם יכולים להתאמן בחורף אבל חייבים לשמור על כושר רכיבה גבוהה.שיעורי ספינינג הם המילה האחרונה בתחום הכושר בארץ. מעבר לכושר ולשריפת השומנים, האווירה בשיעור היא שמחה וקופצנית, המוזיקה קצבית ורועשת, ואחרי שיעור אחד כבר תרגישו שזהו תחום קשה אך עם תוצאות מובטחות.</p>\r\n	<p><b><u>מטרת אימון הספינינג</u></b>:<br>המטרה העיקרית של ספינינג היא שריפת קלוריות, כל אימון אפשר להגיע לשריפה של 500-1000 הכל תלוי בכמה אתם בכושר.מעבר להרזיה ספינינג היא פעילות אירובית בה הלב מגיע לדופק מאוד גבוהה וכך אנו משפרים סיבולת לב ריאה.אחרי 50 דקות של רכיבה אתם תרגישו את שרירי הרגליים - הפעולה גורמת לחיטוב הגוף ולחיזוק השריר.</p>',1,'2013-06-14 19:35:54','2013-06-15 22:09:17'),(9,'זומבה',25,40,4,18,20,'	<img src=\"./img/classes/zumba1.jpg\">\r\n	<p>זומבה הוא אימון פיטנס אנרגטי ותוסס, אשר מאפשר פעילות גופנית ושריפת קלוריות , תוך מסווה שמאפשר לנו לא להרגיש כלל שאנו מתאמנים – המסווה הזה הוא כמובן ריקוד.<br>\r\n	</p><p>הזומבה הינו הפיטנס הלטיני. השיעורים משלבים את מגוון המקצבים הלטיניים : קומביה , רגאטון , סלסה , צה צה צה , מרנגה ועוד, מעלים את האדרנלין ואת האנרגיות וסוחפים אותנו למקום נפלא בו הקלוריות פשוט נעלמות.</p>\r\n	<p>מטרת הזומבה היא לאפשר לכל אדם להתנסות ולהצטרף למסיבה, אין כל צורך בניסיון קודם בריקוד, הריקודים הם פשוטים וקלים לתפיסה, על מנת לאפשר לכל אחד ואחד להצטרף וליהנות!</p>',1,'2013-06-14 19:40:22','2013-06-15 22:13:44'),(10,'יוגה',5,70,4,17,18,'	<img src=\"./img/classes/yoga1.jpg\">\r\n	<p>יוגה היא פילוסופיה רוחנית ואימון גופני-רוחני שמקורם בחכמה ההינדית.</p>\r\n	<p>היוגה היא מסע פיזי מנטאלי ורוחני שמטרתו להרגיע את תנודות התודעה, ודרכו המתרגל מקבל כלים להתמודד עם הקשיים האישיים שלו, פיזיים או נפשיים.</p>\r\n	<p>היוגה הקלאסית מונה כשמונה איברים או שלבים של היוגה במסע לחקר הנשמה:<br>\r\n		1. יאמה- דיברות המוסר האוניברסליות.<br>\r\n		2. ניאמה- היטהרות עצמית על ידי משמעת.<br>\r\n		3. אסאנה- תנוחה.<br>\r\n		4. פראניימה- שליטה בקצב הנשימה.<br>\r\n		5. פרטיהרה- כינוס עצמי וניתוק המחשבה מגורמים חיצוניים.<br>\r\n		6. דהאראנה- ריכוז.<br>\r\n		7. דהיאנה- מדיטציה.<br>\r\n		8. סאמאדהי- מצב של מודעות הנוצר על ידי מדיטציה.<br>\r\n	</p>		',1,'2013-06-14 19:59:10',NULL),(11,'פילאטיס',10,80,3,19,22,'  <img src=\"./img/classes/pilates1.jpg\">\r\n  <p>פילאטיס היא שיטת התעמלות שפותחה על ידי גוזף פילאטיס בשלהי המאה ה-20. שיטה זו מאזנת את כל הגוף תוך התמקדות במתיחות, בחיזוק, וביישור וייצוב עמוד השדרה. על ידי שימוש בעקרונות כמו נשימה, שליטה וזרימה, עוזרים תרגילי פילאטיס לשפר את היציבה ואת השליטה בשרירים, כך שהתנועה הופכת ליעילה יותר.</p>\r\n  <p>שרירים מרכזיים הם המוקד העיקרי באימון פילאטיס ומוקדשת תשומת לב מיוחדת למכאניקה תקינה ולדיוק בתנועות. בנוסף, מושם דגש על החיבור בין גוף לנפש כך שהעבודה מתמקדת באיכות התנועה ולא בכמות.</p>\r\n  <p>באימון פילאטיס משתמשים בגלגיליות ובקפיצים שמספקים התנגדות או סיוע לתנועה, תלוי בפעילות הגופנית שמתבצעת. על ידי מניפולציה על הציוד, יכולה הפעילות הגופנית להתבצע ברמות מאמץ שונות, כמו כן ניתן לשנות לחלוטין את השריר בתנועה שבו מתמקדים ובכך לספק אפשרויות רבות הן לפעילות גופנית והן להתפתחות ולשינוי. </p>',1,'2013-06-14 20:09:51',NULL),(12,'טניס',20,30,1,16,18,'    <img src=\"./img/classes/tennis.jpg\">\r\n    <p><b>אימונים וחוגים בניהולה של טל גרין 054-7222418</b></p>\r\n\r\n    <p><b>אימונים וחוגים</b>:</p>\r\n    <ul>\r\n      <li>חוג טניס לילדים מתחילים ומתקדמים</li>\r\n      <li>טרום טניס טניס לגיל הרך מגיל 4.5</li>\r\n      <li>חוגי טניס ואימונים אישיים למבוגרים</li>\r\n    </ul>\r\n\r\n    <p><b>ימי פעילות</b>:<br>\r\n    שני ורביעי משעה 18:00 עד 22:00</p>\r\n\r\n    <p>קבוצות נוספות תפתחנה בהתאם לביקוש.</p>',1,'2013-06-14 21:48:12',NULL),(13,'כדורסל',30,45,6,13,16,'   <img src=\"./img/classes/basketball1.jpg\">\r\n  <p>חוג הכדורסל מיועד לבנים ובנות בגילאים 7-15</p>\r\n  <p>חוג הכדורסל בשל היותו בראש ובראשונה משחק חברתי מקנה לכם מיומנויות בינאישיות חדשות (או מחדד קיימות), כיכולת הנהגה, עבודת צוות ושיתוף פעולה, בניית חזון אסטרטגי וגזירת תוכנית פעולה ליישומו, ראייה עתידית וראייה מערכתית. בהיבט האישי משחק כדורסל מפחח אצלכם את הביטחון העצמי ויכולת העמידה באתגרים, מלמד אתכם תחרותיות בריאה מהי והתמודדות נכונה עם הפסדים.</p>\r\n  <p>חוג כדורסל הינו חוג בו נוצר שיפור בזריזות ידיים ורגליים, במהירות ובקואורדינציה, הכוח הפיזי גדל, שרירי הגוף מתארכים ומתייעלים, מערכת השלד מתחזקת, הכושר הגופני וסיבולת הלב ריאה משתפרים אף הם.</p>\r\n  <p>חוג כדורסל מועבר על ידי מדריכים מיומנים אשר מלמדים אתכם את יסודות המשחק תוך הנאה וכייף.</p>\r\n  <p>חוג כדורסל הוא חוג ייחודי בעל יתרונות פיזיים ומנטאליים, אישיים ובין אישיים מובהקים אשר מקנה ערכים ספורטיביים וחברתיים ותורם לבריאות הנפשית ולאיכות חיים גבוהה במיוחד.</p>',1,'2013-06-14 21:49:07',NULL);
/*!40000 ALTER TABLE `class` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `contact`
--

DROP TABLE IF EXISTS `contact`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `contact` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `full_name` varchar(200) NOT NULL,
  `phone` varchar(15) NOT NULL,
  `email` varchar(254) NOT NULL,
  `subject` varchar(200) NOT NULL,
  `comment` text,
  `date_created` datetime DEFAULT NULL,
  `date_updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=9 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `contact`
--

LOCK TABLES `contact` WRITE;
/*!40000 ALTER TABLE `contact` DISABLE KEYS */;
INSERT INTO `contact` VALUES (3,'חצב וולף','054343434','dsadsadas@ssssss.com','classes','מה נשמע?',NULL,NULL),(6,'dsadsadas','432432432','dsadas@dsadsa.com','gym','dsadsa','2013-06-24 10:36:57',NULL),(7,'dsadsadsa','543543543','dsadas@dsadsa.com','gym','dsadasdas','2013-06-24 10:37:02',NULL),(8,'דני דין','0545517926','ddsad@dsadsss.com','gym','תחזרו אלי מהר!','2013-06-24 10:49:48',NULL);
/*!40000 ALTER TABLE `contact` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `event`
--

DROP TABLE IF EXISTS `event`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `event` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` varchar(50) NOT NULL,
  `capacity` smallint(6) NOT NULL,
  `description` text,
  `location` varchar(100) DEFAULT NULL,
  `organizer_id` int(11) NOT NULL,
  `when` datetime DEFAULT NULL,
  `date_created` datetime DEFAULT NULL,
  `date_updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `organizer` (`organizer_id`),
  CONSTRAINT `organizer` FOREIGN KEY (`organizer_id`) REFERENCES `user` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=6 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `event`
--

LOCK TABLES `event` WRITE;
/*!40000 ALTER TABLE `event` DISABLE KEYS */;
INSERT INTO `event` VALUES (1,'יום המים בקאנטרי',100,'יום המים נערך מדי שנה ביום הכי חם ביוני. השנה האירוע בשיתוף רדבול!','מגרש הכדורגל בבית יצחק',1,'2013-08-08 12:00:00','2013-06-13 16:58:30','2013-06-13 16:58:30'),(5,'פיינטבול',30,'מלחמת הפיינטבול השנתית בקאנטרי. נא להגיע עם ציוד מלא!','מגרש הטניס',2,'2013-06-30 17:00:00','2013-06-24 12:05:37',NULL);
/*!40000 ALTER TABLE `event` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `field`
--

DROP TABLE IF EXISTS `field`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `field` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `name` varchar(50) NOT NULL,
  `capacity` smallint(6) NOT NULL,
  `date_created` datetime DEFAULT NULL,
  `date_updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `field`
--

LOCK TABLES `field` WRITE;
/*!40000 ALTER TABLE `field` DISABLE KEYS */;
/*!40000 ALTER TABLE `field` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `message`
--

DROP TABLE IF EXISTS `message`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `message` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `title` varchar(200) NOT NULL,
  `description` text,
  `user_id` int(11) NOT NULL,
  `class_id` int(11) DEFAULT NULL,
  `date_created` datetime DEFAULT NULL,
  `date_updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `message_user` (`user_id`),
  KEY `message_class` (`class_id`),
  CONSTRAINT `message_class` FOREIGN KEY (`class_id`) REFERENCES `class` (`id`),
  CONSTRAINT `message_user` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=7 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `message`
--

LOCK TABLES `message` WRITE;
/*!40000 ALTER TABLE `message` DISABLE KEYS */;
INSERT INTO `message` VALUES (3,'יום המים','יום המים זה נורא מגניב, קורים הרבה דברים מעניינים שקשורים למים.',1,NULL,'2013-06-13 16:58:30','2013-06-13 16:58:30'),(4,'חדר כושר','החדר כושר יהיה סגור במהלך יולי לשיפוצים.',2,NULL,'2013-06-24 10:42:55',NULL),(5,'חדר כושר','החדר כושר יהיה סגור במהלך יולי לשיפוצים.',2,NULL,'2013-06-24 10:43:05',NULL);
/*!40000 ALTER TABLE `message` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `first_name` varchar(50) NOT NULL,
  `last_name` varchar(50) NOT NULL,
  `phone` varchar(15) NOT NULL,
  `email` varchar(254) NOT NULL,
  `password` varchar(32) NOT NULL,
  `city` varchar(50) NOT NULL,
  `address` varchar(200) NOT NULL,
  `type` tinyint(4) NOT NULL,
  `date_created` datetime DEFAULT NULL,
  `date_updated` datetime DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=5 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
INSERT INTO `user` VALUES (1,'דביר','אזולאי','0545517926','dvir.azulay@gmail.com','123456','חניאל','רחוב הגיא',0,'2013-06-13 16:58:29','2013-06-15 14:22:58'),(2,'חי','חלפון','0545517926','c.halfon@gmail.com','1234','בית חירות','רחוב הגיא',2,'2013-06-13 16:58:29','2013-06-13 16:58:29'),(3,'חצב','וולף','0545517926','hatzavw@gmail.com','12345','בית יצחק','רחוב הגיא',0,'2013-06-13 16:58:29','2013-06-13 16:58:29');
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user_class`
--

DROP TABLE IF EXISTS `user_class`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_class` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `user_id` int(11) NOT NULL,
  `class_id` int(11) NOT NULL,
  `has_arrived` tinyint(4) DEFAULT NULL,
  `date_created` datetime DEFAULT NULL,
  `date_updated` datetime DEFAULT NULL,
  `date_start` datetime DEFAULT NULL,
  `date_end` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `class_user` (`user_id`),
  KEY `user_class` (`class_id`),
  CONSTRAINT `class_user` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`),
  CONSTRAINT `user_class` FOREIGN KEY (`class_id`) REFERENCES `class` (`id`)
) ENGINE=InnoDB AUTO_INCREMENT=112 DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user_class`
--

LOCK TABLES `user_class` WRITE;
/*!40000 ALTER TABLE `user_class` DISABLE KEYS */;
INSERT INTO `user_class` VALUES (104,1,11,0,'2013-06-15 12:46:07',NULL,NULL,NULL),(106,1,8,0,'2013-06-15 12:50:03',NULL,NULL,NULL),(108,1,12,0,'2013-06-15 12:50:14',NULL,NULL,NULL);
/*!40000 ALTER TABLE `user_class` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user_field`
--

DROP TABLE IF EXISTS `user_field`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_field` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `user_id` int(11) NOT NULL,
  `field_id` int(11) NOT NULL,
  `date_created` datetime DEFAULT NULL,
  `date_updated` datetime DEFAULT NULL,
  `date_start` datetime DEFAULT NULL,
  `date_end` datetime DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `field_user` (`user_id`),
  KEY `user_field` (`field_id`),
  CONSTRAINT `field` FOREIGN KEY (`field_id`) REFERENCES `field` (`id`),
  CONSTRAINT `user` FOREIGN KEY (`user_id`) REFERENCES `user` (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user_field`
--

LOCK TABLES `user_field` WRITE;
/*!40000 ALTER TABLE `user_field` DISABLE KEYS */;
/*!40000 ALTER TABLE `user_field` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2013-06-24 12:24:23
