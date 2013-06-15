DROP DATABASE web;

CREATE DATABASE web
  DEFAULT CHARACTER SET utf8
  DEFAULT COLLATE utf8_general_ci;
USE web;

CREATE TABLE `user` (
    `id` int(11) NOT NULL AUTO_INCREMENT,
    `first_name` VARCHAR(50) NOT NULL,
    `last_name` VARCHAR(50) NOT NULL,
    `phone` VARCHAR(15) NOT NULL,
    `email` VARCHAR(254) NOT NULL,
    `password` VARCHAR(32) NOT NULL,
    `city` VARCHAR(50) NOT NULL,
    `address` VARCHAR(200) NOT NULL,
    `type` TINYINT NOT NULL,
    `date_created` datetime DEFAULT NULL,
    `date_updated` datetime DEFAULT NULL,
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `field` (
    `id` int(11) NOT NULL AUTO_INCREMENT,
    `name` VARCHAR(50) NOT NULL,
    `capacity` SMALLINT NOT NULL,
    `date_created` datetime DEFAULT NULL,
    `date_updated` datetime DEFAULT NULL,
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `class` (
    `id` int(11) NOT NULL AUTO_INCREMENT,
    `name` VARCHAR(50) NOT NULL,
    `capacity` SMALLINT NOT NULL,
    `price` SMALLINT NOT NULL,
    `day` TINYINT NOT NULL,
    `hour_start` TINYINT NOT NULL,
    `hour_end` TINYINT NOT NULL,
    `description` TEXT,
    `teacher_id` int(11) NOT NULL,
    INDEX `teacher` (`teacher_id` ASC) ,
    CONSTRAINT `teacher`
    FOREIGN KEY (`teacher_id`)
    REFERENCES `web`.`user` (`id`),
    `date_created` datetime DEFAULT NULL,
    `date_updated` datetime DEFAULT NULL,
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `user_field` (
    `id` int(11) NOT NULL AUTO_INCREMENT,
    `user_id` int(11) NOT NULL,
    INDEX `field_user` (`user_id` ASC) ,
    CONSTRAINT `user`
    FOREIGN KEY (`user_id`)
    REFERENCES `web`.`user` (`id`),
    `field_id` int(11) NOT NULL,
    INDEX `user_field` (`field_id` ASC) ,
    CONSTRAINT `field`
    FOREIGN KEY (`field_id`)
    REFERENCES `web`.`field` (`id`),
    `date_created` datetime DEFAULT NULL,
    `date_updated` datetime DEFAULT NULL,
    `date_start` datetime DEFAULT NULL,
    `date_end` datetime DEFAULT NULL,
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `user_class` (
    `id` int(11) NOT NULL AUTO_INCREMENT,
    `user_id` int(11) NOT NULL,
    INDEX `class_user` (`user_id` ASC) ,
    CONSTRAINT `class_user`
    FOREIGN KEY (`user_id`)
    REFERENCES `web`.`user` (`id`),
    `class_id` int(11) NOT NULL,
    INDEX `user_class` (`class_id` ASC) ,
    CONSTRAINT `user_class`
    FOREIGN KEY (`class_id`)
    REFERENCES `web`.`class` (`id`),
    `has_arrived` TINYINT,
    `date_created` datetime DEFAULT NULL,
    `date_updated` datetime DEFAULT NULL,
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `event` (
    `id` int(11) NOT NULL AUTO_INCREMENT,
    `title` VARCHAR(50) NOT NULL,
    `capacity` SMALLINT NOT NULL,
    `description` TEXT,
    `location` VARCHAR(100),
    `organizer_id` int(11) NOT NULL,
    INDEX `organizer` (`organizer_id` ASC) ,
    CONSTRAINT `organizer`
    FOREIGN KEY (`organizer_id`)
    REFERENCES `web`.`user` (`id`),
    `when` datetime DEFAULT NULL,
    `date_created` datetime DEFAULT NULL,
    `date_updated` datetime DEFAULT NULL,
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `message` (
    `id` int(11) NOT NULL AUTO_INCREMENT,
    `title` VARCHAR(200) NOT NULL,
    `description` TEXT,
    `user_id` int(11) NOT NULL,
    INDEX `message_user` (`user_id` ASC) ,
    CONSTRAINT `message_user`
    FOREIGN KEY (`user_id`)
    REFERENCES `web`.`user` (`id`),
    `class_id` int(11),
    INDEX `message_class` (`class_id` ASC) ,
    CONSTRAINT `message_class`
    FOREIGN KEY (`class_id`)
    REFERENCES `web`.`class` (`id`),
    `date_created` datetime DEFAULT NULL,
    `date_updated` datetime DEFAULT NULL,
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

CREATE TABLE `contact` (
    `id` int(11) NOT NULL AUTO_INCREMENT,
    `full_name` VARCHAR(200) NOT NULL,
    `phone` VARCHAR(15) NOT NULL,
    `email` VARCHAR(254) NOT NULL,
    `subject` VARCHAR(200) NOT NULL,
    `comment` TEXT,
    `date_created` datetime DEFAULT NULL,
    `date_updated` datetime DEFAULT NULL,
    PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

insert into user (first_name,last_name,phone,email,password,city,address,type,date_created,date_updated) VALUES ('דביר', 'אזולאי', '0545517926', 'dvir.azulay@gmail.com', '123', 'חניאל', 'רחוב הגיא', 0, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
insert into user (first_name,last_name,phone,email,password,city,address,type,date_created,date_updated) VALUES ('חי', 'חלפון', '0545517926', 'c.halfon@gmail.com', '1234', 'בית חירות', 'רחוב הגיא', 0, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
insert into user (first_name,last_name,phone,email,password,city,address,type,date_created,date_updated) VALUES ('חצב', 'וולף', '0545517926', 'hatzavw@gmail.com', '12345', 'בית יצחק', 'רחוב הגיא', 0, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
insert into event (title, capacity, description, location, organizer_id, `when`, date_created, date_updated) VALUES ('יום המים בקאנטרי', 100, 'יום המים נערך מדי שנה ביום הכי חם ביוני. השנה האירוע בשיתוף רדבול!', 'מגרש הכדורגל בבית יצחק', 1, '2013-08-08 12:00:00', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
insert into event (title, capacity, description, location, organizer_id, `when`, date_created, date_updated) VALUES ('יום המים בקאנטרי', 100, 'יום המים נערך מדי שנה ביום הכי חם ביוני. השנה האירוע בשיתוף רדבול!', 'מגרש הכדורגל בבית יצחק', 1, '2013-08-08 12:00:00', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
insert into event (title, capacity, description, location, organizer_id, `when`, date_created, date_updated) VALUES ('יום המים בקאנטרי', 100, 'יום המים נערך מדי שנה ביום הכי חם ביוני. השנה האירוע בשיתוף רדבול!', 'מגרש הכדורגל בבית יצחק', 1, '2013-08-08 12:00:00', CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
insert into message (title, description, user_id, class_id, date_created, date_updated) VALUES ('יום המים', 'יום המים זה נורא מגניב, קורים הרבה דברים מעניינים שקשורים למים.', 1, NULL, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
insert into message (title, description, user_id, class_id, date_created, date_updated) VALUES ('יום המים', 'יום המים זה נורא מגניב, קורים הרבה דברים מעניינים שקשורים למים.', 1, NULL, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);
insert into message (title, description, user_id, class_id, date_created, date_updated) VALUES ('יום המים', 'יום המים זה נורא מגניב, קורים הרבה דברים מעניינים שקשורים למים.', 1, NULL, CURRENT_TIMESTAMP, CURRENT_TIMESTAMP);

