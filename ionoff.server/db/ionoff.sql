-- MySQL dump 10.13  Distrib 5.7.9, for Win64 (x86_64)
--
-- Host: localhost    Database: ionoff
-- ------------------------------------------------------
-- Server version	5.7.20-log

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
-- Table structure for table `areas`
--

DROP TABLE IF EXISTS `areas`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `areas` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `order_` int(11) DEFAULT '0',
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `areas_fk_project_id_idx` (`project_id`),
  CONSTRAINT `areas_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `areas`
--

LOCK TABLES `areas` WRITE;
/*!40000 ALTER TABLE `areas` DISABLE KEYS */;
INSERT INTO `areas` VALUES (10,'Tầng trệt',1,4),(11,'Tầng lầu',2,4),(12,'Tầng trệt',0,1),(13,'Tầng trệt',0,5);
/*!40000 ALTER TABLE `areas` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `controllers`
--

DROP TABLE IF EXISTS `controllers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `controllers` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `ip` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `port` int(11) DEFAULT NULL,
  `key_` varchar(31) CHARACTER SET utf8 DEFAULT NULL,
  `model` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `connected_time` bigint(20) DEFAULT NULL,
  `crash_count` int(11) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `controllers_fk_project_id_idx` (`project_id`),
  CONSTRAINT `controllers_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `controllers`
--

LOCK TABLES `controllers` WRITE;
/*!40000 ALTER TABLE `controllers` DISABLE KEYS */;
INSERT INTO `controllers` VALUES (5,'A30000EF','183.81.14.122',80,'A30000EF','IONOFF_P8',1513941082411,NULL,4),(6,'P4-A30000C0','192.168.1.130',80,'A30000C0','IONOFF_P4',1489336064852,NULL,4),(8,'E410171221Agc5Xr','1.52.34.158',NULL,'E410171221Agc5Xr','IONOFF_E4',1520318612929,NULL,4),(9,'A30000EE','1.52.37.0',NULL,'A30000EE','IONOFF_P8',1515292961198,NULL,5),(10,'E410180116Szc3e7','',NULL,'E410180116Szc3e7','IONOFF_E4',1520318608820,NULL,4),(11,'E410180116Szc3e8','',NULL,'E410180116Szc3e8','IONOFF_E4',1519805824240,2,4);
/*!40000 ALTER TABLE `controllers` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `dashboards`
--

DROP TABLE IF EXISTS `dashboards`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dashboards` (
  `id` bigint(20) NOT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  `zone_id` bigint(20) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `dashboards_user_id_fk_idx` (`user_id`),
  KEY `dashboards_zone_id_fk_idx` (`zone_id`),
  KEY `dashboards_project_id_fk_idx` (`project_id`),
  CONSTRAINT `dashboards_project_id_fk` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `dashboards_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `dashboards_zone_id_fk` FOREIGN KEY (`zone_id`) REFERENCES `zones` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dashboards`
--

LOCK TABLES `dashboards` WRITE;
/*!40000 ALTER TABLE `dashboards` DISABLE KEYS */;
INSERT INTO `dashboards` VALUES (6,1,NULL,4),(7,1,7,4),(8,1,4,4),(9,1,3,4),(10,1,8,4),(11,5,NULL,4),(12,5,4,4),(13,5,7,4),(14,5,3,4),(15,5,8,4),(16,1,NULL,1),(17,1,6,1),(18,1,12,1);
/*!40000 ALTER TABLE `dashboards` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `dashboards_devices`
--

DROP TABLE IF EXISTS `dashboards_devices`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dashboards_devices` (
  `id` bigint(20) NOT NULL,
  `dashboard_id` bigint(20) DEFAULT NULL,
  `device_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `dashboards_devices_dashboard_id_fk_idx` (`dashboard_id`),
  KEY `dashboards_devices_device_id_fk_idx` (`device_id`),
  CONSTRAINT `dashboards_devices_dashboard_id_fk` FOREIGN KEY (`dashboard_id`) REFERENCES `dashboards` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `dashboards_devices_device_id_fk` FOREIGN KEY (`device_id`) REFERENCES `devices` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dashboards_devices`
--

LOCK TABLES `dashboards_devices` WRITE;
/*!40000 ALTER TABLE `dashboards_devices` DISABLE KEYS */;
INSERT INTO `dashboards_devices` VALUES (3,7,20),(6,7,22),(8,6,23),(14,10,12),(15,10,13),(16,6,20),(17,9,10),(18,6,10);
/*!40000 ALTER TABLE `dashboards_devices` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `dashboards_scenes`
--

DROP TABLE IF EXISTS `dashboards_scenes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `dashboards_scenes` (
  `id` bigint(20) NOT NULL,
  `dashboard_id` bigint(20) DEFAULT NULL,
  `scene_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `dashboards_scenes_dashboard_id_fk_idx` (`dashboard_id`),
  KEY `dashboards_scenes_scene_id_fk_idx` (`scene_id`),
  CONSTRAINT `dashboards_scenes_dashboard_id_fk` FOREIGN KEY (`dashboard_id`) REFERENCES `dashboards` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `dashboards_scenes_scene_id_fk` FOREIGN KEY (`scene_id`) REFERENCES `scenes` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `dashboards_scenes`
--

LOCK TABLES `dashboards_scenes` WRITE;
/*!40000 ALTER TABLE `dashboards_scenes` DISABLE KEYS */;
/*!40000 ALTER TABLE `dashboards_scenes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `devices`
--

DROP TABLE IF EXISTS `devices`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `devices` (
  `id` bigint(20) NOT NULL,
  `version_` bigint(20) DEFAULT '0',
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `type_` varchar(255) CHARACTER SET utf8 DEFAULT NULL,
  `time_` datetime DEFAULT NULL,
  `order_` int(11) DEFAULT NULL,
  `ip` varchar(127) COLLATE utf8_unicode_ci DEFAULT NULL,
  `port` int(11) DEFAULT NULL,
  `mac` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `model` varchar(127) COLLATE utf8_unicode_ci DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  `zone_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `devices_fk_zone_id_idx` (`zone_id`),
  KEY `devices_fk_project_id_idx` (`project_id`),
  CONSTRAINT `devices_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `devices_fk_zone_id` FOREIGN KEY (`zone_id`) REFERENCES `zones` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `devices`
--

LOCK TABLES `devices` WRITE;
/*!40000 ALTER TABLE `devices` DISABLE KEYS */;
INSERT INTO `devices` VALUES (6,1,'Đèn trần 1','Appliance',NULL,1,NULL,NULL,NULL,NULL,4,3),(8,2,'Đèn trần 2','Light',NULL,2,NULL,NULL,NULL,NULL,4,3),(9,73,'Mini Server','Player',NULL,3,'192.168.1.254',6600,'B827EB73772B','IMP',4,3),(10,6365,'HPEliteBook','Player','2018-03-10 15:12:46',4,'127.0.0.1',6600,'CND04104CQ','XMP',4,3),(12,140,'Đèn trần 1','Light','2018-03-06 12:36:37',1,NULL,NULL,NULL,NULL,4,8),(13,78,'Đèn trần 2','Light','2018-03-06 12:36:37',1,NULL,NULL,NULL,NULL,4,8),(20,68172,'IMP Raspberry','Player','2018-01-07 09:42:58',1,'42.113.162.27',NULL,'b827ebd1e3e4','IMP',4,7),(21,114,'Đèn bảng hiệu 2','Light','2018-03-06 01:58:10',1,NULL,NULL,NULL,NULL,4,8),(22,23,'Đèn tường tranh','Light','2018-02-28 15:15:22',2,NULL,NULL,NULL,NULL,4,7),(23,115,'Đèn ngoài thềm','Light','2018-03-06 12:38:36',1,NULL,NULL,NULL,NULL,4,8),(26,7058,'XMP Delux','Player','2018-01-06 23:27:21',4,'42.113.162.27',NULL,'UH81410011855','XMP',4,8),(27,12,'Đèn 1','Light','2018-01-07 01:09:39',1,NULL,NULL,NULL,NULL,5,9),(28,0,'Đèn 2','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(29,0,'Đèn 3','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(30,0,'Đèn 4','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(31,0,'Đèn 5','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(32,0,'Đèn 6','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(33,0,'Đèn 7','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(34,0,'Đèn 8','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(35,59,'Quạt treo tường','Appliance','2018-03-06 12:36:37',0,NULL,NULL,NULL,NULL,4,8),(36,16,'Đèn tường giá sách','Light','2018-02-28 15:15:26',1,NULL,NULL,NULL,NULL,4,7),(37,0,'Test','Light',NULL,1,NULL,NULL,NULL,NULL,4,3);
/*!40000 ALTER TABLE `devices` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `modes`
--

DROP TABLE IF EXISTS `modes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `modes` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `time_` datetime DEFAULT NULL,
  `is_scheduled` bit(1) DEFAULT NULL,
  `schedule_repeat` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `schedule_time` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `schedule_day` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `is_activated` bit(1) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `modes_fk_project_id_idx` (`project_id`),
  CONSTRAINT `modes_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `modes`
--

LOCK TABLES `modes` WRITE;
/*!40000 ALTER TABLE `modes` DISABLE KEYS */;
INSERT INTO `modes` VALUES (1,'Ngày mới','2018-03-05 10:40:13','\0',NULL,NULL,NULL,'',4);
/*!40000 ALTER TABLE `modes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `modes_scenes`
--

DROP TABLE IF EXISTS `modes_scenes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `modes_scenes` (
  `id` bigint(20) NOT NULL,
  `mode_id` bigint(20) DEFAULT NULL,
  `zone_id` bigint(20) NOT NULL,
  `scene_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `modes_scenes_fk_mode_id_idx` (`mode_id`),
  KEY `modes_scenes_fk_scene_id_idx` (`scene_id`),
  KEY `modes_scenes_fk_zone_id_idx` (`zone_id`),
  CONSTRAINT `modes_scenes_fk_mode_id` FOREIGN KEY (`mode_id`) REFERENCES `modes` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `modes_scenes_fk_scene_id` FOREIGN KEY (`scene_id`) REFERENCES `scenes` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `modes_scenes_fk_zone_id` FOREIGN KEY (`zone_id`) REFERENCES `zones` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `modes_scenes`
--

LOCK TABLES `modes_scenes` WRITE;
/*!40000 ALTER TABLE `modes_scenes` DISABLE KEYS */;
INSERT INTO `modes_scenes` VALUES (1,1,8,NULL),(2,1,3,NULL),(3,1,4,NULL),(4,1,7,NULL);
/*!40000 ALTER TABLE `modes_scenes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `modes_sensors`
--

DROP TABLE IF EXISTS `modes_sensors`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `modes_sensors` (
  `id` bigint(20) NOT NULL,
  `enabled_` bit(1) DEFAULT NULL,
  `time_buffer` int(11) DEFAULT NULL,
  `reset_time` bigint(20) DEFAULT NULL,
  `mode_id` bigint(20) DEFAULT NULL,
  `sensor_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `modes_sensors_fk_mode_id_idx` (`mode_id`),
  KEY `modes_sensors_fk_sensor_id_idx` (`sensor_id`),
  CONSTRAINT `modes_sensors_fk_mode_id` FOREIGN KEY (`mode_id`) REFERENCES `modes` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_fk_sensor_id` FOREIGN KEY (`sensor_id`) REFERENCES `sensors` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `modes_sensors`
--

LOCK TABLES `modes_sensors` WRITE;
/*!40000 ALTER TABLE `modes_sensors` DISABLE KEYS */;
INSERT INTO `modes_sensors` VALUES (1,'\0',NULL,NULL,1,1);
/*!40000 ALTER TABLE `modes_sensors` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `modes_sensors_scenes`
--

DROP TABLE IF EXISTS `modes_sensors_scenes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `modes_sensors_scenes` (
  `id` bigint(20) NOT NULL,
  `detected_` bit(1) DEFAULT NULL,
  `modesensor_id` bigint(20) DEFAULT NULL,
  `zone_id` bigint(20) DEFAULT NULL,
  `scene_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `modes_sensors_scenes_fk_modesensor_id_idx` (`modesensor_id`),
  KEY `modes_sensors_scenes_fk_scene_id_idx` (`scene_id`),
  KEY `modes_sensors_scenes_fk_zone_id_idx` (`zone_id`),
  CONSTRAINT `modes_sensors_scenes_fk_modesensor_id` FOREIGN KEY (`modesensor_id`) REFERENCES `modes_sensors` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_scenes_fk_scene_id` FOREIGN KEY (`scene_id`) REFERENCES `scenes` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_scenes_fk_zone_id` FOREIGN KEY (`zone_id`) REFERENCES `zones` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `modes_sensors_scenes`
--

LOCK TABLES `modes_sensors_scenes` WRITE;
/*!40000 ALTER TABLE `modes_sensors_scenes` DISABLE KEYS */;
INSERT INTO `modes_sensors_scenes` VALUES (1,'\0',1,8,NULL),(2,'',1,8,NULL),(3,'\0',1,3,NULL),(4,'',1,3,NULL),(5,'\0',1,4,NULL),(6,'',1,4,NULL),(7,'\0',1,7,NULL),(8,'',1,7,NULL);
/*!40000 ALTER TABLE `modes_sensors_scenes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `modes_sensors_users`
--

DROP TABLE IF EXISTS `modes_sensors_users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `modes_sensors_users` (
  `id` bigint(20) NOT NULL,
  `detected_` bit(1) DEFAULT NULL,
  `modesensor_id` bigint(20) DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  `send_sms` bit(1) DEFAULT NULL,
  `send_email` bit(1) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `modes_sensors_users_fk_modesensor_id_idx` (`modesensor_id`),
  KEY `modes_sensors_users_fk_user_id_idx` (`user_id`),
  KEY `modes_sensors_users_fk_project_id_idx` (`project_id`),
  CONSTRAINT `modes_sensors_users_fk_modesensor_id` FOREIGN KEY (`modesensor_id`) REFERENCES `modes_sensors` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_users_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_users_fk_user_id` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `modes_sensors_users`
--

LOCK TABLES `modes_sensors_users` WRITE;
/*!40000 ALTER TABLE `modes_sensors_users` DISABLE KEYS */;
INSERT INTO `modes_sensors_users` VALUES (3,'',1,1,'\0','\0',4),(4,'\0',1,1,'\0','\0',4),(5,'',1,5,'\0','\0',4),(6,'\0',1,5,'\0','\0',4);
/*!40000 ALTER TABLE `modes_sensors_users` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `playleafs`
--

DROP TABLE IF EXISTS `playleafs`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `playleafs` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) DEFAULT NULL,
  `duration` varchar(31) DEFAULT NULL,
  `url` varchar(1023) DEFAULT NULL,
  `mrl` varchar(1023) DEFAULT NULL,
  `authors` varchar(255) DEFAULT NULL,
  `artists` varchar(255) DEFAULT NULL,
  `image` varchar(255) DEFAULT NULL,
  `idx` int(11) DEFAULT NULL,
  `playnode_id` bigint(20) DEFAULT NULL,
  `type_` varchar(31) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `playleafs_fk_playnode_id_idx` (`playnode_id`),
  CONSTRAINT `playleafs_fk_playnode_id` FOREIGN KEY (`playnode_id`) REFERENCES `playnodes` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `playleafs`
--

LOCK TABLES `playleafs` WRITE;
/*!40000 ALTER TABLE `playleafs` DISABLE KEYS */;
INSERT INTO `playleafs` VALUES (1,'Mãi Yêu Người',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/8954fe8ad5184fc7.mp3','Nhạc Ngoại - Lời Quang Dũng','Quang Dũng',NULL,0,1,'file'),(2,'Vô Tình',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/11ff9f88ed2e80e2.mp3','Trần Tiến','Quang Dũng',NULL,1,1,'file'),(3,'Vô Tình',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/dda9a5d535d5a987.mp3','Quốc Dũng','Quang Dũng',NULL,2,1,'file'),(4,'Trái Tim Tội Lỗi',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/18ceaca7b8a99ab1.mp3','Bảo Phúc','Quang Dũng',NULL,3,1,'file'),(5,'Mùa Hoa Bỏ Lại',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/7dc143dc25421c8b.mp3','Việt Anh','Quang Dũng',NULL,4,1,'file'),(6,'Khúc Tự Tình',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/04353ae4cde7a064.mp3','Hà Dũng','Quang Dũng',NULL,5,1,'file'),(7,'Dòng Sông Mùa Thu',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/fba2c1341a4c656d.mp3','Trần Tiến','Quang Dũng',NULL,6,1,'file'),(8,'Một Ngày Xa Xăm',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/5f07c86e65f62bd6.mp3','Hoài An','Quang Dũng',NULL,7,1,'file'),(9,'Chỉ Riêng Mình Anh',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/db2b11424750057b.mp3','Lê Quang','Quang Dũng',NULL,8,1,'file'),(10,'Gót Phiêu Du',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/dfdc61a8eeeb6280.mp3','Vũ Quốc Việt','Quang Dũng',NULL,9,1,'file'),(11,'Chờ Em Nơi Thềm Trăng',NULL,NULL,'http://store.xapxinh.net/media/b5f979620e3e5125/f0300814b692dd40.mp3','Quốc Bảo','Quang Dũng',NULL,10,1,'file'),(12,'Chút Tàn Phai',NULL,NULL,'http://store.xapxinh.net/media/cf3570c38169cb69/64e60a85d29f6c2f.mp3','Bảo Chấn','Lê Hiếu',NULL,0,2,'file'),(13,'Cơn Đau Cuối Cùng',NULL,NULL,'http://store.xapxinh.net/media/cf3570c38169cb69/dac0ca93be7411cc.mp3','Thái Thịnh','Lê Hiếu',NULL,1,2,'file'),(14,'Mơ Một Lần Thôi',NULL,NULL,'http://store.xapxinh.net/media/cf3570c38169cb69/8c5ea42bb0b94e99.mp3','Quốc Tuấn','Lê Hiếu',NULL,2,2,'file'),(15,'Một Ngày Vắng Em',NULL,NULL,'http://store.xapxinh.net/media/cf3570c38169cb69/5825bd0139917b33.mp3','Thái Thịnh','Lê Hiếu',NULL,3,2,'file'),(16,'Mùa Đông Sắp Đến',NULL,NULL,'http://store.xapxinh.net/media/cf3570c38169cb69/e3b911b8abacbb5f.mp3','Đức Huy','Lê Hiếu',NULL,4,2,'file'),(17,'Ngày Mai Em Đi',NULL,NULL,'http://store.xapxinh.net/media/cf3570c38169cb69/40003106cbcaee4f.mp3','Thái Thịnh','Lê Hiếu',NULL,5,2,'file'),(18,'Vài Lần Đón Đưa',NULL,NULL,'http://store.xapxinh.net/media/cf3570c38169cb69/a9c83b79cba1de0d.mp3','Trần Lê','Lê Hiếu',NULL,6,2,'file'),(19,'Thiên Sứ Tình Yêu',NULL,NULL,'http://store.xapxinh.net/media/cf3570c38169cb69/d0b7fee622875add.mp3','Tường Vân','Lê Hiếu',NULL,7,2,'file'),(20,'Linh Hồn Đã Mất',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/f0c9626c08854601.mp3','Bằng Kiều','Bằng Kiều',NULL,0,3,'file'),(21,'Anh Sẽ Nhớ Mãi',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/4667351e6a02fc91.mp3','Đức Trí - Bằng Kiều','Bằng Kiều',NULL,1,3,'file'),(22,'Yêu Thương Mong Manh',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/a87cff8416dee44a.mp3','Đức Trí - Hà Quang Minh','Bằng Kiều, Minh Tuyết',NULL,2,3,'file'),(23,'Phút Cuối',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/3f9f302471049bc2.mp3','Lam Phương','Bằng Kiều',NULL,3,3,'file'),(24,'Những Ngày Đẹp Trời',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/202efcbee89174d6.mp3','Hồ Văn Quân','Bằng Kiều',NULL,4,3,'file'),(25,'Dẫu Có Lỗi Lầm',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/e30e08de2a1bcdd5.mp3','Hồ Hoài Anh','Vân Quỳnh, Bằng Kiều',NULL,5,3,'file'),(26,'Vá Lại Tình Tôi',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/343632a6e2b0b070.mp3','Tâm Nguyên','Bằng Kiều',NULL,6,3,'file'),(27,'Em Đến Thăm Anh Đêm 30',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/be80ebb5e59df184.mp3','Vũ Thành An - Thơ Nguyễn Đình Toàn','Bằng Kiều',NULL,7,3,'file'),(28,'Cơn Gió Thoảng - Trái Tim Tội Lỗi',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/70858c16ee40bd5d.mp3','Quốc Dũng','Bằng Kiều, Thanh Hà',NULL,8,3,'file'),(29,'Chị Tôi',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/183b4296ac323a30.mp3','Trần Tiến','Bằng Kiều',NULL,9,3,'file'),(30,'Mưa Trên Ngày Tháng Đó',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/a2289c5637b11c63.mp3','Từ Công Phụng','Bằng Kiều',NULL,10,3,'file'),(31,'Giờ Thì Anh Đã Biết',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/92908ac6e3719021.mp3','Thái Thịnh','Bằng Kiều, Minh Tuyết',NULL,11,3,'file'),(32,'Buồn Ơi Chào Mi',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/ab8fa583d90e3339.mp3','Nguyễn Ánh 9','Bằng Kiều',NULL,12,3,'file'),(33,'Quên Đi Hết Đam Mê',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/3f884d3cb1a0541f.mp3','Nhật Trung','Bằng Kiều, Thủy Tiên',NULL,13,3,'file'),(34,'Biển Vắng',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/9fd4c107e3e5d58d.mp3','Tùng Giang','Bằng Kiều',NULL,14,3,'file'),(35,'LK Tình Khúc Tháng 6 - Đường Xa Ướt Mưa',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/288c1ce16f9b1b15.mp3','Ngô Thụy Miên - Đức Huy','Bằng Kiều, Trần Thu Hà',NULL,15,3,'file'),(36,'Lắng Nghe Mùa Xuân Về',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/25d7801009054ff2.mp3','Dương Thụ','Bằng Kiều',NULL,16,3,'file'),(37,'Bản Tình Cuối',NULL,NULL,'http://store.xapxinh.net/media/adaf7c2244565bd4/4ce06e4da2569e69.mp3','Ngô Thụy Miên','Bằng Kiều',NULL,17,3,'file');
/*!40000 ALTER TABLE `playleafs` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `playlists`
--

DROP TABLE IF EXISTS `playlists`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `playlists` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) DEFAULT NULL,
  `thumbnail` varchar(511) DEFAULT NULL,
  `is_public` bit(1) DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_playlist_user_id_idx` (`user_id`),
  CONSTRAINT `fk_playlist_user_id` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `playlists`
--

LOCK TABLES `playlists` WRITE;
/*!40000 ALTER TABLE `playlists` DISABLE KEYS */;
INSERT INTO `playlists` VALUES (1,'Quang dung 2',NULL,NULL,1);
/*!40000 ALTER TABLE `playlists` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `playnodes`
--

DROP TABLE IF EXISTS `playnodes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `playnodes` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) DEFAULT NULL,
  `type_` varchar(31) DEFAULT NULL,
  `idx` int(11) DEFAULT NULL,
  `playlist_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `fk_playitem_playlist_id_idx` (`playlist_id`),
  CONSTRAINT `fk_playitem_playlist_id` FOREIGN KEY (`playlist_id`) REFERENCES `playlists` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `playnodes`
--

LOCK TABLES `playnodes` WRITE;
/*!40000 ALTER TABLE `playnodes` DISABLE KEYS */;
INSERT INTO `playnodes` VALUES (1,'Bên Đời Có Em','album',0,1),(2,'Vol.5 Cơn Đau Cuối Cùng','album',1,1),(3,'The Best of Bằng Kiều - Linh Hồn Đã Mất','album',2,1);
/*!40000 ALTER TABLE `playnodes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `projects`
--

DROP TABLE IF EXISTS `projects`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `projects` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `address` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `projects`
--

LOCK TABLES `projects` WRITE;
/*!40000 ALTER TABLE `projects` DISABLE KEYS */;
INSERT INTO `projects` VALUES (1,'IOnOff','61/40 st.48, Thu Duc, HCMC'),(4,'NK Sông Đà','61/40 st. 48, Thu Duc, HCMC'),(5,'Trung\'s Home','Da Lat, Lam Dong, VN');
/*!40000 ALTER TABLE `projects` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `relaygroups`
--

DROP TABLE IF EXISTS `relaygroups`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `relaygroups` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `relaygroups_fk_project_id_idx` (`project_id`),
  CONSTRAINT `relaygroups_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `relaygroups`
--

LOCK TABLES `relaygroups` WRITE;
/*!40000 ALTER TABLE `relaygroups` DISABLE KEYS */;
INSERT INTO `relaygroups` VALUES (1,NULL,4),(2,NULL,4),(3,NULL,4),(4,NULL,4),(5,NULL,4),(6,NULL,4),(7,NULL,4);
/*!40000 ALTER TABLE `relaygroups` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `relays`
--

DROP TABLE IF EXISTS `relays`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `relays` (
  `id` bigint(20) NOT NULL,
  `version_` bigint(20) DEFAULT '0',
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `label` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `time_` datetime DEFAULT NULL,
  `type_` varchar(255) CHARACTER SET utf8 DEFAULT NULL,
  `index_` int(11) DEFAULT NULL,
  `status_` bit(1) DEFAULT NULL,
  `controller_id` bigint(20) DEFAULT NULL,
  `device_id` bigint(20) DEFAULT NULL,
  `group_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `relays_fk_controller_id_idx` (`controller_id`),
  KEY `relays_fk_device_id_idx` (`device_id`),
  KEY `relays_fk_group_id_idx` (`group_id`),
  CONSTRAINT `relays_fk_controller_id` FOREIGN KEY (`controller_id`) REFERENCES `controllers` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `relays_fk_device_id` FOREIGN KEY (`device_id`) REFERENCES `devices` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `relays_fk_group_id` FOREIGN KEY (`group_id`) REFERENCES `relaygroups` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `relays`
--

LOCK TABLES `relays` WRITE;
/*!40000 ALTER TABLE `relays` DISABLE KEYS */;
INSERT INTO `relays` VALUES (49,619,'Relay 1',NULL,'2017-12-22 16:36:08','Switch',0,'',5,NULL,NULL),(50,355,'Relay 2',NULL,'2017-11-26 17:28:52','Switch',1,'\0',5,NULL,NULL),(51,158,'Relay 3',NULL,'2017-12-16 20:54:28','Switch',2,'\0',5,NULL,NULL),(52,99,'Relay 4',NULL,'2017-12-10 13:57:54','Switch',3,'\0',5,NULL,NULL),(53,48,'Relay 5',NULL,'2017-12-10 13:57:52','Switch',4,'\0',5,NULL,NULL),(54,31,'Relay 6',NULL,NULL,'Switch',5,'\0',5,NULL,NULL),(55,49,'Relay 7',NULL,'2017-12-16 10:25:11','Switch',6,'\0',5,NULL,NULL),(56,23,'Relay 8',NULL,NULL,'Switch',7,'\0',5,NULL,NULL),(57,230,'Relay 1',NULL,NULL,'Switch',0,'\0',6,NULL,NULL),(58,492,'Relay 2','Relay 2',NULL,'Switch',1,'\0',6,NULL,NULL),(59,251,'Relay 3',NULL,NULL,'Switch',2,'\0',6,NULL,NULL),(60,99,'Relay 4',NULL,NULL,'Switch',3,'\0',6,NULL,NULL),(81,154,'Relay 1',NULL,'2018-03-06 01:58:10','Switch',0,'\0',8,NULL,NULL),(82,196,'Relay 2',NULL,'2018-03-06 12:38:36','Switch',1,'',8,23,NULL),(83,29,'Relay 3',NULL,'2018-01-02 20:43:05','Switch',2,'\0',8,NULL,NULL),(84,26,'Relay 4',NULL,'2018-01-02 20:43:04','Switch',3,'\0',8,NULL,NULL),(85,91,'Relay 1',NULL,'2018-01-07 01:09:39','Switch',0,'\0',9,27,NULL),(86,1,'Relay 2',NULL,NULL,'Switch',1,'\0',9,28,NULL),(87,1,'Relay 3',NULL,NULL,'Switch',2,'\0',9,29,NULL),(88,1,'Relay 4',NULL,NULL,'Switch',3,'\0',9,30,NULL),(89,1,'Relay 5',NULL,NULL,'Switch',4,'\0',9,31,NULL),(90,1,'Relay 6',NULL,NULL,'Switch',5,'\0',9,32,NULL),(91,1,'Relay 7',NULL,NULL,'Switch',6,'\0',9,33,NULL),(92,0,'Relay 8',NULL,NULL,'Switch',7,'\0',9,NULL,NULL),(93,216,'Relay 1',NULL,'2018-03-06 13:17:12','Switch',0,'',10,35,NULL),(94,219,'Relay 2',NULL,'2018-03-06 13:17:12','Switch',1,'',10,12,NULL),(95,3489,'Relay 3',NULL,'2018-03-06 13:17:11','Switch',2,'',10,13,NULL),(96,126,'Relay 4',NULL,'2017-12-28 14:59:54','Switch',3,'\0',10,NULL,NULL),(97,29,'Relay 1',NULL,'2018-02-28 15:14:42','Switch',0,'',11,NULL,NULL),(98,30,'Relay 2',NULL,'2018-02-28 15:14:49','Switch',1,'\0',11,NULL,NULL),(99,58,'Relay 3',NULL,'2018-02-28 15:15:22','Switch',2,'\0',11,22,NULL),(100,61,'Relay 4',NULL,'2018-02-28 15:15:26','Switch',3,'\0',11,36,NULL);
/*!40000 ALTER TABLE `relays` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `scenes`
--

DROP TABLE IF EXISTS `scenes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `scenes` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `time_` datetime DEFAULT NULL,
  `zone_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `scenes_fk_zone_id_idx` (`zone_id`),
  CONSTRAINT `scenes_fk_zone_id` FOREIGN KEY (`zone_id`) REFERENCES `zones` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `scenes`
--

LOCK TABLES `scenes` WRITE;
/*!40000 ALTER TABLE `scenes` DISABLE KEYS */;
INSERT INTO `scenes` VALUES (1,'Buổi sáng',NULL,7),(2,'Đi ngủ','2018-03-02 22:47:25',8);
/*!40000 ALTER TABLE `scenes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `scenes_actions`
--

DROP TABLE IF EXISTS `scenes_actions`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `scenes_actions` (
  `id` bigint(20) NOT NULL,
  `type_` varchar(255) CHARACTER SET utf8 DEFAULT NULL,
  `action_` varchar(255) CHARACTER SET utf8 DEFAULT NULL,
  `volume` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `album` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `album_type` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `relay_id` bigint(20) DEFAULT NULL,
  `scenedevice_id` bigint(20) DEFAULT NULL,
  `player_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `scenes_actions_fk_relay_id_idx` (`relay_id`),
  KEY `scenes_actions_fk_scenedevice_id_idx` (`scenedevice_id`),
  KEY `scenes_actions_fk_player_id_idx` (`player_id`),
  CONSTRAINT `scenes_actions_fk_player_id` FOREIGN KEY (`player_id`) REFERENCES `devices` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `scenes_actions_fk_relay_id` FOREIGN KEY (`relay_id`) REFERENCES `relays` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `scenes_actions_fk_scenedevice_id` FOREIGN KEY (`scenedevice_id`) REFERENCES `scenes_devices` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `scenes_actions`
--

LOCK TABLES `scenes_actions` WRITE;
/*!40000 ALTER TABLE `scenes_actions` DISABLE KEYS */;
INSERT INTO `scenes_actions` VALUES (1,'SceneRelayAction','Open',NULL,NULL,NULL,100,1,NULL),(2,'ScenePlayerAction','Stop','','',NULL,NULL,2,20),(3,'SceneRelayAction','Open',NULL,NULL,NULL,99,3,NULL),(4,'SceneRelayAction','Open',NULL,NULL,NULL,93,4,NULL),(6,'SceneRelayAction','Open',NULL,NULL,NULL,82,6,NULL),(7,'ScenePlayerAction','None','','',NULL,NULL,7,26),(8,'SceneRelayAction','Open',NULL,NULL,NULL,94,8,NULL),(9,'SceneRelayAction','Open',NULL,NULL,NULL,95,9,NULL);
/*!40000 ALTER TABLE `scenes_actions` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `scenes_devices`
--

DROP TABLE IF EXISTS `scenes_devices`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `scenes_devices` (
  `id` bigint(20) NOT NULL,
  `order_` int(11) DEFAULT NULL,
  `duration` int(11) DEFAULT NULL,
  `scene_id` bigint(20) DEFAULT NULL,
  `device_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `sceneactions_fk_scene_id_idx` (`scene_id`),
  KEY `sceneactions_fk_device_id_idx` (`device_id`),
  CONSTRAINT `sceneactions_fk_device_id` FOREIGN KEY (`device_id`) REFERENCES `devices` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `sceneactions_fk_scene_id` FOREIGN KEY (`scene_id`) REFERENCES `scenes` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `scenes_devices`
--

LOCK TABLES `scenes_devices` WRITE;
/*!40000 ALTER TABLE `scenes_devices` DISABLE KEYS */;
INSERT INTO `scenes_devices` VALUES (1,4,4,1,36),(2,5,3,1,20),(3,8,7,1,22),(4,3,3,2,35),(5,NULL,NULL,2,21),(6,5,5,2,23),(7,NULL,NULL,2,26),(8,8,7,2,12),(9,2,5,2,13);
/*!40000 ALTER TABLE `scenes_devices` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `schedules`
--

DROP TABLE IF EXISTS `schedules`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `schedules` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `repeat_` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `time_` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `day_` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `status_` bit(1) DEFAULT NULL,
  `retry_` int(11) DEFAULT NULL,
  `enabled_` bit(1) DEFAULT NULL,
  `device_id` bigint(20) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `schedules_fk_device_id_idx` (`device_id`),
  KEY `schedules_fk_project_id_idx` (`project_id`),
  CONSTRAINT `schedules_fk_device_id` FOREIGN KEY (`device_id`) REFERENCES `devices` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `schedules_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `schedules`
--

LOCK TABLES `schedules` WRITE;
/*!40000 ALTER TABLE `schedules` DISABLE KEYS */;
INSERT INTO `schedules` VALUES (1,'Bật đèn bảng hiệu','Daily','05:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',21,4),(2,'Tắt đèn bảng hiệu','Daily','11:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',21,4),(3,'Bật đèn thềm','Daily','06:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',23,4),(4,'Tắt đèn thềm','Daily','10:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',23,4),(5,'Bat den bang hieu sang','Daily','04:30 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'\0',21,4),(6,'Tat den bang hieu sang','Daily','05:30 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'\0',21,4);
/*!40000 ALTER TABLE `schedules` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `schedules_actions`
--

DROP TABLE IF EXISTS `schedules_actions`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `schedules_actions` (
  `id` bigint(20) NOT NULL,
  `type_` varchar(255) CHARACTER SET utf8 DEFAULT NULL,
  `action_` varchar(255) CHARACTER SET utf8 DEFAULT NULL,
  `volume` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `album` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `album_type` varchar(25) COLLATE utf8_unicode_ci DEFAULT NULL,
  `relay_id` bigint(20) DEFAULT NULL,
  `schedule_id` bigint(20) DEFAULT NULL,
  `player_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `schedules_actions_fk_relay_id_idx` (`relay_id`),
  KEY `schedules_actions_fk_schedule_id_idx` (`schedule_id`),
  KEY `schedules_actions_fk_player_id_idx` (`player_id`),
  CONSTRAINT `schedules_actions_fk_player_id` FOREIGN KEY (`player_id`) REFERENCES `devices` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `schedules_actions_fk_relay_id` FOREIGN KEY (`relay_id`) REFERENCES `relays` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `schedules_actions_fk_schedule_id` FOREIGN KEY (`schedule_id`) REFERENCES `schedules` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `schedules_actions`
--

LOCK TABLES `schedules_actions` WRITE;
/*!40000 ALTER TABLE `schedules_actions` DISABLE KEYS */;
INSERT INTO `schedules_actions` VALUES (3,'ScheduleRelayAction','Close',NULL,NULL,NULL,82,3,NULL),(4,'ScheduleRelayAction','Open',NULL,NULL,NULL,82,4,NULL);
/*!40000 ALTER TABLE `schedules_actions` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sensors`
--

DROP TABLE IF EXISTS `sensors`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sensors` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `status_` bit(1) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  `controller_id` bigint(20) DEFAULT NULL,
  `controller_input` int(11) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `sensors_fk_controller_id_idx` (`controller_id`),
  KEY `sensors_fk_project_id_idx` (`project_id`),
  CONSTRAINT `sensors_fk_controller_id` FOREIGN KEY (`controller_id`) REFERENCES `controllers` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `sensors_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sensors`
--

LOCK TABLES `sensors` WRITE;
/*!40000 ALTER TABLE `sensors` DISABLE KEYS */;
INSERT INTO `sensors` VALUES (1,'CBCĐ cổng','\0',4,NULL,0);
/*!40000 ALTER TABLE `sensors` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `tokens`
--

DROP TABLE IF EXISTS `tokens`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `tokens` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `value` varchar(511) COLLATE utf8_unicode_ci DEFAULT NULL,
  `expiry` datetime DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `tokens_user_id_fk_idx` (`user_id`),
  CONSTRAINT `tokens_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `tokens`
--

LOCK TABLES `tokens` WRITE;
/*!40000 ALTER TABLE `tokens` DISABLE KEYS */;
/*!40000 ALTER TABLE `tokens` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `usergroups`
--

DROP TABLE IF EXISTS `usergroups`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `usergroups` (
  `id` bigint(20) NOT NULL,
  `name` varchar(127) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `usergroups`
--

LOCK TABLES `usergroups` WRITE;
/*!40000 ALTER TABLE `usergroups` DISABLE KEYS */;
INSERT INTO `usergroups` VALUES (1,'SystemAdmin'),(2,'ProjectAdmin'),(3,'ProjectUser');
/*!40000 ALTER TABLE `usergroups` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `users`
--

DROP TABLE IF EXISTS `users`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `users` (
  `id` bigint(20) NOT NULL,
  `name` varchar(63) COLLATE utf8_unicode_ci NOT NULL,
  `fullname` varchar(127) COLLATE utf8_unicode_ci DEFAULT NULL,
  `password_` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `email` varchar(63) COLLATE utf8_unicode_ci DEFAULT NULL,
  `language` varchar(63) COLLATE utf8_unicode_ci DEFAULT NULL,
  `phone_no` varchar(31) COLLATE utf8_unicode_ci DEFAULT NULL,
  `group_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  UNIQUE KEY `unique_name` (`name`),
  KEY `user_fk_group_id_idx` (`group_id`),
  CONSTRAINT `user_fk_group_id` FOREIGN KEY (`group_id`) REFERENCES `usergroups` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users`
--

LOCK TABLES `users` WRITE;
/*!40000 ALTER TABLE `users` DISABLE KEYS */;
INSERT INTO `users` VALUES (1,'lord','SuperAdmin','dff00dce8c09cb2b1b1d9540130288f44c3e0d9f36e8663ec33ad43ba6882d58149df782d93c39ae','trancongsan@gmail.com',NULL,'0933439994',1),(4,'trung','Đức Trung','f6fa80d03bc3eee84c62b4e563946608cb5af5ab9babca8d193978cffb1ae89a1690762aba67f6d7','trancongsan@gmail.com',NULL,'0933439994',2),(5,'admin','Sann Tran','5e47fc74ca8a68a7e6a42c9a992bf4738592b366892cbe28903d6fd41ae604a2c375d02d69ff6b79','trancongsan@gmail.com',NULL,'0933439994',2);
/*!40000 ALTER TABLE `users` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `users_devices`
--

DROP TABLE IF EXISTS `users_devices`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `users_devices` (
  `id` bigint(20) NOT NULL,
  `role` bit(1) DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  `device_id` bigint(20) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `users_devices_user_id_fk_idx` (`user_id`),
  KEY `users_devices_device_id_fk_idx` (`device_id`),
  KEY `users_devices_project_id_fk_idx` (`project_id`),
  CONSTRAINT `users_devices_device_id_fk` FOREIGN KEY (`device_id`) REFERENCES `devices` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_devices_project_id_fk` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_devices_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_devices`
--

LOCK TABLES `users_devices` WRITE;
/*!40000 ALTER TABLE `users_devices` DISABLE KEYS */;
INSERT INTO `users_devices` VALUES (66,'',1,20,4),(67,'',1,22,4),(68,'',1,36,4),(69,'',1,8,4),(70,'',1,6,4),(71,'',1,9,4),(72,'',1,10,4),(73,'',1,21,4),(74,'',1,35,4),(75,'',1,13,4),(76,'',1,23,4),(77,'',1,26,4),(78,'',1,12,4),(79,'',5,20,4),(80,'',5,36,4),(81,'',5,22,4),(82,'',5,6,4),(83,'',5,8,4),(84,'',5,9,4),(85,'',5,10,4),(86,'',5,35,4),(87,'',5,13,4),(88,'',5,26,4),(89,'',5,21,4),(90,'',5,23,4),(91,'',5,12,4),(92,'',5,37,4),(93,'',1,37,4);
/*!40000 ALTER TABLE `users_devices` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `users_projects`
--

DROP TABLE IF EXISTS `users_projects`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `users_projects` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `role` bit(1) DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `users_projects_user_id_fk_idx` (`user_id`),
  KEY `users_projects_project_id_fk_idx` (`project_id`),
  CONSTRAINT `users_projects_project_id_fk` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_projects_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=19 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_projects`
--

LOCK TABLES `users_projects` WRITE;
/*!40000 ALTER TABLE `users_projects` DISABLE KEYS */;
INSERT INTO `users_projects` VALUES (2,'',1,1),(4,'',1,4),(8,'\0',4,1),(9,'\0',4,4),(12,'\0',1,5),(15,'',4,5),(16,'\0',5,1),(17,'',5,4),(18,'\0',5,5);
/*!40000 ALTER TABLE `users_projects` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `users_scenes`
--

DROP TABLE IF EXISTS `users_scenes`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `users_scenes` (
  `id` bigint(20) NOT NULL,
  `role` bit(1) DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  `scene_id` bigint(20) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `users_scenes_user_id_fk_idx` (`user_id`),
  KEY `users_scenes_scene_id_fk_idx` (`scene_id`),
  KEY `users_scenes_project_id_fk_idx` (`project_id`),
  CONSTRAINT `users_scenes_project_id_fk` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_scenes_scene_id_fk` FOREIGN KEY (`scene_id`) REFERENCES `scenes` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_scenes_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_scenes`
--

LOCK TABLES `users_scenes` WRITE;
/*!40000 ALTER TABLE `users_scenes` DISABLE KEYS */;
INSERT INTO `users_scenes` VALUES (3,'',1,1,4),(4,'',5,1,4),(5,'',5,2,4),(6,'',1,2,4);
/*!40000 ALTER TABLE `users_scenes` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `users_zones`
--

DROP TABLE IF EXISTS `users_zones`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `users_zones` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `role` bit(1) DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  `zone_id` bigint(20) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `users_zones_user_id_fk_idx` (`user_id`),
  KEY `users_zones_zone_id_fk_idx` (`zone_id`),
  KEY `users_zones_project_id_fk_idx` (`project_id`),
  CONSTRAINT `users_zones_project_id_fk` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_zones_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_zones_zone_id_fk` FOREIGN KEY (`zone_id`) REFERENCES `zones` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB AUTO_INCREMENT=61 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_zones`
--

LOCK TABLES `users_zones` WRITE;
/*!40000 ALTER TABLE `users_zones` DISABLE KEYS */;
INSERT INTO `users_zones` VALUES (19,'',4,8,4),(20,'',4,9,5),(22,'',1,9,5),(51,'',1,7,4),(52,'',1,4,4),(53,'',1,3,4),(54,'',1,8,4),(55,'',5,4,4),(56,'',5,7,4),(57,'',5,3,4),(58,'',5,8,4),(59,'',1,6,1),(60,'',1,12,1);
/*!40000 ALTER TABLE `users_zones` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `versions`
--

DROP TABLE IF EXISTS `versions`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `versions` (
  `id` bigint(20) NOT NULL,
  `name` varchar(63) COLLATE utf8_unicode_ci DEFAULT NULL,
  `date_time` varchar(63) COLLATE utf8_unicode_ci DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `versions`
--

LOCK TABLES `versions` WRITE;
/*!40000 ALTER TABLE `versions` DISABLE KEYS */;
INSERT INTO `versions` VALUES (1,'1.2.0','201712221630');
/*!40000 ALTER TABLE `versions` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `zones`
--

DROP TABLE IF EXISTS `zones`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `zones` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `order_` int(11) DEFAULT '0',
  `project_id` bigint(20) DEFAULT NULL,
  `area_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `zones_fk_area_id_idx` (`area_id`),
  KEY `zones_fk_project_id_idx` (`project_id`),
  CONSTRAINT `zones_fk_area_id` FOREIGN KEY (`area_id`) REFERENCES `areas` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `zones_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `zones`
--

LOCK TABLES `zones` WRITE;
/*!40000 ALTER TABLE `zones` DISABLE KEYS */;
INSERT INTO `zones` VALUES (3,'Phòng R&D',1,4,11),(4,'Phòng Bếp',2,4,10),(6,'*Vùng',NULL,1,12),(7,'Phòng ngủ',3,4,11),(8,'Phòng Khách',1,4,10),(9,'Phòng Khách',0,5,13),(12,'Vungf 2',1,1,12);
/*!40000 ALTER TABLE `zones` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2018-03-20 23:48:11
