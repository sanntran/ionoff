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
INSERT INTO `areas` VALUES (10,'Tầng trệt',0,4),(11,'Tầng lầu',1,4),(12,'Tầng trệt',0,1),(13,'Tầng trệt',0,5),(14,'khulau1',1,6);
/*!40000 ALTER TABLE `areas` ENABLE KEYS */;
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
INSERT INTO `dashboards` VALUES (6,1,NULL,1),(7,1,6,1),(8,4,NULL,5),(9,4,9,5),(12,2,NULL,4),(13,2,4,4),(14,2,7,4),(15,2,8,4),(16,2,3,4),(18,5,NULL,6),(19,5,10,6),(20,1,NULL,4),(21,1,3,4),(22,1,4,4),(23,1,7,4),(24,1,8,4);
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
INSERT INTO `dashboards_devices` VALUES (1,14,20),(2,12,20),(4,12,23);
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
INSERT INTO `devices` VALUES (6,9,'Đèn trần 1','Appliance','2018-03-12 19:50:58',301,NULL,NULL,NULL,NULL,4,3),(8,12,'Đèn trần 2','Light','2018-03-12 19:50:58',302,NULL,NULL,NULL,NULL,4,3),(9,1531,'Mini Server','Player','2018-03-02 11:23:14',303,'1.52.38.147',6600,'b827ebd1e3e5','IMP',4,3),(10,6278,'HPEliteBook','Player','2018-05-14 01:35:43',304,'127.0.0.1',6600,'CND04104CQ','XMP',4,3),(12,277,'Đfn trần 1','Light','2018-03-12 20:03:07',100,NULL,NULL,NULL,NULL,4,8),(13,231,'Đèn trần 2','Light','2018-04-09 23:30:02',101,NULL,NULL,NULL,NULL,4,8),(20,200314,'IMP Raspberry','Player','2018-04-02 22:51:01',200,'1.52.33.58',NULL,'b827ebd1e3e4','IMP',4,7),(21,380,'Đèn bảng hiệu','Light','2018-05-13 23:19:00',102,NULL,NULL,NULL,NULL,4,8),(22,98,'Đèn tường tranh','Light','2018-04-07 23:25:06',201,NULL,NULL,NULL,NULL,4,7),(23,331,'Đèn ngoài thềm','Light','2018-04-22 20:30:01',103,NULL,NULL,NULL,NULL,4,8),(26,19227,'XMP Delux','Player','2018-04-09 16:12:55',105,'1.52.33.34',NULL,'UH81410011855','XMP',4,8),(27,18,'Đèn 1','Light','2018-01-14 21:09:21',1,NULL,NULL,NULL,NULL,5,9),(28,0,'Đèn 2','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(29,0,'Đèn 3','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(30,0,'Đèn 4','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(31,0,'Đèn 5','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(32,0,'Đèn 6','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(33,0,'Đèn 7','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(34,0,'Đèn 8','Light',NULL,1,NULL,NULL,NULL,NULL,5,9),(35,149,'Quạt treo tường','Appliance','2018-03-12 20:03:08',104,NULL,NULL,NULL,NULL,4,8),(36,94,'Đèn tường giá sách','Light','2018-05-13 23:26:40',202,NULL,NULL,NULL,NULL,4,7),(37,61,'Đèn ngủ','Light','2018-04-22 18:07:23',1,NULL,NULL,NULL,NULL,6,10),(40,10,'Cân 2','WeighScale','2018-04-23 02:13:23',1,NULL,NULL,'WS10180412000000',NULL,4,3);
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
INSERT INTO `modes` VALUES (1,'Ngay moi','2018-04-09 06:30:01','','Daily','06:30 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',4),(2,'Di ngu','2017-12-11 23:19:09','\0',NULL,NULL,NULL,'\0',4),(3,'Đi ngủ','2018-05-07 23:30:02','','Daily','11:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',6);
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
INSERT INTO `modes_scenes` VALUES (1,1,3,NULL),(2,1,4,NULL),(3,1,7,NULL),(4,2,3,2),(5,2,4,NULL),(6,2,7,NULL),(7,2,8,NULL),(8,1,8,NULL),(9,3,10,NULL);
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
  `condition_` varchar(255) CHARACTER SET utf8 DEFAULT NULL,
  `time_buffer` int(11) DEFAULT NULL,
  `reset_time` bigint(20) DEFAULT NULL,
  `mode_id` bigint(20) DEFAULT NULL,
  `sensor_id` bigint(20) DEFAULT NULL,
  `message` varchar(511) COLLATE utf8_unicode_ci DEFAULT NULL,
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
INSERT INTO `modes_sensors` VALUES (2,'','x == 1',NULL,NULL,2,3,NULL),(3,'','x == 2',NULL,1526231372057,NULL,3,'Báo động nguy hiểm'),(4,'','x == 1',NULL,1526236483729,NULL,4,'Báo động, có người');
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
INSERT INTO `modes_sensors_scenes` VALUES (5,2,3,NULL),(6,2,4,NULL),(7,2,7,NULL),(8,2,8,NULL),(9,3,3,2),(10,3,4,NULL),(11,3,7,NULL),(12,3,8,NULL),(13,4,3,2),(14,4,4,NULL),(15,4,7,NULL),(16,4,8,NULL);
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
INSERT INTO `modes_sensors_users` VALUES (2,2,2,'\0','\0',4),(3,3,2,'\0','',4),(4,4,2,'\0','',4),(5,2,1,'\0','\0',4),(6,2,1,'\0','\0',4);
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
INSERT INTO `playleafs` VALUES (15,'Super Soothing Baby Sleep Music ♥♥♥ Relaxing Bedtime Piano Lullabies Collection ♫♫♫ Sweet Dreams',NULL,'HGz1u94qrgE',NULL,NULL,NULL,NULL,0,6,'youtube'),(16,'Hillsong - 1 hour piano music for babies Lullabies',NULL,'vfPU0_jatAI',NULL,NULL,NULL,NULL,0,7,'youtube'),(17,'Super Relaxing Piano Lullabies Collection ♥♥ Soothing Baby Sleep Music ♫♫ Sweet Dreams Good Night',NULL,'UwxfuLuiIDo',NULL,NULL,NULL,NULL,0,8,'youtube'),(18,'Mozart - Piano Solo',NULL,'x8N7mFboGnc',NULL,NULL,NULL,NULL,0,10,'youtube'),(19,'Meditate with Mozart @ 432Hz Classical Piano | Vol 1',NULL,'A7xYccLlO3g',NULL,NULL,NULL,NULL,0,11,'youtube'),(20,'The Best of Mozart',NULL,'Rb0UmrCXxVA',NULL,NULL,NULL,NULL,0,12,'youtube'),(21,'Beethoven | Piano Sonata No. 8 in C minor \"Pathétique\" | Daniel Barenboim',NULL,'cg9KQ610biU',NULL,NULL,NULL,NULL,0,13,'youtube'),(27,'Jingle Bells - Christmas Song for Children | LooLoo Kids',NULL,'PHGkMMjblL8',NULL,NULL,NULL,NULL,0,30,'youtube'),(28,'Hush Little Baby Lullaby Collection | Songs for Babies to Sleep by HooplaKidz | 66 Min',NULL,'7zSCza_2Eok',NULL,NULL,NULL,NULL,0,31,'youtube'),(29,'BABY SLEEPING CHRISTMAS MUSIC - Christmas Relaxing Music for kids - Jingle Bells, Silent Night',NULL,'mtMjMsKd5ek',NULL,NULL,NULL,NULL,0,32,'youtube');
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
INSERT INTO `playlists` VALUES (3,'Baby piano',NULL,NULL,2),(4,'Mozard piano',NULL,NULL,2),(5,'Bethoven piano',NULL,NULL,2),(10,'Baby wakeup',NULL,NULL,2),(11,'Baby sleep',NULL,NULL,2);
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
INSERT INTO `playnodes` VALUES (5,'Hoa Cỏ Vàng Nơi Ấy','album',0,3),(6,NULL,'youtube',1,3),(7,NULL,'youtube',2,3),(8,NULL,'youtube',3,3),(9,'Yêu','album',0,4),(10,NULL,'youtube',1,4),(11,NULL,'youtube',2,4),(12,NULL,'youtube',3,4),(13,NULL,'youtube',0,5),(30,NULL,'youtube',0,10),(31,NULL,'youtube',0,11),(32,NULL,'youtube',1,11);
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
INSERT INTO `projects` VALUES (1,'IOnOff','61/40 st.48, Thu Duc, HCMC'),(4,'NK Sông Đà','61/40 st. 48, Thu Duc, HCMC'),(5,'Trung\'s Home','Da Lat, Lam Dong, VN'),(6,'Hien\'s Home','Lâm Thị Hố, Q.12. HCM');
/*!40000 ALTER TABLE `projects` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `relaydrivers`
--

DROP TABLE IF EXISTS `relaydrivers`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `relaydrivers` (
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
-- Dumping data for table `relaydrivers`
--

LOCK TABLES `relaydrivers` WRITE;
/*!40000 ALTER TABLE `relaydrivers` DISABLE KEYS */;
INSERT INTO `relaydrivers` VALUES (5,'A30000EF','183.81.14.122',80,'A30000EF','IONOFF_P8',1513941082411,NULL,4),(6,'P4-A30000C0','192.168.1.130',80,'A30000C0','IONOFF_P4',1489336064852,NULL,4),(8,'E410171221Agc5Xr','1.52.34.158',NULL,'E410171221Agc5Xr','IONOFF_E4',1526236572489,NULL,4),(9,'A30000EE','58.187.168.232',NULL,'A30000EE','IONOFF_P8',1515979569519,NULL,5),(10,'E410180116Szc3e7',NULL,NULL,'E410180116Szc3e7','IONOFF_E4',1523555147648,NULL,4),(11,'E410171221XrC5Fr',NULL,NULL,'E410171221XrC5Fr','IONOFF_E4',1526236568215,13,4),(12,'E410180116Szc3e8','',NULL,'E410180116Szc3e8','IONOFF_E4',1526236483375,NULL,4),(13,'E4101802128539A6','',NULL,'E4101802128539A6','IONOFF_E4',1526236551448,NULL,6);
/*!40000 ALTER TABLE `relaydrivers` ENABLE KEYS */;
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
INSERT INTO `relaygroups` VALUES (1,NULL,4),(2,NULL,4),(3,NULL,4),(5,NULL,4),(7,NULL,4);
/*!40000 ALTER TABLE `relaygroups` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `relaygroups_relays`
--

DROP TABLE IF EXISTS `relaygroups_relays`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `relaygroups_relays` (
  `id` bigint(20) NOT NULL,
  `name` varchar(255) DEFAULT NULL,
  `is_leader` bit(1) DEFAULT NULL,
  `relay_id` bigint(20) DEFAULT NULL,
  `group_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `relaygroups_relays_fk_relay_id_idx` (`relay_id`),
  KEY `relaygroups_relays_fk_group_id_idx` (`group_id`),
  CONSTRAINT `relaygroups_relays_fk_group_id` FOREIGN KEY (`group_id`) REFERENCES `relaygroups` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `relaygroups_relays_fk_relay_id` FOREIGN KEY (`relay_id`) REFERENCES `relays` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `relaygroups_relays`
--

LOCK TABLES `relaygroups_relays` WRITE;
/*!40000 ALTER TABLE `relaygroups_relays` DISABLE KEYS */;
INSERT INTO `relaygroups_relays` VALUES (82,NULL,NULL,82,3),(95,NULL,'',95,3);
/*!40000 ALTER TABLE `relaygroups_relays` ENABLE KEYS */;
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
  `is_locked` bit(1) DEFAULT NULL,
  `driver_id` bigint(20) DEFAULT NULL,
  `device_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `relays_fk_device_id_idx` (`device_id`),
  KEY `relays_fk_driver_id_idx` (`driver_id`),
  CONSTRAINT `relays_fk_device_id` FOREIGN KEY (`device_id`) REFERENCES `devices` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `relays_fk_driver_id` FOREIGN KEY (`driver_id`) REFERENCES `relaydrivers` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `relays`
--

LOCK TABLES `relays` WRITE;
/*!40000 ALTER TABLE `relays` DISABLE KEYS */;
INSERT INTO `relays` VALUES (49,624,'Relay 1',NULL,'2017-12-22 16:36:08','Switch',0,'',NULL,5,NULL),(50,355,'Relay 2',NULL,'2017-11-26 17:28:52','Switch',1,'\0',NULL,5,NULL),(51,158,'Relay 3',NULL,'2017-12-16 20:54:28','Switch',2,'\0',NULL,5,NULL),(52,99,'Relay 4',NULL,'2017-12-10 13:57:54','Switch',3,'\0',NULL,5,NULL),(53,49,'Relay 5',NULL,'2017-12-10 13:57:52','Button',4,'\0',NULL,5,35),(54,32,'Relay 6',NULL,NULL,'Button',5,'\0',NULL,5,35),(55,49,'Relay 7',NULL,'2017-12-16 10:25:11','Switch',6,'\0',NULL,5,NULL),(56,23,'Relay 8',NULL,NULL,'Switch',7,'\0',NULL,5,NULL),(57,228,'Relay 1',NULL,NULL,'Switch',0,'\0',NULL,6,NULL),(58,490,'Relay 2','Relay 2',NULL,'Switch',1,'\0',NULL,6,NULL),(59,252,'Relay 3',NULL,NULL,'Button',2,'\0',NULL,6,35),(60,99,'Relay 4',NULL,NULL,'Switch',3,'\0',NULL,6,NULL),(81,467,'Relay 1',NULL,'2018-05-13 23:19:00','Switch',0,'\0',NULL,8,21),(82,475,'Relay 2',NULL,'2018-04-22 20:30:01','Switch',1,'\0',NULL,8,23),(83,38,'Relay 3',NULL,'2018-04-09 20:24:23','Switch',2,'\0',NULL,8,NULL),(84,34,'Relay 4',NULL,'2018-04-09 20:24:21','Switch',3,'\0',NULL,8,NULL),(85,123,'Relay 1',NULL,'2018-01-14 21:09:21','Switch',0,'\0',NULL,9,27),(86,1,'Relay 2',NULL,NULL,'Switch',1,'\0',NULL,9,28),(87,1,'Relay 3',NULL,NULL,'Switch',2,'\0',NULL,9,29),(88,1,'Relay 4',NULL,NULL,'Switch',3,'\0',NULL,9,30),(89,1,'Relay 5',NULL,NULL,'Switch',4,'\0',NULL,9,31),(90,1,'Relay 6',NULL,NULL,'Switch',5,'\0',NULL,9,32),(91,1,'Relay 7',NULL,NULL,'Switch',6,'\0',NULL,9,33),(92,0,'Relay 8',NULL,NULL,'Switch',7,'\0',NULL,9,NULL),(93,548,'Relay 1',NULL,'2018-03-30 12:09:55','Switch',0,'\0',NULL,10,35),(94,652,'Relay 2',NULL,'2018-04-09 23:30:02','Switch',1,'\0',NULL,10,13),(95,3816,'Relay 3',NULL,'2018-04-09 23:17:54','Switch',2,'\0',NULL,10,12),(96,126,'Relay 4',NULL,'2017-12-28 14:59:54','Switch',3,'\0',NULL,10,NULL),(97,38,'Relay 1',NULL,'2018-04-22 20:55:07','Switch',0,'\0',NULL,11,NULL),(98,37,'Relay 2',NULL,'2018-05-14 01:12:06','Switch',1,'',NULL,11,NULL),(99,204,'Relay 3',NULL,'2018-04-22 21:03:57','Switch',2,'\0',NULL,11,22),(100,429,'Relay 4',NULL,'2018-05-13 23:26:40','Switch',3,'\0',NULL,11,36),(101,17,'Relay 1',NULL,'2018-03-12 19:50:58','Switch',0,'\0',NULL,12,6),(102,19,'Relay 2',NULL,'2018-05-14 01:34:43','Switch',1,'\0',NULL,12,8),(103,12,'Relay 3',NULL,'2018-05-14 01:34:43','Switch',2,'\0',NULL,12,NULL),(104,0,'Relay 4',NULL,NULL,'Switch',3,'\0',NULL,12,NULL),(105,9,'Relay 1',NULL,'2018-05-13 15:07:41','Switch',0,'',NULL,13,NULL),(106,73,'Relay 2',NULL,'2018-04-22 18:07:23','Switch',1,'\0',NULL,13,37),(107,4,'Relay 3',NULL,'2018-04-07 22:35:12','Switch',2,'\0',NULL,13,NULL),(108,0,'Relay 4',NULL,NULL,'Switch',3,'\0',NULL,13,NULL);
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
INSERT INTO `scenes` VALUES (2,'Nhạc','2018-05-14 01:34:51',3);
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
INSERT INTO `scenes_actions` VALUES (4,'SceneRelayAction','None',NULL,NULL,NULL,101,4,NULL),(5,'SceneRelayAction','None',NULL,NULL,NULL,102,5,NULL),(6,'ScenePlayerAction','Play','','.albums\\sdasd#456456.album','file',NULL,6,9),(7,'ScenePlayerAction','Play','75','D:\\Media\\.Albums\\Anh Sẽ Đến ... Giấc Mơ Buồn#161.album','file',NULL,7,10);
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
INSERT INTO `scenes_devices` VALUES (4,NULL,NULL,2,6),(5,NULL,NULL,2,8),(6,NULL,NULL,2,9),(7,1,2,2,10),(10,NULL,NULL,2,40);
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
INSERT INTO `schedules` VALUES (1,'Bật đèn bảng hiệu','Daily','06:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',21,4),(2,'Tắt đèn bảng hiệu','Daily','11:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',21,4),(3,'Bật đèn thềm','Daily','06:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',23,4),(4,'Tắt đèn thềm','Daily','08:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',23,4),(5,'Bật đèn bảng hiệu sáng','Daily','04:30 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'\0',21,4),(6,'Tắt đèn bảng hiệu sáng','Daily','05:30 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'\0',21,4),(7,'Bật đèn phòng ngủ','Daily','08:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'\0',22,4),(8,'Tắt đèn phòng ngủ','Daily','01:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'\0',22,4),(9,'Bật đèn phòng khách','Daily','06:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',13,4),(10,'Tắt đèn phòng khách','Daily','11:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat','',0,'',13,4);
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
INSERT INTO `schedules_actions` VALUES (1,'ScheduleRelayAction','Close',NULL,NULL,NULL,81,1,NULL),(2,'ScheduleRelayAction','Open',NULL,NULL,NULL,81,2,NULL),(3,'ScheduleRelayAction','Close',NULL,NULL,NULL,82,3,NULL),(4,'ScheduleRelayAction','Open',NULL,NULL,NULL,82,4,NULL),(5,'ScheduleRelayAction','Close',NULL,NULL,NULL,81,5,NULL),(6,'ScheduleRelayAction','Open',NULL,NULL,NULL,81,6,NULL),(7,'ScheduleRelayAction','Close',NULL,NULL,NULL,99,7,NULL),(8,'ScheduleRelayAction','Open',NULL,NULL,NULL,99,8,NULL),(9,'ScheduleRelayAction','Close',NULL,NULL,NULL,94,9,NULL),(10,'ScheduleRelayAction','Open',NULL,NULL,NULL,94,10,NULL);
/*!40000 ALTER TABLE `schedules_actions` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `schema_version`
--

DROP TABLE IF EXISTS `schema_version`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `schema_version` (
  `version_rank` int(11) NOT NULL,
  `installed_rank` int(11) NOT NULL,
  `version` varchar(50) NOT NULL,
  `description` varchar(200) NOT NULL,
  `type` varchar(20) NOT NULL,
  `script` varchar(1000) NOT NULL,
  `checksum` int(11) DEFAULT NULL,
  `installed_by` varchar(100) NOT NULL,
  `installed_on` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `execution_time` int(11) NOT NULL,
  `success` tinyint(1) NOT NULL,
  PRIMARY KEY (`version`),
  KEY `schema_version_vr_idx` (`version_rank`),
  KEY `schema_version_ir_idx` (`installed_rank`),
  KEY `schema_version_s_idx` (`success`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `schema_version`
--

LOCK TABLES `schema_version` WRITE;
/*!40000 ALTER TABLE `schema_version` DISABLE KEYS */;
INSERT INTO `schema_version` VALUES (1,1,'1','<< Flyway Init >>','INIT','<< Flyway Init >>',NULL,'root','2018-05-18 16:21:11',0,1),(2,2,'2','alter table sensors','SQL','V2__alter_table_sensors.sql',-650510190,'root','2018-05-18 16:21:11',15,1),(3,3,'3','alter_tabler_controllers_relays','SQL','V3__alter_tabler_controllers_relays.sql',-650510190,'root','2018-05-18 16:21:11',15,1),(4,4,'4','alter_tabler_modes_ensors','SQL','V4__alter_tabler_modes_ensors.sql',-650510190,'root','2018-05-18 16:21:11',15,1),(5,5,'5','alter_tbl_relays_add_col_is_locked','SQL','V5__alter_tbl_relays_add_col_is_locked.sql',-650510190,'root','2018-05-18 16:21:11',15,1);
/*!40000 ALTER TABLE `schema_version` ENABLE KEYS */;
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
  `type_` varchar(63) COLLATE utf8_unicode_ci DEFAULT NULL,
  `unit` varchar(63) COLLATE utf8_unicode_ci DEFAULT NULL,
  `device_id` bigint(20) DEFAULT NULL,
  `zone_id` bigint(20) DEFAULT NULL,
  `switch_id` bigint(20) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `sensors_fk_project_id_idx` (`project_id`),
  KEY `sensors_fk_device_id_idx` (`device_id`),
  KEY `sensors_fk_zone_id_idx` (`zone_id`),
  KEY `sensors_fk_switch_id_idx` (`switch_id`),
  CONSTRAINT `sensors_fk_device_id` FOREIGN KEY (`device_id`) REFERENCES `devices` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `sensors_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `sensors_fk_switch_id` FOREIGN KEY (`switch_id`) REFERENCES `switchs` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `sensors_fk_zone_id` FOREIGN KEY (`zone_id`) REFERENCES `zones` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sensors`
--

LOCK TABLES `sensors` WRITE;
/*!40000 ALTER TABLE `sensors` DISABLE KEYS */;
INSERT INTO `sensors` VALUES (3,'Weigh Sensor','ANALOG','kg',40,3,NULL,4),(4,'*Cảm biến',NULL,NULL,NULL,NULL,13,4);
/*!40000 ALTER TABLE `sensors` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sensors_data`
--

DROP TABLE IF EXISTS `sensors_data`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sensors_data` (
  `id` bigint(20) NOT NULL,
  `time_` datetime DEFAULT NULL,
  `value_` double DEFAULT NULL,
  `index_` bigint(20) DEFAULT NULL,
  `sensor_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `sensors_data_fk_sensor_id_idx` (`sensor_id`),
  CONSTRAINT `sensors_data_fk_sensor_id` FOREIGN KEY (`sensor_id`) REFERENCES `sensors` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sensors_data`
--

LOCK TABLES `sensors_data` WRITE;
/*!40000 ALTER TABLE `sensors_data` DISABLE KEYS */;
INSERT INTO `sensors_data` VALUES (1,'2018-04-23 02:11:20',86.1,34,3),(2,'2018-04-23 02:13:23',99.1,35,3),(3,'2018-05-13 22:19:56',2,38,3),(4,'2018-05-13 22:20:46',2,38,3),(5,'2018-05-13 22:31:29',2,38,3),(6,'2018-05-13 22:34:07',2,38,3),(7,'2018-05-13 23:26:29',2,38,3),(8,'2018-05-13 23:29:07',2,38,3),(9,'2018-05-13 23:33:33',2,38,3),(10,'2018-05-13 23:47:53',2,38,3),(11,'2018-05-13 23:51:59',2,38,3),(12,'2018-05-13 23:59:33',2,38,3),(13,'2018-05-14 00:02:19',2,38,3),(14,'2018-05-14 00:04:54',2,38,3),(15,'2018-05-14 00:05:47',2,38,3),(16,'2018-05-14 00:06:02',2,38,3),(17,'2018-05-14 00:09:32',2,38,3);
/*!40000 ALTER TABLE `sensors_data` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sensors_status`
--

DROP TABLE IF EXISTS `sensors_status`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `sensors_status` (
  `sensor_id` bigint(20) NOT NULL,
  `time_` datetime DEFAULT NULL,
  `value_` double DEFAULT NULL,
  `index_` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`sensor_id`),
  CONSTRAINT `sensors_status_fk_sensor_id` FOREIGN KEY (`sensor_id`) REFERENCES `sensors` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sensors_status`
--

LOCK TABLES `sensors_status` WRITE;
/*!40000 ALTER TABLE `sensors_status` DISABLE KEYS */;
INSERT INTO `sensors_status` VALUES (3,'2018-05-14 00:09:32',2,38),(4,'2018-05-14 01:34:44',1,NULL);
/*!40000 ALTER TABLE `sensors_status` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `switchs`
--

DROP TABLE IF EXISTS `switchs`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `switchs` (
  `id` bigint(20) NOT NULL,
  `name` varchar(127) DEFAULT NULL,
  `index_` int(11) DEFAULT NULL,
  `time_` datetime DEFAULT NULL,
  `status_` bit(1) DEFAULT NULL,
  `driver_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `switchs_fk_driver_id_idx` (`driver_id`),
  CONSTRAINT `switchs_fk_driver_id` FOREIGN KEY (`driver_id`) REFERENCES `relaydrivers` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `switchs`
--

LOCK TABLES `switchs` WRITE;
/*!40000 ALTER TABLE `switchs` DISABLE KEYS */;
INSERT INTO `switchs` VALUES (1,NULL,0,NULL,'\0',11),(2,NULL,1,NULL,'\0',11),(3,NULL,2,NULL,'\0',11),(4,NULL,3,NULL,'\0',11),(5,NULL,0,NULL,'\0',8),(6,NULL,1,NULL,'\0',8),(7,NULL,2,NULL,'\0',8),(8,NULL,3,NULL,'\0',8),(9,NULL,0,NULL,'\0',13),(10,NULL,1,NULL,'\0',13),(11,NULL,2,NULL,'\0',13),(12,NULL,3,NULL,'\0',13),(13,NULL,0,NULL,'',12);
/*!40000 ALTER TABLE `switchs` ENABLE KEYS */;
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
INSERT INTO `users` VALUES (1,'lord','Super Admin','b74b820db358adf6cd976a10542fbe88d14c7e5da8e8f2cde526a54235fdb6f486553d9340e328aa','trancongsan@gmail.com',NULL,'0933439994',1),(2,'admin','Home Admin','27bec3835cb29f881ad306d8ffa1a3a56d82bb83cd6bbeef749f9369c765a66f9ae2223dc9dd8c9f','trancongsan@gmail.com','vi','0933439994',2),(4,'trung','Đức Trung','f6fa80d03bc3eee84c62b4e563946608cb5af5ab9babca8d193978cffb1ae89a1690762aba67f6d7','trancongsan@gmail.com',NULL,'0933439994',2),(5,'hien','Chất Hiển','55698dcddb33d38b05e5db1b6fc7a37c0ecb1d19fcbb2b8cf14463a43d2eac22577b1950b80b3965','hiennguyenchat@gmail.com',NULL,'0979173179',2);
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
INSERT INTO `users_devices` VALUES (14,'',4,33,5),(15,'',4,27,5),(16,'',4,28,5),(17,'',4,30,5),(18,'',4,29,5),(19,'',4,31,5),(20,'',4,34,5),(21,'',4,32,5),(22,'',2,36,4),(23,'',2,22,4),(24,'',2,20,4),(25,'',2,23,4),(26,'',2,26,4),(27,'',2,12,4),(28,'',2,35,4),(29,'',2,13,4),(30,'',2,21,4),(31,'',2,8,4),(32,'',2,9,4),(33,'',2,6,4),(34,'',2,10,4),(35,'',5,37,6),(38,'',2,40,4),(39,'',1,40,4),(40,'',1,6,4),(41,'',1,8,4),(42,'',1,9,4),(43,'',1,10,4),(44,'',1,20,4),(45,'',1,22,4),(46,'',1,36,4),(47,'\0',1,12,4),(48,'\0',1,13,4),(49,'',1,21,4),(50,'',1,23,4),(51,'',1,35,4),(52,'',1,26,4);
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
) ENGINE=InnoDB AUTO_INCREMENT=23 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_projects`
--

LOCK TABLES `users_projects` WRITE;
/*!40000 ALTER TABLE `users_projects` DISABLE KEYS */;
INSERT INTO `users_projects` VALUES (2,'',1,1),(3,'\0',2,1),(4,'',1,4),(5,'',2,4),(8,'\0',4,1),(9,'\0',4,4),(12,'\0',1,5),(13,'\0',2,5),(15,'',4,5),(16,'\0',1,6),(17,'\0',2,6),(18,'\0',4,6),(19,'\0',5,1),(20,'\0',5,4),(21,'\0',5,5),(22,'',5,6);
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
INSERT INTO `users_scenes` VALUES (2,'',2,2,4),(3,'',1,2,4);
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
) ENGINE=InnoDB AUTO_INCREMENT=38 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_zones`
--

LOCK TABLES `users_zones` WRITE;
/*!40000 ALTER TABLE `users_zones` DISABLE KEYS */;
INSERT INTO `users_zones` VALUES (19,'',4,8,4),(21,'',2,9,5),(22,'',1,9,5),(27,'',1,6,1),(28,'',4,9,5),(29,'',2,4,4),(30,'',2,7,4),(31,'',2,8,4),(32,'',2,3,4),(33,'',5,10,6),(34,'',1,3,4),(35,'',1,4,4),(36,'',1,7,4),(37,'',1,8,4);
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
INSERT INTO `versions` VALUES (1,'1.5.6','201803230030');
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
INSERT INTO `zones` VALUES (3,'Phòng R&D',100,4,10),(4,'Phòng Bếp',1,4,10),(6,'*Vùng',NULL,1,12),(7,'Phòng ngủ',1,4,11),(8,'Phòng Khách',1,4,10),(9,'Phòng Khách',0,5,13),(10,'Phòng Khách',1,6,14);
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

-- Dump completed on 2018-05-22  0:58:47
