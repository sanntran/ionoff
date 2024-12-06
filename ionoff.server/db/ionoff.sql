-- MySQL dump 10.13  Distrib 5.7.26, for Linux (x86_64)
--
-- Host: localhost    Database: ionoff
-- ------------------------------------------------------
-- Server version	5.7.26-0ubuntu0.18.04.1

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
INSERT INTO `areas` VALUES (10,'Tầng trệt',0,4),(11,'Tầng lầu',1,4),(12,'Tầng trệt',0,1),(13,'Tầng trệt',0,5),(14,'My home',1,6),(15,'TP HCM',1,7),(16,'Đèn',NULL,8),(17,'Nhà',1,10),(18,'Lầu',1,9);
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
  `type_` varchar(255) CHARACTER SET utf8 DEFAULT NULL,
  `ip` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `port` int(11) DEFAULT NULL,
  `key_` varchar(31) CHARACTER SET utf8 DEFAULT NULL,
  `connected_time` bigint(20) DEFAULT NULL,
  `crash_count` int(11) DEFAULT NULL,
  `project_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `controllers_fk_project_id_idx` (`project_id`),
  KEY `controllers_key__idx` (`key_`),
  CONSTRAINT `controllers_fk_project_id` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `controllers`
--

LOCK TABLES `controllers` WRITE;
/*!40000 ALTER TABLE `controllers` DISABLE KEYS */;
INSERT INTO `controllers` VALUES (8,'E410171221Agc5Xr','E4Controller',NULL,NULL,'E410171221Agc5Xr',1564917294485,3,4),(9,'A30000EE','P8Controller','1.52.38.78',NULL,'A30000EE',1525262217763,NULL,5),(10,'E411180412853F78','E4Controller',NULL,NULL,'E411180412853F78',1565694095787,NULL,4),(14,'E41118041285380C','E4Controller',NULL,NULL,'E41118041285380C',1543486230680,NULL,8),(15,'Phòng ngủ','E4Controller','',NULL,'E4111805198537C4',1593848131396,NULL,6),(19,'E312180601170312','E3Controller','',NULL,'E312180601170312',1563387686602,NULL,4),(20,'E313181015696380X','E3Controller','',NULL,'E313181015696380X',1544456595701,NULL,4),(21,'E313181015691E10','E3Controller','',NULL,'E313181015691E10',1560031456237,NULL,4),(22,'E313181015696380','E3Controller','',NULL,'E313181015696380',1564963845901,NULL,4),(30,'iOnOff_test_3nut','E3Controller','',NULL,'E312345678DBF2E5',1559927071203,NULL,7),(32,'E313181015972B38','E3Controller','E313181015972B38',NULL,'E313181015972B38',1594214630790,NULL,10),(38,'my home','E3Controller','E313181015972964',NULL,'E313181015972964',1594874114657,NULL,9),(39,'Phòng bếp','E3Controller','E3131810156960E8',NULL,'E3131810156960E8',1593848114184,NULL,6),(40,'Hiên','E3Controller','E313181015696111',NULL,'E313181015696111',1592676116365,NULL,6),(41,'Phòng Khách [L]','E3Controller','E313181015691DA6',NULL,'E313181015691DA6',1584637538016,NULL,6),(42,'Phòng Khách [R]','E3Controller','E313181015691CF8',NULL,'E313181015691CF8',1593848147803,NULL,6),(43,'E313180610695B3F','E3Controller','',NULL,'E313180610695B3F',1587200604528,NULL,8);
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
INSERT INTO `dashboards` VALUES (6,1,NULL,1),(7,1,6,1),(8,4,NULL,5),(9,4,9,5),(12,2,NULL,4),(13,2,4,4),(14,2,7,4),(15,2,8,4),(16,2,3,4),(20,6,NULL,7),(21,6,11,7),(25,6,12,7),(26,7,NULL,7),(27,7,11,7),(28,7,12,7),(29,8,NULL,8),(30,8,13,8),(40,1,NULL,4),(41,1,3,4),(42,1,4,4),(43,1,7,4),(44,1,8,4),(45,1,15,4),(46,2,15,4),(47,1,16,4),(48,2,16,4),(61,10,NULL,9),(62,11,NULL,10),(66,11,17,10),(67,5,NULL,6),(68,5,14,6),(69,5,10,6),(70,10,18,9),(71,2,NULL,8),(72,2,13,8);
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
INSERT INTO `dashboards_devices` VALUES (1,14,20),(2,12,20),(4,12,23),(6,12,26);
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
  `status_` bit(1) DEFAULT NULL,
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
INSERT INTO `devices` VALUES (6,146,'Đèn trần 1',_binary '','RelayLoad','2018-10-07 19:42:31',301,NULL,NULL,NULL,NULL,4,3),(8,147,'Đèn trần 2',_binary '\0','RelayLoad','2018-10-07 19:42:31',302,NULL,NULL,NULL,NULL,4,3),(9,1532,'Mini Server',_binary '\0','MediaPlayer','2018-03-02 11:23:14',303,'1.52.38.147',6600,'b827ebd1e3e5','IMP',4,3),(10,11564,'HPEliteBook',_binary '\0','MediaPlayer','2020-06-02 14:38:01',304,'1.53.229.85',6600,'CND04104CQ','XMP',4,3),(12,202934,'Đfn trần 1',_binary '\0','RelayLoad','2019-07-05 21:38:08',100,NULL,NULL,NULL,NULL,4,8),(13,206460,'Đèn trần 2',_binary '\0','RelayLoad','2019-07-16 00:12:12',101,NULL,NULL,NULL,NULL,4,8),(20,219963,'IMP Raspberry',_binary '\0','MediaPlayer','2018-11-26 18:01:13',200,'42.118.69.175',NULL,'b827ebd1e3e4','IMP',4,7),(21,332633,'Đèn bảng hiệu',_binary '\0','RelayLoad','2020-07-15 23:00:51',102,NULL,NULL,NULL,NULL,4,8),(22,323654,'Đèn tường tranh',_binary '\0','RelayLoad','2019-08-04 23:15:29',201,NULL,NULL,NULL,NULL,4,7),(23,332684,'Đèn ngoài thềm',_binary '','RelayLoad','2020-07-15 18:00:52',103,NULL,NULL,NULL,NULL,4,8),(26,28376,'XMP Delux',_binary '\0','MediaPlayer','2018-10-08 09:03:51',105,'1.52.35.174',NULL,'UH81410011855','XMP',4,8),(27,82,'Đèn 1',_binary '\0','RelayLoad','2018-04-24 17:59:51',1,NULL,NULL,NULL,NULL,5,9),(28,12,'Đèn 2',_binary '\0','RelayLoad','2018-04-12 13:03:20',1,NULL,NULL,NULL,NULL,5,9),(29,4,'Đèn 3',_binary '\0','RelayLoad','2018-04-12 13:03:19',1,NULL,NULL,NULL,NULL,5,9),(30,2,'Đèn 4',_binary '\0','RelayLoad','2018-04-12 13:03:17',1,NULL,NULL,NULL,NULL,5,9),(31,2,'Đèn 5',_binary '\0','RelayLoad','2018-04-12 13:03:13',1,NULL,NULL,NULL,NULL,5,9),(32,2,'Đèn 6',_binary '\0','RelayLoad','2018-04-12 13:03:15',1,NULL,NULL,NULL,NULL,5,9),(33,4,'Đèn 7',_binary '\0','RelayLoad','2018-04-12 13:03:08',1,NULL,NULL,NULL,NULL,5,9),(34,0,'Đèn 8',_binary '\0','RelayLoad',NULL,1,NULL,NULL,NULL,NULL,5,9),(35,193696,'Quạt treo tường',_binary '\0','RelayLoad','2018-09-27 22:44:28',104,NULL,NULL,NULL,NULL,4,8),(36,323315,'Đèn tường giá sách',_binary '\0','RelayLoad','2019-08-02 18:20:53',202,NULL,NULL,NULL,NULL,4,7),(40,155652,'Đèn Trước Sân',_binary '','RelayLoad','2018-11-29 16:51:46',1,NULL,NULL,NULL,NULL,8,13),(41,155619,'Đèn Lon',_binary '\0','RelayLoad','2018-11-29 16:49:56',1,NULL,NULL,NULL,NULL,8,13),(42,155698,'Đèn 1m2',_binary '','RelayLoad','2018-11-29 16:51:10',1,NULL,NULL,NULL,NULL,8,13),(51,173996,'Đèn Cầu Thang',_binary '\0','RelayLoad','2019-08-13 13:22:47',1,NULL,NULL,NULL,NULL,4,15),(52,171876,'Quạt Treo Tường',_binary '\0','RelayLoad','2019-08-13 16:27:38',2,NULL,NULL,NULL,NULL,4,4),(53,171820,'Đèn Led Tường',_binary '\0','RelayLoad','2019-08-13 17:02:24',3,NULL,NULL,NULL,NULL,4,4),(54,153674,'Đèn Gần Toilet',_binary '\0','RelayLoad','2019-06-09 00:22:15',1,NULL,NULL,NULL,NULL,4,15),(55,153661,'Đèn Gần P. R&D',_binary '\0','RelayLoad','2019-06-09 00:22:16',2,NULL,NULL,NULL,NULL,4,15),(56,127,'Đèn 3',_binary '\0','RelayLoad','2018-10-07 19:42:31',1,NULL,NULL,NULL,NULL,4,3),(58,925,'Đèn Phòng Khách [L]',_binary '\0','RelayLoad','2020-03-19 20:17:38',1,NULL,NULL,NULL,NULL,6,10),(59,925,'Đèn phòng khách [R]',_binary '\0','RelayLoad','2020-06-20 19:33:40',2,NULL,NULL,NULL,NULL,6,10),(60,355,'Đèn Hiên [L]',_binary '\0','RelayLoad','2020-06-09 19:27:39',3,NULL,NULL,NULL,NULL,6,10),(61,554,'Đèn Hiên [R]',_binary '\0','RelayLoad','2020-06-17 22:00:55',4,NULL,NULL,NULL,NULL,6,10),(62,1153,'Đèn phòng thờ',_binary '\0','RelayLoad','2020-07-15 22:00:51',5,NULL,NULL,NULL,NULL,6,14),(63,925,'Đèn Ngủ [L]',_binary '\0','RelayLoad','2020-07-04 12:52:01',6,NULL,NULL,NULL,NULL,6,14),(64,3445,'Đèn Ngủ [R]',_binary '\0','RelayLoad','2020-07-04 12:52:01',7,NULL,NULL,NULL,NULL,6,14),(65,2778,'Đèn bếp',_binary '','RelayLoad','2020-07-04 14:35:14',8,NULL,NULL,NULL,NULL,6,10),(66,8472,'Đèn trần',_binary '\0','RelayLoad','2020-06-09 22:16:29',9,NULL,NULL,NULL,NULL,6,10),(68,94,'Bóng đèn 1',_binary '\0','RelayLoad','2019-06-08 00:04:31',1,NULL,NULL,NULL,NULL,7,11),(69,85,'Bóng đèn 2',_binary '\0','RelayLoad','2019-04-07 15:19:56',2,NULL,NULL,NULL,NULL,7,11),(70,84,'Bóng đèn 3',_binary '\0','RelayLoad','2019-04-07 15:19:56',3,NULL,NULL,NULL,NULL,7,11),(75,1516,'nhà kho ',_binary '\0','RelayLoad','2020-07-08 19:11:51',1,NULL,NULL,NULL,NULL,10,17),(76,1420,'Khách',_binary '\0','RelayLoad','2020-07-08 19:10:53',2,NULL,NULL,NULL,NULL,10,17),(77,969,'Hiên ',_binary '\0','RelayLoad','2020-07-07 22:16:21',3,NULL,NULL,NULL,NULL,10,17),(78,11155,'Tưới rau',_binary '\0','RelayLoad','2020-07-16 11:10:52',1,NULL,NULL,NULL,NULL,9,18),(79,2270,'Đèn ban côn',_binary '\0','RelayLoad','2020-07-15 21:00:52',2,NULL,NULL,NULL,NULL,9,18),(80,1348,'Đèn thờ',_binary '\0','RelayLoad','2020-07-14 15:59:58',3,NULL,NULL,NULL,NULL,9,18),(81,57,'Ora1',_binary '\0','RelayLoad','2020-02-24 10:22:59',1,NULL,NULL,NULL,NULL,8,13),(82,52,'Ora2',_binary '\0','RelayLoad','2020-02-24 13:09:02',1,NULL,NULL,NULL,NULL,8,13),(83,51,'Ora3',_binary '\0','RelayLoad','2020-02-24 13:09:03',1,NULL,NULL,NULL,NULL,8,13);
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
  `order_` int(11) DEFAULT NULL,
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
INSERT INTO `modes` VALUES (1,'Ngay moi',NULL,'2020-07-16 06:30:51',_binary '','Daily','06:30 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',4),(2,'Di ngu',NULL,'2017-12-11 23:19:09',_binary '\0',NULL,NULL,NULL,_binary '\0',4),(3,'*Chế độ',1,'2020-07-16 06:00:51',_binary '','Daily','06:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',9),(4,'Phòng thờ',1,'2020-07-15 18:00:52',_binary '','Daily','06:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',6);
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
INSERT INTO `modes_scenes` VALUES (1,1,3,NULL),(2,1,4,NULL),(3,1,7,NULL),(4,2,3,NULL),(5,2,4,NULL),(6,2,7,NULL),(7,2,8,NULL),(8,1,8,NULL),(11,1,15,NULL),(12,2,15,NULL),(13,1,16,NULL),(14,2,16,NULL),(15,3,18,NULL),(16,4,14,NULL),(17,4,10,NULL);
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
  `condition_` varchar(63) COLLATE utf8_unicode_ci DEFAULT NULL,
  `condition` varchar(63) COLLATE utf8_unicode_ci DEFAULT NULL,
  `value_` double DEFAULT NULL,
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
INSERT INTO `modes_sensors` VALUES (1,_binary '','x == 1',NULL,NULL,NULL,1564915640035,NULL,1,'nguy hiem');
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
INSERT INTO `modes_sensors_scenes` VALUES (1,1,3,NULL),(2,1,4,NULL),(3,1,7,NULL),(4,1,8,NULL);
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
INSERT INTO `modes_sensors_users` VALUES (2,1,2,_binary '\0',_binary '\0',4);
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
INSERT INTO `playleafs` VALUES (15,'Super Soothing Baby Sleep Music ♥♥♥ Relaxing Bedtime Piano Lullabies Collection ♫♫♫ Sweet Dreams',NULL,'HGz1u94qrgE',NULL,NULL,NULL,NULL,0,6,'youtube'),(16,'Hillsong - 1 hour piano music for babies Lullabies',NULL,'vfPU0_jatAI',NULL,NULL,NULL,NULL,0,7,'youtube'),(17,'Super Relaxing Piano Lullabies Collection ♥♥ Soothing Baby Sleep Music ♫♫ Sweet Dreams Good Night',NULL,'UwxfuLuiIDo',NULL,NULL,NULL,NULL,0,8,'youtube'),(18,'Mozart - Piano Solo',NULL,'x8N7mFboGnc',NULL,NULL,NULL,NULL,0,10,'youtube'),(19,'Meditate with Mozart @ 432Hz Classical Piano | Vol 1',NULL,'A7xYccLlO3g',NULL,NULL,NULL,NULL,0,11,'youtube'),(20,'The Best of Mozart',NULL,'Rb0UmrCXxVA',NULL,NULL,NULL,NULL,0,12,'youtube'),(21,'Beethoven | Piano Sonata No. 8 in C minor \"Pathétique\" | Daniel Barenboim',NULL,'cg9KQ610biU',NULL,NULL,NULL,NULL,0,13,'youtube'),(27,'Jingle Bells - Christmas Song for Children | LooLoo Kids',NULL,'PHGkMMjblL8',NULL,NULL,NULL,NULL,0,30,'youtube'),(28,'Hush Little Baby Lullaby Collection | Songs for Babies to Sleep by HooplaKidz | 66 Min',NULL,'7zSCza_2Eok',NULL,NULL,NULL,NULL,0,31,'youtube'),(29,'BABY SLEEPING CHRISTMAS MUSIC - Christmas Relaxing Music for kids - Jingle Bells, Silent Night',NULL,'mtMjMsKd5ek',NULL,NULL,NULL,NULL,0,32,'youtube'),(30,'Michael Jackson - Billie Jean (Official Video)',NULL,'Zi_XLOBDo_Y',NULL,NULL,NULL,NULL,0,35,'youtube');
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
INSERT INTO `playlists` VALUES (3,'Baby piano',NULL,NULL,2),(4,'Mozard piano',NULL,NULL,2),(5,'Bethoven piano',NULL,NULL,2),(10,'Baby wakeup',NULL,NULL,2),(11,'Baby sleep',NULL,NULL,2),(12,'Michael Jackson',NULL,NULL,2);
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
INSERT INTO `playnodes` VALUES (5,'Hoa Cỏ Vàng Nơi Ấy','album',0,3),(6,NULL,'youtube',1,3),(7,NULL,'youtube',2,3),(8,NULL,'youtube',3,3),(9,'Yêu','album',0,4),(10,NULL,'youtube',1,4),(11,NULL,'youtube',2,4),(12,NULL,'youtube',3,4),(13,NULL,'youtube',0,5),(30,NULL,'youtube',0,10),(31,NULL,'youtube',0,11),(32,NULL,'youtube',1,11),(33,NULL,'youtube',0,12),(34,NULL,'youtube',1,12),(35,NULL,'youtube',2,12);
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
INSERT INTO `projects` VALUES (1,'IOnOff Net','61/40 st.48, Thu Duc, HCMC'),(4,'NK Sông Đà','61/40 st. 48, Thu Duc, HCMC'),(5,'Trung\'s Home','Da Lat, Lam Dong, VN'),(6,'Hien\'s Home','Lâm Thị Hố, Q.12. HCM'),(7,'ĐT Phúc Lộc','Lũy Bán Bích, Tân Phú'),(8,'Mỹ Trinh','Lương Qưới Bến Tre'),(9,'Anh Tuệ','113 Lâm Thị Hố st, Tân Chánh Hiệp, q12'),(10,'Hữu Lệ','113 Lâm Thị Hố st, Tân Chánh Hiệp, quận 12');
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
INSERT INTO `relaygroups` VALUES (1,NULL,4),(2,NULL,4),(4,NULL,4);
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
  `name` varchar(255) COLLATE utf8_unicode_ci DEFAULT NULL,
  `is_leader` bit(1) DEFAULT NULL,
  `relay_id` bigint(20) DEFAULT NULL,
  `group_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `relaygroups_relays_fk_relay_id_idx` (`relay_id`),
  KEY `relaygroups_relays_fk_group_id_idx` (`group_id`),
  CONSTRAINT `relaygroups_relays_fk_group_id` FOREIGN KEY (`group_id`) REFERENCES `relaygroups` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `relaygroups_relays_fk_relay_id` FOREIGN KEY (`relay_id`) REFERENCES `relays` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `relaygroups_relays`
--

LOCK TABLES `relaygroups_relays` WRITE;
/*!40000 ALTER TABLE `relaygroups_relays` DISABLE KEYS */;
INSERT INTO `relaygroups_relays` VALUES (4,NULL,NULL,134,4),(6,NULL,NULL,95,4);
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
  `index_` int(11) DEFAULT NULL,
  `status_` bit(1) DEFAULT NULL,
  `is_locked` bit(1) DEFAULT NULL,
  `auto_revert` int(11) DEFAULT NULL,
  `driver_id` bigint(20) DEFAULT NULL,
  `device_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `relays_fk_device_id_idx` (`device_id`),
  KEY `relays_fk_driver_id_idx` (`driver_id`),
  CONSTRAINT `relays_fk_device_id` FOREIGN KEY (`device_id`) REFERENCES `devices` (`id`) ON DELETE SET NULL ON UPDATE CASCADE,
  CONSTRAINT `relays_fk_driver_id` FOREIGN KEY (`driver_id`) REFERENCES `controllers` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `relays`
--

LOCK TABLES `relays` WRITE;
/*!40000 ALTER TABLE `relays` DISABLE KEYS */;
INSERT INTO `relays` VALUES (81,332733,'Relay 1','Đèn bảng hiệu','2020-07-15 23:00:51',0,_binary '\0',NULL,NULL,8,21),(82,332859,'Relay 2','Đèn ngoài thềm','2020-07-15 18:00:52',1,_binary '',_binary '\0',0,8,23),(83,113,'Relay 3',NULL,'2019-08-04 16:51:07',2,_binary '\0',NULL,NULL,8,NULL),(84,91,'Relay 4',NULL,'2019-07-31 18:25:09',3,_binary '\0',NULL,NULL,8,NULL),(85,262,'Relay 1','Đèn 1','2018-05-02 18:32:40',0,NULL,NULL,NULL,9,27),(86,13,'Relay 2','Đèn 2','2018-04-12 13:03:20',1,NULL,NULL,NULL,9,28),(87,5,'Relay 3','Đèn 3','2018-04-12 13:03:19',2,NULL,NULL,NULL,9,29),(88,3,'Relay 4','Đèn 4','2018-04-12 13:03:17',3,NULL,NULL,NULL,9,30),(89,3,'Relay 5','Đèn 5','2018-04-12 13:03:13',4,NULL,NULL,NULL,9,31),(90,3,'Relay 6','Đèn 6','2018-04-12 13:03:15',5,NULL,NULL,NULL,9,32),(91,5,'Relay 7','Đèn 7','2018-04-12 13:03:08',6,NULL,NULL,NULL,9,33),(92,0,'Relay 8',NULL,NULL,7,NULL,NULL,NULL,9,NULL),(93,172463,'Relay 1','Đèn Led Tường','2019-08-13 17:02:24',0,_binary '\0',_binary '\0',NULL,10,53),(94,172633,'Relay 2','Quạt Treo Tường','2019-08-13 16:27:38',1,_binary '\0',_binary '\0',NULL,10,52),(95,177881,'Relay 3','Đèn Cầu Thang','2019-08-13 13:22:47',2,_binary '\0',_binary '\0',NULL,10,51),(96,127,'Relay 4',NULL,'2018-06-10 22:52:46',3,_binary '\0',NULL,NULL,10,NULL),(109,155686,'Relay 1','Đèn Trước Sân','2018-11-29 16:51:46',0,_binary '',NULL,NULL,14,40),(110,155636,'Relay 2','Đèn Lon','2018-11-29 16:49:56',1,_binary '\0',NULL,NULL,14,41),(111,155759,'Relay 3','Đèn 1m2','2018-11-29 16:51:10',2,_binary '',NULL,NULL,14,42),(112,1,'Relay 4',NULL,'2018-06-10 22:52:42',3,_binary '\0',NULL,NULL,14,NULL),(113,165635,'Relay 1','Đèn ngủ #63','2020-07-04 12:52:01',0,_binary '\0',_binary '\0',NULL,15,63),(114,165845,'Relay 2','Đèn phòng thờ #62','2020-07-15 22:00:51',1,_binary '\0',_binary '\0',NULL,15,62),(115,169162,'Relay 3','Đèn ngủ 2 #64','2020-07-04 12:52:01',2,_binary '\0',_binary '\0',NULL,15,64),(116,19,'Bóng điện','Đèn ngủ 2 #64','2019-02-12 23:15:02',3,_binary '\0',_binary '\0',NULL,15,NULL),(128,4913,'Relay 1','Đèn trần 2 #13','2019-07-16 00:12:12',0,_binary '\0',_binary '\0',0,19,13),(129,473,'Relay 2','Đfn trần 1 #12','2019-07-05 22:37:00',1,_binary '\0',_binary '\0',5,19,NULL),(130,1273,'Relay 3','Đfn trần 1 #12','2019-07-05 21:38:08',2,_binary '\0',_binary '\0',0,19,12),(131,36,'Relay 1','Đèn tường tranh #22','2018-11-17 22:55:38',0,_binary '\0',_binary '\0',0,20,NULL),(132,22,'Relay 2','Đèn tường tranh #22','2018-11-17 08:42:11',1,_binary '\0',_binary '\0',0,20,NULL),(133,19,'Relay 3',NULL,'2018-11-17 08:42:10',2,_binary '\0',_binary '\0',0,20,NULL),(134,542,'Relay 1','Đèn Gần Toilet #54','2019-08-13 13:22:47',0,_binary '\0',_binary '\0',0,21,NULL),(135,332,'Relay 2','Đèn Gần P. R&D #55','2019-06-09 00:22:16',1,_binary '\0',_binary '\0',0,21,55),(136,325,'Relay 3','Đèn Gần Toilet #54','2019-06-09 00:22:15',2,_binary '\0',_binary '\0',0,21,54),(137,245,'Relay 1','Đèn tường giá sách #36','2019-08-02 18:20:53',0,_binary '\0',_binary '\0',0,22,36),(138,57,'Relay 2','Đèn tường giá sách #36','2019-08-02 18:20:46',1,_binary '',_binary '\0',0,22,NULL),(139,601,'Relay 3','Đèn tường tranh #22','2019-08-04 23:15:29',2,_binary '\0',_binary '\0',0,22,22),(162,110,'Relay 1','Bong dien 1 #68','2019-06-08 00:04:31',0,_binary '\0',_binary '\0',0,30,68),(163,126,'Relay 2','Bóng đèn 2 #69','2019-04-07 15:19:56',1,_binary '\0',_binary '\0',0,30,69),(164,114,'Relay 3','Bóng đèn 3 #70','2019-04-07 15:19:56',2,_binary '\0',_binary '\0',0,30,70),(168,1512,'Relay 1','nhà kho  #75','2020-07-08 19:11:51',0,_binary '\0',_binary '\0',0,32,75),(169,1421,'Relay 2','Khách #76','2020-07-08 19:10:53',1,_binary '\0',_binary '\0',0,32,76),(170,974,'Relay 3','Hiên  #77','2020-07-07 22:16:21',2,_binary '\0',_binary '\0',0,32,77),(186,1775,'Relay 1','Đèn ban côn #79','2020-07-15 21:00:52',0,_binary '\0',_binary '\0',0,38,79),(187,9241,'Relay 2','Tưới rau #78','2020-07-16 11:10:52',1,_binary '\0',_binary '\0',0,38,78),(188,863,'Relay 3','Đèn thờ #80','2020-07-14 15:59:58',2,_binary '\0',_binary '\0',0,38,80),(189,2780,'Relay 1','Đèn bếp #65','2020-07-04 14:35:14',0,_binary '',_binary '\0',0,39,65),(190,66,'Relay 2',NULL,'2020-05-26 11:03:59',1,_binary '\0',NULL,0,39,NULL),(191,95,'Relay 3',NULL,'2020-07-04 03:19:33',2,_binary '',NULL,0,39,NULL),(192,283,'Relay 1','Đèn Hiên [L] #60','2020-06-09 19:27:39',0,_binary '\0',_binary '\0',0,40,60),(193,8,'Relay 2',NULL,'2020-03-30 08:46:31',1,_binary '\0',NULL,0,40,NULL),(194,285,'Relay 3','Đèn Hiên[R] #61','2020-06-17 22:00:55',2,_binary '\0',_binary '\0',0,40,61),(195,397,'Relay 1','Đèn Phòng Khách [L] #58','2020-03-19 20:17:38',0,_binary '\0',_binary '\0',0,41,58),(196,81,'Relay 2','Đèn Phòng Khách [L] #58','2020-03-18 03:32:04',1,_binary '\0',_binary '\0',0,41,NULL),(197,60,'Relay 3','Đèn Phòng Khách [L] #58','2020-03-18 05:53:52',2,_binary '',_binary '\0',0,41,NULL),(198,503,'Relay 3','Đèn trần #66','2020-06-10 21:32:02',0,_binary '\0',_binary '\0',0,42,NULL),(199,8041,'Relay 2','Đèn trần #66','2020-06-09 22:16:29',1,_binary '\0',_binary '\0',0,42,66),(200,636,'Relay 1','Đèn phòng khách [R] #59','2020-06-20 19:33:40',2,_binary '\0',_binary '\0',0,42,59),(201,58,'Relay 1','Ora1 #81','2020-02-24 10:22:59',0,_binary '\0',_binary '\0',0,43,81),(202,53,'Relay 2','Ora2 #82','2020-02-24 13:09:02',1,_binary '\0',_binary '\0',0,43,82),(203,51,'Relay 3','Ora3 #83','2020-02-24 13:09:03',2,_binary '\0',_binary '\0',0,43,83);
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
  `order_` int(11) DEFAULT NULL,
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
INSERT INTO `scenes` VALUES (3,'Kịch bản 1',1,NULL,11),(4,'123',NULL,NULL,18);
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
INSERT INTO `scenes_actions` VALUES (8,'SceneRelayAction','Close',NULL,NULL,NULL,162,9,NULL),(9,'SceneRelayAction','None',NULL,NULL,NULL,163,10,NULL),(10,'SceneRelayAction','None',NULL,NULL,NULL,164,11,NULL),(14,'SceneRelayAction','None',NULL,NULL,NULL,186,13,NULL),(15,'SceneRelayAction','None',NULL,NULL,NULL,187,12,NULL),(16,'SceneRelayAction','None',NULL,NULL,NULL,188,14,NULL);
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
INSERT INTO `scenes_devices` VALUES (9,1,300,3,68),(10,NULL,NULL,3,69),(11,NULL,NULL,3,70),(12,NULL,NULL,4,78),(13,NULL,NULL,4,79),(14,NULL,NULL,4,80);
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
  `order_` int(11) DEFAULT NULL,
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
INSERT INTO `schedules` VALUES (1,'Bật đèn bảng hiệu',NULL,'Daily','05:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',21,4),(2,'Tắt đèn bảng hiệu',NULL,'Daily','11:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',21,4),(3,'Bật đèn thềm',NULL,'Daily','06:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',23,4),(4,'Tắt đèn thềm',NULL,'Daily','09:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',23,4),(6,'Tắt đèn bảng hiệu sáng',NULL,'Daily','05:30 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '\0',21,4),(7,'Bật đèn phòng ngủ',NULL,'Daily','08:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '\0',22,4),(8,'Tắt đèn phòng ngủ',NULL,'Daily','01:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '\0',22,4),(9,'Bật đèn phòng khách',NULL,'Daily','06:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',13,4),(10,'Tắt đèn phòng khách',NULL,'Daily','11:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',13,4),(13,'Mở đèn phòng thờ',1,'Daily','06:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',62,6),(14,'Đóng đèn phòng thờ',2,'Daily','10:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',62,6),(15,'Mở 6h am',1,'Daily','06:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(16,'Đóng 6h10am',2,'Daily','06:10 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(17,'Mở 7h am',3,'Daily','07:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(18,'Đóng 7h10am',4,'Daily','07:10 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(19,'Mở 8h am',5,'Daily','08:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(20,'Đóng 8h10am',6,'Daily','08:10 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(21,'Mở 9h am',7,'Daily','09:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(22,'Đóng 9h10am',8,'Daily','09:10 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(23,'Mở 10h am',9,'Daily','10:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(24,'Đóng 10h10am',10,'Daily','10:10 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(25,'Mở 11h am',11,'Daily','11:00 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(26,'Đóng 11h10am',12,'Daily','11:10 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(27,'Mở 12h pm',13,'Daily','12:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(28,'Đóng 12h10pm',14,'Daily','12:10 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(29,'Mở 1h pm',15,'Daily','01:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(30,'Đóng 3h10pm',20,'Daily','03:10 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(31,'Mở 3h pm',19,'Daily','03:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(32,'Mở đèn ban công',1,'Daily','06:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',79,9),(33,'Tắt đèn ban công',2,'Daily','09:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',79,9),(34,'Đóng 2h10pm',18,'Daily','02:10 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(35,'Mở 2h pm',17,'Daily','02:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(36,'Đóng 1h10pm',16,'Daily','01:10 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(37,'Mở 4h pm',21,'Daily','04:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(38,'Đóng 4h10pm',22,'Daily','04:10 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(39,'Mở 5h pm',23,'Daily','05:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(40,'Đóng 5h10pm',24,'Daily','05:10 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(41,'Mở 6h pm',25,'Daily','06:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9),(42,'Đóng 6h10pm',26,'Daily','06:10 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',78,9);
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
INSERT INTO `schedules_actions` VALUES (1,'ScheduleRelayAction','Close',NULL,NULL,NULL,81,1,NULL),(2,'ScheduleRelayAction','Open',NULL,NULL,NULL,81,2,NULL),(3,'ScheduleRelayAction','Close',NULL,NULL,NULL,82,3,NULL),(4,'ScheduleRelayAction','Open',NULL,NULL,NULL,82,4,NULL),(6,'ScheduleRelayAction','Open',NULL,NULL,NULL,81,6,NULL),(17,'ScheduleRelayAction','None',NULL,NULL,NULL,128,9,NULL),(18,'ScheduleRelayAction','None',NULL,NULL,NULL,128,10,NULL),(25,'ScheduleRelayAction','None',NULL,NULL,NULL,139,7,NULL),(26,'ScheduleRelayAction','None',NULL,NULL,NULL,139,8,NULL),(29,'ScheduleRelayAction','Close',NULL,NULL,NULL,114,13,NULL),(30,'ScheduleRelayAction','Open',NULL,NULL,NULL,114,14,NULL),(59,'ScheduleRelayAction','Close',NULL,NULL,NULL,186,32,NULL),(60,'ScheduleRelayAction','Open',NULL,NULL,NULL,186,33,NULL),(61,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,24,NULL),(62,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,26,NULL),(63,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,28,NULL),(64,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,36,NULL),(65,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,34,NULL),(66,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,30,NULL),(67,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,38,NULL),(68,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,40,NULL),(69,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,16,NULL),(70,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,42,NULL),(71,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,18,NULL),(72,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,20,NULL),(73,'ScheduleRelayAction','Open',NULL,NULL,NULL,187,22,NULL),(74,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,23,NULL),(75,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,25,NULL),(76,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,27,NULL),(77,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,29,NULL),(78,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,35,NULL),(79,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,31,NULL),(80,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,37,NULL),(81,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,39,NULL),(82,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,15,NULL),(83,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,41,NULL),(84,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,17,NULL),(85,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,19,NULL),(86,'ScheduleRelayAction','Close',NULL,NULL,NULL,187,21,NULL);
/*!40000 ALTER TABLE `schedules_actions` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `schema_version`
--

DROP TABLE IF EXISTS `schema_version`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `schema_version` (
  `installed_rank` int(11) NOT NULL,
  `version` varchar(50) DEFAULT NULL,
  `description` varchar(200) NOT NULL,
  `type` varchar(20) NOT NULL,
  `script` varchar(1000) NOT NULL,
  `checksum` int(11) DEFAULT NULL,
  `installed_by` varchar(100) NOT NULL,
  `installed_on` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `execution_time` int(11) NOT NULL,
  `success` tinyint(1) NOT NULL,
  PRIMARY KEY (`installed_rank`),
  KEY `schema_version_s_idx` (`success`)
) ENGINE=InnoDB DEFAULT CHARSET=ucs2;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `schema_version`
--

LOCK TABLES `schema_version` WRITE;
/*!40000 ALTER TABLE `schema_version` DISABLE KEYS */;
INSERT INTO `schema_version` VALUES (1,'1','<< Flyway Init >>','BASELINE','<< Flyway Init >>',NULL,'root','2018-05-16 15:21:09',0,1),(2,'2','alter table sensors','SQL','V2__alter_table_sensors.sql',1483932616,'root','2018-05-16 15:21:15',5960,1),(3,'3','alter tabler controllers relays','SQL','V3__alter_tabler_controllers_relays.sql',-1975578754,'root','2018-05-16 15:21:17',2383,1),(4,'4','alter tabler modes ensors','SQL','V4__alter_tabler_modes_ensors.sql',-1348911563,'root','2018-05-16 15:21:18',634,1),(5,'5','alter tbl relays add col is locked','SQL','V5__alter_tbl_relays_add_col_is_locked.sql',1195492646,'root','2018-05-19 06:49:18',2791,1),(6,'6','alter tbl relays add col auto revert','SQL','V6__alter_tbl_relays_add_col_auto_revert.sql',-392734399,'root','2018-05-31 01:48:26',2121,1),(7,'7','delete weigh scale','SQL','V7__delete_weigh_scale.sql',242201596,'root','2018-05-31 01:48:26',42,1),(8,'8','delete tbl versions','SQL','V8__delete_tbl_versions.sql',-1000040228,'root','2018-06-10 15:00:45',36,1),(9,'9','add field order','SQL','V9__add_field_order.sql',1206644942,'root','2018-06-10 15:00:45',313,1),(10,'10','set relay label','SQL','V10__set_relay_label.sql',1732181426,'root','2018-06-12 17:51:42',45,1),(11,'11','alter relaydrivers devices','SQL','V11__alter_relaydrivers_devices.sql',-1596264010,'root','2019-06-05 10:06:34',20,1);
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
  `order_` int(11) DEFAULT NULL,
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
INSERT INTO `sensors` VALUES (1,'*Cảm biến',NULL,'DIGITAL',NULL,NULL,NULL,13,4);
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
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sensors_data`
--

LOCK TABLES `sensors_data` WRITE;
/*!40000 ALTER TABLE `sensors_data` DISABLE KEYS */;
INSERT INTO `sensors_data` VALUES (1,'2018-05-09 07:20:04',1,1,NULL),(2,'2018-05-09 07:20:39',5,1,NULL),(3,'2018-05-09 07:21:28',5.5,2,NULL),(4,'2018-05-13 10:51:41',1.1,1,NULL),(5,'2018-05-13 10:53:10',2,2,NULL),(6,'2018-05-13 10:54:01',1.15,3,NULL),(7,'2018-05-13 10:58:13',1.35,1,NULL),(8,'2018-05-13 22:19:58',2,38,NULL),(9,'2018-05-13 22:20:49',2,38,NULL),(10,'2018-05-13 22:22:44',2,38,NULL),(11,'2018-05-13 22:31:32',2,38,NULL),(12,'2018-05-13 22:34:09',2,38,NULL),(13,'2018-05-13 22:35:59',2,38,NULL),(14,'2018-05-13 22:38:07',2,38,NULL),(15,'2018-05-13 22:42:36',2,38,NULL),(16,'2018-05-13 22:43:28',2,38,NULL),(17,'2018-05-13 23:00:29',1.5,1,NULL),(18,'2018-05-13 23:19:19',2,38,NULL),(19,'2018-05-13 23:20:11',2,38,NULL),(20,'2018-05-13 23:26:31',2,38,NULL),(21,'2018-05-13 23:29:10',2,38,NULL),(22,'2018-05-13 23:33:36',2,38,NULL),(23,'2018-05-13 23:47:56',2,38,NULL),(24,'2018-05-13 23:52:03',2,38,NULL),(25,'2018-05-13 23:57:20',2,38,NULL),(26,'2018-05-13 23:59:36',2,38,NULL),(27,'2018-05-14 00:02:22',2,38,NULL),(28,'2018-05-14 00:04:56',2,38,NULL),(29,'2018-05-14 00:05:49',2,38,NULL),(30,'2018-05-14 00:06:04',2,38,NULL),(31,'2018-05-14 00:09:37',2,38,NULL),(32,'2018-05-14 14:31:58',110.6,1,NULL),(33,'2018-05-14 14:32:14',28.2,2,NULL),(34,'2018-05-14 14:32:38',29.15,3,NULL),(35,'2018-05-14 14:33:04',60.6,4,NULL),(36,'2018-05-14 14:33:22',34.05,5,NULL),(37,'2018-05-14 14:33:42',110.6,6,NULL),(38,'2018-05-14 14:33:58',45.6,7,NULL),(39,'2018-05-14 17:02:11',18.15,1,NULL),(40,'2018-05-14 17:03:11',32,2,NULL),(41,'2018-05-17 15:53:03',550,1,NULL),(42,'2018-05-21 10:50:39',25.9,1,NULL),(43,'2018-05-21 10:51:09',25.4,2,NULL),(44,'2018-05-21 10:55:55',28.3,3,NULL),(45,'2018-05-21 10:56:29',25.5,4,NULL),(46,'2018-05-21 10:56:56',25.45,5,NULL),(47,'2018-05-21 11:15:14',26.7,1,NULL),(48,'2018-05-21 11:16:02',27.9,2,NULL),(49,'2018-05-21 11:18:18',4.4,3,NULL),(50,'2018-05-21 11:26:24',2.4,4,NULL),(51,'2018-05-21 11:27:21',2.4,5,NULL),(52,'2018-05-22 07:18:18',2.9,1,NULL),(53,'2018-05-22 07:18:42',2.55,2,NULL),(54,'2018-05-22 07:18:56',2.4,3,NULL),(55,'2018-05-22 07:19:07',2.5,4,NULL),(56,'2018-05-22 07:19:21',2.45,5,NULL),(57,'2018-05-22 07:19:33',2.3,6,NULL),(58,'2018-05-22 07:19:49',2.45,7,NULL),(59,'2018-05-22 07:19:57',2.45,8,NULL),(60,'2018-05-22 07:20:05',2.4,9,NULL),(61,'2018-05-22 07:20:14',2.8,10,NULL),(62,'2018-05-22 07:20:22',2.45,11,NULL),(63,'2018-05-22 07:20:29',2.45,12,NULL),(64,'2018-05-22 07:20:38',3.7,13,NULL),(65,'2018-05-22 07:20:47',2.4,14,NULL),(66,'2018-05-22 07:20:55',2.55,15,NULL),(67,'2018-05-22 07:21:03',2.45,16,NULL),(68,'2018-05-22 07:21:11',2.4,17,NULL),(69,'2018-05-22 07:21:17',2.5,18,NULL),(70,'2018-05-22 07:21:25',2.5,19,NULL),(71,'2018-05-22 07:23:05',2.4,20,NULL),(72,'2018-05-22 07:23:18',2.5,21,NULL),(73,'2018-05-22 07:23:31',2.6,22,NULL),(74,'2018-05-22 07:24:19',2.6,23,NULL);
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
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sensors_status`
--

LOCK TABLES `sensors_status` WRITE;
/*!40000 ALTER TABLE `sensors_status` DISABLE KEYS */;
INSERT INTO `sensors_status` VALUES (1,'2019-08-04 17:47:51',0,NULL);
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
  `name` varchar(127) COLLATE utf8_unicode_ci DEFAULT NULL,
  `index_` int(11) DEFAULT NULL,
  `time_` datetime DEFAULT NULL,
  `status_` bit(1) DEFAULT NULL,
  `driver_id` bigint(20) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `switchs_fk_driver_id_idx` (`driver_id`),
  CONSTRAINT `switchs_fk_driver_id` FOREIGN KEY (`driver_id`) REFERENCES `controllers` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `switchs`
--

LOCK TABLES `switchs` WRITE;
/*!40000 ALTER TABLE `switchs` DISABLE KEYS */;
INSERT INTO `switchs` VALUES (5,NULL,0,NULL,_binary '\0',14),(6,NULL,1,NULL,_binary '\0',14),(7,NULL,2,NULL,_binary '\0',14),(8,NULL,3,NULL,_binary '\0',14),(13,NULL,0,NULL,_binary '\0',8),(14,NULL,1,NULL,_binary '\0',8),(15,NULL,2,NULL,_binary '\0',8),(16,NULL,3,NULL,_binary '\0',8),(17,NULL,0,NULL,_binary '\0',15),(18,NULL,1,NULL,_binary '\0',15),(19,NULL,2,NULL,_binary '\0',15),(20,NULL,3,NULL,_binary '\0',15),(29,NULL,0,NULL,_binary '\0',10),(30,NULL,1,NULL,_binary '\0',10),(31,NULL,2,NULL,_binary '\0',10),(32,NULL,3,NULL,_binary '\0',10),(36,NULL,0,NULL,_binary '\0',19),(37,NULL,1,NULL,_binary '\0',19),(38,NULL,2,NULL,_binary '\0',19),(39,NULL,0,NULL,_binary '\0',20),(40,NULL,1,NULL,_binary '\0',20),(41,NULL,2,NULL,_binary '\0',20),(42,NULL,0,NULL,_binary '\0',21),(43,NULL,1,NULL,_binary '\0',21),(44,NULL,2,NULL,_binary '\0',21),(45,NULL,0,NULL,_binary '\0',22),(46,NULL,1,NULL,_binary '\0',22),(47,NULL,2,NULL,_binary '\0',22),(70,NULL,0,NULL,_binary '',30),(71,NULL,1,NULL,_binary '',30),(72,NULL,2,NULL,_binary '',30),(76,NULL,0,NULL,_binary '\0',32),(77,NULL,1,NULL,_binary '\0',32),(78,NULL,2,NULL,_binary '\0',32),(94,NULL,0,NULL,_binary '\0',38),(95,NULL,1,NULL,_binary '\0',38),(96,NULL,2,NULL,_binary '\0',38),(97,NULL,0,NULL,_binary '',39),(98,NULL,1,NULL,_binary '\0',39),(99,NULL,2,NULL,_binary '\0',39),(100,NULL,0,NULL,_binary '\0',40),(101,NULL,1,NULL,_binary '\0',40),(102,NULL,2,NULL,_binary '\0',40),(103,NULL,0,NULL,_binary '\0',41),(104,NULL,1,NULL,_binary '\0',41),(105,NULL,2,NULL,_binary '\0',41),(106,NULL,0,NULL,_binary '\0',42),(107,NULL,1,NULL,_binary '\0',42),(108,NULL,2,NULL,_binary '',42),(109,NULL,0,NULL,_binary '\0',43),(110,NULL,1,NULL,_binary '\0',43),(111,NULL,2,NULL,_binary '\0',43);
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
INSERT INTO `users` VALUES (1,'lord','Super Admin','$2a$10$j1XHoXMUqluhBeh.4YUKs.TvXX.UGE8apcpnKikoFFR7xF4gfbwv.','trancongsan@gmail.com',NULL,'0933439994',1),(2,'admin','Home Admin','$2a$10$WS6go13b/ugS66Q40jhNh.XEnEPgnA5d0MytSfne8.tA55my5XSM6','trancongsan@gmail.com','en','0933439994',2),(4,'trung','Đức Trung','$2a$10$trT3.R/Nfey62eczbKEnueTcIbJXW.u1ffAo/XfyLpofwNDbEB86O','dtrung86@gmail.com',NULL,'0987584855',2),(5,'hien','Chất Hiển','$2a$10$trT3.R/Nfey62eczbKEnueTcIbJXW.u1ffAo/XfyLpofwNDbEB86O','hiennguyenchat@gmail.com','vi','0979173179',2),(6,'truong','Điện Tử Phúc Lộc','$2a$10$trT3.R/Nfey62eczbKEnueTcIbJXW.u1ffAo/XfyLpofwNDbEB86O','dientuphucloc@gmail.com',NULL,'0938034377',2),(7,'test','Test','$2a$10$trT3.R/Nfey62eczbKEnueTcIbJXW.u1ffAo/XfyLpofwNDbEB86O','test@gmail.com',NULL,'0909090909',3),(8,'Mytrinh','Họ tên','$2a$10$trT3.R/Nfey62eczbKEnueTcIbJXW.u1ffAo/XfyLpofwNDbEB86O','thtech@ yahoo.com',NULL,'Số ĐT',2),(10,'anhtue','Anh Tuệ','$2a$10$/nPGZ6pE0Q5qgYvIFiOiXe1hYp1iCkCespef.KROqkYSBK/3i9eqa','hiennguyen.it1990@gmail.com',NULL,'0979173179',2),(11,'huule','Ông Lệ','$2a$10$trT3.R/Nfey62eczbKEnueTcIbJXW.u1ffAo/XfyLpofwNDbEB86O','hiennguyen.it1990@gmail.com','en','123456',2);
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
INSERT INTO `users_devices` VALUES (14,_binary '',4,33,5),(15,_binary '',4,27,5),(16,_binary '',4,28,5),(17,_binary '',4,30,5),(18,_binary '',4,29,5),(19,_binary '',4,31,5),(20,_binary '',4,34,5),(21,_binary '',4,32,5),(22,_binary '',2,36,4),(23,_binary '',2,22,4),(24,_binary '',2,20,4),(25,_binary '',2,23,4),(26,_binary '',2,26,4),(27,_binary '',2,12,4),(28,_binary '',2,35,4),(29,_binary '',2,13,4),(30,_binary '',2,21,4),(31,_binary '',2,8,4),(32,_binary '',2,9,4),(33,_binary '',2,6,4),(34,_binary '',2,10,4),(42,_binary '',8,40,8),(43,_binary '',8,41,8),(44,_binary '',8,42,8),(68,_binary '',1,6,4),(69,_binary '',1,8,4),(70,_binary '',1,9,4),(71,_binary '',1,10,4),(72,_binary '',1,20,4),(73,_binary '',1,22,4),(74,_binary '',1,36,4),(75,_binary '',1,12,4),(76,_binary '',1,13,4),(77,_binary '',1,21,4),(78,_binary '',1,23,4),(79,_binary '',1,35,4),(80,_binary '',1,26,4),(83,_binary '',2,51,4),(84,_binary '',1,51,4),(85,_binary '',2,52,4),(86,_binary '',1,52,4),(87,_binary '',2,53,4),(88,_binary '',1,53,4),(89,_binary '',2,54,4),(90,_binary '',1,54,4),(91,_binary '',2,55,4),(92,_binary '',1,55,4),(93,_binary '',2,56,4),(94,_binary '',1,56,4),(107,_binary '\0',7,68,7),(108,_binary '',6,68,7),(109,_binary '\0',7,69,7),(110,_binary '',6,69,7),(111,_binary '\0',7,70,7),(112,_binary '',6,70,7),(131,_binary '',11,75,10),(133,_binary '',11,76,10),(135,_binary '',11,77,10),(139,_binary '',5,62,6),(140,_binary '',5,63,6),(141,_binary '',5,64,6),(142,_binary '',5,58,6),(143,_binary '',5,59,6),(144,_binary '',5,60,6),(145,_binary '',5,61,6),(146,_binary '',5,65,6),(147,_binary '',5,66,6),(148,_binary '',10,78,9),(149,_binary '',10,79,9),(150,_binary '',10,80,9),(151,_binary '',2,40,8),(152,_binary '',2,41,8),(153,_binary '',2,42,8),(154,_binary '',2,81,8),(155,_binary '',8,81,8),(156,_binary '',2,82,8),(157,_binary '',8,82,8),(158,_binary '',2,83,8),(159,_binary '',8,83,8);
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
) ENGINE=InnoDB AUTO_INCREMENT=87 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_projects`
--

LOCK TABLES `users_projects` WRITE;
/*!40000 ALTER TABLE `users_projects` DISABLE KEYS */;
INSERT INTO `users_projects` VALUES (2,_binary '',1,1),(3,_binary '\0',2,1),(4,_binary '',1,4),(5,_binary '',2,4),(8,_binary '\0',4,1),(9,_binary '\0',4,4),(12,_binary '\0',1,5),(13,_binary '\0',2,5),(15,_binary '',4,5),(16,_binary '\0',1,6),(17,_binary '\0',2,6),(18,_binary '\0',4,6),(19,_binary '\0',5,1),(20,_binary '\0',5,4),(21,_binary '\0',5,5),(22,_binary '',5,6),(23,_binary '\0',1,7),(24,_binary '\0',2,7),(25,_binary '\0',4,7),(26,_binary '\0',5,7),(27,_binary '\0',6,1),(28,_binary '\0',6,4),(29,_binary '\0',6,5),(30,_binary '\0',6,6),(31,_binary '',6,7),(32,_binary '\0',7,1),(33,_binary '\0',7,4),(34,_binary '\0',7,5),(35,_binary '\0',7,6),(36,_binary '',7,7),(37,_binary '\0',1,8),(38,_binary '',2,8),(39,_binary '\0',4,8),(40,_binary '\0',5,8),(41,_binary '\0',6,8),(42,_binary '\0',7,8),(43,_binary '\0',8,1),(44,_binary '\0',8,4),(45,_binary '\0',8,5),(46,_binary '\0',8,6),(47,_binary '\0',8,7),(48,_binary '',8,8),(55,_binary '\0',10,1),(56,_binary '\0',10,4),(57,_binary '\0',10,5),(58,_binary '\0',10,6),(59,_binary '\0',10,7),(60,_binary '\0',10,8),(61,_binary '\0',1,9),(62,_binary '\0',2,9),(63,_binary '\0',4,9),(64,_binary '\0',5,9),(65,_binary '\0',6,9),(66,_binary '\0',7,9),(67,_binary '\0',8,9),(69,_binary '',10,9),(70,_binary '\0',11,1),(71,_binary '\0',11,4),(72,_binary '\0',11,5),(73,_binary '\0',11,6),(74,_binary '\0',11,7),(75,_binary '\0',11,8),(76,_binary '\0',11,9),(77,_binary '\0',1,10),(78,_binary '\0',2,10),(79,_binary '\0',4,10),(80,_binary '\0',5,10),(81,_binary '\0',6,10),(82,_binary '\0',7,10),(83,_binary '\0',8,10),(85,_binary '\0',10,10),(86,_binary '',11,10);
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
INSERT INTO `users_scenes` VALUES (4,_binary '\0',7,3,7),(5,_binary '',6,3,7),(6,_binary '',10,4,9);
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
) ENGINE=InnoDB AUTO_INCREMENT=64 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_zones`
--

LOCK TABLES `users_zones` WRITE;
/*!40000 ALTER TABLE `users_zones` DISABLE KEYS */;
INSERT INTO `users_zones` VALUES (19,_binary '',4,8,4),(21,_binary '',2,9,5),(22,_binary '',1,9,5),(27,_binary '',1,6,1),(28,_binary '',4,9,5),(29,_binary '',2,4,4),(30,_binary '',2,7,4),(31,_binary '',2,8,4),(32,_binary '',2,3,4),(34,_binary '',6,11,7),(37,_binary '',6,12,7),(38,_binary '\0',7,11,7),(39,_binary '\0',7,12,7),(40,_binary '',8,13,8),(48,_binary '',1,3,4),(49,_binary '',1,4,4),(50,_binary '',1,7,4),(51,_binary '',1,8,4),(52,_binary '',1,15,4),(53,_binary '',2,15,4),(54,_binary '',1,16,4),(55,_binary '',2,16,4),(59,_binary '',11,17,10),(60,_binary '',5,14,6),(61,_binary '',5,10,6),(62,_binary '',10,18,9),(63,_binary '',2,13,8);
/*!40000 ALTER TABLE `users_zones` ENABLE KEYS */;
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
INSERT INTO `zones` VALUES (3,'Phòng R&D',23,4,10),(4,'Phòng Bếp',12,4,10),(6,'*Vùng',NULL,1,12),(7,'Phòng ngủ',22,4,11),(8,'Phòng Khách',11,4,10),(9,'Phòng Khách',0,5,13),(10,'Tầng trệt',1,6,14),(11,'Trạm Tân Phú',1,7,15),(12,'Go Vap',2,7,15),(13,'Đèn',NULL,8,16),(14,'Lầu',2,6,14),(15,'Hành Lang',21,4,11),(16,'Bên Ngoài',10,4,10),(17,'Nhà ',1,10,17),(18,'Sân thượng',1,9,18);
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

-- Dump completed on 2020-07-16 11:36:00
