-- MySQL dump 10.13  Distrib 5.7.23, for Linux (x86_64)
--
-- Host: localhost    Database: ionoff
-- ------------------------------------------------------
-- Server version	5.7.23-0ubuntu0.16.04.1

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
INSERT INTO `areas` VALUES (10,'Tầng trệt',0,4),(11,'Tầng lầu',1,4),(12,'Tầng trệt',0,1);
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
INSERT INTO `dashboards` VALUES (6,1,NULL,1),(7,1,6,1),(12,2,NULL,4),(13,2,4,4),(14,2,7,4),(15,2,8,4),(16,2,3,4),(40,1,NULL,4),(41,1,3,4),(42,1,4,4),(43,1,7,4),(44,1,8,4),(45,1,15,4),(46,2,15,4),(47,1,16,4),(48,2,16,4);
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
INSERT INTO `devices` VALUES (13,140894,'Đèn trần',_binary '\0','Light','2018-08-26 21:16:56',101,NULL,NULL,NULL,NULL,4,8),(20,200318,'Music player',_binary '\0','Player','2018-04-02 22:51:01',200,'1.52.33.58',NULL,'b8aeed7980c4','IMP',4,7),(21,209561,'Đèn bảng hiệu',_binary '','Light','2018-08-26 21:17:34',102,NULL,NULL,NULL,NULL,4,8),(23,209576,'Đèn ngoài thềm',_binary '','Light','2018-08-26 21:17:34',103,NULL,NULL,NULL,NULL,4,8),(35,133065,'Quạt tường',_binary '\0','Appliance','2018-08-26 21:16:56',104,NULL,NULL,NULL,NULL,4,8),(36,201343,'Đèn tường',_binary '','Light','2018-08-26 21:17:39',202,NULL,NULL,NULL,NULL,4,7),(53,126810,'Đèn tường',_binary '\0','Light','2018-08-26 21:17:25',3,NULL,NULL,NULL,NULL,4,4);
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
INSERT INTO `modes` VALUES (1,'Ngày mới',NULL,'2018-08-26 06:30:43',_binary '','Daily','06:30 AM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',4),(2,'Đi ngủ',NULL,'2017-12-11 23:19:09',_binary '\0',NULL,NULL,NULL,_binary '\0',4);
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
INSERT INTO `modes_scenes` VALUES (1,1,3,NULL),(2,1,4,NULL),(3,1,7,NULL),(4,2,3,NULL),(5,2,4,NULL),(6,2,7,NULL),(7,2,8,NULL),(8,1,8,NULL),(11,1,15,NULL),(12,2,15,NULL),(13,1,16,NULL),(14,2,16,NULL);
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
INSERT INTO `projects` VALUES (1,'Lord\'s Home','IOnOff Technology'),(4,'Admin\'s Home','IOnOff Technology');
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
INSERT INTO `relaydrivers` VALUES (12,'E411180603E1E2E3',NULL,NULL,'E411180603E1E2E3','IONOFF_E4',1535293035257,NULL,4),(18,'E312180601170312',NULL,NULL,'E312180601170312','IONOFF_E3',1535293015792,NULL,4);
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
INSERT INTO `relaygroups` VALUES (1,NULL,4),(2,NULL,4);
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
  CONSTRAINT `relays_fk_driver_id` FOREIGN KEY (`driver_id`) REFERENCES `relaydrivers` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `relays`
--

LOCK TABLES `relays` WRITE;
/*!40000 ALTER TABLE `relays` DISABLE KEYS */;
INSERT INTO `relays` VALUES (101,116076,'Relay 1','Đèn Gần P. R&D','2018-08-26 21:17:15',0,_binary '\0',_binary '\0',NULL,12,NULL),(102,115941,'Relay 2','Đèn Gần Toilet','2018-08-26 21:17:15',1,_binary '',_binary '\0',NULL,12,NULL),(103,2746,'Relay 3',NULL,'2018-08-25 22:08:11',2,_binary '\0',_binary '\0',NULL,12,NULL),(104,1,'Relay 4',NULL,'2018-06-10 23:32:31',3,_binary '\0',NULL,NULL,12,NULL),(125,140687,'Relay 1','Đfn trần 1 #12','2018-08-26 21:16:56',0,_binary '\0',_binary '\0',0,18,NULL),(126,140647,'Relay 2','Đèn trần 2','2018-08-26 21:16:56',1,_binary '\0',_binary '\0',0,18,13),(127,132936,'Relay 3','Quạt treo tường #35','2018-08-26 21:16:56',2,_binary '\0',_binary '\0',1,18,35);
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
INSERT INTO `scenes` VALUES (2,'Nhạc',NULL,'2018-03-21 01:35:53',3);
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
INSERT INTO `schedules` VALUES (1,'Bật đèn bảng hiệu',NULL,'Daily','06:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',21,4),(2,'Tắt đèn bảng hiệu',NULL,'Daily','11:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',21,4),(3,'Bật đèn thềm',NULL,'Daily','06:00 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',23,4),(4,'Tắt đèn thềm',NULL,'Daily','09:30 PM','Sun, Mon, Tue, Wed, Thu, Fri, Sat',_binary '',0,_binary '',23,4);
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
) ENGINE=InnoDB DEFAULT CHARSET=ucs2;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `schema_version`
--

LOCK TABLES `schema_version` WRITE;
/*!40000 ALTER TABLE `schema_version` DISABLE KEYS */;
INSERT INTO `schema_version` VALUES (1,1,'1','<< Flyway Init >>','INIT','<< Flyway Init >>',NULL,'root','2018-05-16 15:21:09',0,1),(10,10,'10','set relay label','SQL','V10__set_relay_label.sql',-691159078,'root','2018-06-12 17:51:42',45,1),(2,2,'2','alter table sensors','SQL','V2__alter_table_sensors.sql',-650510190,'root','2018-05-16 15:21:15',5960,1),(3,3,'3','alter tabler controllers relays','SQL','V3__alter_tabler_controllers_relays.sql',1469277880,'root','2018-05-16 15:21:17',2383,1),(4,4,'4','alter tabler modes ensors','SQL','V4__alter_tabler_modes_ensors.sql',1346754495,'root','2018-05-16 15:21:18',634,1),(5,5,'5','alter tbl relays add col is locked','SQL','V5__alter_tbl_relays_add_col_is_locked.sql',429648576,'root','2018-05-19 06:49:18',2791,1),(6,6,'6','alter tbl relays add col auto revert','SQL','V6__alter_tbl_relays_add_col_auto_revert.sql',1899611256,'root','2018-05-31 01:48:26',2121,1),(7,7,'7','delete weigh scale','SQL','V7__delete_weigh_scale.sql',242201596,'root','2018-05-31 01:48:26',42,1),(8,8,'8','delete tbl versions','SQL','V8__delete_tbl_versions.sql',-1000040228,'root','2018-06-10 15:00:45',36,1),(9,9,'9','add field order','SQL','V9__add_field_order.sql',-398955826,'root','2018-06-10 15:00:45',313,1);
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
  CONSTRAINT `switchs_fk_driver_id` FOREIGN KEY (`driver_id`) REFERENCES `relaydrivers` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `switchs`
--

LOCK TABLES `switchs` WRITE;
/*!40000 ALTER TABLE `switchs` DISABLE KEYS */;
INSERT INTO `switchs` VALUES (33,NULL,0,NULL,_binary '\0',12),(34,NULL,1,NULL,_binary '\0',12),(35,NULL,2,NULL,_binary '\0',12),(36,NULL,3,NULL,_binary '\0',12),(37,NULL,0,NULL,_binary '\0',18),(38,NULL,1,NULL,_binary '\0',18),(39,NULL,2,NULL,_binary '\0',18);
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
INSERT INTO `users` VALUES (1,'lord','Super Admin','9fa6033770d560bf216251a9c9fb3db884311de98f1bfdd7add0628ae444825a0301ac666143d1de','trancongsan@gmail.com',NULL,'0933439994',1),(2,'admin','Home Admin','a19880ed5f109d2ee1c7fb7908ba82e3977bbff60c0806dafceb85298691614566946be5e587f87c','trancongsan@gmail.com',NULL,'0933439994',2);
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
INSERT INTO `users_devices` VALUES (22,_binary '',2,36,4),(24,_binary '',2,20,4),(25,_binary '',2,23,4),(28,_binary '',2,35,4),(29,_binary '',2,13,4),(30,_binary '',2,21,4),(72,_binary '',1,20,4),(74,_binary '',1,36,4),(76,_binary '',1,13,4),(77,_binary '',1,21,4),(78,_binary '',1,23,4),(79,_binary '',1,35,4),(87,_binary '',2,53,4),(88,_binary '',1,53,4);
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
) ENGINE=InnoDB AUTO_INCREMENT=49 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_projects`
--

LOCK TABLES `users_projects` WRITE;
/*!40000 ALTER TABLE `users_projects` DISABLE KEYS */;
INSERT INTO `users_projects` VALUES (2,_binary '',1,1),(3,_binary '\0',2,1),(4,_binary '',1,4),(5,_binary '',2,4);
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
INSERT INTO `users_scenes` VALUES (2,_binary '',2,2,4),(3,_binary '',1,2,4);
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
) ENGINE=InnoDB AUTO_INCREMENT=56 DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `users_zones`
--

LOCK TABLES `users_zones` WRITE;
/*!40000 ALTER TABLE `users_zones` DISABLE KEYS */;
INSERT INTO `users_zones` VALUES (27,_binary '',1,6,1),(29,_binary '',2,4,4),(30,_binary '',2,7,4),(31,_binary '',2,8,4),(32,_binary '',2,3,4),(48,_binary '',1,3,4),(49,_binary '',1,4,4),(50,_binary '',1,7,4),(51,_binary '',1,8,4),(52,_binary '',1,15,4),(53,_binary '',2,15,4),(54,_binary '',1,16,4),(55,_binary '',2,16,4);
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
INSERT INTO `zones` VALUES (3,'Phòng R&D',23,4,10),(4,'Phòng Bếp',12,4,10),(6,'*Vùng',NULL,1,12),(7,'Phòng Ngủ',22,4,11),(8,'Phòng Khách',11,4,10),(15,'Hành Lang',21,4,11),(16,'Bên Ngoài',10,4,10);
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

-- Dump completed on 2018-08-26 21:53:38
