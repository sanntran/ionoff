mysqladmin -u root -p"ubuntu$db" password "ionoff$db"

CREATE SCHEMA ionoff DEFAULT CHARACTER SET utf8 COLLATE utf8_unicode_ci ;

-- Import database
-- Update database

ALTER TABLE `players` 
DROP COLUMN `accept_request`,
DROP COLUMN `license_key`,
CHANGE COLUMN `port` `port` INT(11) NULL DEFAULT NULL AFTER `ip`;

CREATE TABLE `controllers` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(255) NULL,
  `ip` VARCHAR(255) NULL,
  `port` INT NULL,
  `model` VARCHAR(255) NULL,
  `home_id` BIGINT NULL,
  PRIMARY KEY (`id`));
  
  ALTER TABLE `controllers` 
ADD INDEX `controllers_fk_home_id_idx` (`home_id` ASC);
ALTER TABLE `controllers` 
ADD CONSTRAINT `controllers_fk_home_id`
  FOREIGN KEY (`home_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

  
  CREATE TABLE `relays` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `name` VARCHAR(255) NULL,
  `type` VARCHAR(255) NULL,
  `index_` INT NULL,
  `controller_id` BIGINT NULL,
  `device_id` BIGINT NULL,
  PRIMARY KEY (`id`));
  
  ALTER TABLE `relays` 
ADD INDEX `relays_fk_controller_id_idx` (`controller_id` ASC),
ADD INDEX `relays_fk_device_id_idx` (`device_id` ASC);
ALTER TABLE `relays` 
ADD CONSTRAINT `relays_fk_controller_id`
  FOREIGN KEY (`controller_id`)
  REFERENCES `controllers` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
ADD CONSTRAINT `relays_fk_device_id`
  FOREIGN KEY (`device_id`)
  REFERENCES `devices` (`id`)
  ON DELETE SET NULL
  ON UPDATE CASCADE;

CREATE  TABLE `sensors` (
  `id` BIGINT NOT NULL ,
  `name` VARCHAR(255) NULL ,
  `controller_id` BIGINT NULL ,
  `controller_input` INT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `sensors_fk_controller_id_idx` (`controller_id` ASC) ,
  CONSTRAINT `sensors_fk_controller_id`
    FOREIGN KEY (`controller_id` )
    REFERENCES `controllers` (`id` )
    ON DELETE SET NULL
    ON UPDATE CASCADE);
    
    ALTER TABLE `sensors` ADD COLUMN `home_id` BIGINT NULL  AFTER `name` , 
  ADD CONSTRAINT `sensors_fk_home_id`
  FOREIGN KEY (`home_id` )
  REFERENCES `homes` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE
, ADD INDEX `sensors_fk_home_id_idx` (`home_id` ASC) ;

ALTER TABLE `sensors` CHANGE COLUMN `id` `id` BIGINT(20) NOT NULL AUTO_INCREMENT  ;

ALTER TABLE `users` DROP COLUMN `address` ;
ALTER TABLE `users` DROP COLUMN `language` ;

ALTER TABLE `users_devices` DROP FOREIGN KEY `users_devices_fk_device_id` , DROP FOREIGN KEY `users_devices_fk_user_id` ;
ALTER TABLE `users_devices` CHANGE COLUMN `user_id` `user_id` BIGINT(20) NULL  , CHANGE COLUMN `device_id` `device_id` BIGINT(20) NULL  , 
  ADD CONSTRAINT `users_devices_fk_device_id`
  FOREIGN KEY (`device_id` )
  REFERENCES `devices` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE, 
  ADD CONSTRAINT `users_devices_fk_user_id`
  FOREIGN KEY (`user_id` )
  REFERENCES `users` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE
, DROP PRIMARY KEY ;


ALTER TABLE `users_devices` ADD COLUMN `id` BIGINT NOT NULL AUTO_INCREMENT  AFTER `device_id` 
, ADD PRIMARY KEY (`id`) ;

ALTER TABLE `users_devices` CHANGE COLUMN `id` `id` BIGINT(20) NOT NULL AUTO_INCREMENT  FIRST ;

ALTER TABLE `users_devices` ADD COLUMN `role` BIT NULL  AFTER `device_id` ;

ALTER TABLE `users_devices` CHANGE COLUMN `role` `role` BIT(1) NULL DEFAULT NULL  AFTER `id` ;

CREATE  TABLE `modes` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(255) NULL ,
  `is_active` BIT NULL ,
  `schedule_time` VARCHAR(255) NULL ,
  `schedule_day` VARCHAR(255) NULL ,
  `home_id` BIGINT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `modes_fk_home_id_idx` (`home_id` ASC) ,
  CONSTRAINT `modes_fk_home_id`
    FOREIGN KEY (`home_id` )
    REFERENCES `homes` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE);

    
    CREATE  TABLE `scenes` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `name` VARCHAR(255) NULL ,
  `area_id` BIGINT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `scenes_fk_area_id_idx` (`area_id` ASC) ,
  CONSTRAINT `scenes_fk_area_id`
    FOREIGN KEY (`area_id` )
    REFERENCES `areas` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE);

    
    CREATE  TABLE `modes_scenes` (
  `mode_id` BIGINT NOT NULL ,
  `scene_id` BIGINT NOT NULL ,
  PRIMARY KEY (`mode_id`, `scene_id`) ,
  INDEX `modes_scenes_fk_mode_id_idx` (`mode_id` ASC) ,
  INDEX `modes_scenes_fk_scene_id_idx` (`scene_id` ASC) ,
  CONSTRAINT `modes_scenes_fk_mode_id`
    FOREIGN KEY (`mode_id` )
    REFERENCES `modes` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `modes_scenes_fk_scene_id`
    FOREIGN KEY (`scene_id` )
    REFERENCES `scenes` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE);
    
    CREATE  TABLE `sceneactions` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `action` VARCHAR(255) NULL ,
  `scene_id` BIGINT NULL ,
  `device_id` BIGINT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `sceneactions_fk_scene_id_idx` (`scene_id` ASC) ,
  INDEX `sceneactions_fk_device_id_idx` (`device_id` ASC) ,
  CONSTRAINT `sceneactions_fk_scene_id`
    FOREIGN KEY (`scene_id` )
    REFERENCES `scenes` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `sceneactions_fk_device_id`
    FOREIGN KEY (`device_id` )
    REFERENCES `devices` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE);


ALTER TABLE `modes` 
CHANGE COLUMN `is_active` `is_scheduled` BIT(1) NULL DEFAULT NULL ,
ADD COLUMN `schedule_repeat` VARCHAR(255) NULL AFTER `is_scheduled`;

ALTER TABLE `sceneactions` 
DROP COLUMN `action`, RENAME TO  `scenes_devides` ;

CREATE TABLE `scenes_actions` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `action` VARCHAR(255) NULL,
  `relay_id` BIGINT NULL,
  `scenedevice_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `scenes_actions_fk_relay_id_idx` (`relay_id` ASC),
  INDEX `scenes_actions_fk_scenedevice_id_idx` (`scenedevice_id` ASC),
  CONSTRAINT `scenes_actions_fk_relay_id`
    FOREIGN KEY (`relay_id`)
    REFERENCES `relays` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `scenes_actions_fk_scenedevice_id`
    FOREIGN KEY (`scenedevice_id`)
    REFERENCES `scenes_devides` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
    
    
    CREATE TABLE `schedules` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `repeat` VARCHAR(255) NULL,
  `time_` VARCHAR(255) NULL,
  `day_` VARCHAR(255) NULL,
  `is_enable` BIT NULL,
  `device_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `schedules_fk_device_id_idx` (`device_id` ASC),
  CONSTRAINT `schedules_fk_device_id`
    FOREIGN KEY (`device_id`)
    REFERENCES `devices` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
ALTER TABLE `schedules` 
ADD COLUMN `name` VARCHAR(255) NULL AFTER `id`;


CREATE TABLE `schedules_actions` (
  `id` INT NOT NULL,
  `action` VARCHAR(255) NULL,
  `relay_id` BIGINT NULL,
  `schedule_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `schedules_actions_fk_relay_id_idx` (`relay_id` ASC),
  INDEX `schedules_actions_fk_schedule_id_idx` (`schedule_id` ASC),
  CONSTRAINT `schedules_actions_fk_relay_id`
    FOREIGN KEY (`relay_id`)
    REFERENCES `relays` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `schedules_actions_fk_schedule_id`
    FOREIGN KEY (`schedule_id`)
    REFERENCES `schedules` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);

ALTER TABLE `schedules_actions` 
CHANGE COLUMN `id` `id` BIGINT NOT NULL AUTO_INCREMENT ;

ALTER TABLE `schedules` CHANGE COLUMN `is_enable` `enabled` BIT(1) NULL DEFAULT NULL  ;

ALTER TABLE `schedules` ADD COLUMN `home_id` BIGINT NULL  AFTER `device_id` , 
  ADD CONSTRAINT `schedules_fk_home_id`
  FOREIGN KEY (`home_id` )
  REFERENCES `homes` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE
, ADD INDEX `schedules_fk_home_id_idx` (`home_id` ASC) ;

ALTER TABLE `schedules` 
CHANGE COLUMN `repeat` `repeat_` VARCHAR(255) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NULL DEFAULT NULL ,
CHANGE COLUMN `enabled` `enabled_` BIT(1) NULL DEFAULT NULL ;

ALTER TABLE `schedules_actions` ADD COLUMN `album` VARCHAR(255) NULL  AFTER `action` , ADD COLUMN `volume` VARCHAR(255) NULL  AFTER `album` ;

ALTER TABLE `scenes_actions` ADD COLUMN `album` VARCHAR(255) NULL  AFTER `action` , ADD COLUMN `volume` VARCHAR(255) NULL  AFTER `album` ;

ALTER TABLE `scenes_actions` ADD COLUMN `type` VARCHAR(255) NULL  AFTER `id` ;

ALTER TABLE `schedules_actions` ADD COLUMN `type` VARCHAR(255) NULL  AFTER `id` ;

ALTER TABLE `schedules_actions` CHANGE COLUMN `volume` `volume` VARCHAR(255) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NULL DEFAULT NULL  AFTER `action` , ADD COLUMN `album_type` VARCHAR(25) NULL  AFTER `album` ;

ALTER TABLE `scenes_actions` CHANGE COLUMN `volume` `volume` VARCHAR(255) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NULL DEFAULT NULL  AFTER `action` , ADD COLUMN `album_type` VARCHAR(255) NULL  AFTER `album` ;

ALTER TABLE `modes_scenes` CHANGE COLUMN `mode_id` `mode_id` BIGINT(20) NULL  , CHANGE COLUMN `scene_id` `scene_id` BIGINT(20) NULL  , ADD COLUMN `id` BIGINT NOT NULL  FIRST 
, DROP PRIMARY KEY 
, ADD PRIMARY KEY (`id`) ;

ALTER TABLE `modes_scenes` ADD COLUMN `area_id` BIGINT NOT NULL  AFTER `mode_id` 
, ADD INDEX `modes_scenes_fk_area_id_idx` (`area_id` ASC) ;

ALTER TABLE `modes_scenes` 
DROP FOREIGN KEY `modes_scenes_fk_scene_id`,
DROP FOREIGN KEY `modes_scenes_fk_mode_id`;

ALTER TABLE `modes_scenes` 
  ADD CONSTRAINT `modes_scenes_fk_mode_id`
  FOREIGN KEY (`mode_id` )
  REFERENCES `modes` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE, 
  ADD CONSTRAINT `modes_scenes_fk_area_id`
  FOREIGN KEY (`area_id` )
  REFERENCES `areas` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE, 
  ADD CONSTRAINT `modes_scenes_fk_scene_id`
  FOREIGN KEY (`scene_id` )
  REFERENCES `scenes` (`id` )
  ON DELETE SET NULL
  ON UPDATE CASCADE;

  ALTER TABLE `modes_scenes` CHANGE COLUMN `id` `id` BIGINT(20) NOT NULL AUTO_INCREMENT  ;

  ALTER TABLE `modes_scenes` 
DROP FOREIGN KEY `modes_scenes_fk_area_id`;
ALTER TABLE `modes_scenes` 
CHANGE COLUMN `area_id` `area_id` BIGINT(20) NULL ;
ALTER TABLE `modes_scenes` 
ADD CONSTRAINT `modes_scenes_fk_area_id`
  FOREIGN KEY (`area_id`)
  REFERENCES `areas` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  CREATE  TABLE `modes_sensors` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `enabled` BIT NULL ,
  `detected` BIT NULL ,
  `interval` INT NULL ,
  `time_point` BIGINT NULL ,
  `mode_id` BIGINT NULL ,
  `sensor_id` BIGINT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `modes_sensors_fk_mode_id_idx` (`mode_id` ASC) ,
  INDEX `modes_sensors_fk_sensor_id_idx` (`sensor_id` ASC) ,
  CONSTRAINT `modes_sensors_fk_mode_id`
    FOREIGN KEY (`mode_id` )
    REFERENCES `modes` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_fk_sensor_id`
    FOREIGN KEY (`sensor_id` )
    REFERENCES `sensors` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE);

    
    CREATE  TABLE `modes_sensors_scenes` (
  `id` BIGINT NOT NULL AUTO_INCREMENT ,
  `detected` BIT NULL ,
  `modesensor_id` BIGINT NULL ,
  `area_id` BIGINT NULL ,
  `scene_id` BIGINT NULL ,
  PRIMARY KEY (`id`) ,
  INDEX `modes_sensors_scenes_fk_modesensor_id_idx` (`modesensor_id` ASC) ,
  INDEX `modes_sensors_scenes_fk_area_id_idx` (`area_id` ASC) ,
  INDEX `modes_sensors_scenes_fk_scene_id_idx` (`scene_id` ASC) ,
  CONSTRAINT `modes_sensors_scenes_fk_modesensor_id`
    FOREIGN KEY (`modesensor_id` )
    REFERENCES `modes_sensors` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_scenes_fk_area_id`
    FOREIGN KEY (`area_id` )
    REFERENCES `areas` (`id` )
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_scenes_fk_scene_id`
    FOREIGN KEY (`scene_id` )
    REFERENCES `scenes` (`id` )
    ON DELETE SET NULL
    ON UPDATE CASCADE);
    
    ALTER TABLE `modes_sensors` 
CHANGE COLUMN `enabled` `enabled_` BIT(1) NULL DEFAULT NULL ,
CHANGE COLUMN `detected` `detected_` BIT(1) NULL DEFAULT NULL ,
CHANGE COLUMN `interval` `interval_` INT(11) NULL DEFAULT NULL ;

ALTER TABLE `modes_sensors_scenes` 
CHANGE COLUMN `detected` `detected_` BIT(1) NULL DEFAULT NULL ;

ALTER TABLE `devices` DROP FOREIGN KEY `devices_fk_subarea_id` ;
ALTER TABLE `devices` 
  ADD CONSTRAINT `devices_fk_subarea_id`
  FOREIGN KEY (`subarea_id` )
  REFERENCES `subareas` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  ALTER TABLE `subareas` DROP FOREIGN KEY `subareas_fk_area_id` ;
ALTER TABLE `subareas` 
  ADD CONSTRAINT `subareas_fk_area_id`
  FOREIGN KEY (`area_id` )
  REFERENCES `areas` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  ALTER TABLE `schedules` ADD COLUMN `status_` BIT NULL  AFTER `day_` , 
  ADD COLUMN `retry_` INT NULL  AFTER `status_` ;

ALTER TABLE `relays` ADD COLUMN `status_` BIT NULL  AFTER `device_id` ;

ALTER TABLE `schedules_actions` ADD COLUMN `player_id` BIGINT NULL  AFTER `schedule_id` , 
  ADD CONSTRAINT `schedules_actions_fk_player_id`
  FOREIGN KEY (`player_id` )
  REFERENCES `devices` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE
, ADD INDEX `schedules_actions_fk_player_id_idx` (`player_id` ASC) ;

ALTER TABLE `scenes_actions` ADD COLUMN `player_id` BIGINT NULL  AFTER `scenedevice_id` , 
  ADD CONSTRAINT `scenes_actions_fk_player_id`
  FOREIGN KEY (`player_id` )
  REFERENCES `devices` (`id` )
  ON DELETE CASCADE
  ON UPDATE CASCADE
, ADD INDEX `scenes_actions_fk_player_id_idx` (`player_id` ASC) ;

ALTER TABLE `modes` ADD COLUMN `is_activated` BIT NULL  AFTER `name` ;

ALTER TABLE `modes_sensors` CHANGE COLUMN `time_point` `reset_time` BIGINT(20) NULL DEFAULT NULL  ;

ALTER TABLE `sensors` ADD COLUMN `status_` BIT NULL  AFTER `name` ;

ALTER TABLE `controllers` ADD COLUMN `status_` BIT NULL  AFTER `model` ;

ALTER TABLE `modes_sensors` CHANGE COLUMN `interval_` `time_buffer` INT(11) NULL DEFAULT NULL  ;

ALTER TABLE `modes_sensors` DROP COLUMN `detected_` ;

ALTER TABLE `devices` ADD COLUMN `status_` BIT NULL  AFTER `mac` ;

ALTER TABLE `devices` DROP COLUMN `status_` ;

ALTER TABLE `devices` 
ADD COLUMN `status_` INT NULL AFTER `mac`;

ALTER TABLE `devices` 
CHANGE COLUMN `status_` `status_` BIT NULL ,
ADD COLUMN `vlc_status` BIT NULL AFTER `status_`,
ADD COLUMN `xxk_status` BIT NULL AFTER `vlc_status`;

ALTER TABLE `devices` 
DROP COLUMN `status_`,
ADD COLUMN `time_` BIGINT NULL AFTER `type`;

ALTER TABLE `devices` 
ADD COLUMN `version_` BIGINT NULL AFTER `name`;

ALTER TABLE `relays` CHANGE COLUMN `status_` `status_` BIT(1) NULL DEFAULT NULL  AFTER `index_` , ADD COLUMN `version_` BIGINT NULL  AFTER `status_` ;
ALTER TABLE `relays` CHANGE COLUMN `version_` `version_` BIGINT(20) NULL DEFAULT 0  AFTER `id` ;
ALTER TABLE `devices` CHANGE COLUMN `version_` `version_` BIGINT(20) NULL DEFAULT 0  AFTER `id` ;

ALTER TABLE `devices` 
DROP COLUMN `xxk_status`,
DROP COLUMN `vlc_status`;

ALTER TABLE `scenes_devides` 
ADD CONSTRAINT `sceneactions_fk_scene_id`
  FOREIGN KEY (`scene_id`)
  REFERENCES `scenes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
  
ADD CONSTRAINT `sceneactions_fk_device_id`
  FOREIGN KEY (`device_id`)
  REFERENCES `devices` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

ALTER TABLE `modes` 
CHANGE COLUMN `is_activated` `activated_time` BIGINT NULL DEFAULT NULL ;

ALTER TABLE `controllers` 
CHANGE COLUMN `status_` `conntected` BIT(1) NULL DEFAULT NULL ;

ALTER TABLE `controllers` 
CHANGE COLUMN `conntected` `connected` BIT(1) NULL DEFAULT NULL ;


CREATE TABLE `users_scenes` (
  `id` BIGINT NOT NULL,
  `role` BIT NULL,
  `user_id` BIGINT NULL,
  `scene_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `users_scenes_user_id_fk_idx` (`user_id` ASC),
  INDEX `users_scenes_scene_id_fk_idx` (`scene_id` ASC),
  CONSTRAINT `users_scenes_user_id_fk`
    FOREIGN KEY (`user_id`)
    REFERENCES `users` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `users_scenes_scene_id_fk`
    FOREIGN KEY (`scene_id`)
    REFERENCES `scenes` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE)
ENGINE = InnoDB;

ALTER TABLE `users_scenes` 
CHANGE COLUMN `id` `id` BIGINT(20) NOT NULL AUTO_INCREMENT ;

CREATE TABLE `modes_sensors_users` (
  `id` bigint(20) NOT NULL AUTO_INCREMENT,
  `detected_` bit(1) DEFAULT NULL,
  `modesensor_id` bigint(20) DEFAULT NULL,
  `user_id` bigint(20) DEFAULT NULL,
  `send_sms` bit(1) DEFAULT NULL,
  `send_email` bit(1) DEFAULT NULL,
  PRIMARY KEY (`id`),
  KEY `modes_sensors_users_fk_modesensor_id_idx` (`modesensor_id`),
  KEY `modes_sensors_users_fk_user_id_idx` (`user_id`),
  CONSTRAINT `modes_sensors_users_fk_modesensor_id` FOREIGN KEY (`modesensor_id`) REFERENCES `modes_sensors` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `modes_sensors_users_fk_user_id` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE SET NULL ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;


ALTER TABLE `devices` 
DROP COLUMN `port`,
DROP COLUMN `ip`;


ALTER TABLE `sessions` 
DROP FOREIGN KEY `sessions_fk_user_id`;
ALTER TABLE `sessions` 
DROP COLUMN `user_id`,
DROP COLUMN `session_id`,
DROP COLUMN `language`,
CHANGE COLUMN `expired` `name` VARCHAR(63) NULL DEFAULT NULL ,
CHANGE COLUMN `extend` `dateTime` VARCHAR(63) NULL DEFAULT NULL ,
DROP INDEX `sessions_fk_user_id_idx` ;

ALTER TABLE `sessions` 
CHARACTER SET = utf8 , COLLATE = utf8_unicode_ci ;

ALTER TABLE `sessions` 
RENAME TO  `versions` ;

ALTER TABLE `controllers` 
ADD COLUMN `password_` CHAR(15) NULL AFTER `port`;

ALTER TABLE `controllers` 
CHANGE COLUMN `password_` `password_` VARCHAR(31) CHARACTER SET 'utf8' NULL DEFAULT NULL ;

ALTER TABLE `devices` 
ADD COLUMN `order_` INT NULL AFTER `time_`;

 TABLE `controllers` 
CHANGE COLUMN `connected` `connected_time` BIGINT(20) NULL DEFAULT NULL ;

ALTER TABLE `devices` 
CHANGE COLUMN `status_` `connected_time` BIGINT(20) NULL DEFAULT NULL ;

ALTER TABLE `modes` 
ADD COLUMN `is_activated` BIT(1) NULL DEFAULT NULL AFTER `schedule_day`;

ALTER TABLE `controllers` 
CHANGE COLUMN `password_` `key_` VARCHAR(31) CHARACTER SET 'utf8' NULL DEFAULT NULL ;

-- HSQL compatible

ALTER TABLE `devices` 
CHANGE COLUMN `type` `type_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL ;

ALTER TABLE `relays` 
CHANGE COLUMN `type` `type_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL ;

ALTER TABLE `scenes_actions` 
CHANGE COLUMN `type` `type_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL ,
CHANGE COLUMN `action` `action_` VARCHAR(255) CHARACTER SET 'utf8' NULL ;

ALTER TABLE `schedules_actions` 
CHANGE COLUMN `type` `type_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL ,
CHANGE COLUMN `action` `action_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL ;

ALTER TABLE `versions` 
CHANGE COLUMN `id` `id` BIGINT(20) NOT NULL ,
CHANGE COLUMN `dateTime` `date_time` VARCHAR(63) CHARACTER SET 'latin1' NULL ;

ALTER TABLE `versions` 
CHANGE COLUMN `name` `name` VARCHAR(63) NULL ;

ALTER TABLE `versions` 
CHANGE COLUMN `date_time` `date_time` VARCHAR(63) NULL DEFAULT NULL ;

ALTER TABLE `devices` 
DROP COLUMN `order`;


ALTER TABLE `devices` 
ADD COLUMN `ip` VARCHAR(127) NULL AFTER `order_`,
ADD COLUMN `port` INT NULL AFTER `ip`,
ADD COLUMN `model` VARCHAR(127) NULL AFTER `mac`;

ALTER TABLE `scenes_devides` 
RENAME TO  `scenes_devices` ;

ALTER TABLE `areas` 
ADD COLUMN `order_` INT NULL DEFAULT 0 AFTER `name`;

ALTER TABLE `subareas` 
ADD COLUMN `order_` INT NULL DEFAULT 0 AFTER `name`;


CREATE TABLE `playlist` (
  `id` BIGINT(20) NOT NULL,
  `name` VARCHAR(255) NULL,
  `image_url` VARCHAR(511) NULL,
  `user_id` BIGINT(20) NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_user_id_idx` (`user_id` ASC),
  CONSTRAINT `fk_user_id`
    FOREIGN KEY (`user_id`)
    REFERENCES `users` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
    
 
CREATE TABLE `playitem` (
  `id` BIGINT(20) NOT NULL,
  `name` VARCHAR(255) NULL,
  `url` VARCHAR(1023) NULL,
  `type` VARCHAR(31) NULL,
  `playlist_id` BIGINT(20) NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_playitem_playlist_id_idx` (`playlist_id` ASC),
  CONSTRAINT `fk_playitem_playlist_id`
    FOREIGN KEY (`playlist_id`)
    REFERENCES `playlist` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
    

ALTER TABLE `playlist` 
DROP FOREIGN KEY `fk_user_id`;
ALTER TABLE `playlist` 
DROP INDEX `fk_user_id_idx` ,
ADD INDEX `fk_playlist_user_id_idx` (`user_id` ASC);
ALTER TABLE `playlist` 
ADD CONSTRAINT `fk_playlist_user_id`
  FOREIGN KEY (`user_id`)
  REFERENCES `users` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

ALTER TABLE `playlist` 
ADD COLUMN `scope` VARCHAR(31) NULL AFTER `name`;



CREATE TABLE `homes_playlists` (
  `id` BIGINT NOT NULL,
  `home_id` BIGINT NULL,
  `playlist_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `fk_homes_playlists_home_id_idx` (`home_id` ASC),
  INDEX `fk_homes_playlists_playlist_id_idx` (`playlist_id` ASC),
  CONSTRAINT `fk_homes_playlists_home_id`
    FOREIGN KEY (`home_id`)
    REFERENCES `homes` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `fk_homes_playlists_playlist_id`
    FOREIGN KEY (`playlist_id`)
    REFERENCES `playlist` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
    
    
ALTER TABLE `playitem` 
RENAME TO  `playitems` ;

ALTER TABLE `playitems` 
CHANGE COLUMN `type` `type_` VARCHAR(31) CHARACTER SET 'utf8' NULL DEFAULT NULL ;


ALTER TABLE `playlist` 
RENAME TO  `playlists` ;

ALTER TABLE `playitems` 
ADD COLUMN `idx` INT NULL AFTER `type_`;


ALTER TABLE `playlists` 
CHANGE COLUMN `scope` `is_public` BIT NULL DEFAULT NULL ;

DROP TABLE `homes_playlists`;

ALTER TABLE `playitems` 
DROP COLUMN `url`, RENAME TO  `playnodes` ;

CREATE TABLE `playleafs` (
  `id` BIGINT NOT NULL,
  `name` VARCHAR(255) NULL,
  `duration` VARCHAR(31) NULL,
  `uri` VARCHAR(1023) NULL,
  `authors` VARCHAR(255) NULL,
  `artists` VARCHAR(255) NULL,
  `image` VARCHAR(255) NULL,
  `idx` INT NULL,
  `playnode_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `playleafs_playnode_id_fk_idx` (`playnode_id` ASC),
  CONSTRAINT `playleafs_playnode_id_fk`
    FOREIGN KEY (`playnode_id`)
    REFERENCES `playnodes` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
    
ALTER TABLE `playleafs` 
DROP FOREIGN KEY `playleafs_playnode_id_fk`;
ALTER TABLE `playleafs` 
ADD CONSTRAINT `playleafs_fk_playnode_id`
  FOREIGN KEY (`playnode_id`)
  REFERENCES `playnodes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
ALTER TABLE `playleafs` 
DROP INDEX `playleafs_playnode_id_fk_idx` ,
ADD INDEX `playleafs_fk_playnode_id_idx` (`playnode_id` ASC);

ALTER TABLE `playlists` 
CHANGE COLUMN `image_url` `thumbnail` VARCHAR(511) NULL DEFAULT NULL AFTER `name`;

ALTER TABLE `playleafs` 
ADD COLUMN `type_` VARCHAR(31) NULL AFTER `playnode_id`;

ALTER TABLE `playleafs` 
CHANGE COLUMN `uri` `url` VARCHAR(1023) NULL DEFAULT NULL ;

ALTER TABLE `playleafs` 
ADD COLUMN `mrl` VARCHAR(1023) NULL AFTER `url`;

CREATE TABLE `tokens` (
  `id` BIGINT NOT NULL,
  `name` VARCHAR(255) NULL,
  `value` VARCHAR(511) NULL,
  `expiry` DATETIME NULL,
  `user_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `tokens_user_id_fk_idx` (`user_id` ASC),
  CONSTRAINT `tokens_user_id_fk`
    FOREIGN KEY (`user_id`)
    REFERENCES `users` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
    
UPDATE `groups` SET `id`='1', `name`='SystemAdmin' WHERE `id`='3';
UPDATE `groups` SET `id`='3', `name`='ProjectUser' WHERE `id`='2';
INSERT INTO `groups` (`id`, `name`) VALUES ('2', 'ProjectAdmin');

ALTER TABLE `relays` 
ADD COLUMN `feature` VARCHAR(255) NULL AFTER `name`;

ALTER TABLE `relays` 
CHANGE COLUMN `feature` `label` VARCHAR(255) CHARACTER SET 'utf8' COLLATE 'utf8_unicode_ci' NULL DEFAULT NULL ;

ALTER TABLE `relays` 
ADD COLUMN `time_` DATETIME NULL AFTER `label`;

ALTER TABLE `devices` 
CHANGE COLUMN `time_` `time_` DATETIME NULL DEFAULT NULL ;

ALTER TABLE `devices` 
DROP COLUMN `connected_time`;


ALTER TABLE `subareas` 
RENAME TO  `zones` ;

ALTER TABLE `devices` 
DROP FOREIGN KEY `devices_fk_subarea_id`;
ALTER TABLE `devices` 
CHANGE COLUMN `subarea_id` `zone_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `devices_fk_subarea_id_idx` ,
ADD INDEX `devices_fk_zone_id_idx` (`zone_id` ASC);
ALTER TABLE `devices` 
ADD CONSTRAINT `devices_fk_zone_id`
  FOREIGN KEY (`zone_id`)
  REFERENCES `zones` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

  
  ALTER TABLE `zones` 
DROP FOREIGN KEY `subareas_fk_area_id`,
DROP FOREIGN KEY `subareas_fk_home_id`;
ALTER TABLE `zones` 
ADD CONSTRAINT `zones_fk_area_id`
  FOREIGN KEY (`area_id`)
  REFERENCES `areas` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
ADD CONSTRAINT `zones_fk_home_id`
  FOREIGN KEY (`home_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

  
  ALTER TABLE `zones` 
DROP INDEX `subareas_fk_area_id_idx` ,
ADD INDEX `zones_fk_area_id_idx` (`area_id` ASC),
DROP INDEX `subareas_fk_home_id_idx` ,
ADD INDEX `zones_fk_home_id_idx` (`home_id` ASC);


ALTER TABLE `areas` 
DROP FOREIGN KEY `areas_fk_home_id`;
ALTER TABLE `areas` 
CHANGE COLUMN `home_id` `project_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `areas_fk_home_id_idx` ,
ADD INDEX `areas_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `areas` 
ADD CONSTRAINT `areas_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

ALTER TABLE `controllers` 
DROP FOREIGN KEY `controllers_fk_home_id`;
ALTER TABLE `controllers` 
CHANGE COLUMN `home_id` `project_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `controllers_fk_home_id_idx` ,
ADD INDEX `controllers_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `controllers` 
ADD CONSTRAINT `controllers_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  ALTER TABLE `devices` 
DROP FOREIGN KEY `devices_fk_home_id`;
ALTER TABLE `devices` 
CHANGE COLUMN `home_id` `project_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `devices_fk_home_id_idx` ,
ADD INDEX `devices_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `devices` 
ADD CONSTRAINT `devices_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

ALTER TABLE `modes` 
DROP FOREIGN KEY `modes_fk_home_id`;
ALTER TABLE `modes` 
CHANGE COLUMN `home_id` `project_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `modes_fk_home_id_idx` ,
ADD INDEX `modes_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `modes` 
ADD CONSTRAINT `modes_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

  
  ALTER TABLE `schedules` 
DROP FOREIGN KEY `schedules_fk_home_id`;
ALTER TABLE `schedules` 
CHANGE COLUMN `home_id` `project_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `schedules_fk_home_id_idx` ,
ADD INDEX `schedules_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `schedules` 
ADD CONSTRAINT `schedules_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

  
  ALTER TABLE `sensors` 
DROP FOREIGN KEY `sensors_fk_home_id`;
ALTER TABLE `sensors` 
CHANGE COLUMN `home_id` `project_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `sensors_fk_home_id_idx` ,
ADD INDEX `sensors_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `sensors` 
ADD CONSTRAINT `sensors_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  ALTER TABLE `users` 
DROP FOREIGN KEY `user_fk_home_id`;
ALTER TABLE `users` 
CHANGE COLUMN `home_id` `project_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `user_fk_home_id_idx` ,
ADD INDEX `user_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `users` 
ADD CONSTRAINT `user_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

  
  ALTER TABLE `zones` 
DROP FOREIGN KEY `zones_fk_home_id`;
ALTER TABLE `zones` 
CHANGE COLUMN `home_id` `project_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `zones_fk_home_id_idx` ,
ADD INDEX `zones_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `zones` 
ADD CONSTRAINT `zones_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `homes` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  ALTER TABLE `homes` 
RENAME TO  `projects` ;


CREATE TABLE `users_zones` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `role` BIT NULL,
  `user_id` BIGINT NULL,
  `zone_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `users_zones_user_id_fk_idx` (`user_id` ASC),
  INDEX `users_zones_zone_id_fk_idx` (`zone_id` ASC),
  CONSTRAINT `users_zones_user_id_fk`
    FOREIGN KEY (`user_id`)
    REFERENCES `users` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `users_zones_zone_id_fk`
    FOREIGN KEY (`zone_id`)
    REFERENCES `zones` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);

    
    CREATE TABLE `users_projects` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `role` BIT NULL,
  `user_id` BIGINT NULL,
  `project_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `users_projects_user_id_fk_idx` (`user_id` ASC),
  INDEX `users_projects_project_id_fk_idx` (`project_id` ASC),
  CONSTRAINT `users_projects_user_id_fk`
    FOREIGN KEY (`user_id`)
    REFERENCES `users` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `users_projects_project_id_fk`
    FOREIGN KEY (`project_id`)
    REFERENCES `projects` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);

DELETE FROM `scenes` WHERE `id`>'0';

ALTER TABLE `scenes` 
DROP FOREIGN KEY `scenes_fk_area_id`;
ALTER TABLE `scenes` 
CHANGE COLUMN `area_id` `zone_id` BIGINT(20) NULL DEFAULT NULL ,
ADD INDEX `scenes_fk_zone_id_idx` (`zone_id` ASC),
DROP INDEX `scenes_fk_area_id_idx` ;
ALTER TABLE `scenes` 
ADD CONSTRAINT `scenes_fk_zone_id`
  FOREIGN KEY (`zone_id`)
  REFERENCES `zones` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

DELETE FROM `modes_scenes` WHERE `id`>'0';

DELETE FROM `modes_sensors_scenes` WHERE `id`>'0';

ALTER TABLE `modes_sensors_scenes` 
DROP FOREIGN KEY `modes_sensors_scenes_fk_area_id`;
ALTER TABLE `modes_sensors_scenes` 
CHANGE COLUMN `area_id` `zone_id` BIGINT(20) NULL DEFAULT NULL ,
ADD INDEX `modes_sensors_scenes_fk_zone_id_idx` (`zone_id` ASC),
DROP INDEX `modes_sensors_scenes_fk_area_id_idx` ;
ALTER TABLE `modes_sensors_scenes` 
ADD CONSTRAINT `modes_sensors_scenes_fk_zone_id`
  FOREIGN KEY (`zone_id`)
  REFERENCES `zones` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  ALTER TABLE `modes_scenes` 
DROP FOREIGN KEY `modes_scenes_fk_area_id`;
ALTER TABLE `modes_scenes` 
CHANGE COLUMN `area_id` `zone_id` BIGINT(20) NOT NULL ,
ADD INDEX `modes_scenes_fk_zone_id_idx` (`zone_id` ASC),
DROP INDEX `modes_scenes_fk_area_id_idx` ;
ALTER TABLE `modes_scenes` 
ADD CONSTRAINT `modes_scenes_fk_zone_id`
  FOREIGN KEY (`zone_id`)
  REFERENCES `zones` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

DROP TABLE `users_scenes`;

DROP TABLE `users_devices`;

ALTER TABLE `users_zones` 
ADD COLUMN `project_id` BIGINT(20) NULL AFTER `zone_id`,
ADD INDEX `users_zones_project_id_fk_idx` (`project_id` ASC);
ALTER TABLE `users_zones` 
ADD CONSTRAINT `users_zones_project_id_fk`
  FOREIGN KEY (`project_id`)
  REFERENCES `projects` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

INSERT INTO users_projects (role, user_id, project_id)
	SELECT true as role, user_.id as user_id, project.id as project_id
        FROM `users` as user_, projects AS project;
        
        
INSERT INTO users_zones (role, user_id, zone_id, project_id)
	SELECT true as role, user_.id as user_id, zone_.id as zone_id, zone_.project_id as project_id
        FROM `users` as user_, zones AS zone_, users_projects AS user_project
        WHERE zone_.project_id = user_project.project_id
          AND user_project.role=true
          AND user_.id = user_project.user_id;
          
ALTER TABLE `users` 
ADD COLUMN `language` VARCHAR(63) NULL AFTER `email`;

ALTER TABLE `users` 
DROP FOREIGN KEY `user_fk_project_id`;
ALTER TABLE `users` 
DROP COLUMN `project_id`,
DROP INDEX `user_fk_project_id_idx` ;

ALTER TABLE `scenes` 
ADD COLUMN `time_` DATETIME NULL AFTER `name`;


ALTER TABLE `modes` 
DROP COLUMN `activated_time`;

ALTER TABLE `modes` 
ADD COLUMN `time_` DATETIME NULL AFTER `name`;

UPDATE `users` SET `password_`='dff00dce8c09cb2b1b1d9540130288f44c3e0d9f36e8663ec33ad43ba6882d58149df782d93c39ae' WHERE `id`>'0';

ALTER TABLE `devices` 
CHANGE COLUMN `time_` `time_` DATETIME NULL DEFAULT NULL ;


ALTER TABLE `controllers` 
ADD COLUMN `crash_count` INT NULL AFTER `connected_time`;


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
  CONSTRAINT `users_scenes_scene_id_fk` FOREIGN KEY (`scene_id`) REFERENCES `scenes` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_scenes_project_id_fk` FOREIGN KEY (`project_id`) REFERENCES `projects` (`id`) ON DELETE CASCADE ON UPDATE CASCADE,
  CONSTRAINT `users_scenes_user_id_fk` FOREIGN KEY (`user_id`) REFERENCES `users` (`id`) ON DELETE CASCADE ON UPDATE CASCADE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;

CREATE TABLE `dashboards` (
  `id` BIGINT NOT NULL,
  `user_id` BIGINT NULL,
  `zone_id` BIGINT NULL,
  `project_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `dashboards_user_id_fk_idx` (`user_id` ASC),
  INDEX `dashboards_zone_id_fk_idx` (`zone_id` ASC),
  INDEX `dashboards_project_id_fk_idx` (`project_id` ASC),
  CONSTRAINT `dashboards_user_id_fk`
    FOREIGN KEY (`user_id`)
    REFERENCES `users` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `dashboards_zone_id_fk`
    FOREIGN KEY (`zone_id`)
    REFERENCES `zones` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `dashboards_project_id_fk`
    FOREIGN KEY (`project_id`)
    REFERENCES `projects` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);
    
    CREATE TABLE `dashboards_devices` (
  `id` BIGINT NOT NULL,
  `dashboard_id` BIGINT NULL,
  `device_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `dashboards_devices_dashboard_id_fk_idx` (`dashboard_id` ASC),
  INDEX `dashboards_devices_device_id_fk_idx` (`device_id` ASC),
  CONSTRAINT `dashboards_devices_dashboard_id_fk`
    FOREIGN KEY (`dashboard_id`)
    REFERENCES `dashboards` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `dashboards_devices_device_id_fk`
    FOREIGN KEY (`device_id`)
    REFERENCES `devices` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);

CREATE TABLE `dashboards_scenes` (
  `id` BIGINT NOT NULL,
  `dashboard_id` BIGINT NULL,
  `scene_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `dashboards_scenes_dashboard_id_fk_idx` (`dashboard_id` ASC),
  INDEX `dashboards_scenes_scene_id_fk_idx` (`scene_id` ASC),
  CONSTRAINT `dashboards_scenes_dashboard_id_fk`
    FOREIGN KEY (`dashboard_id`)
    REFERENCES `dashboards` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE,
  CONSTRAINT `dashboards_scenes_scene_id_fk`
    FOREIGN KEY (`scene_id`)
    REFERENCES `scenes` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);

ALTER TABLE `modes_sensors_users` 
ADD COLUMN `project_id` BIGINT NULL AFTER `send_email`,
ADD INDEX `modes_sensors_users_fk_project_id_idx` (`project_id` ASC);
ALTER TABLE `modes_sensors_users` 
ADD CONSTRAINT `modes_sensors_users_fk_project_id`
  FOREIGN KEY (`project_id`)
  REFERENCES `projects` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
DELETE FROM sensors where id > 0;
  
DELETE FROM modes_sensors_users where id > 0;



--
ALTER TABLE `groups` 
RENAME TO  `usergroups` ;

CREATE TABLE `relaygroups` (
  `id` BIGINT NOT NULL,
  `name` VARCHAR(255) NULL,
  `project_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `relaygroups_fk_project_id_idx` (`project_id` ASC),
  CONSTRAINT `relaygroups_fk_project_id`
    FOREIGN KEY (`project_id`)
    REFERENCES `projects` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);

    ALTER TABLE `relays` 
ADD COLUMN `group_id` BIGINT NULL DEFAULT NULL AFTER `device_id`,
ADD INDEX `relays_fk_group_id_idx` (`group_id` ASC);
ALTER TABLE `relays` 
ADD CONSTRAINT `relays_fk_group_id`
  FOREIGN KEY (`group_id`)
  REFERENCES `relaygroups` (`id`)
  ON DELETE SET NULL
  ON UPDATE CASCADE;

--
  ALTER TABLE `scenes_devices` 
ADD COLUMN `order_` INT NULL AFTER `id`,
ADD COLUMN `duration` INT NULL AFTER `order_`;


ALTER TABLE `relays` 
ADD COLUMN `is_leader` BIT NULL AFTER `status_`;
