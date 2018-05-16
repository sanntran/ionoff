CREATE TABLE `switchs` (
  `id` BIGINT NOT NULL,
  `name` VARCHAR(127) NULL,
  `index_` INT NULL,
  `time_` DATETIME NULL,
  `status_` BIT NULL,
  `driver_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `switchs_fk_driver_id_idx` (`driver_id` ASC),
  CONSTRAINT `switchs_fk_driver_id`
    FOREIGN KEY (`driver_id`)
    REFERENCES `controllers` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);

DELETE FROM `sensors` WHERE `id`>='1';

ALTER TABLE `sensors`
DROP FOREIGN KEY `sensors_fk_controller_id`;

ALTER TABLE `sensors`
DROP COLUMN `controller_input`,
DROP COLUMN `status_`,
DROP COLUMN `controller_id`,
ADD COLUMN `type_` VARCHAR(63) NULL AFTER `name`,
ADD COLUMN `unit` VARCHAR(63) NULL AFTER `type_`,
ADD COLUMN `device_id` BIGINT NULL AFTER `unit`,
ADD COLUMN `zone_id` BIGINT NULL AFTER `device_id`,
ADD COLUMN  `switch_id` BIGINT(20) NULL DEFAULT NULL AFTER `zone_id`,
ADD INDEX `sensors_fk_device_id_idx` (`device_id` ASC),
ADD INDEX `sensors_fk_zone_id_idx` (`zone_id` ASC),
ADD INDEX `sensors_fk_switch_id_idx` (`switch_id` ASC),
DROP INDEX `sensors_fk_controller_id_idx` ;
ALTER TABLE `sensors`
ADD CONSTRAINT `sensors_fk_device_id`
  FOREIGN KEY (`device_id`)
  REFERENCES `devices` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
ADD CONSTRAINT `sensors_fk_zone_id`
  FOREIGN KEY (`zone_id`)
  REFERENCES `zones` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
ADD CONSTRAINT `sensors_fk_switch_id`
  FOREIGN KEY (`switch_id`)
  REFERENCES `switchs` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;

  CREATE TABLE `sensors_data` (
  `id` BIGINT NOT NULL,
  `time_` DATETIME NULL,
  `value_` DOUBLE NULL,
  `sensor_id` BIGINT NULL,
  PRIMARY KEY (`id`),
  INDEX `sensors_data_fk_sensor_id_idx` (`sensor_id` ASC),
  CONSTRAINT `sensors_data_fk_sensor_id`
    FOREIGN KEY (`sensor_id`)
    REFERENCES `sensors` (`id`)
    ON DELETE SET NULL
    ON UPDATE CASCADE);

CREATE TABLE `sensors_status` (
  `sensor_id` BIGINT NOT NULL,
  `time_` DATETIME NULL,
  `value_` DOUBLE NULL,
  PRIMARY KEY (`sensor_id`),
  CONSTRAINT `sensors_status_fk_sensor_id`
    FOREIGN KEY (`sensor_id`)
    REFERENCES `sensors` (`id`)
    ON DELETE CASCADE
    ON UPDATE CASCADE);

DELETE FROM `modes_sensors` WHERE `id`>='1';

ALTER TABLE `modes_sensors`
ADD COLUMN `condition_` VARCHAR(63) NULL AFTER `enabled_`;

ALTER TABLE `modes_sensors_users`
DROP COLUMN `detected_`;

ALTER TABLE `modes_sensors_scenes`
DROP COLUMN `detected_`;

ALTER TABLE `sensors_status`
ADD COLUMN `index_` BIGINT NULL AFTER `value_`;

ALTER TABLE `sensors_data`
ADD COLUMN `index_` BIGINT NULL AFTER `value_`;
