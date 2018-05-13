ALTER TABLE .`modes_sensors` 
DROP COLUMN `value_`;
ALTER TABLE `modes_sensors` 
CHANGE COLUMN `condition` `condition_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL ;
ALTER TABLE `modes_sensors` 
ADD COLUMN `message` VARCHAR(511) NULL AFTER `sensor_id`;
