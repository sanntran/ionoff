ALTER TABLE .`modes_sensors` 
DROP COLUMN `value_`;
ALTER TABLE `modes_sensors` 
CHANGE COLUMN `condition` `condition_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL ;
