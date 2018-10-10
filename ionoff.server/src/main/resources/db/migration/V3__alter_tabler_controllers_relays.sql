ALTER TABLE `controllers` 
RENAME TO  `relaydrivers` ;
		
ALTER TABLE `relays` 
DROP FOREIGN KEY `relays_fk_controller_id`;
ALTER TABLE `relays` 
CHANGE COLUMN `controller_id` `driver_id` BIGINT(20) NULL DEFAULT NULL ,
DROP INDEX `relays_fk_controller_id_idx` ,
ADD INDEX `relays_fk_driver_id_idx` (`driver_id` ASC);
ALTER TABLE `relays` 
ADD CONSTRAINT `relays_fk_driver_id`
  FOREIGN KEY (`driver_id`)
  REFERENCES `relaydrivers` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
