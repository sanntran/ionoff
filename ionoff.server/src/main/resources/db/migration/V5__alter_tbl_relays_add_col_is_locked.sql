ALTER TABLE `relays` 
ADD COLUMN `is_locked` BIT NULL AFTER `is_leader`;

CREATE TABLE `relaygroups_relays` (
  `id` BIGINT NOT NULL,
  `name` VARCHAR(255) NULL,
  `relay_id` BIGINT NULL,
  `group_id` BIGINT NULL,
  PRIMARY KEY (`id`));

ALTER TABLE `relaygroups_relays` 
ADD INDEX `relaygroups_relays_fk_relay_id_idx` (`relay_id` ASC),
ADD INDEX `relaygroups_relays_fk_group_id_idx` (`group_id` ASC);
ALTER TABLE `relaygroups_relays` 
ADD CONSTRAINT `relaygroups_relays_fk_relay_id`
  FOREIGN KEY (`relay_id`)
  REFERENCES `relays` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE,
ADD CONSTRAINT `relaygroups_relays_fk_group_id`
  FOREIGN KEY (`group_id`)
  REFERENCES `relaygroups` (`id`)
  ON DELETE CASCADE
  ON UPDATE CASCADE;
  
  ALTER TABLE `relaygroups_relays` 
ADD COLUMN `is_leader` BIT NULL AFTER `name`;

  
 INSERT INTO `relaygroups_relays` (`id`, `name`, `is_leader`, `relay_id`, `group_id`)  
SELECT `id`, null, `is_leader` , `id`, `group_id`
  FROM `relays`
 WHERE `group_id` is not null;
  
ALTER TABLE `relays` 
DROP FOREIGN KEY `relays_fk_group_id`;
ALTER TABLE `relays` 
DROP COLUMN `group_id`,
DROP COLUMN `is_leader`,
DROP INDEX `relays_fk_group_id_idx` ;
