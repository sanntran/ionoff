ALTER TABLE `relays` 
ADD COLUMN `auto_revert` INT NULL AFTER `is_locked`;

UPDATE `relays` SET `auto_revert` = 1 WHERE `type_` = 'Button';

ALTER TABLE `relays` 
DROP COLUMN `type_`;