ALTER TABLE `devices`
ADD COLUMN `status_` BIT NULL AFTER `name`;

ALTER TABLE `modes`
ADD COLUMN `order_` INT NULL AFTER `name`;

ALTER TABLE `scenes`
ADD COLUMN `order_` INT NULL AFTER `name`;

ALTER TABLE `sensors`
ADD COLUMN `order_` INT NULL AFTER `name`;

ALTER TABLE `schedules`
ADD COLUMN `order_` INT NULL AFTER `name`;
