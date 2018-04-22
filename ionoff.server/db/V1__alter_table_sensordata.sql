ALTER TABLE `sensors_status`
DROP COLUMN `setup`,
DROP COLUMN `total_`;

ALTER TABLE `sensors_data`
DROP COLUMN `setup`,
DROP COLUMN `total_`;

ALTER TABLE `sensors_status`
ADD COLUMN `index_` BIGINT NULL AFTER `value_`;

ALTER TABLE `sensors_data`
ADD COLUMN `index_` BIGINT NULL AFTER `value_`;
