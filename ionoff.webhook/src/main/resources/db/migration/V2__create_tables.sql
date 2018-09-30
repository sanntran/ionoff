CREATE TABLE `centers` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `ip` VARCHAR(127) NULL,
  `key_` VARCHAR(255) NULL,
  `time_` DATETIME NULL,
  PRIMARY KEY (`id`),
  INDEX `centers_key_idx` (`key_` ASC));
