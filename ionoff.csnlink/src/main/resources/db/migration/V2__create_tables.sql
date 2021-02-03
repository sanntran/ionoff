CREATE TABLE IF NOT EXISTS `csnlink`.`links` (
  `id` BIGINT NOT NULL AUTO_INCREMENT,
  `link` VARCHAR(512) NOT NULL,
  `status` VARCHAR(31) NOT NULL DEFAULT 'PENDING',
  PRIMARY KEY (`id`),
  UNIQUE INDEX `val_UNIQUE` (`link` ASC));
