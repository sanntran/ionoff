ALTER TABLE `relaydrivers`
RENAME TO  `controllers` ;

UPDATE `controllers` SET `model`='P8Controller' WHERE `model`='IONOFF_P8';
UPDATE `controllers` SET `model`='P4Controller' WHERE `model`='IONOFF_P4';
UPDATE `controllers` SET `model`='E4Controller' WHERE `model`='IONOFF_E4';
UPDATE `controllers` SET `model`='E3Controller' WHERE `model`='IONOFF_E3';
UPDATE `controllers` SET `model`='Ep2Controller' WHERE `model`='HLAB_EP2';
UPDATE `controllers` SET `model`='Ec100Controller' WHERE `model`='HBQ_EC100';

ALTER TABLE `controllers`
CHANGE COLUMN `model` `type_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL AFTER `name`;

ALTER TABLE `controllers`
ADD INDEX `controllers_key__idx` (`key_` ASC);

update `controllers` set `key_` = CONCAT('EP', 200000 + `id`) WHERE `type_` = 'Ep2Controller';
update `controllers` set `key_` = CONCAT('EC', 100000 + `id`) WHERE `type_` = 'Ec100Controller';

update `users` set `password_` = '$2a$10$trT3.R/Nfey62eczbKEnueTcIbJXW.u1ffAo/XfyLpofwNDbEB86O' where `id` > 0;

update `devices` set `type_` = 'RelayLoad' where `type_` = 'Device';
update `devices` set `type_` = 'RelayLoad' where `type_` = 'Light';
update `devices` set `type_` = 'RelayLoad' where `type_` = 'Appliance';
update `devices` set `type_` = 'MediaPlayer' where `type_` = 'Player';
