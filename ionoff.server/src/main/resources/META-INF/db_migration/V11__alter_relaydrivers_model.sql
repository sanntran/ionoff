UPDATE `relaydrivers` SET `model`='P8RelayDriver' WHERE `model`='IONOFF_P8';
UPDATE `relaydrivers` SET `model`='P4RelayDriver' WHERE `model`='IONOFF_P4';
UPDATE `relaydrivers` SET `model`='E4RelayDriver' WHERE `model`='IONOFF_E4';
UPDATE `relaydrivers` SET `model`='E4RelayDriver' WHERE `model`='IONOFF_E3';
UPDATE `relaydrivers` SET `model`='Ep2RelayDriver' WHERE `model`='HLAB_EP2';
UPDATE `relaydrivers` SET `model`='Ec100RelayDriver' WHERE `model`='HBQ_EC100';

ALTER TABLE `relaydrivers`
CHANGE COLUMN `model` `type_` VARCHAR(255) CHARACTER SET 'utf8' NULL DEFAULT NULL AFTER `name`;
