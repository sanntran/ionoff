ALTER TABLE `relaydrivers`
ADD INDEX `controllers_key__idx` (`key_` ASC);

update `relaydrivers` set `key_` = CONCAT('EP', 200000 + `id`) WHERE `type_` = 'Ep2RelayDriver';
update `relaydrivers` set `key_` = CONCAT('EC', 100000 + `id`) WHERE `type_` = 'Ec100RelayDriver';